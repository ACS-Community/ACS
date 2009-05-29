/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2006
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package com.cosylab.logging.engine.cache;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;
import java.util.Vector;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

/**
 * The queue of entries.
 * <P>
 * This class has been introduced to avoid keeping in memory a
 * never ending queue of {@link CacheEntry} and reduce the
 * chance to face an out of memory at run-time.
 * <P>
 * <code>CacheEntriesQueue</code> own a {@link LinkedBlockingQueue} to
 * keep in memory only a defined subset of items.
 * All exceeding items are flushed on disk. 
 * <P>
 * <B>Implementation note</B><BR>
 * <code>CacheEntry</code> items  are read only with the <code>poll</code> 
 * or an equivalent method and pushed with <code>put</code>.
 * <P><I>Adding entries</I>:<BR>
 * If there is enough room in the queue (i.e. <code>size()<MAX_QUEUE_LENGTH</code>) 
 * then a new entry is stored directly in the queue otherwise it is added
 * to a vector (<code>cachedEntries</code>) ready to be written on file.
 * If the size of the vector is greater of the size of a page (<code>PAGE_LEN</code>),
 * the page is flushed on disk. Note that what is in the vector is added at the
 * end of the file.
 * <P><I>Getting entries</I>:<BR>
 * The entry to get is always in the queue.
 * After getting the entry, it checks if the the size of the queue allows to get 
 * new entries from the file (i.e. read a new page) or from the vector.
 * Note that the right order is first the file and then the vector. 
 * In fact the vector, <code>cachedEntries</code>, contains the last received entries, 
 * packed to be transferred on a new page on disk while the first entries to push
 * in the queue are on a page disk (if any).
 * 
 * @author acaproni
 *
 */
public class CacheEntriesQueue {
	
	/**
	 * The entries to keep in memory.
	 */
	private final LinkedBlockingQueue<CacheEntry> inMemoryQueue = new LinkedBlockingQueue<CacheEntry>(MAX_QUEUE_LENGTH);
	
	/**
	 * The max number of entries kept in memory.
	 */
	public static final int MAX_QUEUE_LENGTH = 20000;
	
	/**
	 * The number of {@link CacheEntry} to read/write from/to disk 
	 * on each I/O
	 */
	public static final int PAGE_LEN = 5000;
	
	/**
	 * The size (in bytes) of a page
	 */
	private static final int PAGE_SIZE = PAGE_LEN*CacheEntry.ENTRY_LENGTH;
	
	/**
	 * When in the {@link LinkedBlockingQueue} there are less 
	 * entries then the <code>THRESHOLD</code> then the 
	 * entries in the buffer are flushed in the queue
	 */
	public static final int THRESHOLD=12500;
	
	/**
	 * The buffer for each I/O
	 */
	private byte[] fileBuffer = null;
	
	/**
	 * The buffer containing the hexadecimal string of a <code>CacheEntry</code> 
	 */
	private byte[] entryBuffer =null;
	
	/**
	 * 
	 * This Vector contains the entries that will be written on the file.
	 * 
	 */
	private List<CacheEntry> cachedEntries=Collections.synchronizedList(new LinkedList<CacheEntry>());
	
	/**
	 * The file to buffer entries on disk.
	 */
	private File file=null;
	
	/**
	 * The {@link RandomAccessFile} to read/write entries
	 * created from <code>bufferFile</code>.
	 * <P>
	 * The I/O is paginated i.e. each read or write is done
	 * for a block of <code>PAGE_LEN</code> entries. 
	 */
	private RandomAccessFile raFile=null;
	
	/**
	 * The number of pages written on file and not yet read
	 */
	private volatile int pagesOnFile=0;
	
	/**
	 * The number of the next page to read from file.
	 * <P>
	 * <I>Note</I>: a new page is always added at the end of the 
	 *  			file while the reading happens in a different
	 *  			order.
	 */
	private volatile int nextPageToRead=0;
	
	/**
	 * The object to wait events from
	 */
	private Object semaphore = new Object();
	
	/**
	 * Put an entry in Cache.
	 * <P>
	 * If the cache is full the entry is added to the buffer.
	 * 
	 * @throws InterruptedException 
	 * @throws IOException In case of I/O error while flushing the cache on disk
	 */
	public void put(CacheEntry entry) throws InterruptedException, IOException {
		if (inMemoryQueue.size()<MAX_QUEUE_LENGTH && pagesOnFile==0 && cachedEntries.isEmpty()) {
			inMemoryQueue.put(entry);
		} else {
			cachedEntries.add(entry);
			if (cachedEntries.size()>=PAGE_LEN) {
				// Wake up the thread
				writePageOnFile();
			}
		}
	}
	
	/**
	 * Get the next value from the queue.
	 */
	public CacheEntry poll(long timeout, TimeUnit unit) throws InterruptedException {
		CacheEntry e = inMemoryQueue.poll(timeout, unit);
		if (e!=null && inMemoryQueue.size()<THRESHOLD && (cachedEntries.size()>0 || pagesOnFile>0)) {
			flushEntriesInQueue();
		}
		return e;
	}
	
	/**
	 * Clear the queue and the file (if any)
	 */
	public void clear() {
		inMemoryQueue.clear();
		cachedEntries.clear();
		pagesOnFile=0;
		nextPageToRead=0;
		fileBuffer=null;
		entryBuffer=null;
		if (raFile!=null) {
			synchronized (raFile) {
				try {
					raFile.close();
				} catch (Exception e) {
					System.err.println("Error closing file: "+e.getMessage());
				}
			}
		}
		raFile=null;
		if (file!=null) {
			file.delete();
		}
		file=null;
	}
	
	/**
	 * Return the number of cache entries waiting in queue
	 */
	public int size() {
		return inMemoryQueue.size()+cachedEntries.size()+pagesOnFile*PAGE_LEN;
	}
	
	/**
	 * Attempts to create the file for the strings in several places
	 * before giving up.
	 * 
	 * @return A new temporary file
	 *          <code>null</code> if it was not possible to create a new file
	 * @throws IOException In case of error creating the temporary file
	 */
	private File getNewFile() throws IOException {
		String name=null;
		File f=null;
		try {
			// Try to create the file in $ACSDATA/tmp
			String acsdata = System.getProperty("ACS.data");
			acsdata=acsdata+File.separator+"tmp"+File.separator;
			File dir = new File(acsdata);
			f = File.createTempFile("jlogEngineCache",".tmp",dir);
			name=acsdata+f.getName();
		} catch (IOException ioe) {
			// Another error :-O
			String homeDir = System.getProperty("user.dir");
			File homeFileDir = new File(homeDir);
			if (homeFileDir.isDirectory() && homeFileDir.canWrite()) {
				do {
					// Try to create the file in the home directory
					int random = new Random().nextInt();
					name = homeDir +File.separator + "jlogEngineCache"+random+".jlog";
					f = new File(name);
				} while (f.exists());
			} else {
				// last hope, try to get a system temp file
				f = File.createTempFile("jlogEngineCache",".tmp");
				name=f.getAbsolutePath();
			}
		}
		if (f!=null) {
			f.deleteOnExit();
		}
		return f;
	}
	
	/**
	 * Move the entries from the file or the vector into the queue
	 * <P>
	 * The vector contains the last added entries so if there are pages in
	 * the file they are flushed before the vector
	 * 
	 */
	private synchronized void flushEntriesInQueue() {
		if (pagesOnFile==0) {
			// Just to be sure, we check if there is enough room
			// to flush all the vector in the queue otherwise we risk
			// to lock everything because the queue has a fixed mx size
			if (cachedEntries.size()!=0) {
				if (cachedEntries.size()<MAX_QUEUE_LENGTH-inMemoryQueue.size()) {
					inMemoryQueue.addAll(cachedEntries);
					cachedEntries.clear();
				} else {
					while (cachedEntries.size()>0 && inMemoryQueue.size()<MAX_QUEUE_LENGTH) {
						if (inMemoryQueue.offer(cachedEntries.get(0))) {
							cachedEntries.remove(0);
						} else {
							break;
						}
					}
				}
			}
		} else {
			// Get the next page from disk
			readNextPageFromFile();
		}
	}
	
	/**
	 * Read page from the file putting all the <code>CacheEntry</code> it contains
	 * in the queue.
	 */
	private synchronized void readNextPageFromFile() {
		if (pagesOnFile==0) {
			throw new IllegalStateException("No pages available on file");
		}
		if (raFile==null || file==null) {
			throw new IllegalStateException("The file (random or buffer) is null!");
		}
		if (inMemoryQueue.size()<PAGE_LEN) {
			throw new IllegalStateException("Not enough room in queue!");
		}
		if (fileBuffer==null) {
			// This should not be null because it must have been initialized 
			// by a previous writing
			throw new IllegalStateException("The array of bytes should not be null!");
		}
		if (entryBuffer==null) {
			// This should not be null because it must have been initialized 
			// by a previous writing
			throw new IllegalStateException("The entryBuffer array of bytes should not be null!");
		}
		if (file.length()<nextPageToRead*(PAGE_SIZE+1)-1) {
			throw new IllegalStateException("File out of bound exception");
		}
		int bytesRead=-1;
		synchronized (raFile) {
			try {
				raFile.seek(nextPageToRead*PAGE_SIZE);
				bytesRead=raFile.read(fileBuffer);
			} catch (Throwable t) {
				//An error reading the page form the file
				//
				// This should never happen
				// At the present I think the best is to print a message and
				// continue without changing the value of nextPageToRead
				// so it will try to read the same page again at the next iteration
				t.printStackTrace(System.err);
				return;
			}
		}
		if (bytesRead!=fileBuffer.length) {
			System.err.println("Got "+bytesRead+" bytes instead of "+fileBuffer.length);
			return;
		}
		nextPageToRead++;
		pagesOnFile--;
		for (int t=0; t<fileBuffer.length; t+=CacheEntry.ENTRY_LENGTH) {
			for (int y=t; y<t+CacheEntry.ENTRY_LENGTH; y++) {
				entryBuffer[y-t]=fileBuffer[y];
			}
			CacheEntry e = new CacheEntry(new String(entryBuffer));
			if (!inMemoryQueue.add(e)) {
				System.err.println("Failed adding item "+t+" to the queue");
			}
		}
		// If there are no pages it means that the file is empty and we can cut it
		// It saves disk space...
		if (pagesOnFile==0) {
			try {
				synchronized (raFile) {
					raFile.setLength(0);
				}
			} catch (Exception e) {
				// We can skip this one
			}
		}
	}
	
	/**
	 * Write a page of <code>CacheEntry</code> in the file
	 * 
	 * @throws IOException In case of error creating a new temporary file
	 */
	private synchronized void writePageOnFile() throws IOException {
		if (file==null) {
			file=getNewFile();
			try {
				raFile=new RandomAccessFile(file,"rw");
			} catch (FileNotFoundException e) {
				// Ops an error creating the file
				// print a message and exit: in this way it will try again
				// at next iteration
				file=null;
				raFile=null;
				IOException ioe = new IOException("Error creating the random file",e);
				throw ioe;
			}
		}
		if (entryBuffer==null) {
			entryBuffer=new byte[CacheEntry.ENTRY_LENGTH];
		}
		if (fileBuffer==null) {
			fileBuffer=new byte[PAGE_SIZE];
		}
		
		if (cachedEntries.size()<PAGE_LEN) {
			throw new IllegalStateException("Not enough entries in vector");
		}
		for (int t=0; t<PAGE_LEN; t++) {
			CacheEntry e = cachedEntries.get(t);
			byte[] hexBytes=e.toHexadecimal().getBytes();
			for (int y=0; y<hexBytes.length; y++) {
				fileBuffer[t*CacheEntry.ENTRY_LENGTH+y]=hexBytes[y];
			}
			
		}
		synchronized (raFile) {
			raFile.seek(raFile.length());
			raFile.write(fileBuffer);
		}
		// Better to remove here so if the
		// writing returned an error we have all
		// the data still in the vector
		for (int t=0; t<PAGE_LEN; t++) {
			cachedEntries.remove(0);
		}
		pagesOnFile++;		
	}
	
}
