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
import java.util.LinkedList;
import java.util.List;
import java.util.Random;
import java.util.concurrent.LinkedBlockingQueue;

/**
 * The queue of entries.
 * <P>
 * This class has been introduced to avoid keeping in memory a
 * never ending queue of {@link CacheEntry} and reduce the
 * chance to face an out of memory at run-time.
 * <BR>
 * This class is composed of two lists and a file.
 * <code>inMemoryQueue</code> is the list of the entries to get.
 * When this list contains few items then some more items are read from the file.
 * <BR>
 * The other list, <code>CacheEntriesQueue</code>, is a buffer where the entries
 * are stored ready to be flushed on disk. 
 * This is done to write more entries at once reducing the I/O and increasing
 * the performances.
 * <P>
 * <B>Implementation note</B><BR>
 * <code>CacheEntry</code> items  are read only with the <code>get</code> 
 * method and pushed with the <code>put</code>.
 * <P>
 * <I>Adding entries</I>:<BR>
 * If there is enough room in <code>inMemoryQueue</code> 
 * (i.e. <code>inMemoryQueue.size()<MAX_QUEUE_LENGTH</code>) 
 * then a new entry is stored directly in that list; otherwise it is added
 * to <code>cachedEntries</code> ready to be written on file.
 * If the size of <code>cachedEntries</code> is greater then <code>PAGE_LEN</code>, 
 * the size of a page, then a page is flushed on disk. 
 * Note that what is in the list, <code>cacheEntries</code> is added at the end of the file.
 * <P>
 * <I>Getting entries</I>:<BR>
 * The entry to get is always in <code>inMemoryQueue</code>.
 * After getting an entry, it checks if the the size of the queue allows to get 
 * new entries from the file or from the <code>cachedEntries</code>.
 * Note that the right order is first the file and then <code>cachedEntries</code>. 
 * In fact <code>cachedEntries</code>, contains the last received entries, 
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
	private final List<CacheEntry> inMemoryQueue =new LinkedList<CacheEntry>();
	
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
	private byte[] fileBuffer = new byte[PAGE_SIZE];
	
	/**
	 * The buffer containing the hexadecimal string of a <code>CacheEntry</code> 
	 */
	private byte[] entryBuffer =new byte[CacheEntry.ENTRY_LENGTH];
	
	/**
	 * 
	 * This Vector contains the entries that will be written on the file.
	 * 
	 */
	private List<CacheEntry> cachedEntries=new LinkedList<CacheEntry>();
	
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
	 * Put an entry in Cache.
	 * <P>
	 * If the cache is full the entry is added to the buffer.
	 * 
	 * @param entry The not <code>null</code> {@link CacheEntry} to add to the queue 
	 * @throws IOException In case of I/O error while flushing the cache on disk
	 */
	public synchronized void put(CacheEntry entry) throws IOException {
		if (entry==null) {
			throw new IllegalArgumentException("The queue do not contain null items!");
		}
		if (inMemoryQueue.size()<MAX_QUEUE_LENGTH && pagesOnFile==0 && cachedEntries.isEmpty()) {
			inMemoryQueue.add(entry);
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
	 * 
	 * @return The next item in the queue or <code>null</code> if the
	 * 			queue is empty
	 * 
	 * @throws IOException In case of error during I/O
	 */
	public synchronized CacheEntry get() throws IOException {
		if (inMemoryQueue.isEmpty()) {
			return null;
		}
		CacheEntry e = inMemoryQueue.remove(0);
		if (e!=null && inMemoryQueue.size()<THRESHOLD && (cachedEntries.size()>0 || pagesOnFile>0)) {
			flushEntriesInQueue();
		}
		return e;
	}
	
	/**
	 * Clear the queue and the file (if any)
	 */
	public synchronized void clear() {
		inMemoryQueue.clear();
		cachedEntries.clear();
		pagesOnFile=0;
		nextPageToRead=0;
		fileBuffer=null;
		entryBuffer=null;
		if (raFile!=null) {
			try {
				raFile.close();
			} catch (Exception e) {
				System.err.println("Error closing file: "+e.getMessage());
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
	public synchronized int size() {
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
			name=f.getAbsolutePath();
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
	 * @throws IOException In case of error during I/O
	 */
	private void flushEntriesInQueue() throws IOException {
		if (pagesOnFile==0) {
			if (cachedEntries.size()!=0) {
				inMemoryQueue.addAll(cachedEntries);
				cachedEntries.clear();
			}
		} else {
			// Get the next page from disk
			readNextPageFromFile();
		}
	}
	
	/**
	 * Read page from the file putting all the <code>CacheEntry</code> it contains
	 * in the queue.
	 * 
	 * @throws IOException In case of error during I/O
	 */
	private void readNextPageFromFile() throws IOException {
		if (pagesOnFile==0) {
			throw new IllegalStateException("No pages available on file");
		}
		if (raFile==null || file==null) {
			throw new IllegalStateException("The file (random or buffer) is null!");
		}
		if (inMemoryQueue.size()<PAGE_LEN) {
			throw new IllegalStateException("Not enough room in queue!");
		}
		if (file.length()<nextPageToRead*(PAGE_SIZE+1)-1) {
			throw new IllegalStateException("File out of bound exception file length="
					+file.length()+", index to read="+
					(nextPageToRead*(PAGE_SIZE+1)-1)+", inMemoryQueue="+
					inMemoryQueue.size()+", cachedEntries="+cachedEntries.size()+
					", pages to read on disk="+pagesOnFile);
		}
		int bytesRead=-1;
		raFile.seek(nextPageToRead*PAGE_SIZE);
		bytesRead=raFile.read(fileBuffer);
		if (bytesRead==-1) {
			throw new IllegalStateException("EOF! but... pagesOnFile="+pagesOnFile);
		}
		if (bytesRead!=fileBuffer.length) {
			throw new IllegalStateException("Not read all the bytes?!? Is the file shorter then expected?!?!?");
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
			raFile.setLength(0);
			nextPageToRead=0;
		}
	}
	
	/**
	 * Write a page of <code>CacheEntry</code> in the file
	 * 
	 * @throws IOException In case of error creating a new temporary file
	 */
	private void writePageOnFile() throws IOException {
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
		raFile.seek(raFile.length());
		raFile.write(fileBuffer);
		// Better to remove here so if the
		// writing returned an error we have all
		// the data still in the vector
		for (int t=0; t<PAGE_LEN; t++) {
			cachedEntries.remove(0);
		}
		pagesOnFile++;		
	}
	
}
