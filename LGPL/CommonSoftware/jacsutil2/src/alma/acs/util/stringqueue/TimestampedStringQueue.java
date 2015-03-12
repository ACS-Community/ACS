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
package alma.acs.util.stringqueue;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;


/**
 * Objects from this class implement a FIFO queue of strings.
 * The strings stored in the queue must be timestamped i.e. they always contain a ISO timestamp 
 * in a definite position. Example of such strings in ACS are the XML logs and the alarms.
 * <BR>
 * <code>TimestampedStringQueue</code> does not assume any format for the strings pushed
 * in the queue. It assumes that the timestamp immediately follows a string, passed in the constructor.
 * <P>
 * The main purpose of this class is to write on files all the strings pushed in the queue and let the
 * user able to reuse such files knowing the timestamps of the strings it contains.
 * Implementors of {@link TimestampedStringQueueFileHandler} are notified of the min
 * and max timestamps contained in each file of the cache through 
 * {@link TimestampedStringQueueFileHandler#fileProcessed(File, String, String)}.
 * <P> 
 * Users of this class pushes string by invoking {@link #push(String)} and get strings out of the queue
 * by calling {@link #pop()}.
 * The insertion of a new string is immediate.
 * Getting a string returns immediately if the queue contains at least one string; otherwise it waits for
 * a new element until a timeout happens.
 * <P>
 * The strings are written on disk by using a set of files: a new file is created whenever
 * the dimension of the current file becomes greater then a fixed size.
 * For each entry in the queue, a record is created and kept in a in-memory list. 
 * <P>
 * When all the strings in a file have been red, the file is deleted to reduce the disk usage. 
 * The deletion of unused files is done by a thread. 
 * <P>
 * The length of each file of cache can be customized by passing a parameter in the constructor
 * or setting {@value TimestampedStringQueueFileHandler#MAXSIZE_PROPERTY_NAME} java property. 
 * If both those values are not given, a default length is used ({@value TimestampedStringQueueFileHandler#DEFAULT_SIZE}).
 * <P> 
 * <code>files</code> contains all the files used by the cache, identified by a key.
 * When a file does not contain unread entries then its key is pushed into <code>filesToDelete</code> 
 * and deleted. 
 * The thread that deletes the files from disk, removes the {@link QueueFile} object from
 * <code>files</code> too.
 * <P>
 * Life cycle: {@link #start()} must be called at the beginning and {@link #close(boolean)} at the end. 
 *  
 * @author acaproni
 *
 */
public class TimestampedStringQueue extends Thread {
	
	
	
	/**
	 * Each file of the cache is identified by a key.
	 * <P>
	 * The key is always positive.
	 */
	private final AtomicInteger fileKey = new AtomicInteger(0);
	
	/**
	 * The file used to write the strings into.
	 * When the size of this file is greater then <code>maxSize</code> then a new file
	 * is created for output.
	 */
	private volatile QueueFile outCacheFile=null;
	
	/**
	 * The timestamp in each string pushed in the queue must follow this string
	 * otherwise it is not find.
	 * <P>
	 * The queue dues not assume any format for the strings it contains so it can't 
	 * for example parse it to look for a XML TGA because the string can or cannot
	 * be XML. 
	 * This choice implies that all the strings follow the same pattern.
	 * <P>
	 * In the case of logs, they all have the same pattern:
	 * &lt;LEVEL Timestamp="...."....&gt;
	 * In this example tstampIdentifier must be set to <code>Timestamp="</code>.
	 * <P>
	 * To improve performances, the queue looks for this string in the push (case insensitive)
	 * and assume that he timestamp starts from the next character in the string.
	 * A better solution could be to use a regular expression but it would be less performant. 
	 */
	private final String tstampIdentifier;
	
	/**
	 * The file used to read the previous record.
	 * It is used to know when all the records in a file have been read.
	 */
	private volatile QueueFile inCacheFile=null;
	
	/**
	 * The entries in the cache.
	 * <P>
	 * The items of the list are organized  in a FIFO policy.
	 * This is particularly important because this order is used to know when a file
	 * is not used anymore and can be deleted.
	 * 
	 * @see {@link TimestampedStringQueue.files}
	 */
	private final EntriesQueue entries = new EntriesQueue();
	
	/**
	 * A list of keys of unused files to delete.
	 */
	private final LinkedBlockingQueue<QueueFile> filesToDelete = new LinkedBlockingQueue<QueueFile>();
	
	/**
	 * The files used by the cache.
	 * 
	 * The entries in this vector have the same order of the creation of the files:
	 * the last created file is in the last position of the vector.
	 * 
	 * This property can be used to verify for the correctness of the computation because
	 * every time we have to delete a file, it must be the first item of this vector 
	 * 
	 * @see {@link TimestampedStringQueue.entries}
	 */
	private final Map<Integer,QueueFile> files = Collections.synchronizedMap(new LinkedHashMap<Integer,QueueFile>());
	
	/** 
	 * <code>true</code> if the cache is closed.
	 * It signals the thread to terminate.
	 */
	private AtomicBoolean closed=new AtomicBoolean(false);
	
	/**
	 * The handler to create and delete the file of the this cache.
	 */
	private final TimestampedStringQueueFileHandler fileHandler;
	
	/**
	 * The property to set the timeout while getting a string
	 * through {@link #pop()}.
	 */
	private static final String TIMEOUT_PROPERTY_NAME= "acs.util.stringqueue.maxFilesSize";
	
	/**
	 * The default timeout (msecs) to wait when getting a string through {@link #pop()}.
	 */
	private static final int DEFAULT_TIMEOUT=250;
	
	/**
	 * The max amount of time (msecs) to wait for a new element when the queue is empty.
	 * 
	 * @see TimestampedStringQueue#pop().
	 * 
	 */
	private final AtomicInteger maxWaitingTime = new AtomicInteger(Integer.getInteger(TIMEOUT_PROPERTY_NAME, DEFAULT_TIMEOUT));
	
	/**
	 * Build a cache with the default file handler {@link DefaultQueueFileHandlerImpl}
	 * @param timestampIdentifier The string to find the timestamp in each pushed string
	 */
	public TimestampedStringQueue(String timestampIdentifier) {
		super("TimestampedStringQueue");
		fileHandler=new DefaultQueueFileHandlerImpl();
		if (timestampIdentifier==null || timestampIdentifier.isEmpty()) {
			throw new IllegalArgumentException("Invalid timestamp identifier.");
		}
		this.tstampIdentifier=timestampIdentifier;
	}
	
	/**
	 * Build the cache with the passed maximum size for each file of the cache.
	 * <P>
	 * This constructor must be used to instantiate the default file handler ({@link DefaultQueueFileHandlerImpl})
	 * with a customized file size. 
	 * 
	 * @param size The max size of each file of the cache
	 * @param timestampIdentifier The string to find the timestamp in each pushed string
	 */
	public TimestampedStringQueue(long size, String timestampIdentifier) {
		super("TimestampedStringQueue");
		if (size<=1024) {
			throw new IllegalArgumentException("The size can't be less then 1024");
		}
		fileHandler=new DefaultQueueFileHandlerImpl(size);
		if (timestampIdentifier==null || timestampIdentifier.isEmpty()) {
			throw new IllegalArgumentException("Invalid timestamp identifier.");
		}
		this.tstampIdentifier=timestampIdentifier;
	}
	
	/**
	 * Build the cache with the passed file handler. This method should be used 
	 * <OL>
	 * 	<LI>when a custom implementation of {@link StringQueueFileHandler}
	 *  <LI>when the default handler with the default size of files must be instantiated. 
	 *      The default size means {@link StringQueueFileHandler#DEFAULT_SIZE} or whatever is 
	 *      set in the  {@value StringQueueFileHandler#MAXSIZE_PROPERTY_NAME} java property. 
	 * </OL>
	 * 
	 * @param handler The not <code>null</code> handler to create and delete the files.
	 * @param timestampIdentifier The string to find the timestamp in each pushed string
	 */
	public TimestampedStringQueue(TimestampedStringQueueFileHandler handler, String timestampIdentifier) {
		super("TimestampedStringQueue");
		if (handler==null) {
			throw new IllegalArgumentException("The file handler can't be null.");
		}
		fileHandler=handler;
		if (timestampIdentifier==null || timestampIdentifier.isEmpty()) {
			throw new IllegalArgumentException("Invalid timestamp identifier.");
		}
		this.tstampIdentifier=timestampIdentifier;
	}
	
	/**
	 * Attempts to create the file for the strings in several places
	 * before giving up.
	 * 
	 * @return A new temporary file
	 *          <code>null</code> if it was not possible to create a new file
	 * @throws If it was not possible to create a temporary file
	 */
	private File getNewFile() throws IOException {
		File ret= fileHandler.getNewFile();
		return ret;
	}
	
	/**
	 * Close and delete a file.
	 * 
	 * @param itemToDel The item to delete
	 * @return true if the file is deleted
	 */
	private void releaseFile(QueueFile itemToDel) {
		if (itemToDel==null) {
			throw new IllegalArgumentException("The item to delete can't be null");
		}
		itemToDel.close();
		try {
			File f = itemToDel.getFile();
		} catch (FileNotFoundException fnfe) {
			System.err.println("Error deleting "+itemToDel.fileName+" (key "+itemToDel.key+")");
			System.err.println("Will try to notify the QueueFileHandler anyhow...");
			fnfe.printStackTrace(System.err);
		}
		try {
			fileHandler.fileProcessed(itemToDel.getFile(),itemToDel.minDate(), itemToDel.maxDate());
		} catch (Throwable t) {
			System.err.println("Error calling fileProcessed in the QueueFileHandler: "+t.getMessage());
			t.printStackTrace(System.err);
		}
	}
	
	/**
	 * Return the number of entries in cache.
	 * @return the number of entries in cache
	 */
	public int size() {
		return entries.size();
	}
	
	/**
	 * 
	 * @return <code>true</code> if the queue is empty;
	 *         <code>false</code> otherwise.
	 */
	public boolean isEmpty() {
		return entries.isEmpty();
	}
	
	/**
	 * Generate a new key for a file.
	 * <P>
	 * Each new key is generated by increasing the value of the current key.
	 * If the max integer value is reached then the key rests to the min value.
	 * 
	 * @return A new key for a file
	 */
	private Integer getNextFileKey() {
		if (fileKey.get()<Integer.MAX_VALUE) {
			fileKey.incrementAndGet();
		} else {
			// Ops we need to reset the key...
			fileKey.set(0);
		}
		Integer ret = Integer.valueOf(fileKey.get());
		// Check if the key is already used
		if (files.containsKey(fileKey.get())) {
			throw new IllegalStateException("Key already used!");
		}
		return ret;
	}
	
	/**
	 * 
	 * @return The number of files used by the cache
	 */
	public int getActiveFilesSize() {
		return files.size();
	}
	
	/**
	 * Push an entry in the cache.
	 * If the current file is <code>null</code> or its size is greater then <code>maxSize</code>,
	 * then a new file is created.
	 * 
	 * @param string The string to write in the cache
	 * @throws IOException In case of error writing the string on disk
	 * @throws StringQueueException
	 */
	public synchronized void push(String string) throws IOException, StringQueueException {
		if (string==null || string.length()==0) {
			throw new IllegalArgumentException("The string can't be null nor empty");
		}
		if (closed.get()) {
			return;
		}
		
		// Check if a new file must be created
		if (outCacheFile==null || outCacheFile.getFileLength()>=fileHandler.getMaxFileSize()) {
			File f = getNewFile();
			if (f==null) {
				throw new IOException("Error creating a cache file");
			}
			String name = f.getAbsolutePath();
			RandomAccessFile raF = new RandomAccessFile(f,"rw");
			outCacheFile = new QueueFile(name,getNextFileKey(), raF,f,tstampIdentifier);
			outCacheFile.setWritingMode(true);
			files.put(outCacheFile.key,outCacheFile);
		}
		if (!string.endsWith("\n")) {
			string=string+"\n";
		}
		// Write the string in the file
		QueueEntry entry = outCacheFile.writeOnFile(string, outCacheFile.key);
		entries.put(entry);
	}
	
	/**
	 * Get and remove the next string from the cache.
	 * <P>
	 * This method returns immediately if the queue is not empty otherwise
	 * waits until a new element is inserted in the queue or a timeout elapses ({@link QueueEntry#maxWaitingTime}).
	 * However, a timeout of 0 let this method return immediately even if the queue is empty.
	 * 
	 * @return The next string entry in cache.
	 *         <code>null</code> If the timeout happened
	 * @throws IOException In case of error reading from the file
	 */
	public synchronized String pop() throws IOException {
		if (closed.get()) {
			return null;
		}
		// Get a new entry if it exists or wait until timeout
		
		if (entries.isEmpty()) {
			if (maxWaitingTime.get()==0) {
				return null;
			}
			try {
				wait(maxWaitingTime.get());
			} catch (InterruptedException ie) {
				return null;
			}
			// If still empty then return
			if (entries.isEmpty()) {
				return null;
			}
		}
		
		
		
		QueueEntry entry=entries.get();
		if (entry==null) {
			// No entry in QueueEntry
			return null;
		}
		
		if (inCacheFile==null) {
			inCacheFile=files.get(entry.key);
			inCacheFile.setReadingMode(true);
		} else if (inCacheFile.key!=entry.key) {
			// If the key differs then we have to start reading from another file
			inCacheFile.setReadingMode(false);
			files.remove(inCacheFile.key);
			if (!filesToDelete.offer(inCacheFile)) {
				// Most unlikely to happen: the queue is full!
				releaseFile(inCacheFile);
			} 
			inCacheFile=files.get(entry.key);
			
			inCacheFile.setReadingMode(true);
		}
		String ret= inCacheFile.readFromFile(entry);
		if (ret.endsWith("\n")) {
			return ret.substring(0,ret.length()-1);
		} else {
			return ret;
		}
	}
	
	/**
	 * Start the thread.
	 */
	public void start() {
		setDaemon(true);
		setPriority(MIN_PRIORITY);
		super.start();
	}
	
	/**
	 * Close the cache: delete all the entries and all the files the exit.
	 * <P>
	 * <B>Note</B>: this must be the last operation executed by the cache
	 * 
	 * @param sync <code>true</code> if must wait the termination of the threads before exiting
	 */
	public void close(boolean sync) {
		closed.set(true);
		interrupt();
		if (inCacheFile!=null) {
			inCacheFile.close();
		}
		if (outCacheFile!=null) {
			outCacheFile.close();
		}
		while (sync && isAlive()) {
			try {
				Thread.sleep(250);
			} catch (InterruptedException ie) {}
		}
		// Release all the files still in the queue
		synchronized (files) {
			if (!files.isEmpty()) {
				Set<Integer> keys = files.keySet();
				for (Integer key: keys) {
					QueueFile cf = files.get(key);
					releaseFile(cf);
				}
				files.clear();
			}	
		}
	}
	
	/**
	 * The method to get and delete unused files
	 */
	public void run() {
		while (!closed.get()) {
			QueueFile cacheFile;
			try {
				cacheFile = filesToDelete.poll(15, TimeUnit.MINUTES);
			} catch (InterruptedException ie) {
				continue;
			}
			if (cacheFile==null) {
				// timeout elapsed
				continue;
			}
			releaseFile(cacheFile);
		}
	}
	
	/**
	 * Set a new timeout when getting new element and the queue is empty.
	 * <P>
	 *  If  the timeout is <code>0</code>, then {@link #pop()} returns immediately
	 *  if the queue is empty.
	 *  
	 * @param val The new timeout (must be equal or greater then <code>0</code>).
	 * @see #pop()
	 */
	public void setTimeout(int val) {
		if (val<0) {
			throw new IllegalArgumentException("Invalid timeout "+val);
		}
		maxWaitingTime.set(val);
	}
}
