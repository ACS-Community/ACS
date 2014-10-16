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
import java.text.ParseException;
import java.util.Date;
import java.util.concurrent.atomic.AtomicBoolean;

import alma.acs.util.IsoDateFormat;

/**
 * Each file used by the cache.
 * <P>
 * The cache is composed by
 * <UL> 
 *  <LI>a {@link File} to know the length of the file on disc.
 *  <LI>a {@link RandomAccessFile} for reading and writing
 * </UL>
 * There are two booleans signaling if  the file is used for reading and writing.
 * In this way we know when the reads and writes are terminated and the file
 * can be safely closed.
 *  
 * @author acaproni
 *
 */
public class QueueFile {
	
	/**
	 * The name of the file
	 */
	public final String fileName;
	
	/**
	 * The key identifying this file.
	 * <P>
	 * The key is stored only to perform run-time tests of correctness.
	 */
	public final Integer key;
	
	/**
	 * The <code>RandomAccessFile</code> of the file of this entry.
	 * <P>
	 * This is not <code>null</code> only when used for I/O.
	 */
	private RandomAccessFile raFile = null;
	
	/**
	 * The file used to build <code>raFile</code>.
	 * <P>
	 * It is not null as soon as raFile is not null
	 */
	private File file=null;
	
	/**
	 * Signal if the file is used for reading
	 */
	private boolean reading=false;
	
	/**
	 * Signal if the file is used for writing
	 */
	private boolean writing=false;
	
	/**
	 * The date of the oldest log in this file in milliseconds
	 */
	private long oldestLogDateMillis=0;
	
	/**
	 * The date of the youngest log in this file in milliseconds
	 */
	private long youngestLogDateMillis=0;
	
	/**
	 * The (case insensitive) string to look for in the pushed string while getting the date.
	 * <P>
	 * For example, if the queue is used to store logs, this string is 
	 * <code>TIMESTAMP="</code>
	 */
	private final String timestampStrTag;
	
	/**
	 * All ISO timestamps like ("2014-10-09T14:04:25.984")
	 * have the same size!
	 */
	private static final int timeStampLength=23;
	
	/**
	 * Constructor 
	 * 
	 * @param fName The name of the file
	 * @param key The key of this entry
	 * @param rf The <code>RandomAccessFile</code> used for I/O
	 * @param f The <code>File</code> used to get the length
	 * @param tstampStrTag The string to find the timestamp in each pushed string
	 * @throws IOException In case of error writing in the file
	 * 
	 * @see {@link QueueFile(String fName, Integer key)}
	 */
	public QueueFile(String fName, Integer key, RandomAccessFile rf, File f, String tstampStrTag) 
			throws IOException {
		if (fName==null || fName.isEmpty()) {
			throw new IllegalArgumentException("The file name can't be null not empty");
		}
		if (key==null) {
			throw new IllegalArgumentException("Invalid null key");
		}
		if (rf==null) {
			throw new IllegalArgumentException("Invalid null random file");
		}
		if (tstampStrTag==null || tstampStrTag.isEmpty()) {
			throw new IllegalArgumentException("Invalid timestamp identifier.");
		}
		fileName=fName;
		this.key=key;
		raFile=rf;
		file=f;
		this.timestampStrTag=tstampStrTag.toUpperCase();
	}
	
	/**
	 * An helper methods that returns the <code>File</code>.
	 * <P>
	 * As soon as <code>raFile</code> is not <code>null</code>, <code>file</code> is not <code>null</code> too.
	 *  
	 * A new {@link File} is built if <code>file</code> is <code>null</code> 
	 * otherwise the method returns a reference to <code>file</code>.
	 * 
	 * @return The file
	 * @throws FileNotFoundException If the file does not exist
	 */
	public File getFile() throws FileNotFoundException {
		if (file!=null) {
			return file;
		}
		File f = new File(fileName);
		if (!f.exists()) {
			throw new FileNotFoundException("The cache file "+fileName+" does not exist");
		}
		if (!f.canRead() || !f.canWrite()) {
			throw new IllegalStateException("Impossible to read/write "+fileName);
		}
		file=f;
		return file;
	}
	
	/**
	 * An helper method that returns a <code>RandomAccessFile</code> by the file name 
	 * <code>fileName</code>.
	 * <P>
	 * The random access file is built from the <code>fileName</code>.
	 * 
	 * @return The file to read and/or write items
	 * @throws IOException In case of error creating the <code>File</code>.
	 * @throws FileNotFoundException If the file does not exist
	 */
	private void openFile() throws FileNotFoundException {
		raFile = new RandomAccessFile(getFile(),"rw");
	}
	
	/**
	 * Release all the resources (for instance it releases the random
	 * file).
	 */
	public void close() {
		if (raFile!=null) {
			try {
				synchronized (raFile) {
					raFile.close();
				}
			} catch (Throwable t) {
				// Nothing to do here: print a message and go ahead.
				System.err.println("Error closing the file "+fileName+": "+t.getMessage());
			}
			raFile=null;
		}
		file=null;
	}
	
	/**
	 * Check if the file is used for reading or writing and 
	 * if not used, close the random file.
	 */
	private void checkRaFileUsage() {
		// Release raFile and file if both the file for input
		// and output are null (unused)
		if (!reading && !writing) {
			try {
				raFile.close();
			} catch (Throwable t) {
				// An error closing the file: do not stop the computation but cross the fingers!
				System.err.println("Error closing "+fileName+": "+t.getMessage());
			}
			raFile=null;
			file=null;
		}
	}
	
	/**
	 * Return the size of the file
	 * 
	 * @return the size of the file
	 */
	public long getFileLength() {
		if (file==null) {
			throw new IllegalStateException("The file is null");
		}
		return file.length();
	}
	
	/**
	 * Write the passed string in the file.
	 * 
	 * @param str The string to write in the file
	 * @return The ending position of the string in the file
	 * 
	 * @throws IOException In case of error while performing I/O
	 * @throws StringQueueException In case of error reading the date of the log from str
	 */
	public synchronized QueueEntry writeOnFile(String str, Integer key) throws IOException, StringQueueException {
		if (str==null || str.isEmpty()) {
			throw new IllegalArgumentException("Invalid string to write on file");
		}
		if (!this.key.equals(key)) {
			throw new IllegalArgumentException("Wrong key while writing");
		}
		if (raFile==null) {
			openFile();
		}
		writing=true;
		long startPos;
		long endPos;
		synchronized (raFile) {
			startPos = file.length();
			raFile.seek(startPos);
			raFile.writeBytes(str);
			endPos = file.length();	
		}
		updateLogDates(str);
		return new QueueEntry(key,startPos,endPos);
	}
	
	/**
	 * Read a string from the file
	 * 
	 * @param entry The cache entry to saying how to read the entry
	 * 
	 * @return The string read from the file
	 */
	public synchronized String readFromFile(QueueEntry entry) throws IOException {
		if (entry==null) {
			throw new IllegalArgumentException("The QueueEntry can't be null");
		}
		if (entry.key!=key) {
			throw new IllegalArgumentException("Wrong key while reading");
		}
		if (raFile==null) {
			openFile();
		}
		reading=true;
		byte buffer[] = new byte[(int)(entry.end-entry.start)];
		raFile.seek(entry.start);
		int bytesRead=raFile.read(buffer);
		if (bytesRead!=buffer.length) {
			throw new IOException("read returned "+bytesRead+" instead of "+buffer.length);
		}
		return new String(buffer);
	}
	
	/**
	 * Set the reading mode of the file.
	 * 
	 * @param reading <code>true</code> if the file is used for reading
	 */
	public synchronized void setReadingMode(boolean reading) {
		this.reading=reading;
		checkRaFileUsage();
	}
	
	/**
	 * Set the writing mode of the file.
	 * 
	 * @param writing <code>true</code> if the file is used for writing
	 */
	public synchronized void setWritingMode(boolean writing) {
		this.writing=writing;
		checkRaFileUsage();
	}
	
	/**
	 * Update the times of the youngest and oldest log
	 * in this file.
	 * 
	 * @param str The string representing the log
	 */
	private void updateLogDates(String str) throws StringQueueException{
		long millis=0; // Date of current log read from str

		// To improve performances I don't want to parse the string
		// in this method so I look for the
		// timestamp that has always the same format
		str=str.toUpperCase();
		int pos=str.indexOf(timestampStrTag);
		if (pos==-1) {
			String msg=timestampStrTag+" not found in: ["+str+"]!!!";
			throw new StringQueueException(msg);
		}
		// switch to the end of the TIMESTAMP string
		int startPosOfTimestamp=pos+timestampStrTag.length();
		String timestamp=str.substring(startPosOfTimestamp, startPosOfTimestamp+timeStampLength);
		try {
			millis=IsoDateFormat.parseIsoTimestamp(timestamp).getTime();
		} catch (ParseException e) {
			throw new StringQueueException("Error parsing the date from: ["+timestamp+"]",e);
		}
		
		// check and store the date
		if (millis<youngestLogDateMillis || youngestLogDateMillis==0) {
			youngestLogDateMillis=millis;
		}
		if (millis>oldestLogDateMillis || oldestLogDateMillis==0) {
			oldestLogDateMillis=millis;
		}
	}
	
	/**
	 * @return the date of the youngest log in this file
	 * in ISO format
	 */
	public String minDate() {
		if (youngestLogDateMillis==0) {
			return null;
		} else {
			return IsoDateFormat.formatDate(new Date(youngestLogDateMillis));
		}
	}
	
	/**
	 * @return the date of the oldest log in this file
	 * in ISO format
	 */
	public String maxDate() {
		if (oldestLogDateMillis==0) {
			return null;
		} else {
			return IsoDateFormat.formatDate(new Date(oldestLogDateMillis));
		}
	}
}
