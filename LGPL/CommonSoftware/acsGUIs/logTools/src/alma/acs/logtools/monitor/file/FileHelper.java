/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2010
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
package alma.acs.logtools.monitor.file;

import java.io.File;
import java.io.FileOutputStream;
import java.io.PrintStream;

import alma.acs.util.IsoDateFormat;

/**
 * Help managing the text file to write stats into.
 * <P>  
 * The helper generates a new file when the size 
 * of the current size is greater then a threshold.
 * <P>
 * The name of each file is composed of the extension
 * plus the time stamp of the creation.
 * <P>
 * <B>Note</B>: in case of error creating a file, the object will 
 * discard all next writing requests.
 * 
 * @author acaproni
 * @since ACS 8.1.0
 */
public class FileHelper {
	
	/**
	 * To improve performance the checking of the length of the
	 * files is done after <code>CHECK_INTERVAL</code> writings.
	 */
	private static final int  CHECK_INTERVAL=6;
	
	/**
	 * The property remember how many writings have been
	 * done since the last time the length of the file
	 * was checked.
	 */
	private int currentWritingOps=0;
	
	/**
	 * The max dimension of each file of log in bytes.
	 */
	private final long MAX_LOG_FILE_SIZE=1024*1024*1024; // 1 Gb

	/**
	 * The folder to create file into
	 */
	public final String folder;
	
	/**
	 * The folder to create file into
	 */
	public final String extension;
	
	/**
	 * The header to write on top of each new file
	 * (can be <code>null</code> and empty)
	 */
	private final String header;
	
	/**
	 * The writer for output
	 */
	private PrintStream outStream=null;
	
	/**
	 * The file for output
	 */
	private File outputFile=null;
	
	/**
	 * If <code>true</code> the object has been closed
	 */
	private volatile boolean closed=false;
	
	/**
	 * Constructor
	 * @param folder The folder
	 * @param ext The extension
	 * @param header The header to write on top of each new file
	 * 				(can be <code>null</code> and empty)
	 */
	public FileHelper(String folder, String ext, String header) {
		if (folder==null || folder.isEmpty()) {
			throw new IllegalArgumentException("Invalid folder");
		}
		if (ext==null || ext.isEmpty()) {
			throw new IllegalArgumentException("Invalid extension");
		}
		// Check if the folder is valid
		File f = new File(folder);
		if (!f.isDirectory() || !f.canWrite()) {
			throw  new IllegalArgumentException("Invalid folder: "+folder);
		}
		this.folder=folder;
		this.extension=ext;
		this.header=header;
		getNewFile();
	}
	
	/**
	 * Check if the file is valid and eventually creates
	 * a new one
	 */
	private void checkFile() {
		if (outputFile==null || outputFile.length()>MAX_LOG_FILE_SIZE) {
			getNewFile();
		}
	}
	
	/**
	 * Check the length of the file and eventually creates
	 * a new one.
	 * 
	 */
	private void getNewFile() {
		closeFile();
		String now=IsoDateFormat.formatCurrentDate();
		String fileName=extension+"_"+now+".dat";
		outputFile= new File(fileName);
		FileOutputStream fos;
		outStream=null;
		try {
			fos= new FileOutputStream(outputFile);
		} catch (Throwable t) {
			outputFile=null;
			closed=true;
			return;
		}
		outStream=new PrintStream(fos);
		if (header!=null && !header.isEmpty()) {
			outStream.print(header);
		}
	}
	
	/**
	 * Close the current file
	 */
	private void closeFile() {
		if (outputFile!=null) {
			try {
				outStream.flush();
				outStream.close();
			} catch (Throwable t) {
				// Nothing to do... log a message
				System.err.println("Error closing file: "+t.getMessage());
				t.printStackTrace(System.err);
			} finally {
				outStream=null;
				outputFile=null;
			}
		}
	}

	/**
	 * Write the string in the file.
	 * 
	 * @param s The string to append to the file
	 */
	public synchronized void put(String s) {
		if (s==null || s.isEmpty() || closed) {
			return;
		}
		outStream.print(s);
		if (++currentWritingOps>=CHECK_INTERVAL) {
			checkFile();
			currentWritingOps=0;
		}
	}
	
	/**
	 * Close the object in order to reject any further
	 * writing request and release the file.
	 */
	public synchronized void close() {
		closed=true;
		closeFile();
	}
}
