/*
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2008 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
package alma.acs.logging.dialogs.error;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Timer;
import java.util.TimerTask;

/**
 * <code>ErrorLogFile</code> encapsulates a file adding methods to close
 * the file when it has not been since used for long time.
 * <P>
 * In case of error while writing or creating the temporary file, 
 * the behavior depends on the value of <code>retryOnError</code>.
 * If it is <code>true</code>.
 * <P>
 * The purpose of this class is to avoid having a file open when no I/O
 * is performed for a long time.
 * This is achieved:
 * <UL>
 * 	<LI>by creating the file only when the first writing is requested
 * 	<LI>closing the file when no I/O is performed for a defined time interval
 * </UL> 
 * The used file is identified by its name;
 * 
 * @author acaproni
 *
 */
public class ErrorLogFile extends TimerTask {
	/**
	 * The timer to check the time before two
	 * flush.
	 */
	private Timer timer = new Timer("ErrorLogFile",true);
	
	/**
	 * The number of seconds before closing the file
	 */
	private final int timeout;
	
	/**
	 * The time when the last written has been performed.
	 * <P>
	 * This can be indirectly used to know if the file is empty as it happens
	 * in <code>copy()</code>.
	 */
	private long lastWriteTime=-1;
	
	/**
	 * If <code>true</code> the file will be deleted when the application terminates.
	 */
	private final boolean deleteOnExit;
	
	/**
	 * If <code>true</code> the application tries to  perform I/O in the file
	 * even if a previous attempt resulted in an error.
	 */
	private final boolean retryOnError;
	
	/**
	 * The prefix of the temporary file
	 */
	private final String prefix;
	
	/**
	 * The suffix of the temporary file
	 */
	private final String suffix;
	
	/**
	 * The folder where the temporary file must be written
	 */
	private final String folder;
	
	/**
	 * The name of the file for I/O.
	 */
	private String fileName;
	
	/**
	 * The file stream for output.
	 */
	private FileOutputStream outFile=null;
	
	/**
	 * Remember if there was an error associated to the temporary file for 
	 * flushing errors.
	 * <P>
	 *The behavior of object of this class when an error happens depends
	 *on the value of <code>retryonError</code>.
	 * 
	 */
	private boolean fileError=false;
	
	/**
	 * <code>true</code> if the object has been closed
	 */
	private boolean closed=false;

	
	/**
	 * Constructor.
	 * 
	 * @param timeout The number of seconds before closing the file
	 * @param prefix The prefix of the temporary file
	 * @param suffix The suffix of the temporary file
	 * @param folder The name of the folder where the new file must be created;
	 * 					if it is <code>null</code> or empty, then the current 
	 * 					folder is used
	 * @param deleteOnExit if <code>true</code> the file s deleted when the
	 * 					application exits
	 * @param retryOnError if <code>true</code> tries to open the file even if 
	 * 					a previous attempt failed
	 */
	public ErrorLogFile(int timeout,
			String prefix, String suffix,
			String folder,
			boolean deleteOnExit,
			boolean retryOnError) {
		if (timeout<=0) {
			throw new IllegalArgumentException("The timeout msut be greater then 0");
		}
		if (prefix==null || prefix.length()==0) {
			throw new IllegalArgumentException("Invalid prefix");
		}
		if (suffix==null || suffix.length()==0) {
			throw new IllegalArgumentException("Invalid suffix");
		}
		if (folder==null || folder.length()==0) {
			this.folder=".";
		} else {
			this.folder=folder;
		}
		this.timeout=timeout;
		this.deleteOnExit=deleteOnExit;
		this.retryOnError=retryOnError;
		this.prefix=prefix;
		this.suffix=suffix;
		timer.schedule(this, 1000*timeout, 1000*timeout);
	}

	/**
	 * The method executed by the <code>Timer</code>.
	 * 
	 * @see {@link Timer}
	 */
	@Override
	public synchronized  void run() {
		if (lastWriteTime<0 || outFile==null) {
			// Nothing has been written in the file yet
			return;
		}
		if (lastWriteTime+timeout*1000<System.currentTimeMillis()) {
			// timeout elapsed ==>> close the output strem
			try {
				outFile.flush();
				outFile.close();	
			} catch (IOException e) {
				fileError=true;
			}
			outFile=null;
		}
	}
	
	/**
	 * Create the temporary file for flushing the log.
	 * 
	 * @throws IOException 
	 */
	private void initTmpFile() throws IOException {
		// Get a name for the file
		File tmpFile = File.createTempFile(prefix, suffix, new File(folder));
		if (deleteOnExit) {
			tmpFile.deleteOnExit();
		}
		fileName = tmpFile.getAbsolutePath();
	}
	
	/**
	 * Append a string to the temporary file.
	 * 
	 * @param str The string to append in the file
	 */
	public synchronized void append(String str) throws IOException {
		if (str==null || str.length()==0) {
			throw new IllegalArgumentException("The string to write can't be null nor empty");
		}
		if (closed) {
			return;
		}
		if (fileError && !retryOnError) {
			return;
		}
		if (fileName==null) {
			initTmpFile();
		}
		if (outFile==null) {
			outFile=new FileOutputStream(fileName,true);
		}
		outFile.write(str.getBytes());
		lastWriteTime=System.currentTimeMillis();
	}
	
	/**
	 * Copy the file in the passed output stream
	 * 
	 * @param file The stream to copy the content of the file into 
	 */
	public synchronized void copy(OutputStream file) throws FileNotFoundException, IOException {
		if (file==null) {
			throw new IllegalArgumentException("The file can't be null");
		}
		if (lastWriteTime<0) {
			// The file is empty
			return;
		}
		FileInputStream inF = new FileInputStream(fileName);
		
		byte[] buffer = new byte[512];
    	int bytesRead;
    	do {
   			bytesRead=inF.read(buffer);
    		if (bytesRead>0) {
   				file.write(buffer, 0, bytesRead);
    		}
    	} while (bytesRead>=0);
    	inF.close();
	}
	
	/**
	 * Close the file(s) freeing all the resources
	 * <P>
	 * This is the last method executed by this class 
	 */
	public synchronized void clear() {
		if (outFile!=null) {
			try {
				outFile.close();
			} catch (Throwable t) {
				System.err.println("Ignored error while clearing the temporary file "+fileName+": "+t.getMessage());
				t.printStackTrace();
			}
			outFile=null;
		}
		lastWriteTime=-1;
		fileName=null;
		fileError=false;
	}
	
	/**
	 * Flush and close the file when the object is destroyed by the GC
	 */
	@Override
	protected void finalize() throws Throwable {
		try {
			close();
		} finally {
			super.finalize();
		}
	}
	
	/**
	 * Close clear the file and stop the timer.
	 * <P>
	 * This is the last method to execute.
	 */
	public synchronized void close() {
		closed=true;
		timer.cancel();
		clear();
	}

	/**
	 * @return the fileName
	 */
	public synchronized String getFileName() {
		return fileName;
	}
	
}
