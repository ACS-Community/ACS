/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.logging.engine.io;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Collection;
import java.util.Iterator;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import javax.swing.JOptionPane;

import alma.acs.logging.engine.parser.ACSLogParser;
import alma.acs.logging.engine.parser.ACSLogParserFactory;
import alma.acs.util.StopWatch;

import com.cosylab.logging.engine.LogMatcher;
import com.cosylab.logging.engine.ACS.ACSRemoteErrorListener;
import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.ACS.ACSRemoteRawLogListener;
import com.cosylab.logging.engine.log.ILogEntry;

/**
 * An helper class to perform synchronous I/O operations like load and save.
 * <P>
 * Load and save methods are executed in a synchronous way i.e. they do not return
 * until the I/O is terminated (or an exception occurs).
 * <BR>
 * Intermediate results useful to monitor the progress of the I/O are communicated
 * to the listeners implementing the <code>IOProgressListener</code> interface.
 * <P>
 * The load and save methods of this class are <code>synchronized</code> but I would not say
 * that this class is thread safe because it does not hold and lock the objects it receives
 * as parameters like for example the <code>BufferReader</code> and the <code>BufferWriter</code>.
 * So thread safety must be ensured by the owner of such objects.
 * <P><B>Loading</B><BR>
 * The loading is performed through one of the overloaded <code>loadLogs</code> methods.
 * The bytes read and the number of the logs successfully read are sent to the listeners 
 * implementing the <code>IOProgressListener</code> interface.
 * If there are filters, an audience or a discard level defined, then each log is checked
 * before being sent to the listener.
 * 
 *  <P><B>Saving</B><BR>
 *  The saving of logs can be done by passing a <code>Collection </code> of logs or an <code>Iterator</code>to one of the 
 *  overloaded <code>saveLogs</code> methods.
 *  Such methods communicates the progress to the listener implementing the 
 *  <code>IOProgressListener</code> interface.
 *  <P>
 *  Saving logs by passing the name of the file does not require any extra steps.
 *  <BR>
 *  Saving logs by passing a <code>BufferedWriter</code> is always a three steps procedure:
 *  <OL>
 *  	<LI>call the <code>prepareSaveFile</code> to write XML header
 *  	<LI>save the logs by calling one of the <code>saveLogs</code> or the <code>saveLog</code>
 *  	<LI>execute <code>Save</code> to add the closing XML tags, flush and close the buffer
 *  </OL> 
 * <P>
 * Load and save can be very long operations. To stop an I/O, the <code>stopIO()</code>
 * must be executed.
 * 
 * @author acaproni
 *
 */
public class IOHelper extends LogMatcher {
	
	/** 
	 * Signal that a load or a save must be stopped
	 */
	protected volatile boolean stopped=false;
	
	/**
	 * The parser
	 */
	private ACSLogParser parser;
	
	/**
	 * Inject the log into the engine 
	 * 
	 * @param logStr The string representation of the log
	 * @param logListener The listener i.e. the callback for each new log to add
	 */
	private void injectLog (
			StringBuilder logStr, 
			ACSRemoteLogListener logListener, 
			ACSRemoteRawLogListener rawLogListener,
			ACSRemoteErrorListener errorListener) {
		if (errorListener==null || (logListener==null && rawLogListener==null)) {
			throw new IllegalArgumentException("Listeners can't be null");
		}
		if (logListener!=null) {
			ILogEntry log=null;
			try {
				log = parser.parse(logStr.toString().trim());
			} catch (Exception e) {
				errorListener.errorReceived(logStr.toString().trim());
				System.err.println("Exception parsing a log: "+e.getMessage()+" ["+logStr+"]");
				e.printStackTrace(System.err);
				return;
			}
			if (match(log)) {
				logListener.logEntryReceived(log);
			}
		}
		if (rawLogListener!=null) {
			rawLogListener.xmlEntryReceived(logStr.toString().trim());
		}
	}
	
	/**
	 * Load the logs from the file with the given name.
	 * <P>
	 * The logs are sent to the <code>ACSRemoteLogListener</code> and /or
	 * to the <code>ACSRemoteRawLogListener</code>.
	 *  
	 * @param fileName The name of the file to read logs from
	 * @param logListener The callback for each new log read from the IO
	 * @param rawLogListener The callback for each new XML log read from the IO
	 * @param errorListener The listener for errors
	 * @param progressListener The listener to be notified about the bytes read
	 * @param gzip If <code>true</code> the file to read is compressed in GZIP format
	 * @return The length of the file to read
	 * @throws IOException In case of an IO error while reading the file
	 * @throws Exception In case of error building the parser
	 */
	public synchronized long loadLogs(
			String fileName,
			ACSRemoteLogListener logListener,
			ACSRemoteRawLogListener rawLogListener,
			ACSRemoteErrorListener errorListener, 
			IOPorgressListener progressListener,
			boolean gzip) throws IOException, Exception {
		if (fileName==null || fileName.isEmpty()) {
			throw new IllegalArgumentException("Invalid file name: "+fileName);
		}
		if (progressListener==null) {
			throw new IllegalArgumentException("The progress listener can't be null");
		}
		File f = new File(fileName);
		InputStream inStream = new FileInputStream(f);
		if (gzip) {
			inStream= new GZIPInputStream(inStream);
		}
		loadLogs(inStream, logListener, rawLogListener, errorListener,progressListener);
		return f.length();
	}
	
	/**
	 * Load the logs from the given <code>BufferedReader</code>.
	 * <P>
	 * The logs are sent to the <code>ACSRemoteLogListener</code> and /or
	 * to the <code>ACSRemoteRawLogListener</code>.
	 *  
	 * @param fis The stream to read logs from
	 * @param logListener The callback for each new log read from the IO
	 * @param rawLogListener The callback for each new XML log read from the IO
	 * @param errorListener The listener for errors
	 * @param progressListener The listener to be notified about the bytes read
	 * @throws IOException In case of an IO error while reading the file
	 * @throws Exception In case of error building the parser
	 */
	public synchronized void loadLogs(
			InputStream fis,
			ACSRemoteLogListener logListener,
			ACSRemoteRawLogListener rawLogListener,
			ACSRemoteErrorListener errorListener, 
			IOPorgressListener progressListener) throws IOException, Exception {
		if (fis==null || errorListener==null) {
			throw new IllegalArgumentException("Parameters can't be null");
		}
		if (logListener==null && rawLogListener==null) {
			throw new IllegalArgumentException("No log listeners defined");
		}
		if (progressListener==null) {
			throw new IllegalArgumentException("The progress listener can't be null");
		}
		
		// Build the parser
		if (parser == null && logListener!=null) {
			parser = ACSLogParserFactory.getParser();
		}
		
		stopped=false;
		
		// The "clever" buffer
		LogStringBuffer buffer = new LogStringBuffer();
		
		// Read one char per iteration
		int chRead;
		
		// Count the bytes read
		int bytesRead=0;
		
		int logRecordsRead = 0;
		
		// Here the buffer writes the XML of each log
		StringBuilder xmlStr = new StringBuilder();
		
		/**
		 * The size of the buffer
		 */
		final int size=16384;
		
		/** 
		 * The buffer of data read from the file
		 */
		byte[] buf =new byte[size];
		
		/**
		 * The cursor to scan the buffer (circular)
		 */
		int actualPos=-1;
		
		/**
		 * When it is 0, then we have to read another block from the file
		 */
		int bytesInBuffer=0;
		
		try {
			StopWatch stopWatch = new StopWatch();
		
			while (true && !stopped) {
				// Read a block from the file if the buffer is empty
				if (bytesInBuffer==0) {
					bytesInBuffer = fis.read(buf,0,size);
				}
				if (bytesInBuffer<=0) { // EOF
					break;
				}
				bytesInBuffer--;
				actualPos=(actualPos+1)%size;
				chRead=buf[actualPos];
				
				bytesRead++;
				buffer.append((char)chRead,xmlStr);
				if (xmlStr.length()>0) {
					// A new log has been found
					injectLog(xmlStr,logListener, rawLogListener, errorListener);
					logRecordsRead++;
					xmlStr.delete(0, xmlStr.length());
					progressListener.bytesRead(bytesRead);
					if (logRecordsRead%25==0) {
						progressListener.logsRead(logRecordsRead);
					}
				}
			}
			System.out.println("XML log record import finished with " + logRecordsRead + " records in " + 
						stopWatch.getLapTimeMillis()/1000 + " seconds.");
		} catch (IOException ioe) {
			System.err.println("Exception loading the logs: "+ioe.getMessage());
			ioe.printStackTrace(System.err);
			JOptionPane.showMessageDialog(null, "Exception loading "+ioe.getMessage(),"Error loading",JOptionPane.ERROR_MESSAGE);
		}
	}
	
	/**
	 * Write the XML header in the buffered writer
	 * 
	 * @param bw
	 * @throws IOException
	 */
	public synchronized void writeHeader(OutputStream stream) throws IOException {
		if (stream==null) {
			throw new IllegalArgumentException("The BufferedWriter can't be null");
		}
		String header = new String("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n<Header Name=\"NameForXmlDocument\" Type=\"LOGFILE\" />\n<Log>\n");
		stream.write(header.getBytes());
	}
	
	/**
	 * Terminate the saving.
	 * <P>
	 * This method must executed when the save terminates.
	 * It does the following:
	 * <UL>
	 * 	<LI>write the closing XML tags
	 *  <LI>flush the output file
	 *  <li>close the file
	 * </UL>
	 * @param outBW The file to close
	 * @param close If <code>true</code> the <code>BufferedWriter</code> is closed
	 * @throws IOException In case of an IO error
	 */
	public synchronized void terminateSave(OutputStream outBW, boolean close) throws IOException {
		outBW.write((new String("</Log>")).getBytes());
		outBW.flush();
		if (close) {
			outBW.close();
		}
	}
	
	/** 
	 * Save a log in the passed file
	 * 
	 * @param outF The buffered writer where the logs have to be stored
	 * @param log The log to save
	 * @param progressListener The listener to be notified about the bytes written
	 * @throws IOException In case of an IO error while writing logs into the file
	 */
	public synchronized int saveLog(OutputStream outF, ILogEntry log) throws IOException {
		if (outF==null) {
			throw new IllegalArgumentException("BufferedWriter can't be null");
		}
		if (log==null) {
			throw new IllegalArgumentException("The log can't be null");
		}
		byte[] bytes=(log.toXMLString()+"\n").getBytes();
		outF.write(bytes);
		return bytes.length;
	}
	
	/**
	 * Save a collection of logs on disk
	 * 
	 * @param fileName The name of the file to store logs into
	 * @param logs The non empty collection of logs to save
	 * @param progressListener The listener to be notified about the number of bytes written
	 * @param append <UL><LI>if <code>true</code> if the logs in the collection must be appended to an existing file</LI>
	 *               <LI>if <code>false</code> and the file aredy exists, it is deleted before writing 
	 *               </UL>
	 * @param gzip If <code>true</code> the file is compressed (GZIP) 
	 * @throws IOException In case of error writing
	 */
	public synchronized void saveLogs(String fileName, Collection<ILogEntry> logs, IOPorgressListener progressListener, boolean append, boolean gzip) throws IOException {
		if (logs==null || logs.isEmpty()) {
			throw new IllegalArgumentException("No logs to save");
		}
		if (progressListener==null) {
			throw new IllegalArgumentException("The progress listener can't be null");
		}
		Iterator<ILogEntry> iterator = logs.iterator();
		saveLogs(fileName, iterator, progressListener,append,gzip);
	}
	
	/**
	 * Save a collection of logs on a <code>BufferedWriter</code>.
	 * <P>
	 * The buffer must be initialized and terminated i.e. the <code>prepareSaveFile</code>
	 * and the <code>terminateSave</code> are not executed by this method.
	 * 
	 * @param outFile The file to write logs into
	 * @param logs The non empty collection of logs to save
	 * @param progressListener The listener to be notified about the number of bytes written
	 * @param gzip If <code>true</code> the file is compressed (GZIP)
	 * @throws IOException In case of error writing
	 */
	public synchronized void saveLogs(FileOutputStream outFile, Collection<ILogEntry> logs, IOPorgressListener progressListener) throws IOException {
		if (logs==null || logs.isEmpty()) {
			throw new IllegalArgumentException("No logs to save");
		}
		if (progressListener==null) {
			throw new IllegalArgumentException("The progress listener can't be null");
		}
		Iterator<ILogEntry> iterator = logs.iterator();
		saveLogs(outFile, iterator, progressListener);
	}
	
	/**
	 * Save the logs available through an <code>Iterator</code>
	 * 
	 * @param filename The name of the file to write logs into
	 * @param logs The non empty collection of logs to save
	 * @param progressListener The listener to be notified about the number of bytes written
	 * @param append <UL><LI>if <code>true</code> if the logs in the collection must be appended to an existing file</LI>
	 *               <LI>if <code>false</code> and the file already exists, it is deleted before writing 
	 *               </UL> 
	 * @param gzip If <code>true</code> the file is compressed (GZIP)    
	 * @throws IOException In case of error writing
	 */
	public synchronized void saveLogs(String fileName, Iterator<ILogEntry>iterator, IOPorgressListener progressListener, boolean append, boolean gzip) throws IOException {
		if (iterator==null || !iterator.hasNext()) {
			throw new IllegalArgumentException("No logs to save");
		}
		if (progressListener==null) {
			throw new IllegalArgumentException("The progress listener can't be null");
		}
		OutputStream outStream=new FileOutputStream(fileName,append);
		if (gzip) {
			outStream = new GZIPOutputStream(outStream);
		}
		writeHeader(outStream);
		saveLogs(outStream, iterator,progressListener);
		terminateSave(outStream,true);
	}
	
	/**
	 * Save a collection of logs on a <code>BufferedWriter</code>.
	 * <P>
	 * The buffer must be initialized and terminated i.e. the <code>prepareSaveFile</code>
	 * and the <code>terminateSave</code> are not executed by this method.
	 * 
	 * @param outF The buffer to write logs into
	 * @param logs The non empty collection of logs to save
	 * @param progressListener The listener to be notified about the number of bytes written
	 * @throws IOException In case of error writing
	 */
	public synchronized void saveLogs(OutputStream outF, Iterator<ILogEntry> iterator, IOPorgressListener progressListener) throws IOException {
		if (iterator==null || !iterator.hasNext()) {
			throw new IllegalArgumentException("No logs to save");
		}
		if (progressListener==null) {
			throw new IllegalArgumentException("The progress listener can't be null");
		}
		stopped=false;
		long len=0; 
		int logsWritten=0;
		while (iterator.hasNext() && !stopped) {
			ILogEntry log = iterator.next();
			len+=saveLog(outF, log);
			progressListener.bytesWritten(len);
			progressListener.logsWritten(++logsWritten);
		}
	}
	
	/**
	 * Call this method if you wish to interrupt a load or a save.
	 * <P>
	 * Load and save are executed synchronously so this method has to be called by a separate 
	 * thread.
	 * A typical example is the "Abort" button of a dialog: when the user presses such a button, this method
	 * is invoked by the swing thread.
	 */
	public void stopIO() {
		stopped=true;
	}
}
