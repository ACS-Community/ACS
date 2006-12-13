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
package alma.acs.logging.tools;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;

import com.cosylab.logging.engine.ACS.ACSLogParser;
import com.cosylab.logging.engine.ACS.ACSLogParserDOM;
import com.cosylab.logging.engine.ACS.ACSRemoteRawLogListener;import com.cosylab.logging.engine.log.ILogEntry;

/**
 * An object to split a log file in several other shorter file.
 * The criteria is the number of logs or the time (in minutes)
 * 
 * A progressive number is appended to the name of the destination
 * for each created file.
 * If the time is the criteria to split the input file, the 
 * the date of the first log is appended to the name of the file too.
 * 
 * @author acaproni
 *
 */
public class LogFileSplitter implements ACSRemoteRawLogListener, AsynchronousOperationListener {

	// The name of the input file
	private String inFileName;
	
	// The name for the output files 
	// A progressive number/starting date is appended at the end of each name
	private String destFileName;
	
	// The progressive number appended at the end of each generated file
	private int index=0;
	
	// The number of logs in each splitted file
	private Integer number=null;
	
	// The time frame of the logs in each splitted file (in msec)
	private Integer time=null;
	
	// Counts the number of logs read (needed for number criteria)
	private int logsRead=0;
	
	// The date of the first log in the current output file (needed for time criteria)
	private long firstLogDate=-1;
	
	// The reader for the file of logs
	private LogFile logReader;
	
	// The parser to translate XML logs into ILogEntry
	private ACSLogParser parser = null;
	
	// The writer to write the dstination files
	private final int OUTPUT_BUFFER_SIZE=8192;
	private BufferedWriter outF=null;
	
	// The format of the date in the name of the file
	public static final String TIME_FORMAT = "yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'SSS";
	private SimpleDateFormat dateFormat = new SimpleDateFormat(TIME_FORMAT);
	
	/**
	 * Constructor
	 * 
	 * @param inputFile The file of log to read
	 * @param outputFiles The names of the files created splitting
	 * @param num The number of logs per file (can be null)
	 * @param mins The minutes of the logs per file (can be null)
	 */
	public LogFileSplitter(String inputFile, String outputFiles, Integer num, Integer mins) {
		if (inputFile==null || outputFiles==null) {
			throw new IllegalArgumentException("The sorce and dest file name can't be null");
		}
		if (num==null && mins==null) {
			throw new IllegalArgumentException("Missing criteria (num and time are null)");
		}
		if (num!=null && mins!=null) {
			throw new IllegalArgumentException("Number and time criteria requested");
		}
		if (num!=null && num<=0) {
			throw new IllegalArgumentException("Invalid number "+num);
		}
		if (mins!=null && mins<=0) {
			throw new IllegalArgumentException("Invalid minutes "+num);
		}
		if (num!=null && num<50000) {
			System.out.println("Warning splitting for less then 50000 logs can create a big number of files");
		}
		if (mins!=null) {
			// Create the parser
			try {
				parser = new ACSLogParserDOM();
			} catch (Exception e) {
				System.err.println("Error creating the parser: "+e.getMessage());
				System.exit(-1);
			}
		}
		inFileName=inputFile;
		destFileName=outputFiles;
		number=num;
		if (mins!=null) {
			time=mins*60*1000;
		}
		
		logReader = new LogFile(this,this);
	}
	
	
	/**
	 * Split the input file
	 * @throws Exception in case of errors while splitting
	 */
	public void split() throws Exception {
		LogFile.IOAction action = logReader.getLoadAction(inFileName);
		logReader.submitAsyncAction(action);
		synchronized(this) {
			wait();
		}
		action = logReader.getTerminationAction();
		logReader.submitAsyncAction(action);
	}
	
	/**
	 * Exceuted when a new log has been read
	 * 
	 * @param xmlLogString The XML log read
	 */
	public void xmlEntryReceived(String xmlLogString) {
		if (number!=null ) {
			// Number criteria
			if (outF==null || ++logsRead>number) {
				closeOutputFile(outF);
				outF=getOutputFile(destFileName,index++,null);
				logsRead=1;
			}
		} else {
			// Time criteria
			ILogEntry log = null;
			try {
				log = parser.parse(xmlLogString);
			} catch (Exception e) {
				System.err.println("Error parsing a log: "+e.getMessage());
				System.err.println("The log that caused the exception: "+xmlLogString);
				System.exit(-1);
			}
			long logDate = ((Date)log.getField(ILogEntry.FIELD_TIMESTAMP)).getTime();
			if (firstLogDate==-1 || logDate-firstLogDate>time) {
				firstLogDate=logDate;
				closeOutputFile(outF);
				outF=getOutputFile(destFileName,index++,(Date)log.getField(ILogEntry.FIELD_TIMESTAMP));
			}
		}
		try {
			outF.write(xmlLogString);
		} catch (IOException e) {
			System.err.println("Error writing a log: "+e.getMessage());
			System.exit(-1);
		}
	}
	
	public void operationTerminated(int id) {
		synchronized(this) {
			notifyAll();
		}
	}
	
	public void operationStarted(int ID) {
	}
	
	public void errorDetected(Exception e, int ID) {
		e.printStackTrace();
	}
	
	public void operationProgress(long start, long end, long current, int ID) {
		System.out.println("Bytes read: "+current+"\t"+(end-current)+" to go");
	}
	
	/**
	 * Create a new file for output.
	 * 
	 * @param dest The name of the destination file
	 * @param index The index to append to the name
	 * @param startingDate The date of the first log to append to the
	 *                     name of the file
	 *                     It can be null.
	 *                     
	 * @return The writer for output
	 */
	private BufferedWriter getOutputFile(String dest, int idx, Date startingDate) {
		// Build the name of the file
		StringBuilder name = new StringBuilder(dest);
		name.append('-');
		name.append(idx);
		if (startingDate!=null) {
			name.append('-');
			name.append(startingDate.getTime()); 
		}
		name.append(".xml");
		// Create and return the file
		FileWriter outFile=null;
		try {
			outFile = new FileWriter(name.toString(),false);
		} catch (IOException e) {
			System.err.print("Error creating output file "+name.toString()+": ");
			System.err.print(e.getMessage());
			System.exit(-1);
		}
		return new BufferedWriter(outFile,OUTPUT_BUFFER_SIZE);
	}
	
	/**
	 * Flush and close the file
	 * 
	 * @param file The file to close
	 */
	private void closeOutputFile(BufferedWriter file) {
		//	Flush and close the old file
		if (file!=null) {
			try {
				file.flush();
				file.close();
				file=null;
			} catch (IOException e) {
				System.err.print("Error closing output file: ");
				System.err.print(e.getMessage());
				System.exit(-1);
			}
		}
	}
}
