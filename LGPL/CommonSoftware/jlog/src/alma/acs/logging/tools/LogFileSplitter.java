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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.text.FieldPosition;
import java.text.SimpleDateFormat;
import java.util.Date;

import com.cosylab.logging.engine.ACS.ACSRemoteErrorListener;
import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogField;

import alma.acs.logging.engine.io.IOHelper;
import alma.acs.logging.engine.io.IOPorgressListener;
import alma.acs.logging.engine.parser.ACSLogParser;
import alma.acs.logging.engine.parser.ACSLogParserFactory;
import alma.acs.util.IsoDateFormat;

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
public class LogFileSplitter implements ACSRemoteLogListener, ACSRemoteErrorListener, IOPorgressListener {

	/**
	 * The name of the input files
	 */
	private String[] inFileNames;
	
	/**
	 * The name for the output files
	 * <P>
	 * A progressive number/starting date is appended at the end of each name
	 */
	private String destFileName;
	
	/**
	 * The progressive number appended at the end of each generated file
	 */
	private int index=0;
	
	/**
	 * The number of logs in each splitted file
	 */
	private Integer number=null;
	
	/**
	 * The time frame of the logs in each splitted file (in msec)
	 */
	private Integer time=null;
	
	/**
	 * Counts the number of logs read (needed for number criteria)
	 */
	private int logsRead=0;
	
	/**
	 * The date of the first log in the current output file (needed for time criteria)
	 */
	private long firstLogDate=-1;
	
	/**
	 * The size of the buffer for writing 
	 */
	private final int OUTPUT_BUFFER_SIZE=8192;
	
	/**
	 * The writer to write the destination files
	 */
	private BufferedWriter outF=null;
	
	/**
	 * The format of the date in the name of the file
	 */
	private SimpleDateFormat dateFormat = new IsoDateFormat();
	
	/**
	 * The converter to format the log before saving
	 */
	private final LogConverter converter;
	
	/**
	 * Constructor
	 * 
	 * @param inputFiles The files of log to read
	 * @param outputFiles The names of the files created splitting
	 * @param num The number of logs per file (can be null)
	 * @param mins The minutes of the logs per file (can be null)
	 * @param converter The converter to format the logs before saving
	 */
	public LogFileSplitter(
			String[] inputFiles, 
			String outputFiles, 
			Integer num, 
			Integer mins,
			LogConverter converter) {
		if (outputFiles==null) {
			throw new IllegalArgumentException("The dest file name can't be null");
		}
		if (inputFiles!=null && inputFiles.length==0) {
			throw new IllegalArgumentException("No source files");
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
		if (converter==null) {
			throw new IllegalArgumentException("The converter can't be null");
		}
		this.converter=converter;
		inFileNames=inputFiles;
		destFileName=outputFiles;
		number=num;
		if (mins!=null) {
			time=mins*60*1000;
		}
	}
	
	
	/**
	 * Split the input file
	 * @throws Exception in case of errors while splitting
	 */
	public void split() throws Exception {
		IOHelper ioHelper = new IOHelper();
		if (inFileNames==null) {
			// Read from stdin
			BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
			ioHelper.loadLogs(in, this, null, this, this);
		} else {
			for (String inFileName: inFileNames) {
				System.out.println("Processing "+inFileName);
				ioHelper.loadLogs(inFileName, this, null, this, this);
			}
		}
		if (outF!=null) {
			closeOutputFile(outF);
		}
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
			StringBuffer buffer = new StringBuffer();
			dateFormat.format(startingDate,buffer, new FieldPosition(0));
			name.append(buffer.toString()); 
		}
		// Add the extension
		if (converter instanceof XMLConverter) {
			name.append(".xml");
		} else {
			name.append(".txt");
		}
		// Create and return the file
		FileWriter outFile=null;
		try {
			outFile = new FileWriter(name.toString(),false);
		} catch (IOException e) {
			System.err.print("Error creating output file "+name.toString()+": ");
			System.err.print(e.getMessage());
			System.exit(-1);
		}
		System.out.println("Writing logs on "+name);
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


	/**
	 * @see alma.acs.logging.engine.io.IOPorgressListener#bytesRead(long)
	 */
	@Override
	public void bytesRead(long bytes) {}


	/**
	 * @see alma.acs.logging.engine.io.IOPorgressListener#bytesWritten(long)
	 */
	@Override
	public void bytesWritten(long bytes) {}


	/**
	 * @see alma.acs.logging.engine.io.IOPorgressListener#logsRead(int)
	 */
	@Override
	public void logsRead(int numOfLogs) {}


	/**
	 * @see alma.acs.logging.engine.io.IOPorgressListener#logsWritten(int)
	 */
	@Override
	public void logsWritten(int numOfLogs) {}


	/**
	 * @see com.cosylab.logging.engine.ACS.ACSRemoteErrorListener#errorReceived(java.lang.String)
	 */
	@Override
	public void errorReceived(String xml) {
		System.err.println("Error parsing the following: ["+xml+"]");
	}


	/**
	 * @see com.cosylab.logging.engine.ACS.ACSRemoteLogListener#logEntryReceived(com.cosylab.logging.engine.log.ILogEntry)
	 */
	@Override
	public void logEntryReceived(ILogEntry logEntry) {
		if (number!=null ) {
			// Number criteria
			if (outF==null || ++logsRead>number) {
				closeOutputFile(outF);
				outF=getOutputFile(destFileName,index++,null);
				logsRead=1;
			}
		} else {
			// Time criteria
			long logDate = ((Long)logEntry.getField(LogField.TIMESTAMP));
			if (firstLogDate==-1 || logDate-firstLogDate>time) {
				firstLogDate=logDate;
				closeOutputFile(outF);
				outF=getOutputFile(destFileName,index++,new Date(logDate));
			}
		}
		try {
			outF.write(converter.convert(logEntry));
		} catch (IOException e) {
			System.err.println("Error writing a log: " + e.getMessage());
			System.exit(-1);
		}
	}
}
