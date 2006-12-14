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
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Date;

import com.cosylab.logging.engine.FiltersVector;
import com.cosylab.logging.engine.ACS.ACSLogParser;
import com.cosylab.logging.engine.ACS.ACSLogParserDOM;
import com.cosylab.logging.engine.ACS.ACSRemoteRawLogListener;
import com.cosylab.logging.engine.log.ILogEntry;

/**
 * An object to extract the logs that matches the given criteria.
 * It reads the input file and parse each log and writes in the output 
 * all the logs matching the criteria.
 * It is possible to specify a start and/or end date or pass the name 
 * of a filter to load. The filters are the same used by jlog so
 * jlog can be used to edit and save such file.
 * 
 * @author acaproni
 *
 */
public class LogFileExtractor implements ACSRemoteRawLogListener, AsynchronousOperationListener {
	
	// The start and end date of the logs in millisec.
	// If one of them is -1, the test is assumed passed
	// (i.e. the date is not checked)
	private long start=-1;
	private long end=-1;
	
	// The name of the file of filter
	private String filterFileName=null;
	
	// The filters
	private FiltersVector filters = null;
	
	// The name of the file for reading
	private String inFileName;
	
	// The name of the output file
	private String destFileName;
	
	// The reader for the file of logs
	private LogFile logReader;
	
	// The parser to translate XML logs into ILogEntry
	private ACSLogParser parser = null;
	
	// The writer to write the dstination files
	private final int OUTPUT_BUFFER_SIZE=8192;
	private BufferedWriter outF=null;
	
	// If true the output is written as CSV
	private boolean writeAsCSV = false;
	
	// The converter from ILogEntry to CSV
	CSVConverter csv;
	
	// The fields and thier positions in the CSV
	private String cols=null;
	
	/**
	 * Constructor
	 * The parameters defines the criteria. They can be null (but not all null of
	 * course).
	 * All the criteria are applied in AND.
	 * If the start/end date are not present, the dates are not checked.
	 * 
	 * @param inputFile The name of the file of logs for input
	 * @param outputFile The name of the file with the selected logs
	 * @param startDate The start date of logs (can be null)
	 * @param endDate The end date of the logs (can be null) 
	 * @param filterName The name of a file of filters to apply to select
	 *                   logs (can be null)
	 * @param csvFormat if true the output is written as CSV instead of XML
	 * @param cols The fields to write in the CSV
	 */
	public LogFileExtractor(
			String inputFile, 
			String outputFile, 
			Date startDate, 
			Date endDate, 
			String filterName,
			boolean csvFormat,
			String cols) {
		if (startDate==null && endDate==null && filterName==null) {
			throw new IllegalArgumentException("No criteria for extraction");
		}
		if (inputFile==null || outputFile==null) {
			throw new IllegalArgumentException("The source and destination files can't be null");
		}
		inFileName=inputFile;
		destFileName=outputFile;
		if (startDate!=null) {
			start=startDate.getTime();}
		
		if (endDate!=null) {
			end=endDate.getTime();
		}
		if (end<=start && end!=-1 && start!=-1) {
			throw new IllegalArgumentException("Start date greater then end date");
		}
		filterFileName=filterName;
		if (filterFileName!=null) {
			File f = new File(filterFileName);
			if (!f.canRead()) {
				throw new IllegalArgumentException(filterFileName+" is unreadable");
			}
			filters = new FiltersVector();
			filters.loadFilters(f,true,null);
		}
		writeAsCSV=csvFormat;
		if (writeAsCSV) {
			this.cols=cols;
			csv = new CSVConverter(cols);
		}
		
		// Create the parser
		try {
			parser = new ACSLogParserDOM();
		} catch (Exception e) {
			System.err.println("Error creating the parser: "+e.getMessage());
			System.exit(-1);
		}
		logReader = new LogFile(this,this);
	}
	
	/**
	 * Exceuted when a new log has been read
	 * 
	 * @param xmlLogString The XML log read
	 */
	public void xmlEntryReceived(String xmlLogString) {
		boolean matches=true;
		ILogEntry log=null;
		try { 
			log= parser.parse(xmlLogString);
		} catch (Exception e) {
			System.err.println("Error parsing a log: "+e.getMessage());
			System.err.println("The log that caused the exception: "+xmlLogString);
			System.exit(-1);
		}
		if (start!=-1 || end!=-1) {
			matches = checkDate(log);
		}
		if (matches && filters!=null) {
			matches = filters.applyFilters(log);
		}
		if (matches) {
			if (writeAsCSV) {
				try {
					outF.write(
							csv.convert(log));
				} catch (IOException e) {
					System.err.println("Error writing a log: "+e.getMessage());
					System.exit(-1);
				}
			} else {
				try {
					outF.write(xmlLogString);
				} catch (IOException e) {
					System.err.println("Error writing a log: "+e.getMessage());
					System.exit(-1);
				}
			}
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
	}

	/**
	 * / Open the destination file
	 * 
	 * @return The file for output
	 */
	private BufferedWriter openDestFile() {
		if (destFileName.length()==0) {
			throw new IllegalArgumentException("Wrong dest file name");
		}
		String temp = destFileName.toLowerCase();
		if (!temp.endsWith(".xml") && !writeAsCSV) {
			destFileName=destFileName.concat(".xml");
		}
		FileWriter outFile=null;
		try {
			outFile = new FileWriter(destFileName,false);
		} catch (IOException e) {
			System.err.print("Error creating output file "+destFileName+": ");
			System.err.print(e.getMessage());
			System.exit(-1);
		}
		return new BufferedWriter(outFile,OUTPUT_BUFFER_SIZE);
	}
	
	/**
	 * Extract the logs from the isource to the destination
	 * applying the selection criteria given in the
	 * constructor
	 */
	public void extract() throws Exception {
		outF=openDestFile();
		// Sttart the loading
		LogFile.IOAction action = logReader.getLoadAction(inFileName);
		logReader.submitAsyncAction(action);
		synchronized(this) {
			wait();
		}
		// Terminate the thread
		action = logReader.getTerminationAction();
		logReader.submitAsyncAction(action);
		// Flush and close the output
		try {
			outF.flush();
			outF.close();
		} catch (IOException e) {
			System.err.println("Error closing the destination: "+e.getMessage());
		}
	}
	
	/**
	 * Check if the date of the log is between the requested start and
	 * end date
	 * 
	 * @param log The log to check
	 * @return true if the time stamp of the log is between the start and the
	 *              end date (inclusive)
	 */
	private boolean checkDate(ILogEntry log) {
		Date logDate = (Date)log.getField(ILogEntry.FIELD_TIMESTAMP);
		long date = logDate.getTime();
		boolean matches = true;
		matches = date>=start;
		if (matches && end!=-1) {
			matches = date<=end;
		}
		return matches;
	}
}
