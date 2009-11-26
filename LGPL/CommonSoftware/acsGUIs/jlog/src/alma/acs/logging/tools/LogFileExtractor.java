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
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Date;

import alma.acs.logging.engine.io.IOHelper;
import alma.acs.logging.engine.io.IOPorgressListener;
import alma.acs.logging.engine.parser.ACSLogParser;
import alma.acs.logging.engine.parser.ACSLogParserFactory;

import com.cosylab.logging.engine.FiltersVector;
import com.cosylab.logging.engine.ACS.ACSRemoteErrorListener;
import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogField;

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
public class LogFileExtractor implements ACSRemoteLogListener, ACSRemoteErrorListener, IOPorgressListener {
	
	// The start and end date of the logs in millisec.
	// If one of them is -1, the test is assumed passed
	// (i.e. the date is not checked)
	private long start=-1;
	private long end=-1;
	
	/**
	 * The name of the file of filter
	 */
	private String filterFileName=null;
	
	/**
	 * The filters
	 */
	private FiltersVector filters = null;
	
	/**
	 * The name of the files for reading
	 */
	private String[] inFileNames;
	
	/**
	 * The name of the output file
	 */
	private String destFileName;
	
	/**
	 * The size of the buffer for writing
	 */
	private final int OUTPUT_BUFFER_SIZE=8192;
	
	/**
	 * The writer to write the destination files
	 */
	private BufferedWriter outF=null;
	
	/**
	 * The converter to format the log before saving
	 */
	private final LogConverter converter;
	
	/**
	 * Constructor
	 * The parameters defines the criteria. They can be null (but not all null of
	 * course).
	 * All the criteria are applied in AND.
	 * If the start/end date are not present, the dates are not checked.
	 * 
	 * @param inputFiles The name of the files of logs for input
	 * @param outputFile The name of the file with the selected logs
	 * @param startDate The start date of logs (can be null)
	 * @param endDate The end date of the logs (can be null) 
	 * @param filterName The name of a file of filters to apply to select
	 *                   logs (can be null)
	 * @param converter The convert to save logs in different output formats
	 * @throws <code>Exception</code> In case of error
	 */
	public LogFileExtractor(
			String inputFiles[], 
			String outputFile, 
			Date startDate, 
			Date endDate, 
			String filterName,
			LogConverter converter)  throws Exception {
		if (startDate==null && endDate==null && filterName==null){
			throw new IllegalArgumentException("No criteria for extraction");
		}
		if (outputFile==null) {
			throw new IllegalArgumentException("The source can't be null");
		}
		if (inputFiles!=null && inputFiles.length==0) {
			throw new IllegalArgumentException("No source files");
		}
		if (converter==null) {
			throw new IllegalArgumentException("The converter can't be null");
		}
		this.converter=converter;
		inFileNames=inputFiles;
		destFileName=outputFile;
		// Add the extension to the outputfilename if not
		// already present
		String extension;
		if (converter instanceof XMLConverter) {
			extension=".xml";
		} else {
			extension=".txt";
		}
		if (!destFileName.toLowerCase().endsWith(extension)) {
			destFileName=destFileName+extension;
		}
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
	}

	/**
	 * Create the <code>BufferedWriter</code> and the <code>IOHelper</code>
	 * for writing logs.
	 * <P>
	 * This method uses a new <code>IOHelper</code> to open a new file 
	 * where the logs are saved into.
	 * It sets up:
	 * <UL>
	 * 	<LI>the <code>outHelper</code>
	 *  <LI>the <code>outF</code>
	 * </UL>
	 */
	private void openDestFile() {
		if (destFileName.length()==0) {
			throw new IllegalArgumentException("Wrong dest file name");
		}
		try {
			outF = new BufferedWriter(new FileWriter(destFileName));
		} catch (Exception e) {
			System.err.println("Error creating a file for saving named "+destFileName);
			e.printStackTrace(System.err);
			System.exit(-1);
		}
	}
	
	/**
	 * Extract the logs from the source to the destination
	 * applying the selection criteria given in the
	 * constructor
	 */
	public void extract() throws Exception {
		IOHelper inputHelper = new IOHelper();
		openDestFile();
		// Start the loading
		if (inFileNames==null) {
			// Read from stdin
			BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
			inputHelper.loadLogs(in, this, null, this, this);
		} else for (String inFileName: inFileNames) {
			System.out.println("Processing "+inFileName);
			inputHelper.loadLogs(inFileName, this, null, this, this);
		}
		// Flush and close the output
		outF.flush();
		outF.close();
		outF=null;
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
		Long date = (Long)log.getField(LogField.TIMESTAMP);
		boolean matches = true;
		matches = date>=start;
		if (matches && end!=-1) {
			matches = date<=end;
		}
		return matches;
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
	 */
	@Override
	public void errorReceived(String xml) {
		System.err.println("Error with the following: "+xml);
	}

	/**
	 * @see com.cosylab.logging.engine.ACS.ACSRemoteLogListener#logEntryReceived(com.cosylab.logging.engine.log.ILogEntry)
	 */
	@Override
	public void logEntryReceived(ILogEntry logEntry) {
		boolean matches=true;
		if (start!=-1 || end!=-1) {
			matches = checkDate(logEntry);
		}
		if (matches && filters!=null) {
			matches = filters.applyFilters(logEntry);
		}
		if (matches) {
			try {
				outF.write(converter.convert(logEntry));
			} catch (IOException e) {
				System.err.println("Error writing a log: "+e.getMessage());
				System.exit(-1);
			}
		}
	}
}
