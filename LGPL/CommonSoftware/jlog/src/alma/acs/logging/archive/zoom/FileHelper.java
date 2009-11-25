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
package alma.acs.logging.archive.zoom;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import com.cosylab.logging.engine.Filter;
import com.cosylab.logging.engine.FiltersVector;
import com.cosylab.logging.engine.InvalidFilterConstraintException;
import com.cosylab.logging.engine.ACS.ACSRemoteErrorListener;
import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.LogField;

import alma.acs.logging.engine.io.IOHelper;
import alma.acs.logging.engine.io.IOPorgressListener;

/**
 * An helper glass to get logs from a file..
 * <P>
 * The class read a file with the help of {@link IOHelper} and adds
 * a filter for the start and end dates and another one for the levels.
 * 
 * @author acaproni
 *
 */
public class FileHelper implements ACSRemoteErrorListener {
	
	/**
	 * The min log level to read from the file (inclusive)
	 */
	public final LogTypeHelper minLogLevel;
	
	/**
	 * The max log level to read from the file (inclusive)
	 */
	public final LogTypeHelper maxLogLevel;
	
	/**
	 * The start time of the logs to read from file (>=0)
	 */
	public final long startTime;
	
	/**
	 * The end time of the logs to read from file
	 */
	public final long endTime;
	
	/**
	 * The file to read logs from
	 */
	private final File inputFile;
	
	/**
	 * The helper to read logs from a file
	 */
	private IOHelper ioHelper;
	
	/**
	 * Remembers if there were error while parsing logs read from the file.
	 * 
	 * @see loadLogs(...)
	 */
	private boolean errorParsingLogs=false;
	
	/**
	 * While loading logs, the caller can set a listener for errors.
	 * <P>
	 * This is the lister that receives the string of an XML log that returned an
	 * error while parsing.
	 * 
	 * @see {@link ACSRemoteErrorListener}
	 */
	private ACSRemoteErrorListener externalErrorListener=null;
	
	/**
	 * Constructor
	 * 
	 * @param inFile The file to read logs from
	 * @param start The start time (>=0)
	 * @param end The ending time
	 * @param lowlvl The lowest log level to read (inclusive)
	 * @param hiLvl The highest level to read (inclusive)
	 * 
	 * @throws IOException If the file is unreadable
	 */
	public FileHelper(File inFile, long start, long end, LogTypeHelper lowLvl, LogTypeHelper hiLvl) throws ZoomException {
		if (inFile==null) {
			throw new IllegalArgumentException("The file can't be null");
		} else {
			if (!inFile.canRead()) {
				throw new ZoomException(inFile.getAbsolutePath()+" is unreadable");
			}
		}
		if (end<start || start<0) {
			throw new IllegalArgumentException("Invalid time range ["+start+", "+end+"]");
		}
		if (lowLvl==null) {
			throw new IllegalArgumentException("The min log level can't be null");
		}
		if (hiLvl==null) {
			throw new IllegalArgumentException("The max log level can't be null");
		}
		if (lowLvl.ordinal()>hiLvl.ordinal()) {
			throw new IllegalArgumentException("Invalid log level range ["+lowLvl+", "+hiLvl+"]");
		}
		startTime=start;
		endTime=end;
		minLogLevel=lowLvl;
		maxLogLevel=hiLvl;
		inputFile=inFile;
	}
	
	/**
	 * Load the logs for the file.
	 * <P>
	 * This method return errors in two ways:
	 * <UL>
	 * 	<LI>with an exception when the error arise while setting up the file and the filters for loading 
	 * 		while reading the file
	 * 	<LI>returning <code>false</code> in case of errors parsing logs read from file
	 * </ul>
	 * <P>
	 * The error listener passed as parameter can be <code>null</code>. 
	 * In fact the error listener gets the strings of the logs that returned an error
	 * while parsing. If the caller is not interested in those strings but only to
	 * know if there were parsing errors then it will be enough for the caller to check
	 * the value of the returned boolean.
	 *  
	 * @param logListener The listener of the logs read from the file
	 * @param ioListener The listener for the progress of loading
	 * @param errorListener The listener of errors parsing logs (can'be <code>null</code>)
	 * @return <code>false</code> in case of errors loading logs;
	 * 			<code>true</code> otherwise  
	 * 
	 *  @throws ZoomException In case of errors creating filters
	 *  @throws FileNotFoundException If the file was not found
	 */
	public boolean loadLogs(
			ACSRemoteLogListener logListener, 
			IOPorgressListener ioListener, 
			ACSRemoteErrorListener errorListener) throws ZoomException, FileNotFoundException {
		ioHelper = new IOHelper();
		FiltersVector filters = setupFilters();
		ioHelper.setFilters(filters);
		errorParsingLogs=false;
		externalErrorListener=errorListener;
		FileReader fileReader = new FileReader(inputFile);
		BufferedReader reader = new BufferedReader(fileReader);
		try {
			ioHelper.loadLogs(reader, logListener, null, this, ioListener);
		} catch (Throwable t) {
			throw new ZoomException("Error loading logs from file",t);
		}
		
		return !errorParsingLogs;
	}
	
	/**
	 * Create the filters for loading logs;:
	 * <UL>
	 * 	<LI>time range
	 * 	<LI>min and max log levels
	 * </UL>
	 * @return
	 */
	private FiltersVector setupFilters() throws ZoomException {
		FiltersVector filters = new FiltersVector();
		Filter dateFilter=null;
		Filter levelFilter=null;
		try {
			dateFilter = new Filter(LogField.TIMESTAMP,false,startTime,endTime,false);
			levelFilter = new Filter(LogField.ENTRYTYPE,false,minLogLevel.ordinal(),maxLogLevel.ordinal(),false);
		} catch (InvalidFilterConstraintException e) {
			throw new ZoomException("Error setting the filters", e);
		}
		filters.addFilter(dateFilter,true);
		filters.addFilter(levelFilter,true);
		return filters;
	}

	/**
	 * 
	 * @see com.cosylab.logging.engine.ACS.ACSRemoteErrorListener#errorReceived(java.lang.String)
	 */
	@Override
	public void errorReceived(String xml) {
		errorParsingLogs=true;
		if (externalErrorListener!=null) {
			externalErrorListener.errorReceived(xml);
		}
	}
	
	/**
	 * Stop loading logs.
	 * <P>
	 * <code>stopLoading</code> does nothing if no load is in progress.
	 */
	public void stopLoading() {
		if (ioHelper!=null) {
			ioHelper.stopIO();
		}
	}
}
