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

import java.io.File;
import java.io.FilenameFilter;
import java.text.ParseException;
import java.util.Date;

import alma.acs.util.IsoDateFormat;

import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.log.LogTypeHelper;

/**
 * <code>FilesManager</code> organizes the files for the zooming:
 * <UL>
 * 	<LI>owns a list of files of logs 
 * 	<LI>return the right file(s) for zooming by date/time interval
 * </UL>
 * <P>
 * The folder to access files in this version is read from a java property.
 * In future releases it could be read out of the CDB.
 * <P> 
 * <I>Implementation note</i>: To reduce memory usage, objects of this class
 * 				do not read the list of files in the folder until a request of loading
 * 				is issued.
 * 
 * 				<BR>
 * 				File names have the following format: <code>logOutput&lt;StartDate&gt;--&lt;EndDate&gt;.xml</code>
 * 
 * <P>
 * ARCHIVE does not ensure that the logs in a file are only those having the timestamp 
 * between the start and the end date in the name of the file.
 * This means that
 * <UL>
 * 	<LI>while zooming jlog should check not only in the files with the start and end date
 * 		that are part of the select interval but also on other files near to the requested interval
 * 		<BR>What does it mean <i>near</i> is something not clear at the present.
 * 	<LI>a filter should be applied while reading files to discard logs that are
 * 		out of the time interval
 * </UL>
 * Current version bases the selection of the files on the requested interval and the
 * timestamp in the name of the files i.e. there is a chance to miss logs.
 * 
 * 
 * @author acaproni
 *
 */
public class FilesManager {
	
	/**
	 * The filter for the file names.
	 * <P>
	 * The file names have the following format: <code>logOutput&lt;StartDate&gt;--&lt;EndDate&gt;.xml</code>.
	 * like for example <code>logOutput2008-09-19T11_21_50.115--2008-09-19T11_21_51.637.xml</code>.
	 * <P>
	 * The file names accepted by this filter are all the file names with the format described above
	 * and whose start and end dates are in a given time interval, defined in the constructor.
	 * 
	 * 
	 * @author acaproni
	 *
	 */
	public class FileNameFilter implements FilenameFilter {
		
		/**
		 * The start name of each log file
		 */
		private static final String header="logOutput";
		
		/**
		 * The start date in the file name
		 */
		private final long start;
		
		/**
		 * The end date in the file name
		 */
		private final long end;
		
		/**
		 * The start and end dates to accept in the file name
		 * 
		 * @param start The start date (can be 0)
		 * @param end The end date
		 */
		public FileNameFilter(long start, long end) {
			if (start<0 || end==0 || end<start) {
				throw new IllegalArgumentException("Invalid time interval ["+start+", "+end+"]");
			}
			this.start=start;
			this.end=end;
		}

		/**
		 * @see {@link FilenameFilter}
		 */
		@Override
		public boolean accept(File dir, String name) {
			if (name.indexOf(header)==-1) {
				return false;
			}
			if (!name.toLowerCase().endsWith(".xml")) {
				return false;
			}
			// Remove the header and the .xml
			//
			// For example if name was  logOutput2008-09-19T11:21:49.670--2008-09-19T11:21:50.115.xml
			// The following substring returns 2008-09-19T11:21:49.670--2008-09-19T11:21:50.115
			name=name.substring(header.length(),name.length()-4);
			
			// Get the start and end dates
			String[] dates=name.split("--");
			if (dates==null || dates.length!=2) {
				return false;
			}
			dates[0]=dates[0].replaceAll("_", ":");
			dates[1]=dates[1].replaceAll("_", ":");
			
			Date startFileDate;
			Date endFileDate;
			
			try {
				startFileDate=dateFormat.parse(dates[0]);
				endFileDate=dateFormat.parse(dates[1]);
			} catch (Throwable t) {
				return false;
			}
			long startFile = startFileDate.getTime();
			long endFile = endFileDate.getTime();
			if (startFile>endFile) {
				// Something wrong in the file name
				return false;
			}
			
			return !(endFile<=start) && !(startFile>end);
		}
		
	}
	
	/**
	 * The name of the property containing the folder where ARCHIVES writes
	 * files into.
	 * <P> 
	 * <I>Note</I>: this way of getting the folder could change in further releases.
	 */
	public static final String FILES_LOCATION_PROPRTY_NAME="jlog.archive.filesFolder";
	
	/**
	 * The folder containing XML files of logs written by ARCHIVE
	 */
	public final String filesFolder;
	
	/**
	 * The date format to read ISO dates
	 */
	private IsoDateFormat dateFormat = new IsoDateFormat();
	
	/**
	 * Constructor
	 * 
	 * @throws Exception If the folder containing XML files is not found/readable
	 */
	public FilesManager() throws ZoomException {
		filesFolder=getFolderOfFiles();
	}
	
	/**
	 * Get the name of the folder where ARCHIVE writes files into.
	 * <P>
	 * In this version the name of the folder is read from a system property.
	 * 
	 * @return The name of the folder of files; 
	 * 			<code>null</code> if the folder is not found or is not readable.
	 */
	private String getFolderOfFiles() throws ZoomException {
		String ret = System.getProperty(FILES_LOCATION_PROPRTY_NAME);
		if (ret!=null && !ret.isEmpty()) {
			// Check if the folder is readable
			File f = new File(ret);
			if (!f.isDirectory() || !f.canRead()) {
				ret=null;
			} else {
				ret=f.getAbsolutePath();
			}
		}
		if (ret==null) {
			throw new ZoomException("Invalid folder of XML files");
		}
		return ret;
	}
	
	/**
	 * Get logs from files.
	 * 
	 * @param startDate The start date of the loading (ISO format);
	 * 					if <code>null</code> the start date is not limited (i.e.
	 * 					the start date will be that of the first available log in the files)
	 * @param endDate The end date of the loading (ISO format); 
	 * 					if <code>null</code> the current time is used
	 * @param listener The listener to send logs to
	 * @param minLevel The min log level of logs to read from files (inclusive);
	 * 					if <code>null</code>, the lowest level (<code>TRACE</code>) is used
	 * @param maxLevel The max log level of logs to read from files (inclusive);
	 * 					if <code>null</code>, the highest log level (<code>EMERGENCY</code>) is used
	 */
	public void getLogs(
			String startDate, 
			String endDate, 
			ACSRemoteLogListener listener,
			LogTypeHelper minLevel,
			LogTypeHelper maxLevel) {
		long start;
		long end;
		
		// Get the start date
		if (startDate==null) {
			start=0L;
		} else {
			Date date;
			try { 
				date= dateFormat.parse(startDate);
			} catch (ParseException e) {
				throw new IllegalArgumentException("Invalid start date: "+endDate,e);
			}
			start = date.getTime();
		}
		
		
		// Get the end date
		if (endDate==null) {
			end = System.currentTimeMillis();
		} else {
			Date date;
			try {
				date= dateFormat.parse(endDate); 
			} catch (ParseException e) {
				throw new IllegalArgumentException("Invalid end date: "+endDate,e);
			}
			end=date.getTime();
		}
		
		if (listener==null) {
			throw new IllegalArgumentException("The listener can't be null");
		}
		
		if (minLevel==null) {
			minLevel=LogTypeHelper.values()[0];
		}
		if (maxLevel==null) {
			maxLevel=LogTypeHelper.values()[LogTypeHelper.values().length-1];
		}
		if (maxLevel.ordinal()<minLevel.ordinal()) {
			throw new IllegalArgumentException("Invalid level range ["+minLevel+", "+maxLevel+"]");
		}
		
		// Get the files containing logs in the selected range
		File[] files = getFileList(start, end);
		// Read the files
		for (File inFile: files) {
			
		}
	}
	
	/**
	 * Get the XML files of logs between the start and the end date 
	 * (in msec as defined in {@link Date}).
	 *<P> TODO: use a better heuristic here instead of plain starts/end dates
	 * 
	 * @param start The start date of the XML file (0 means unbounded)
	 * @param end The end date of the file
	 * 
	 * @return A list of files with the given start and end dates
	 * 			in their names. The array can be empty if there are
	 * 			no files in the folder. 
	 * 			A value of <code>null</code> means that an error happened during I/O
	 */
	public File[] getFileList(long start, long end) {
		if (start>end) {
			throw new IllegalArgumentException("Invalid time range");
		}
		if (filesFolder==null || filesFolder.isEmpty()) {
			throw new IllegalStateException("Folder of files not initialized!");
		}
		FileNameFilter filter = new FileNameFilter(start,end);
		File f = new File(filesFolder);
		if (!f.isDirectory() || !f.canRead()) {
			// This exception is an illegal state because the properties
			// of the folder were already been checked in the constructor
			// This exception means that something happened outside of jlog
			// (for example someone deleted the folder)
			throw new IllegalStateException(filesFolder+" unreadable/does not exist!");
		}
		return f.listFiles(filter);
	}

}
