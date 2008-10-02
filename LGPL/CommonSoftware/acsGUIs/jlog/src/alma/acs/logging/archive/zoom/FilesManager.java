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
import java.io.FileNotFoundException;
import java.io.FilenameFilter;
import java.text.ParseException;
import java.util.Date;

import alma.acs.logging.engine.io.IOPorgressListener;
import alma.acs.util.IsoDateFormat;

import com.cosylab.logging.engine.ACS.ACSRemoteErrorListener;
import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.log.LogTypeHelper;

/**
 * <code>FilesManager</code> organizes the files for the zooming:
 * <UL>
 * 	<LI>owns a list of files of logs 
 * 	<LI>return the right file(s) for zooming by date/time interval
 * 	<LI>get the logs of the given time and level intervals
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
 * <P>
 * This has been agreed with ARCHIVE. They claim that there is a constant to tune in the
 * CDB that should avoid this problem.
 * See http://almasw.hq.eso.org/almasw/bin/view/Archive/ArchiveEightDotZero for further details. 
 * 
 * 
 * @author acaproni
 *
 */
public class FilesManager {
	
	/**
	 * A class to get events while loading logs from the files.
	 * <P>
	 * In this version, <code>ProgressListener</code> does nothing. 
	 * 
	 * @author acaproni
	 *
	 */
	public class ProgressListener implements IOPorgressListener {

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
		
	}
	
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
	 * The folder containing XML files of logs written by ARCHIVE
	 */
	public final String filesFolder;
	
	/**
	 * The date format to read ISO dates
	 */
	private IsoDateFormat dateFormat = new IsoDateFormat();
	
	/**
	 * Signal that the loading must be interrupted.
	 */
	private volatile boolean stopLoading;
	
	/**
	 * The file where the manager is reading logs
	 */
	private FileHelper fileHelper=null;

	
	/**
	 * Constructor
	 * 
	 * @param folder The folder containing files of logs
	 * @throws ZoomException
	 */
	public FilesManager(String folder) throws ZoomException {
		filesFolder=checkFolderOfFiles(folder);
	}
	
	/**
	 * Check if the folder where ARCHIVE writes files into is valid 
	 * and points to a readable directory.
	 * 
	 * @param folder The path of folder of log files 
	 * @return The absolute path name of the folder of files; 
	 * 
	 * @throws ZoomException In case the folder is not found or not readable
	 */
	private String checkFolderOfFiles(String folder) throws ZoomException {
		if (folder==null || folder.isEmpty()) {
			throw new IllegalArgumentException("Invalid folder: "+folder);
		}
		// Check if the folder is readable
		File f = new File(folder);
		if (!f.isDirectory() || !f.canRead()) {
			throw new ZoomException("Invalid folder of XML files: "+folder);
		} else {
			folder=f.getAbsolutePath();
		}
		return folder;
	}
	
	/**
	 * Get logs from files.
	 * 
	 * @param startDate The start date of the loading (ISO format);
	 * 					if <code>null</code> the start date is not limited (i.e.
	 * 					the start date will be that of the first available log in the files)
	 * @param endDate The end date of the loading (ISO format); 
	 * 					if <code>null</code> the current time is used
	 * @param logListener The listener to send logs to
	 * @param minLevel The min log level of logs to read from files (inclusive);
	 * 					if <code>null</code>, the lowest level (<code>TRACE</code>) is used
	 * @param maxLevel The max log level of logs to read from files (inclusive);
	 * 					if <code>null</code>, the highest log level (<code>EMERGENCY</code>) is used
	 * @param zoomListener The listener for monitoring the activity of the zoom engine.
	 * 					It can be <code>null</code>.
	 * @param errorListener The listener to be notified about logs that was not possible to parse
	 * @return <code>false</code> in case of errors, <code>false</code> otherwise.
	 * 
	 * @see {@link ACSRemoteLogListener}
	 * @see {@link ZoomProgressListener}
	 */
	public boolean getLogs(
			String startDate, 
			String endDate, 
			ACSRemoteLogListener logListener,
			LogTypeHelper minLevel,
			LogTypeHelper maxLevel,
			ZoomProgressListener zoomListener,
			ACSRemoteErrorListener errorListener) throws FileNotFoundException, ZoomException {
		stopLoading=false;
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
		
		if (logListener==null) {
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
		if (zoomListener!=null) {
			zoomListener.zoomReadingFile(0);
		}
		boolean ret=true;
		// Get the files containing logs in the selected range
		File[] files = getFileList(start, end);
		if (zoomListener!=null) {
			zoomListener.zoomTotalFileToRead(files.length);
		}
		// Read the files
		int index=0;
		for (File inFile: files) {
			if (zoomListener!=null) {
				zoomListener.zoomReadingFile(++index);
			}
			fileHelper = new FileHelper(inFile, start, end, minLevel, maxLevel);
			if (stopLoading) {
				return false;
			}
			ret = ret && fileHelper.loadLogs(logListener, new ProgressListener(), errorListener);
		}
		return ret;
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
	
	/**
	 * Return <code>true</code> if the file manager is operative i.e. if:
	 * <UL>
	 * 	<LI>the folder of files exists and is readable
	 * 	<LI>the folder of files contains files of log suitable for zooming
	 * </UL>
	 * <P>
	 * The folder of files is checked when the object is created too.
	 * However someone from outside could have deleted, renamed 
	 * or changed the accessing properties of the folder. 
	 * <P>
	 * <I>Note</I>: this method could be slow if the folder contains
	 * 		a big number of files.
	 * @return <code>true</code> if the file manager is operative
	 */
	public boolean isOperational() {
		try {
			checkFolderOfFiles(filesFolder);
		} catch (Exception e) {
			return false;
		}
		File f = new File(filesFolder);
		return getFileList(0, System.currentTimeMillis()).length>0;
	}
	
	/**
	 * Stop loading logs.
	 * <P>
	 * <code>stopLoading</code> does nothing if no load is in progress.
	 */
	public void stopLoading() {
		stopLoading=true;
		if (fileHelper!=null) {
			fileHelper.stopLoading();
		}
	}

}
