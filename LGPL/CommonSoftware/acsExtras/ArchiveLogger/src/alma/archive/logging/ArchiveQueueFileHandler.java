/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
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
 *
 *    Created on Oct 8, 2010
 *
 */
package alma.archive.logging;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.nio.file.Files;
import java.sql.Timestamp;
import java.util.logging.Logger;

import alma.acs.util.IsoDateFormat;
import alma.acs.util.stringqueue.TimestampedStringQueueFileHandler;

/**
 * 
 * @author almadev
 *
 */
public class ArchiveQueueFileHandler extends TimestampedStringQueueFileHandler {
	private static final String FILENAME_PREFIX = "log";
	private static final String FILENAME_SUFFIX = ".xml";
	private static final String FILENAME_TEMPLATE = FILENAME_PREFIX + "%s_%s" + FILENAME_SUFFIX;

	// TBD - can I log in the logger? tests don't seem to create an infinite recursion
	private Logger m_logger;
	// path to location for log files, consists of logFilePath and the prefix
	private File logDir;
	private boolean fileWriteAlarmActive = false;
	// max number of files in rotating queue dir
	private int maxNumLogFiles;
	private AlarmHandler alarmHandler;
	private FilenameFilter logFileFilter = new FilenameFilter() {
		@Override
		public boolean accept(File dir, String name) {
			return name.startsWith(FILENAME_PREFIX);
		}
	};

	/**
	 * @throws Exception
	 *  
	 */
	public ArchiveQueueFileHandler(Logger myLogger, String logDir,
			int myFileMax, long myMaxFileSize) throws Exception {
		super(myMaxFileSize, FILENAME_PREFIX);
		m_logger = myLogger;
		maxNumLogFiles = myFileMax;
		this.logDir = new File(logDir);
	}

	/**
	 * @see com.cosylab.logging.engine.cache.ILogQueueFileHandler#getNewFile()
	 */
	@Override
	public File getNewFile() throws IOException {
		// ICT-4314 defines the filename format for a new file
		final String startTimestamp = IsoDateFormat.formatCurrentDate();
		final String endTimestamp = "YYYY-MM-DDTHH:MM:SS.mmm";
		String fileName = String.format(FILENAME_TEMPLATE, startTimestamp, endTimestamp);
		return new File(logDir, fileName);
	}

	/**
	 * @see com.cosylab.logging.engine.cache.ILogQueueFileHandler#fileProcessed(java.io.File,
	 *      java.lang.String, java.lang.String)
	 */
	@Override
	public void fileProcessed(File oldFile, String earliestLogTimestamp, String lastLogTimestamp) {
		final String newFileName = String.format(FILENAME_TEMPLATE, earliestLogTimestamp, lastLogTimestamp);
		final File newFile = new File(logDir, newFileName);
		try {
			Files.move(oldFile.toPath(), newFile.toPath());
			// now check if the log files in the log directory are not being moved somewhere elses
			int numLogFiles = logDir.list(logFileFilter).length;
			if (numLogFiles > this.maxNumLogFiles) {
				m_logger.warning("log file directory contains " + numLogFiles + " log files, exceeding the limit of " + this.maxNumLogFiles);
				if (!fileWriteAlarmActive) alarmHandler.sendAlarm(2);
				fileWriteAlarmActive = true;
			}
		}
		catch (Exception e) {
			m_logger.warning("Could not rename log file from \"" + oldFile.getAbsolutePath() + "\" to \"" + newFile.getAbsolutePath() + "\"");
			if (!fileWriteAlarmActive) alarmHandler.sendAlarm(2);
			fileWriteAlarmActive = true;
		}
	}

	/**
	 * 
	 * @param alarmHandler
	 */
	public void setAlarmHandler(AlarmHandler alarmHandler) {
		this.alarmHandler = alarmHandler;
	}
}