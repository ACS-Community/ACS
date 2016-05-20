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
package alma.acs.xmlfilestore.common;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.nio.file.Files;
import java.util.logging.Logger;

import alma.acs.container.ContainerServices;
import alma.acs.util.IsoDateFormat;
import alma.acs.util.stringqueue.TimestampedStringQueueFileHandler;

/**
 * 
 * @author almadev
 *
 */
public class QueueFileHandler extends TimestampedStringQueueFileHandler {
	private static final String FILENAME_SUFFIX = ".xml";
	
	/**
	 * The template of each file name
	 */
	private final String fileNameTemplate;
	
	/**
	 * The prefix of each file ("log' for logs, "alarm" for alarms and so on)
	 */
	private final String fileNamePrefix;

	/**
	 * Th elogger
	 */
	private final Logger m_logger;
	
	/**
	 * The FaultFamily to send alarms (the FaultMemeber is the name of the component)
	 */
	private final String faultFamily; 
	
	/**
	 * The path to location to save files files
	 */
	private File folderForXMLs;
	
	private boolean fileWriteAlarmActive = false;
	
	/**
	 *  max number of files in rotating queue dir
	 */
	private int maxNumOfXmlFiles;
	
	private FilenameFilter logFileFilter = new FilenameFilter() {
		@Override
		public boolean accept(File dir, String name) {
			return name.startsWith(fileNamePrefix);
		}
	};
	
	/**
	 * Container services
	 */
	private final ContainerServices cs;

	/**
	 * Constructor
	 * 
	 * @param myLogger The logger
	 * @param folder The folder to save XMLs
	 * @param myFileMax Max number of XML files to keep in the folder
	 * @param myMaxFileSize Max length of each XML file
	 * @param fileNamePrefix The prefix to each name of XML file
	 * @param faultFamily The FF to send alarms
	 * @throws Exception
	 *  
	 */
	public QueueFileHandler(ContainerServices cs, String folder,
			int myFileMax, long myMaxFileSize, String fileNamePrefix, String faultFamily) throws Exception {
		super(myMaxFileSize, fileNamePrefix);
		this.cs=cs;
		this.faultFamily=faultFamily;
		this.fileNamePrefix=fileNamePrefix;
		this.fileNameTemplate = fileNamePrefix+"%s_%s.xml";
		this.m_logger = cs.getLogger();
		this.maxNumOfXmlFiles = myFileMax;
		this.folderForXMLs = new File(folder);
	}

	/**
	 * @see TimestampedStringQueueFileHandlerr#getNewFile()
	 */
	@Override
	public File getNewFile() throws IOException {
		// ICT-4314 defines the filename format for a new file
		final String startTimestamp = IsoDateFormat.formatCurrentDate();
		final String endTimestamp = "YYYY-MM-DDTHH:MM:SS.mmm";
		String fileName = String.format(fileNameTemplate, startTimestamp, endTimestamp);
		return new File(folderForXMLs, fileName);
	}

	/**
	 * @see TimestampedStringQueueFileHandlerr#fileProcessed(java.io.File,java.lang.String, java.lang.String)
	 */
	@Override
	public void fileProcessed(File oldFile, String earliestTimestamp, String lastTimestamp) {
		final String newFileName = String.format(fileNameTemplate, earliestTimestamp, lastTimestamp);
		final File newFile = new File(folderForXMLs, newFileName);
		try {
			Files.move(oldFile.toPath(), newFile.toPath());
			// now check if the files in the log directory are not being moved somewhere else
			int numLogFiles = folderForXMLs.list(logFileFilter).length;
			if (numLogFiles > this.maxNumOfXmlFiles) {
				m_logger.warning(fileNamePrefix+" file directory contains " + numLogFiles + " XML files, exceeding the limit of " + this.maxNumOfXmlFiles);
				if (!fileWriteAlarmActive) {
					cs.getAlarmSource().setAlarm("Logging", cs.getName(), 2, true);
				}
				fileWriteAlarmActive = true;
			}
		}
		catch (Exception e) {
			m_logger.warning("Could not rename XML file from \"" + oldFile.getAbsolutePath() + "\" to \"" + newFile.getAbsolutePath() + "\"");
			if (!fileWriteAlarmActive) {
				cs.getAlarmSource().setAlarm("Logging", cs.getName(), 2, true);
			}
			fileWriteAlarmActive = true;
		}
	}
}