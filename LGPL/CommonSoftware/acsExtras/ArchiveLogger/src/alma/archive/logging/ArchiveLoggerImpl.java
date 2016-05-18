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
 *    Created on Sep 21, 2005
 *
 */
package alma.archive.logging;

import java.io.File;
import java.sql.Timestamp;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.alarmsystem.source.ACSAlarmSystemInterface;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;
import alma.archive.database.helpers.ArchiveConfiguration;
import alma.archive.database.helpers.DBConfiguration;
import alma.archive.exceptions.general.DatabaseException;

import com.cosylab.logging.engine.ACS.LCEngine;

/**
 * @author hmeuss
 * 
 * Important notice:
 * 
 * All methods, which are used to process incoming logs have to be added to
 * methodsNotLogged in class. alma.xmlstore.LoggerImpl.LoggerHelper. (In the
 * moment, this is xmlEntryReceived.) In addition, the method
 * requiresOrbCentralLogSuppression() in alma.xmlstore.LoggerImpl.LoggerHelper
 * must be overwritten, so that it returns true. If this is not done, we will
 * run into a vicious circle of log messages.
 * 
 *  
 */
public class ArchiveLoggerImpl extends ComponentImplBase implements
		alma.xmlstore.LoggerOperations {
	private ContainerServices cs;
	private Logger m_logger = Logger.getAnonymousLogger();
	// The engine that connects to the logging client
	private LCEngine engine;
	private ACSAlarmSystemInterface alarmSource;
	private short archiveLogLevel;

	/**
	 *  
	 */
	public ArchiveLoggerImpl() {
		super();
	}

	/**
	 * @see alma.acs.component.ComponentLifecycle#initialize()
	 */
	public void initialize(ContainerServices containerServices)
			throws ComponentLifecycleException {
		super.initialize(containerServices);
		cs = containerServices;
		m_logger = cs.getLogger();
		try {
			alarmSource = ACSAlarmSystemInterfaceFactory.createSource("ARCHIVE_LOGGER");
		} catch (Exception e) {
			m_logger.severe("Could not create alarmSource: "+e);
		}
		
		try {
			DBConfiguration config = ArchiveConfiguration.instance(m_logger);
			String logFilePath = getString(config, "archive.log.dir");
			File f = new File(logFilePath);
			if (!f.exists()) f.mkdirs();
			short fileMax = getInteger(config, "archive.log.maxNumberFiles").shortValue();
			int fileSizeLimit = getInteger(config, "archive.log.maxFileSize").intValue();
			// the properties above are mandatory:
			if (fileMax < 1 || fileSizeLimit < 100000 ) {
				throw new ComponentLifecycleException("Properties archive.log.maxNumberFiles or archive.log.maxFileSize unreasonably small, please check archiveConfig.properties.");
			}
			m_logger.info("Retrieved config info for ArchiveLogger. Log file dir: " + logFilePath
					+ ". Max log file size: " + fileSizeLimit
					+ ". Max # log files: " + fileMax);

			connectToLoggingChannel(logFilePath, fileMax, fileSizeLimit);
		}
		catch (DatabaseException e) {
			String errorMessage = "could not configure component: " + e.getMessage();
			m_logger.severe(errorMessage);
			sendAlarm(2);
			throw new ComponentLifecycleException(errorMessage);
		}
	}

    /**
	 * 
	 * @param logFilePath
	 * @param fileMax
	 * @param fileSizeLimit
	 * @throws ComponentLifecycleException
	 */
	private void connectToLoggingChannel(String logFilePath, short fileMax,
			int fileSizeLimit) throws ComponentLifecycleException {
		try {
			// connect to LoggingChannel
			ArchiveQueueFileHandler queueFileHandler = new ArchiveQueueFileHandler(m_logger, logFilePath, fileMax, fileSizeLimit);
			queueFileHandler.setAlarmHandler(new AlarmHandler() {				
				@Override
				public void sendAlarm(int code) {
					this.sendAlarm(code);
				}
			});
			engine = new LCEngine(queueFileHandler);
			engine.connect(cs.getAdvancedContainerServices().getORB(), null);
			engine.enableAutoReconnection(true);
		} catch (Throwable e) {
			m_logger
					.severe("Could not initialize connection to logging channel.");
			sendAlarm(2);
			throw new ComponentLifecycleException(e);
		}
	}

	/**
	 * 
	 * @param config
	 * @throws ComponentLifecycleException
	 */
	private Integer getInteger(DBConfiguration config, String parameterName)
			throws ComponentLifecycleException {
		try {
			return Integer.parseInt(config.get(parameterName));
		} 
		catch (NumberFormatException e) {
			String errorMessage = "Invalid integer value for " + parameterName + " \"" + config.get(parameterName) + "\"";
			m_logger.severe(errorMessage);
			sendAlarm(2);
			throw new ComponentLifecycleException(errorMessage);
		}
	}
	
	/**
	 * 
	 * @param config
	 * @throws ComponentLifecycleException
	 */
	private String getString(DBConfiguration config, String parameterName)
			throws ComponentLifecycleException {
		String value = config.get(parameterName);
		if (value == null || value.trim().isEmpty()) {
			String errorMessage = "Invalid integer value for " + parameterName + " \"" + config.get(parameterName) + "\"";
			m_logger.severe(errorMessage);
			sendAlarm(2);
			throw new ComponentLifecycleException(errorMessage);
			
		}
		return value;
	}

	/**
	 * 
	 */
	public void cleanUp() throws alma.maciErrType.wrappers.AcsJComponentCleanUpEx {
		super.cleanUp();
		if (m_logger.isLoggable(Level.FINE)) m_logger.fine("cleaning up");
		engine.close(true);
	}

	/**
	 * 
	 */
	private void sendAlarm(int code) {
		try {
			ACSFaultState fs = ACSAlarmSystemInterfaceFactory.createFaultState(
					"Logging", "ARCHIVE_LOGGER", code);
			fs.setDescriptor(ACSFaultState.TERMINATE);
			fs.setUserTimestamp(new Timestamp(System.currentTimeMillis()));

			Properties props = new Properties();
			props.setProperty(ACSFaultState.ASI_PREFIX_PROPERTY, "prefix");
			props.setProperty(ACSFaultState.ASI_SUFFIX_PROPERTY, "suffix");
			fs.setUserProperties(props);
			alarmSource.push(fs);
		} catch (Exception e) {
			m_logger.warning("Failure when sending alarm: " + e);
		}
	}

	@Override
	public short getArchiveLevel() {
		return this.archiveLogLevel;
	}

	@Override
	public void setArchiveLevel(short newLevel) {
		this.archiveLogLevel = newLevel;
	}
}
