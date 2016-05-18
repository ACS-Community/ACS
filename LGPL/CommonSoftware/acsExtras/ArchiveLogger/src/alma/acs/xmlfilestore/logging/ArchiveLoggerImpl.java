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
package alma.acs.xmlfilestore.logging;

import java.io.File;
import java.sql.Timestamp;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Level;
import java.util.logging.Logger;

import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogLevel;
import alma.alarmsystem.source.ACSAlarmSystemInterface;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;

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
		alma.xmlFileStore.LoggerOperations {
	
	/**
	 * Container services
	 */
	private ContainerServices cs;
	
	/**
	 * The logger
	 */
	private Logger m_logger;
	
	/**
	 *  The logging client engine that connects to the logging client
	 */
	private LCEngine engine;
	
	/**
	 * The name of the java property to set the log level
	 */
	static public final String MINLOGLEVEL_PROPNAME = "alma.acs.extras.archivelogger.minloglevel";
	
	/**
	 * The default min log level
	 */
	static public final int DEFAULTMINLOGLEVEL = AcsLogLevel.INFO.intValue();
	
	/**
	 * The name of the java property to set the max size of each file of logs
	 */
	static public final String MAXFILESIZE_PROPNAME = "alma.acs.extras.archivelogger.maxFileSize";
	
	/**
	 * The default max size of each file of logs
	 */
	static public final int DEFAULTMAXFILESIZE = 134217728;
	
	/**
	 * The name of the java property to set the max number of file of logs
	 */
	static public final String MAXNUMBEROFFILES_PROPNAME = "alma.acs.extras.archivelogger.maxNumberFiles";
	
	/**
	 * The default max number of file of logs
	 */
	static public final int DEFAULTMAXNUMBEROFFILES = 750;
	
	/**
	 * The name of the java property to set the path to save the file of logs
	 */
	static public final String LOGDIR_PROPNAME = "alma.acs.extras.archivelogger.logDir";
	
	/**
	 * The default max number of file of logs
	 */
	static public final String DEFAULLOGDIR = "/mnt/gas02/data1/AcsLogs-8.1";
	
	/**
	 * The log level is initialized from the value assigned to the 
	 * {@link #MINLOGLEVEL_PROPNAME} java property or to the value of
	 * {@link #DEFAULTMINLOGLEVEL}
	 */
	private final AtomicInteger archiveLogLevel = 
			new AtomicInteger(Integer.getInteger(ArchiveLoggerImpl.MINLOGLEVEL_PROPNAME, ArchiveLoggerImpl.DEFAULTMINLOGLEVEL));

	/**
	 *  Constructor
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
			String logFilePath = System.getProperty(ArchiveLoggerImpl.LOGDIR_PROPNAME, ArchiveLoggerImpl.DEFAULLOGDIR);
			File f = new File(logFilePath);
			if (!f.exists()) {
				f.mkdirs();
			}
			
			int fileMax = Integer.getInteger(ArchiveLoggerImpl.MAXNUMBEROFFILES_PROPNAME, ArchiveLoggerImpl.DEFAULTMAXNUMBEROFFILES);
			int fileSizeLimit = Integer.getInteger(ArchiveLoggerImpl.MAXFILESIZE_PROPNAME, ArchiveLoggerImpl.DEFAULTMAXFILESIZE);
			if (fileMax < 1 || fileSizeLimit < 100000 ) {
				StringBuilder str = new StringBuilder(ArchiveLoggerImpl.MAXNUMBEROFFILES_PROPNAME);
				str.append(" must be greater then 1 and ");
				str.append(ArchiveLoggerImpl.MAXFILESIZE_PROPNAME);
				str.append(" must be greater then 100000");
				throw new ComponentLifecycleException(str.toString());
			}
			
			StringBuilder str = new StringBuilder("Will save log files in : ");
			str.append(logFilePath);
			str.append(" (max log file size: ");
			str.append(fileSizeLimit);
			str.append(", max # log files: " );
			str.append(fileMax);
			str.append(')');
			m_logger.info(str.toString());
			
			connectToLoggingChannel(logFilePath, fileMax, fileSizeLimit);
		} catch (Throwable t) {
			String errorMessage = "could not configure component: " + t.getMessage();
			m_logger.severe(errorMessage);
			sendAlarm(2);
			throw new ComponentLifecycleException(errorMessage,t);
		}
	}

    /**
	 * 
	 * @param logFilePath
	 * @param fileMax
	 * @param fileSizeLimit
	 * @throws ComponentLifecycleException
	 */
	private void connectToLoggingChannel(String logFilePath, int fileMax,
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
		cs.getAlarmSource().setAlarm("Logging", "ARCHIVE_LOGGER", code, true);
	}

	@Override
	public short getArchiveLevel() {
		return this.archiveLogLevel.shortValue();
	}

	@Override
	public void setArchiveLevel(short newLevel) {
		this.archiveLogLevel.set(newLevel);
	}
}
