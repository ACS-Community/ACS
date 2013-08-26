/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2009
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
 */
package alma.acs.alarmsystem.acsimpl;

import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.ClientLogManager;
import alma.acs.logging.config.LogConfig;
import alma.acs.logging.config.LogConfigException;
import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.alarmsystem.AlarmServicePOA;
import alma.alarmsystem.Triplet;
import alma.acs.alarmsystem.corbaservice.AlarmSystemCorbaServer;

import alma.acs.util.XmlNormalizer;
import alma.maci.loggingconfig.UnnamedLogger;

import org.omg.CosPropertyService.Property;

import alma.ACSErrTypeCommon.BadParameterEx;
import alma.ACSErrTypeCommon.UnexpectedExceptionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJIllegalArgumentEx;
import alma.Logging.IllegalLogLevelsEx;
import alma.Logging.LoggerDoesNotExistEx;
import alma.Logging.LoggingConfigurablePackage.LogLevels;

public class AcsAlarmSystem extends AlarmServicePOA {
	
	/**
	 * The CORBA server
	 */
	private final AlarmSystemCorbaServer corbaServer;
	
	/**
	 * Set to <code>true</code> if the alarm service has been shut down
	 */
	private volatile boolean closed=false;
	
	/**
	 * The logger
	 */
	private final Logger logger;
	
	/**
	 * A class to terminate the alarm service asynchronously.
	 * <P>
	 * The alarm service is stopped by calling the shutdown IDL method.
	 * But inside such a method, the ORB can't be closed.
	 * This class shuts down the servant outside of the ORB thread.
	 * 
	 * @author acaproni
	 *
	 */
	public class AcsComponentTerminator implements Runnable {
		public void run() {
			corbaServer.shutdown();
			logger.log(AcsLogLevel.DEBUG,"See you soon :-)");
			
		}
	}
	
	/**
	 * Constructor
	 */
	public AcsAlarmSystem(AlarmSystemCorbaServer corbaServer) throws Exception {
		if (corbaServer==null) {
			throw new Exception("The CORBA server can't be null");
		}
		this.corbaServer=corbaServer;
		logger=corbaServer.getLogger();
	}
	
	/**
	 * Return the type of alarm system
	 * 
	 * @return always <code>true</code>
	 */
	public boolean isACSAlarmService() {
		return true;
	}
	
	/**
	 * Shutdown the alarm service
	 */
	public synchronized void shutdown() {
		if (closed) {
			return;
		}
		closed=true;
		logger.log(AcsLogLevel.DEBUG,"Shutting down");
		Thread t = new Thread(new AcsComponentTerminator(),"LaserComponentTerminator");
		t.start();
	}
	
	/**
	 * IDL method: submit an alarm without.
	 * <P>
	 * Build a message to sent to the {@link AlarmMessageProcessorImpl#process(Message)}.
	 * 
	 * @param triplet The triplet of the alarm
	 * @param active if <code>true</code> the alarm is active
	 * @param sourceHostName The name of the host of the source
	 * @param timestamp The timestamp of the source
	 * @param alarmProperties Additional user-defined properties of the alarm
	 */
	public synchronized void submitAlarm(
			Triplet triplet,
			boolean active,
			String sourceHostName,
			String sourceName,
			long timestamp,
			Property[] alarmProperties) throws BadParameterEx, UnexpectedExceptionEx {
		String activeString=active?"ACTIVE":"TERMINATE";
		StringBuilder sb = new StringBuilder("Alarm sent: <");
		sb.append(triplet.faultFamily+','+triplet.faultMember+','+triplet.faultCode+"> ");
		sb.append(activeString);
		logger.log(AcsLogLevel.ALERT,XmlNormalizer.normalize(sb.toString()));
	}
	
    // ///////////////////////////////////////////////////////////
	// LoggingConfigurable interface
	// ///////////////////////////////////////////////////////////

	// init logging 
	LogConfig logConfig = ClientLogManager.getAcsLogManager().getLogConfig();

	/**
	 * Gets the log levels of the default logging configuration. These levels
	 * are used by all loggers that have not been configured individually.
	 */
	public LogLevels get_default_logLevels() {
		LogLevels logLevels = new LogLevels();
		logLevels.useDefault = false;
		logLevels.minLogLevel = (short) logConfig.getDefaultMinLogLevel().value;
		logLevels.minLogLevelLocal = (short) logConfig.getDefaultMinLogLevelLocal().value;
		return logLevels;
	}

	/**
	 * Sets the log levels of the default logging configuration. These levels
	 * are used by all loggers that have not been configured individually.
	 */
	public void set_default_logLevels(LogLevels levels) throws IllegalLogLevelsEx {
		try {
			logConfig.setDefaultMinLogLevel(AcsLogLevelDefinition.fromInteger(levels.minLogLevel));
			logConfig.setDefaultMinLogLevelLocal(AcsLogLevelDefinition.fromInteger(levels.minLogLevelLocal));
		} catch (AcsJIllegalArgumentEx ex) {
			//throw ex.toIllegalArgumentEx();
			IllegalLogLevelsEx ille = new IllegalLogLevelsEx(ex.getErrorDesc());
			throw ille;
		}
	}

	/**
	 * Gets the names of all loggers, to allow configuring their levels
	 * individually. The names are those that appear in the log records in the
	 * field "SourceObject". This includes the container logger, ORB logger,
	 * component loggers, and (only C++) GlobalLogger.
	 * <p>
	 * The returned logger names are randomly ordered.
	 */
	public String[] get_logger_names() {
		Set<String> loggerNames = logConfig.getLoggerNames();
		return loggerNames.toArray(new String[loggerNames.size()]);
	}

	/**
	 * Gets log levels for a particular named logger. If the returned field
	 * LogLevels.useDefault is true, then the logger uses the default levels,
	 * see get_default_logLevels(); otherwise the returned local and remote
	 * levels apply.
	 * <p>
	 * For possible convenience, the default levels are returned in addition to 
	 * setting {@link LogLevels#useDefault} to <code>true</code>.
	 */
	public LogLevels get_logLevels(String logger_name) throws LoggerDoesNotExistEx {
		UnnamedLogger xsdLevels = logConfig.getNamedLoggerConfig(logger_name);
		boolean useDefault = !logConfig.hasCustomConfig(logger_name); 
		LogLevels ret = AcsLogLevelDefinition.createIdlLogLevelsFromXsd(useDefault, xsdLevels);
		return ret;
	}

	/**
	 * Sets log levels for a particular named logger. If levels.useDefault is
	 * true, then the logger will be reset to using default levels; otherwise it
	 * will use the supplied local and remote levels.
	 */
	public void set_logLevels(String logger_name, LogLevels levels) throws LoggerDoesNotExistEx, IllegalLogLevelsEx {
		if (levels.useDefault) {
			logConfig.clearNamedLoggerConfig(logger_name);
		}
		else {
			try {
				UnnamedLogger config = AcsLogLevelDefinition.createXsdLogLevelsFromIdl(levels);
				logConfig.setNamedLoggerConfig(logger_name, config);
			} catch (AcsJIllegalArgumentEx ex) {
				//throw ex.toIllegalArgumentEx();
				IllegalLogLevelsEx ille = new IllegalLogLevelsEx(ex.getErrorDesc());
				throw ille;
			}
		}
	}

	/**
	 * Commands the container or manager to read in again the logging
	 * configuration from the CDB and to reconfigure the loggers accordingly.
	 * This allows for persistent changes in the logging configuration to become
	 * effective, and also for changes of more advanced parameters.
	 * <p>
	 * Note that unlike for the logging initialization in {@link #initialize()},
	 * now we give precedence to the CDB values over any previous settings.
	 */
	public void refresh_logging_config() {
		try {
			logConfig.initialize(true);
		} catch (LogConfigException ex) {
			// if the CDB can't be read, we still want to run the container, thus we only log the problem here
			logger.log(Level.FINE, "Failed to configure logging (default values will be used).", ex);
		}
	}


		
	/** ************************ END LoggingConfigurable ************************ */
	
}
