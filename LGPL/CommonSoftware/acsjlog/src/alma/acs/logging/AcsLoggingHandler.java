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
 */
package alma.acs.logging;

import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import alma.acs.logging.config.LogConfig;
import alma.acs.logging.config.LogConfigSubscriber;

/**
 * The logging handler used by ACS for remote logging.
 * All log records are immediately fed to a {@link alma.acs.logging.DispatchingLogQueue}.
 */
public class AcsLoggingHandler extends Handler implements LogConfigSubscriber
{
	/**
	 * The queue that this handler puts all log records in.
	 */
    private DispatchingLogQueue logQueue;

	/**
	 * Variable to enable messages.
     * TODO: check if this flag should be integrated with LogConfig classes
	 */
	private static boolean DEBUG = Boolean.getBoolean("alma.acs.logging.verbose");

	private LogConfig logConfig;
	private String loggerName;

    private boolean isClosed;

    
    
	public AcsLoggingHandler(DispatchingLogQueue logQueue, LogConfig logConfig, String loggerName)	{
        this.logQueue = logQueue;
        this.logConfig = logConfig;
        this.loggerName = loggerName;
        logConfig.addSubscriber(this);
        configureLogging(logConfig);
	}

    
    /**
     * Called whenever the logging configuration is updated, for example when the CDB is read.
     * @see alma.acs.logging.config.LogConfigSubscriber#configureLogging(alma.acs.logging.config.LogConfig)
     */
    public void configureLogging(LogConfig logConfig) {
        
        // all remote Loggers get their levels configured so that isLoggable() returns correct results
        // for both local and remote logging. 
        // In case the threshold for local logging is lower than for remote logging,
        // this handler still needs to filter out log records whose levels are in between the thresholds.
        try {
            int minLogLevelACS = logConfig.getNamedLoggerConfig(loggerName).getMinLogLevel();
            AcsLogLevel minLogLevelJDK = AcsLogLevel.fromAcsCoreLevel(minLogLevelACS); // JDK Level style
            if (minLogLevelJDK != null) {
            	setLevel(minLogLevelJDK);
            }
            else {
            	throw new NullPointerException("No JDK log level found for ACS log level " + minLogLevelACS);
            }
        } catch (Exception ex) {
        	publish(new LogRecord(Level.WARNING, "Failed to configure remote log handler: " + ex.toString()));
        }
    }

    
	/**
	 * @see java.util.logging.Handler#publish(java.util.logging.LogRecord)
	 * Writes a single log into an array of strings.
	 * @param logRecord
	 */
	public void publish(LogRecord logRecord)
	{
        if (isClosed) {
            if (DEBUG) {
                System.out.println("AcsLoggingHandler: ignoring record with msg='" + logRecord.getMessage() + 
                        "' because this logging handler is already closed.");
            }
            return; // or throw an exc. 
        }
        
		if (!isLoggable(logRecord) || logRecord.getLevel().intValue() == Level.OFF.intValue()) { // abusing Level.OFF for a log call is not caught by the JDK!
            if (DEBUG) {
                System.out.println("AcsLoggingHandler: ignoring record with msg='" + logRecord.getMessage() + 
                        "' because isLoggable() was false.");            
            }            
			return;
        }

        // must trigger a call to LogRecord#inferCaller before the log record gets processed by a different thread        
        logRecord.getSourceClassName();

        logQueue.log(logRecord);
        if (AcsLogLevel.getNativeLevel(logRecord.getLevel()).getAcsLevel() >= logConfig.getImmediateDispatchLevel()) {
            if (DEBUG) {
                System.out.println("flushing log queue because of log record with level " + logRecord.getLevel().getName());
            }
            logQueue.flush();
        }   
	}

    
	/**
     * Forwards the flush request to the underlying queue. 
	 * @see java.util.logging.Handler#flush()
	 */
	public void flush() {
        logQueue.flush();
	}

	/**
     * Required method. Not sure if it's useful for us.
	 * @see java.util.logging.Handler#close()
	 * Cleans handler up before exiting.
	 */
	public void close() {
        isClosed = true;
        logConfig.removeSubscriber(this);
	}

	/**
	 * Used in the test. Sets debug to true.
	 */
	protected static void debug()
	{
		DEBUG = true;
		if (DEBUG)
		{
			System.out.println("AcsLoggingHandler#DEBUG set to true...");
		}
	}

}
