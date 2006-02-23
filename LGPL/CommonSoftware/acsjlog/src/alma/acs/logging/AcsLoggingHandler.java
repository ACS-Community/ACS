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
import java.util.logging.LogManager;
import java.util.logging.LogRecord;

/**
 * The logging handler used by ACS for remote logging.
 * All log records are immediately fed to a {@link alma.acs.logging.DispatchingLogQueue}.
 */
public class AcsLoggingHandler extends Handler
{
	/**
	 * The queue that this handler puts all log records in.
	 */
    private DispatchingLogQueue logQueue;


	/**
	 * Variable to enable messages.
	 */
	private static boolean DEBUG = Boolean.getBoolean("alma.acs.logging.verbose");

//	/**
//	 * Variable used to set the minimum priority of the log records.
//	 */
//	private static final String MIN_CACHE_PRIORITY = "MinCachePriority"; // 0
//
//	/**
//	 * Variable used to set the maximum priority of the log records.
//	 */
//	private static final String MAX_CACHE_PRIORITY = "MaxCachePriority"; // 31
//
	/**
	 * Variable used to set the minimum priority of the log records according to
	 * the logging specifications, e.g. ACSCoreLevel.ACS_LEVEL_UNKNOWN.
	 */
	private static final int DEFAULT_MIN_CACHE_PRIORITY = 0;

	/**
	 * Variable used to set the maximum priority of the log records according to
	 * the logging specifications, e.g. ACSCoreLevel.ACS_LEVEL_EMERGENCY.
	 */
	private static final int DEFAULT_MAX_CACHE_PRIORITY = 11;

	/**
	 * Variable used to set the maximum priority of the log records according to
	 * the logging specifications, e.g. ACSCoreLevel.ACS_LEVEL_EMERGENCY.
	 */
	private int minCachePriority = DEFAULT_MIN_CACHE_PRIORITY;

	/**
	 * Variable used to set the maximum priority of the log records according to
	 * the logging specifications, e.g. ACSCoreLevel.ACS_LEVEL_EMERGENCY.
	 */
	private int maxCachePriority = DEFAULT_MAX_CACHE_PRIORITY;

    private boolean isClosed;

	public AcsLoggingHandler(DispatchingLogQueue logQueue)
	{
        this.logQueue = logQueue;
        setLevel(getLevelProp());
        
        // todo: replace cache priority stuff with calls to flush() depending on log level.
        // such calls could be handler-dependent, so even though we have one queue, different kind of handlers could have different thresholds 
	}

//	/**
//	 * Sets the variables for every instance of this class.
//	 */
//	protected void configure()
//	{
//		m_acsRemoteHandler = new AcsRemoteHandler();
//
//		setLevel(getLevelProp());
//
//		// disables caching if the default priority used is less than
//		// the minimum to be considered.
//		if (maxCachePriority < minCachePriority)
//		{
//			m_acsRemoteHandler.setCache(false);
//		}
//	}



    
	/**
	 * Gets the level for this instance of AcsLoggingHandler
	 * as specified in the logging properties. If null, assumes ALL by default.
	 * @return Level
	 */
	public Level getLevelProp()
	{
		String level = LogManager.getLogManager().getProperty(getClass().getName() + ".level");
		if (level == null)
		{
			return AcsLogLevel.parse("ALL");
		}
		if (level.indexOf(".") == -1)
		{
			String startName = level.substring(0, 1);
			String name = startName + level.substring(1).toUpperCase();
			return AcsLogLevel.parse(name);
		}
		else if (level.startsWith("Level."))
		{
			int start = level.indexOf('.');
			String lvl = level.substring(start);
			String name = lvl.substring(1).toUpperCase();
			return AcsLogLevel.parse(name);
		}
		else
		{
			System.err.println("Please set a level according to the Java Logging API!");
			return AcsLogLevel.parse("OFF");
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
        
		if (!isLoggable(logRecord)) {
            if (DEBUG) {
                System.out.println("AcsLoggingHandler: ignoring record with msg='" + logRecord.getMessage() + 
                        "' because isLoggable() was false.");            
            }            
			return;
        }

        // must trigger a call to LogRecord#inferCaller before the log record gets processed by a different thread        
        logRecord.getSourceClassName();

        logQueue.log(logRecord);
        // todo: revisit this level stuff
        if (AcsLogLevel.getNativeLevel(logRecord.getLevel()).getAcsLevel() > maxCachePriority) {
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
