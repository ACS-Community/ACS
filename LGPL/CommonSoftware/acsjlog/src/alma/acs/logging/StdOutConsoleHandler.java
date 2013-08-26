/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
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

import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.StreamHandler;

import alma.acs.logging.config.LogConfig;
import alma.acs.logging.config.LogConfigSubscriber;
import alma.acs.logging.level.AcsLogLevelDefinition;

/**
 * Copied over from <code>ConsoleHandler</code>, but using <code>System.out</code> instead of <code>System.err</code>.
 * <p>
 * Note that we can't inherit from <code>ConsoleHandler</code> and reset the output stream 
 * using <code>setOutputStream(System.out)</code>, because this would yield a call to <code>System.err.close()</code>.
 * 
 * @author hsommer
 * created Jun 1, 2005 5:52:36 PM
 */
public class StdOutConsoleHandler extends StreamHandler implements LogConfigSubscriber {
	
	private LogConfig logConfig;
	private final String loggerName;
	
	/**
	 * Optional log throttle, may be null if not used.
	 */
	private final LogThrottle logThrottle;
	
	/**
	 * @param logConfig  to get configuration data from, and subscribe for future updates
	 * @param loggerName
	 * @param logThrottle  optional log throttle, may be null if not used.
	 */
	public StdOutConsoleHandler(LogConfig logConfig, String loggerName, LogThrottle logThrottle) {
		this.logConfig = logConfig;
		this.loggerName = loggerName;
		this.logThrottle = logThrottle;
		setOutputStream(System.out);
		configureLogging(logConfig);
		logConfig.addSubscriber(this); // passing "this" should only be done when this object is fully constructed.
	}

	/**
	 * @see alma.acs.logging.config.LogConfigSubscriber#configureLogging(alma.acs.logging.config.LogConfig)
	 */
	public void configureLogging(LogConfig newLogConfig) {
		// just in case some day LogConfig is no longer used as a singleton
		this.logConfig = newLogConfig;

		try {
			AcsLogLevelDefinition minLogLevelACS = AcsLogLevelDefinition.fromXsdLogLevel(
					logConfig.getNamedLoggerConfig(loggerName).getMinLogLevelLocal());
			setLevel(AcsLogLevel.getLowestMatchingJdkLevel(minLogLevelACS));
		} catch (Exception ex) {
			publish(new LogRecord(Level.WARNING, "Failed to configure stdout log handler: " + ex.toString()));
		}
	}
    
    /**
     * Publish a <tt>LogRecord</tt>.
     * <p>
     * The logging request was made initially to a <tt>Logger</tt> object,
     * which initialized the <tt>LogRecord</tt> and forwarded it here.
     * <p>
     * 
     * @param record
     *            description of the log event. A null record is silently
     *            ignored and is not published
     */
	public synchronized void publish(LogRecord record) {
		if (logThrottle == null || logThrottle.checkPublishLogRecordLocal()) {
			super.publish(record);
			flush();
		}
	}

    /**
     * Override <tt>StreamHandler.close</tt> to do a flush but not to close
     * the output stream. That is, we do <b>not</b> close <tt>System.err</tt>.
     */
    public void close() {
        flush();
        logConfig.removeSubscriber(this);
    }

}
