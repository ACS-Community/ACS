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

import java.util.HashMap;
import java.util.Map;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import alma.acs.logging.config.LogConfig;
import alma.acs.logging.config.LogConfigData;
import alma.acs.logging.config.LogConfigSubscriber;
import alma.acs.logging.formatters.LogParameterExtractor;

/**
 * A <code>Logger</code> that attaches additional information to the produced <code>LogRecord</code>s.
 * <p>
 * Design note: the additional data (thread name, line of code) are really most interesting for the remotely sent log messages.
 * Thus an alternative implementation could put the code from {@link #log(LogRecord)} into class {@link alma.acs.logging.AcsLoggingHandler},
 * and not use a custom Logger class at all.
 * The main reason we do it anyway is to avoid throwing the dummy exception (that delivers the stack trace) twice.
 * 
 * @author hsommer
 * created May 30, 2005 4:09:47 PM
 */
public class AcsLogger extends Logger implements LogConfigSubscriber {
    
    private final String thisClassName;

    public AcsLogger(String name, String resourceBundleName, LogConfig logConfig) {
        super(name, resourceBundleName);
        thisClassName = getClass().getName();
        logConfig.addSubscriber(this);
        configureLogging(logConfig);
    }

    
    /**
     * Logs the given <code>LogRecord</code>.
     * <p>
     * Adding of context information:
     * <ul>
     * <li> If the LogRecord has a parameter that is a map which contains additional information 
     * about the line of code, thread, etc., the log record will be taken as provided, and no context
     * information will be added. This can be useful if
     *   <ul>
     *   <li> the log record was reconstructed from a remote error by the ACS error handling code
     *        (see <code>AcsJException</code>, or
     *   <li> if in very exceptional cases application code needs to manipulate such information by hand.
     *   </ul>
     * <li> otherwise, context information is inferred, similar to {@link LogRecord#inferCaller()},
     *   but also including thread name and line of code.
     * </ul>  
     * Note that by overloading this method, we intercept all logging activities of the base class.
     * 
     * @see java.util.logging.Logger#log(java.util.logging.LogRecord)
     */
    public void log(LogRecord record) {
        // check if this record alreay has context data attached
        LogParameterExtractor paramExtractor = new LogParameterExtractor(record);
        String threadName = paramExtractor.extractStringProperty(LogParameterExtractor.PARAM_THREAD_NAME, null);
                
        if (threadName == null) {
            // todo: centralize this log parameter coding/decoding in one class similar to LogParameterExtractor
            Map<String, Object> logProperties = new HashMap<String, Object>();
            record.setParameters(new Object[] {logProperties} );
            
            threadName = Thread.currentThread().getName();
            logProperties.put(LogParameterExtractor.PARAM_THREAD_NAME, threadName);
    
            // Get the stack trace
            StackTraceElement stack[] = (new Throwable()).getStackTrace();
            // search for the first frame before the "Logger" class.
            int ix = 0;
            while (ix < stack.length) {
                StackTraceElement frame = stack[ix];
                String cname = frame.getClassName();
                if (!cname.equals(thisClassName) && !cname.equals("java.util.logging.Logger")) {
                    // We've found the relevant frame.
                    record.setSourceClassName(cname);
                    record.setSourceMethodName(frame.getMethodName());
                    int lineNumber = frame.getLineNumber();
                    logProperties.put(LogParameterExtractor.PARAM_LINE, new Long(lineNumber));
                    break;
                }
                ix++;
            }
            // We haven't found a suitable frame, so just punt. This is
            // OK as we are only committed to making a "best effort" here.
        }
        
        super.log(record);        
    }

    /**
     * @see alma.acs.logging.config.LogConfigSubscriber#configureLogging(alma.acs.logging.config.LogConfig)
     */
    public void configureLogging(LogConfig logConfig) {
    	LogConfigData logConfigData;
		try {
			logConfigData = logConfig.getLogConfigData(getName());
	    	configureJDKLogger(this, logConfigData);
		} catch (Exception e) {
			info("Failed to configure logger.");
		}
    }
    
    /**
     * Service method for configuring even a non-ACS Logger. 
     * Shares code with {@link #configureLogging(LogConfig)}.
     * @param jdkLogger 
     * @param logConfigData
     */
    static void configureJDKLogger(Logger jdkLogger, LogConfigData logConfigData) {
        int minLogLevelACS; // small integer level
        try {
        	// the logger must let through the lowest log level required for either local or remote logging.
            minLogLevelACS = Math.min(logConfigData.getMinLogLevel(), logConfigData.getMinLogLevelLocal());
            AcsLogLevel minLogLevelJDK = AcsLogLevel.fromAcsCoreLevel(minLogLevelACS); // JDK Level style 
            jdkLogger.setLevel(minLogLevelJDK);
        } catch (Exception ex) {
        	jdkLogger.info("Failed to configure logger.");
        }
    	
    }
}