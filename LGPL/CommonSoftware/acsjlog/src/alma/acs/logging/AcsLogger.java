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

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Filter;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import alma.acs.logging.adapters.JacORBFilter;
import alma.acs.logging.config.LogConfig;
import alma.acs.logging.config.LogConfigSubscriber;
import alma.maci.loggingconfig.NamedLogger;
import alma.maci.loggingconfig.UnnamedLogger;

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

	// private in base class, need to redeclare here
    protected static final int offValue = Level.OFF.intValue();

	/** the logger class, which must be known to unwind the stack trace. Will be this class unless we use delegation. */
    private Set<String> loggerClassNames = new HashSet<String>();
        
    private String loggerName;
    private String processName;
    private String sourceObject;

    public AcsLogger(String name, String resourceBundleName, LogConfig logConfig) {
        super(name, resourceBundleName);
        addLoggerClass(getClass());
        addLoggerClass(Logger.class);
        logConfig.addSubscriber(this);
        configureLogging(logConfig);
    }

    /**
     * Optionally sets a logger name that can be different from the {@link Logger#name} passed in the constructor.
     * The new name will be used for the <code>LogRecord</code>s produced by this class.
     * This allows changing the name later on, e.g. when a container name or JUnit test name should be prepended to the simple name of a Corba logger.
     * @param loggerName
     */
    void setLoggerName(String loggerName) {
            this.loggerName = loggerName;
    }
    void setProcessName(String processName) {
            this.processName = processName;
    }
    public String getProcessName(){
            return this.processName;
    }
    void setSourceObject(String sourceObject) {
            this.sourceObject = sourceObject;
    }
    public String getSourceObject(){
            return this.sourceObject;
    }

    public void logToAudience(Level level, String msg, String audience) {
    	AcsLogRecord lr = createAcsLogRecord(level, msg);
    	lr.setAudience(audience);
    	log(lr);
    }
    
    public void logToAudience(Level level, String msg, Throwable thr, String audience) {
    	AcsLogRecord lr = createAcsLogRecord(level, msg);
    	lr.setAudience(audience);
    	lr.setThrown(thr);
    	log(lr);    	
    }
    
    public AcsLogRecord createAcsLogRecord(Level level, String msg) {
    	AcsLogRecord lr = new AcsLogRecord(level, msg, getName());
    	return lr;
    }
    
    /**
     * Logs the given <code>LogRecord</code>. 
     * The record can be modified or dropped by the optional filters provided in {@link #addLogRecordFilter(alma.acs.logging.AcsLogger.LogRecordFilter)}. 
     * <p>
     * Adding of context information:
     * <ul>
     * <li> If the LogRecord has a parameter that is a map which contains additional information 
     * about the line of code, thread, etc., the log record will be taken as provided, and no context
     * information will be added. This can be useful if
     *   <ul>
     *   <li> the log record was reconstructed from a remote error by the ACS error handling code
     *        (see <code>AcsJException</code>), or
     *   <li> if in very exceptional cases application code needs to manipulate such information by hand.
     *   </ul>
     * <li> otherwise, context information is inferred, similar to {@link LogRecord#inferCaller()},
     *   but additionally including thread name and line of code.
     * </ul>  
     * Note that by overloading this method, we intercept all logging activities of the base class.
     *  
     * @see java.util.logging.Logger#log(java.util.logging.LogRecord)
     */
    public void log(LogRecord record) {
    	
        // Level could be null and must be inherited from the ancestor loggers, 
    	// e.g. during JDK shutdown when the log level is nulled by the JDK LogManager 
    	Logger loggerWithLevel = this;
    	while (loggerWithLevel.getLevel() == null) {
    		loggerWithLevel = loggerWithLevel.getParent();
    	}
    	
    	// filter by log level to avoid unnecessary retrieval of context data.
    	// The same check will be repeated by the base class implementation of this method that gets called afterwards.
    	int levelValue = loggerWithLevel.getLevel().intValue();
    	if (record.getLevel().intValue() < levelValue || levelValue == offValue) {
    	    return;
    	}
    	
    	// modify the logger name if necessary
    	if (loggerName != null) {
    		record.setLoggerName(loggerName);
    	}
    	
        // check if this record alreay has the context data attached which ACS needs but the JDK logging API does not provide
        LogParameterUtil paramUtil = new LogParameterUtil(record);
        Map<String, Object> specialProperties = paramUtil.extractSpecialPropertiesMap();
        
        if (specialProperties == null) {
        	// we prepend the special properties map to the other parameters
        	specialProperties = LogParameterUtil.createPropertiesMap();
        	List<Object> paramList = paramUtil.getNonSpecialPropertiesMapParameters();
        	paramList.add(0, specialProperties);
            record.setParameters(paramList.toArray() );
            
            String threadName = Thread.currentThread().getName();
            specialProperties.put(LogParameterUtil.PARAM_THREAD_NAME, threadName);

            specialProperties.put(LogParameterUtil.PARAM_PROCESSNAME, this.processName);
            specialProperties.put(LogParameterUtil.PARAM_SOURCEOBJECT, this.sourceObject);

            
    
            // Get the stack trace
            StackTraceElement stack[] = (new Throwable()).getStackTrace();
            // search for the first frame before the "Logger" class.
            int ix = 0;
            while (ix < stack.length) {
                StackTraceElement frame = stack[ix];
                String cname = frame.getClassName();
                if (!loggerClassNames.contains(cname))  {
                    // We've found the relevant frame.
                    record.setSourceClassName(cname);
                    record.setSourceMethodName(frame.getMethodName());
                    int lineNumber = frame.getLineNumber();
                    specialProperties.put(LogParameterUtil.PARAM_LINE, new Long(lineNumber));
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
		try {
			NamedLogger config = logConfig.getSpecialLoggerConfig(getName());
	    	configureJDKLogger(this, config);	    	
		} catch (Exception e) {
			info("Failed to configure logger.");
		}
		
		// forward log level to optional JacORB filter
		// Perhaps this dependency is too dirty, then we need a more general
		// filter registration mechanism parallel to what the JDK foresees.
		Filter logFilter = getFilter();
		if (logFilter != null && logFilter instanceof JacORBFilter) {
			((JacORBFilter) logFilter).setLogLevel(getLevel());
		}
    }
    
    /**
     * Service method for configuring even a non-ACS Logger. 
     * Shares code with {@link #configureLogging(LogConfig)}.
     * @param jdkLogger 
     * @param logConfigData
     */
    static void configureJDKLogger(Logger jdkLogger, UnnamedLogger loggerConfig) {
        int minLogLevelACS; // small integer level
        try {
        	// the logger must let through the lowest log level required for either local or remote logging.
            minLogLevelACS = Math.min(loggerConfig.getMinLogLevel(), loggerConfig.getMinLogLevelLocal());
            AcsLogLevel minLogLevelJDK = AcsLogLevel.fromAcsCoreLevel(minLogLevelACS); // JDK Level style 
            jdkLogger.setLevel(minLogLevelJDK);
        } catch (Exception ex) {
        	jdkLogger.info("Failed to configure logger.");
        }
    	
    }

	/**
	 * Adds a logger class, which will be used to skip entries in the stack trace until the original logging method is found.
	 * If you have a delegation chain that involves loggers besides AcsLogger and the normal JDK Logger, 
	 * make sure you call this method for each of them.
	 * @param loggerClass
	 */
	public void addLoggerClass(Class<?> loggerClass) {
		loggerClassNames.add(loggerClass.getName());
	}			
}
