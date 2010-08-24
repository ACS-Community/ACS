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
import java.util.logging.LogManager;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import alma.acs.logging.adapters.JacORBFilter;
import alma.acs.logging.config.LogConfig;
import alma.acs.logging.config.LogConfigSubscriber;
import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.maci.loggingconfig.UnnamedLogger;

/**
 * A <code>Logger</code> that attaches additional information to the produced <code>LogRecord</code>s.
 * <p>
 * This class should be used only by ACS or by similar framework layers in the Operator GUI etc.
 * Normal application code should get the Logger object from the <code>ContainerServices</code>. 
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

	/**
	 * Usually this is null, but factory method {@link #wrapJdkLogger(Logger)} could supply this delegate.
	 */
	protected final Logger delegate;
	
	/** the logger class, which must be known to unwind the stack trace. Will be this class unless we use delegation. 
	 * We don't share this set among Logger instances to avoid threading overheads for fast access.
	 */
	private final Set<String> loggerClassNames = new HashSet<String>();

	/**
	 * Concatenation of class and method names, with a "#" in between. 
	 * Used for fast comparison of log stack frames, see {@link #addIgnoreLogs(String, String)}.
	 */
	private final Set<String> callStacksToBeIgnored = new HashSet<String>();

	/**
	 * Configuration data. May be <code>null</code> for instances created from non-standard factory methods.
	 * @see #configureLogging(LogConfig)
	 */
	private LogConfig logConfig;

	private String loggerName;
	private String processName;

    /**
     * TODO: check why we set the SourceObject as a field here, and also take the loggerName 
     * in the formatters to fill in the source object field there.
     */
    private String sourceObject;
    
    private boolean noLevelWarningPrinted = false;

    private final boolean DEBUG = Boolean.getBoolean("alma.acs.logging.verbose");

	/**
	 * Standard constructor that configures this logger from <code>logConfig</code> and also registers for log config
	 * changes.
	 * 
	 * @param name
	 *            the logger's name
	 * @param resourceBundleName
	 *            may be <code>null</code>
	 * @param logConfig
	 *            the ACS logging configuration object, which gives optional access to the CDB.
	 */
	public AcsLogger(String name, String resourceBundleName, LogConfig logConfig) {
		this(name, resourceBundleName, logConfig, false, null);
	}

	/**
	 * Auxiliary ctor. Don't use it directly from outside of this class.
	 * 
	 * @param logConfig
	 *            may be null if <code>allowNullLogConfig==true</code>
	 */
	protected AcsLogger(String name, String resourceBundleName, LogConfig logConfig, boolean allowNullLogConfig, Logger delegate) {
		super(name, resourceBundleName);
		this.delegate = delegate;
		this.logConfig = logConfig;

		if (DEBUG) {
			System.out.println("*** AcsLogger running in DEBUG mode!");
		}

		addLoggerClass(AcsLogger.class);
		addLoggerClass(Logger.class);
		if (logConfig != null) {
			configureLogging(logConfig);
			logConfig.addSubscriber(this); // passing "this" should only be done when this object is fully constructed.
		} else if (!allowNullLogConfig) {
			throw new NullPointerException("LogConfig must not be null");
		}
	}

    /**
     * Non-standard factory method to be used only for special offline or testing purposes
     * where typically an AcsLogger must be provided by an alternative implementation of ContainerServices.
     * The returned AcsLogger is just like a JDK Logger obtained from {@link Logger#getLogger(String, String)}.
     * <p>
     * Note that we do not supply a {@link LogConfig} and therefore the new AcsLogger cannot register itself 
     * for initial configuration or later configuration change notifications. <br>
     * <b>It is the client's responsibility to configure the log level and parent logger of the returned AcsLogger!</b>
     * 
     * @param name  the logger's name
     * @param resourceBundleName
     * @return <code>AcsLogger</code> that is as close to a normal JDK Logger as possible.
     * @throws IllegalArgumentException 
     *              If a Logger of the given <code>name</code> exists but is not an <code>AcsLogger</code>,
     *              or if an AcsLogger of the given <code>name</code> exists but has a different <code>resourceBundleName</code>.
     */
    public static AcsLogger createUnconfiguredLogger(String name, String resourceBundleName) {
    	
    	// the following code is copied and modified from Logger.getLogger 
    	
    	LogManager manager = LogManager.getLogManager();
    	Logger jdkLogger = manager.getLogger(name);
    	if (jdkLogger != null && !(jdkLogger instanceof AcsLogger)) {
    		throw new IllegalArgumentException("Logger " + name + " already exists but is not of subtype AcsLogger.");
    	}
    	AcsLogger result = (AcsLogger) jdkLogger;
    	
    	if (result == null) {
    	    // Create a new logger.
    	    // Note: we may get a MissingResourceException here.
    	    result = new AcsLogger(name, resourceBundleName, null, true, null);
    	    manager.addLogger(result);
    	    result = (AcsLogger) manager.getLogger(name);
    	}
    	
    	// unlike in the JDK logger, we can't fix the resource bundle name if the logger from cache had null and now one is given.
    	// however we check that the old and new bundle are consistent.
    	if (result.getResourceBundleName() != null && !result.getResourceBundleName().equals(resourceBundleName)) {
    	    throw new IllegalArgumentException(result.getResourceBundleName() +
    				" != " + resourceBundleName);
    	}
    	return result;
    }

	/**
	 * Client applications that use ACS class <code>ComponentClient</code> may need to turn their own JDK Logger into
	 * an <code>AcsLogger</code>.
	 * <p>
	 * If <code>logger</code> is itself of sub-type <code>AcsLogger</code> then it is returned directly without being wrapped.
	 * The wrapping logger shares the parent logger and the log level with the provided logger.
	 * 
	 * @param logger
	 *            the JDK logger
	 * @param wrapperName
	 *            Name of the returned AcsLogger. May be <code>null</code> in which case the delegate's name plus
	 *            "wrapper" is taken.
	 * @return an AcsLogger that delegates to the given <code>logger</code>.
	 * @since ACS 8.0
	 */
	public static AcsLogger fromJdkLogger(Logger logger, String wrapLoggerName) {
		if (logger instanceof AcsLogger) {
			return (AcsLogger) logger;
		}
		String acsLoggerName = (wrapLoggerName != null ? wrapLoggerName.trim() : logger.getName() + "wrapper");
		AcsLogger ret = new AcsLogger(acsLoggerName, logger.getResourceBundleName(), null, true, logger);
		ret.setLevel(logger.getLevel());
		ret.setParent(logger.getParent());
		return ret;
	}


    /**
	 * Optionally sets a logger name that can be different from the {@link Logger#name} passed in the constructor. The
	 * new name will be used for the <code>LogRecord</code>s produced by this class. This allows changing the name later
	 * on, e.g. when a container name or JUnit test name should be appended to the simple name of a Corba logger.
	 * 
	 * @param loggerName
	 */
	void setLoggerName(String newLoggerName) {
		if (!getLoggerName().equals(newLoggerName)) {
			String oldLoggerName = getLoggerName();
			if (DEBUG) {
				System.out.println("Renaming logger '" + oldLoggerName + "' to '" + newLoggerName + "'.");
			}
			this.loggerName = newLoggerName;
			// fix the named logger config
			if (logConfig != null) {
				logConfig.renameNamedLoggerConfig(oldLoggerName, newLoggerName);
				configureLogging(logConfig);
			}
		}
	}
	
	String getLoggerName() {
		return (loggerName != null ? loggerName : getName());
	}

	void setProcessName(String processName) {
		this.processName = processName;
	}

	public String getProcessName() {
		return this.processName;
	}

	void setSourceObject(String sourceObject) {
		this.sourceObject = sourceObject;
	}

	public String getSourceObject() {
		return this.sourceObject;
	}

    /**
     * The audience strings allowed in alma are defined in IDL, 
     * but are available as type-safe enums in {@link alma.acs.logging.domainspecific.AudienceLogger},
     * which thus should be preferred over this generic method.
     */
    public void logToAudience(Level level, String msg, String audience) {
    	AcsLogRecord lr = createAcsLogRecord(level, msg);
    	lr.setAudience(audience);
    	log(lr);
    }
    
    /**
     * The audience strings allowed in alma are defined in IDL, 
     * but are available as type-safe enums in {@link alma.acs.logging.domainspecific.AudienceLogger},
     * which thus should be preferred over this generic method.
     */
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
    	// Throw exception if level OFF was used to log this record, see http://jira.alma.cl/browse/COMP-1928
    	// Both Level.OFF and AcsLogLevel.OFF use the same value INTEGER.max, but we anyway check for both.
    	if (record.getLevel().intValue() == Level.OFF.intValue() ||
    		record.getLevel().intValue() == AcsLogLevel.OFF.intValue()) {
    		throw new IllegalArgumentException("Level OFF must not be used for actual logging, but only for level filtering.");
    	}
    	
    	// Level could be null and must be inherited from the ancestor loggers, 
    	// e.g. during JDK shutdown when the log level is nulled by the JDK LogManager 
    	Logger loggerWithLevel = this;
    	while (loggerWithLevel != null && loggerWithLevel.getLevel() == null && loggerWithLevel.getParent() != null) {
    		loggerWithLevel = loggerWithLevel.getParent();
    	}
    	int levelValue = -1;
    	if (loggerWithLevel.getLevel() == null) {
    		// HSO 2007-09-05: With ACS 6.0.4 the OMC uses this class (previously plain JDK logger) and has reported 
    		// that no level was found, which yielded a NPE. To be investigated further. 
    		// Probably #createUnconfiguredLogger was used without setting parent logger nor log level. 
    		// Just to be safe I add the necessary checks and warning message that improve over a NPE.
    		if (!noLevelWarningPrinted) {
	    		System.err.println("Logger configuration error: no log level found for logger " + getLoggerName() + 
	    				" or its ancestors. Will use Level.ALL instead.");
	    		noLevelWarningPrinted = true;
    		}
    		// @TODO: decide if resorting to ALL is desirable, or to use schema defaults, INFO, etc
    		levelValue = Level.ALL.intValue();
    	}
    	else {
    		// level is fine, reset the flag to print the error message again when log level is missing.
    		noLevelWarningPrinted = false;
        	levelValue = loggerWithLevel.getLevel().intValue();
    	}
    	
    	// filter by log level to avoid unnecessary retrieval of context data.
    	// The same check will be repeated by the base class implementation of this method that gets called afterwards.
    	if (record.getLevel().intValue() < levelValue || levelValue == offValue) {
    	    return;
    	}
    	
    	// modify the logger name if necessary
    	if (loggerName != null) {
    		record.setLoggerName(loggerName);
    	}
    	
        // check if this record already has the context data attached which ACS needs but the JDK logging API does not provide
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
            boolean foundNonLogFrame = false;
            while (ix < stack.length) {
                StackTraceElement frame = stack[ix];
                String cname = frame.getClassName();
                if (!foundNonLogFrame && !loggerClassNames.contains(cname)) {
                    // We've found the relevant frame.
                    record.setSourceClassName(cname);
                    record.setSourceMethodName(frame.getMethodName());
                    int lineNumber = frame.getLineNumber();
                    specialProperties.put(LogParameterUtil.PARAM_LINE, Long.valueOf(lineNumber));
                    foundNonLogFrame = true;
                    if (this.callStacksToBeIgnored.isEmpty()) {
                    	break; // performance optimization: avoid checking all "higher" stack frames
                    }
                }
                if (foundNonLogFrame) {
                	if (callStacksToBeIgnored.contains(concatenateIgnoreLogData(cname, frame.getMethodName()))) {
                		//System.out.println("Won't log record with message " + record.getMessage());
                		return;
                	}
                }
                ix++;
            }
            // We haven't found a suitable frame, so just punt. This is
            // OK as we are only committed to making a "best effort" here.
        }
        // Let the delegate or Logger base class handle the rest.
        if (delegate != null) {
        	delegate.log(record);
        }
        else {
        	super.log(record);
        }
    }

    /**
     * Callback method, configures this logger from the data in logConfig.
     * @see alma.acs.logging.config.LogConfigSubscriber#configureLogging(alma.acs.logging.config.LogConfig)
     */
	public void configureLogging(LogConfig newLogConfig) {
		if (newLogConfig == null) {
			throw new IllegalArgumentException("newLogConfig must not be null");
		}
		logConfig = newLogConfig;
		try {
			UnnamedLogger config = logConfig.getNamedLoggerConfig(getLoggerName());
			if (DEBUG) {
				System.out.println("*** AcsLogger#configureLogging: name=" + getLoggerName() + 
						" minLevel=" + config.getMinLogLevel() + " minLevelLocal=" + config.getMinLogLevelLocal());
			}
			configureLevels(config);
		} catch (Exception e) {
			log(Level.INFO, "Failed to configure logger.", e);
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
	 * Extracted from {@link #configureLogging(LogConfig)} to support also configuration of Loggers created with 
	 * {@link #createUnconfiguredLogger(String, String)} which do not know about a shared {@link LogConfig}. 
	 * 
	 * @param loggerConfig
	 */
	void configureLevels(UnnamedLogger loggerConfig) {
		try {
			// the logger must let through the lowest log level required for either local or remote logging.
			AcsLogLevelDefinition minLogLevelACS = AcsLogLevelDefinition.fromXsdLogLevel(
					loggerConfig.getMinLogLevel().getType() < loggerConfig.getMinLogLevelLocal().getType() 
					? loggerConfig.getMinLogLevel()
					: loggerConfig.getMinLogLevelLocal());

			setLevel(AcsLogLevel.getLowestMatchingJdkLevel(minLogLevelACS));
		} catch (Exception ex) {
			log(Level.INFO, "Failed to configure logger.", ex);
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
	
	/**
	 * The AcsLogger can be configured to ignore certain logs. 
	 * Note that this feature should not be used as a substitute for properly adjusting log levels 
	 * and using repeat guards etc throughout the code.
	 * A valid use case would be to avoid "positive feedback" when the sending of a log produces 
	 * one or more log messages, some of which may only be produced under special conditions,
	 * e.g. those coming from jacorb.
	 * @param className  class name where the log comes from. Must not be null.
	 * @param methodName  method name where the log comes from. Must not be null.
	 */
	public void addIgnoreLogs(String className, String methodName) {
		if (className == null) {
			throw new IllegalArgumentException("className must not be null");
		}
		if (methodName == null) {
			throw new IllegalArgumentException("methodName must not be null");
		}
		callStacksToBeIgnored.add(concatenateIgnoreLogData(className, methodName));
	}

	/**
	 * Ensures that the same format of concatenated String is used in {@link #addIgnoreLogs(String, String)}
	 * and {@link #log(LogRecord)}.
	 * Being private final, the compiler will hopefully inline calls to this method.
	 * @param fileName
	 * @param methodName
	 * @return
	 */
	private final String concatenateIgnoreLogData(String className, String methodName) {
		return className + '#' + methodName;
	}
}
