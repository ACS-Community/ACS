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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Handler;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import org.omg.CORBA.ORB;
import org.omg.DsLogAdmin.LogOperations;
import org.slf4j.impl.ACSLoggerFactory;

import si.ijs.maci.Manager;

import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.Logging.AcsLogServiceHelper;
import alma.Logging.AcsLogServiceOperations;
import alma.acs.logging.config.LogConfig;
import alma.acs.logging.config.LogConfigException;
import alma.acs.logging.config.LogConfigSubscriber;
import alma.acs.logging.formatters.AcsBinLogFormatter;
import alma.acs.logging.formatters.AcsXMLLogFormatter;
import alma.acs.logging.formatters.ConsoleLogFormatter;
import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.maciErrType.CannotGetComponentEx;
import alma.maciErrType.ComponentConfigurationNotFoundEx;
import alma.maciErrType.ComponentNotAlreadyActivatedEx;
import alma.maciErrType.NoPermissionEx;


/**
 * This class provides methods for getting the loggers for components and containers.
 * Both the loggers and the handlers are organized in a hierarchical namespace so that
 * children may inherit some properties from their parents in the namespace.
 * <p>
 * To set the policy for remote logging, either {@link #initRemoteLogging(ORB, Manager, int, boolean)}
 * or {@link #suppressRemoteLogging()} must be called. 
 * As long as none of these methods are called, it is assumed that remote logging will be initialized 
 * at some point later, and all log messages that are meant for remote logging will be stored in
 * a queue. <code>suppressRemoteLogging</code> will throw away this queue, while <code>initRemoteLogging</code>
 * will send all queued log records to the remote logger.
 * <p>
 * Since ACS 7.0, this class acts pretty much as a replacement for the JDK's {@link LogManager}
 * and the {@linkplain AcsLogManager} subclass. <br> 
 * If needed, we should investigate how to support the JMX logger management 
 * via {@linkplain java.util.logging.LoggingMXBean} which is useless now. 
 * 
 * @author hsommer
 */
public class ClientLogManager implements LogConfigSubscriber
{

	public enum LoggerOwnerType { ContainerLogger, ComponentLogger, OrbLogger, OtherLogger, UnknownLogger }
	
    /** instance of a singleton */
	private volatile static ClientLogManager s_instance;

	
    /** The logging configurator shared by various classes of this package */  
    private final LogConfig sharedLogConfig;
    
	/** The (CORBA-) name of the remote logging service to which logs will be sent */
	private volatile String logServiceName;

	/** The time in seconds between the periodic log queue flushings */
	private volatile int flushPeriodSeconds;


	private static class AcsLoggerInfo {
		AcsLogger logger;
		AcsLoggingHandler remoteHandler;
		StdOutConsoleHandler localHandler;
		LoggerOwnerType loggerOwnerType = LoggerOwnerType.UnknownLogger;
		boolean needsProcessNameUpdate = false;
	}
	
	/**
	 * We store all loggers in this map instead of giving them to 
	 * {@link LogManager} which does not allow to remove loggers
	 * and would thus prevent component loggers from being garbage collected. 
	 */
	private final Map<String, AcsLoggerInfo> loggers = new HashMap<String, AcsLoggerInfo>();
	
    /** 
     * Parent logger of all remote loggers. 
     * Not really needed any more now that handlers are not shared.
     */ 
    private Logger parentRemoteLogger;

	/**
	 * The log queue can be null if remote logging has been suppressed. Otherwise it is non-null even if remote logging
	 * is not active, because it stores the records to be sent off later.
	 */
	protected volatile DispatchingLogQueue logQueue;
	private final ReentrantLock logQueueLock = new ReentrantLock();
	
	/**
	 * The log dispatcher object serves as a flag for whether remote logging has been activated via
	 * {@link #initRemoteLogging(ORB, Manager, int, boolean)}.
	 */
	private RemoteLogDispatcher logDispatcher;

    /** 
     * Logger used by the classes in this logging package.
     * TODO: allow container or client to replace this logger with their own 
     * logger, because these classes belong to their respective process.
     */
	private final Logger m_internalLogger;

	/** for testing */
	private boolean DEBUG = Boolean.getBoolean("alma.acs.logging.verbose");
    private boolean LOG_BIN_TYPE = Boolean.getBoolean("ACS.loggingBin");

	/** 
	 * The process name gets used as a data field in the log record, and also 
	 * gets appended to the name for the Corba logger, 
	 * so that different ORB instances in the system can be distinguished.
	 */ 
    private volatile String processName;
    private final ReentrantLock processNameLock = new ReentrantLock();
    
	/**
	 * If true then the Corba (ORB) logger will not send logs to the Log service.
	 */
	private volatile boolean suppressCorbaRemoteLogging = false;

	private final LogThrottle logThrottle;

	/**
	 * Used to optionally raise/clear alarms from ClientLogManager or its dependent objects. 
	 * This field is <code>null</code>, unless it gets set in {@link #enableLoggingAlarms(LogAlarmHandler)}.
	 */
	private volatile LogAlarmHandler logAlarmHandler;
	
	
	/**
	 * Singleton accessor.
	 * <p>
	 * TODO: rename, because now that we have class {@link AcsLogManager}, this method name is confusing.
	 * 
	 * @return ClientLogManager
	 */
	public static synchronized ClientLogManager getAcsLogManager() {
		if (s_instance == null) {
			s_instance = new ClientLogManager();
		}
		return s_instance;
	}

	/**
	 * 
	 */
	protected ClientLogManager()
	{
        sharedLogConfig = new LogConfig();
        try {
            sharedLogConfig.initialize(false); // will determine the default values
            configureLogging(sharedLogConfig);
        } catch (LogConfigException ex) {
            System.err.println("Failed to configure logging: " + ex.toString());
        }
        
        // parentRemoteLogger is not removed in method disableRemoteLogging, and thus does not need to be
        // (re-) created in method prepareRemoteLogging (unlike the queue and the handler).
        // Therefore we can create it once here in the ctor.
        parentRemoteLogger = Logger.getLogger("AcsRemoteLogger");
        parentRemoteLogger.setUseParentHandlers(false); // otherwise the default log handler on the root logger will dump all logs to stderr
        prepareRemoteLogging();

        m_internalLogger = getAcsLogger("alma.acs.logging", LoggerOwnerType.OtherLogger);
        sharedLogConfig.setInternalLogger(m_internalLogger);
        
		ThrottleCallback throttleCallback = new ThrottleCallback();
		logThrottle = new LogThrottle(sharedLogConfig, throttleCallback);

        if (DEBUG) {
            m_internalLogger.fine("ClientLogManager instance is created.");
        }
        sharedLogConfig.addSubscriber(this); // passing "this" should only be done when this object is fully constructed.
	}

	/**
	 * @see alma.acs.logging.config.LogConfigSubscriber#configureLogging(alma.acs.logging.LogConfig)
	 */
	public void configureLogging(LogConfig logConfig) {

		if (!logConfig.getCentralizedLogger().equals(logServiceName)) {
			if (logServiceName == null) {
				logServiceName = logConfig.getCentralizedLogger();
			} else {
				m_internalLogger.warning("Dynamic switching of Log service not yet supported!");
				// @TODO: switch connection to new log service
			}
		}

		flushPeriodSeconds = logConfig.getFlushPeriodSeconds();
		
		logQueueLock.lock();
		try {
			if (logQueue != null) {
				// don't call this while logQueue is not ready for remote dispatching, because it would produce an ugly error message.
				if (logQueue.hasRemoteDispatcher()) {
					logQueue.setPeriodicFlushing(flushPeriodSeconds * 1000); 
				}

				// Set the log queue size.
				logQueue.setMaxQueueSize(logConfig.getMaxLogQueueSize());
			}
		} finally {
			logQueueLock.unlock();
		}
	}


	/**
	 * Gets the <code>LogConfig</code> object that is shared between the ClientLogManager singleton and any other
	 * objects in the process (e.g. Java container or manager classes, Loggers, Handlers).
	 */
    public LogConfig getLogConfig() {
        return sharedLogConfig;
    }
    

	/**
	 * If not done already, sets up remote handlers for all loggers using a shared queue.
	 */
	protected void prepareRemoteLogging() {
		logQueueLock.lock();
		try {
			if (logQueue == null) {
				logQueue = createDispatchingLogQueue();
				logQueue.setMaxQueueSize(getLogConfig().getMaxLogQueueSize());
			}
		} finally {
			logQueueLock.unlock();
		}
		synchronized (loggers) {
			// attach remote handlers to all loggers
			for (String loggerName : loggers.keySet()) {
				AcsLoggerInfo loggerInfo = loggers.get(loggerName);
				AcsLogger logger = loggerInfo.logger;

				// sanity check on loggerName: map key vs. Logger
				if (logger.getLoggerName() == null || !logger.getLoggerName().equals(loggerName)) {
					logger.setLoggerName(loggerName);
					logger.info("Logging name mismatch resolved for '" + loggerName
							+ "'. Should be reported to ACS developers");
				}

				addRemoteHandler(logger);
			}
		}
	}

	/**
	 * Factory method broken out from {@link #prepareRemoteLogging()} to support tests.
	 */
	protected DispatchingLogQueue createDispatchingLogQueue() {
		return new DispatchingLogQueue();
	}


	/**
	 * Removes, closes, and nulls all remote handlers, so that no more records are put into the queue, and queue and
	 * remote handlers can be garbage collected.
	 * <p>
	 * GC of the queue can be an advantage when logs have been queued for remote sending, but then instead of connecting
	 * to the remote logger, we get a call to {@link #suppressRemoteLogging()}. All messages have been logged locally
	 * anyway, so in this case we want to clean up all these <code>LogRecord</code>s.
	 */
    protected void disableRemoteLogging() {
        synchronized (loggers) {
            for (String loggerName : loggers.keySet()) {
            	try {
	           		AcsLoggerInfo loggerInfo = loggers.get(loggerName);
	            	AcsLogger logger = loggerInfo.logger;
	            	if (loggerInfo.remoteHandler != null) {
	            		logger.removeHandler(loggerInfo.remoteHandler);
	            		loggerInfo.remoteHandler.close(); // also unsubscribes from sharedLogConfig
	            		loggerInfo.remoteHandler = null;
	            	}
            	} catch (Throwable thr) {
            		// better just print to stderr because remote logging may be in a delicate state
            		System.err.println("Unexpected exception while disabling remote logging for '" + loggerName + "': " + thr.toString());
            	}
            }
        }
    }

    /**
     * Attaches a remote log handler to the given logger, if remote logging has not been suppressed.
     * 
     * Note that this method does not require <code>logger</code> to be 
     * already registered in {@linkplain #loggers}, but if it is, 
     * then the new AcsLoggingHandler is set in the map's AcsLoggerInfo as well.
     * 
     * @param logger  logger to be set up for remote logging
     * @return the remote handler that was added to the logger.
     */
    private AcsLoggingHandler addRemoteHandler(AcsLogger logger) {
    	AcsLoggingHandler remoteHandler = null;
        synchronized (loggers) {
	    	String loggerName = logger.getLoggerName();
	    	AcsLoggerInfo loggerInfo = loggers.get(loggerName);	    	
	    	
	    	// logQueue == null serves as indicator for remote log suppression
	    	if (logQueue != null) {
		    	// try to find an existing handler 
		    	for (Handler handler : logger.getHandlers()) {
					if (handler instanceof AcsLoggingHandler) {
						remoteHandler = (AcsLoggingHandler) handler;
						if (loggerInfo != null && !remoteHandler.equals(loggerInfo.remoteHandler)) {
							logger.info("Remote logging handler mismatch resolved for '" + loggerName + "'. Should be reported to ACS developers");
						}
						break;
					}
				}

				if (remoteHandler == null) {
					remoteHandler = new AcsLoggingHandler(logQueue, sharedLogConfig, loggerName, logThrottle); // subscribes to sharedLogConfig
					logger.addHandler(remoteHandler);
					if (loggerInfo != null && loggerInfo.remoteHandler != null) {
						logger.info("Logging handler mismatch resolved for '" + loggerName + "'. Should be reported to ACS developers");
					}
				}
	    	}
			if (loggerInfo != null) {
				loggerInfo.remoteHandler = remoteHandler;
			}
        }
        return remoteHandler;
    }


	/**
	 * Adds a local logging handler to the provided logger. Unlike with remote handlers in
	 * {@link #prepareRemoteLogging()}, for local handlers we don't allow removing and re-adding the handlers later.
	 * <p>
	 * Note that this method does not require <code>logger</code> to be already registered in {@linkplain #loggers}, but
	 * if it is, then the new StdOutConsoleHandler is set in the map's AcsLoggerInfo as well.
	 * 
	 * @param logger
	 *            logger to be set up for local logging
	 * @return the local handler that was added to the logger.
	 */
	private StdOutConsoleHandler addLocalHandler(AcsLogger logger) {
		StdOutConsoleHandler localHandler = null;
		synchronized (loggers) {
			String loggerName = logger.getLoggerName();
			AcsLoggerInfo loggerInfo = loggers.get(loggerName);

			// try to find an existing handler (should not happen)
			for (Handler handler : logger.getHandlers()) {
				if (handler instanceof StdOutConsoleHandler) {
					localHandler = (StdOutConsoleHandler) handler;
					if (loggerInfo != null && !localHandler.equals(loggerInfo.localHandler)) {
						logger.info("Stdout logging handler mismatch resolved for '" + loggerName + "'. Should be reported to ACS developers");
					}
					break;
				}
			}
		
			if (localHandler == null) {
				localHandler = createStdOutConsoleHandlerWithFormatter(sharedLogConfig, loggerName, logThrottle);
				logger.addHandler(localHandler);
				if (loggerInfo != null && loggerInfo.localHandler != null) {
					logger.info("Logging handler mismatch resolved for '" + loggerName + "'. Should be reported to ACS developers");
				}
			}
			if (loggerInfo != null) {
				loggerInfo.localHandler = localHandler;
			}
        }
        return localHandler;
    }

	/**
	 * Factory method broken out to allow tests to easily use an instrumented variant.
	 */
	protected StdOutConsoleHandler createStdOutConsoleHandlerWithFormatter(LogConfig logConfig, String loggerName, LogThrottle throttle) {
		StdOutConsoleHandler localHandler = new StdOutConsoleHandler(logConfig, loggerName, throttle); // subscribes to sharedLogConfig
		localHandler.setFormatter(new ConsoleLogFormatter());
		return localHandler;
	}

    
    /**
     * Creates or reuses a logger, normally with both a local and a remote handler attached.
     * If remote logging is suppressed then no remote handler is attached.
     * <p>
     * Logger names must be unique. If the requested logger name matches a given logger 
     * of the same LoggerOwnerType then that logger is returned. If however a logger of a different
     * type is matched, then a name uniqueness violation is avoided by appending 
     * small integer numbers to the logger name. 
     * Therefore the name of the returned Logger may be different from <code>loggerName</code>!
     * The idea of this strategy is that uniqueness of component names will be enforced 
     * by the system, while a component may in sick cases have the name of a container or the orb logger.
     *  
     * @param loggerName The unique logger name
     * @param loggerOwnerType  enum for the type of the logger owner.
     *        With ACS 7.0, this replaces the namespace strings previously prepended to the logger names.
     * @return
     * @throws IllegalArgumentException if loggerName is null or empty
     */
    private AcsLogger getAcsLogger(String loggerName, LoggerOwnerType loggerOwnerType) {

    	if (loggerName == null || loggerName.trim().isEmpty()) {
    		throw new IllegalArgumentException("loggerName must not be null or empty");
    	}
    	
        AcsLoggerInfo loggerInfo = null;
        
        // just to make sure we never throw an exception
        try {
        	synchronized (loggers) {
            	loggerInfo = loggers.get(loggerName);
	        	// try to reuse existing logger
            	// avoid false "reuse" by making new logger name unique
	        	int counter = 2; // that is to make "name" into "name_2"
	            while (loggerInfo != null && loggerOwnerType != loggerInfo.loggerOwnerType) {
	            	// the current loggerName exists already for a different logger type. 
	            	// Try a different logger name.
	            	int lastIndexUnderscore = loggerName.lastIndexOf('_');
	            	if (lastIndexUnderscore > 0 && loggerName.length() > lastIndexUnderscore + 1) {
	            		try {
							int oldCounter = Integer.parseInt(loggerName.substring(lastIndexUnderscore + 1));
							counter = Math.max(counter, oldCounter+1);
							loggerName = loggerName.substring(0, lastIndexUnderscore+1) + counter;
			            	loggerInfo = loggers.get(loggerName);
			            	continue;
						} catch (NumberFormatException ex) {
							// we had an '_' but no number behind it. Same as no '_' at all.
						}
	            	}
	            	loggerName += "_" + counter;
	            	loggerInfo = loggers.get(loggerName);
            	}

	            if (loggerInfo == null) {
	            	
	            	// not yet in cache, so we create the logger
	            	loggerInfo = new AcsLoggerInfo();
	            	loggerInfo.logger = new AcsLogger(loggerName, null, sharedLogConfig); // ctor registers itself in loggers map
	            	loggerInfo.logger.setParent(parentRemoteLogger);
	            	loggerInfo.logger.setUseParentHandlers(false); // since ACS 7.0 all AcsLoggers have their own handlers 
	
	                // set the value for the "ProcessName" and "SourceObject" fields in the produced log records
	            	loggerInfo.logger.setProcessName(this.processName);
	            	if (loggerOwnerType == LoggerOwnerType.ComponentLogger || loggerOwnerType == LoggerOwnerType.OrbLogger) {
	            		loggerInfo.logger.setSourceObject(loggerName);
	            	}
	                else {
	                	// TODO: check why we don't always use the logger name as SourceObject
	                	loggerInfo.logger.setSourceObject(this.processName);
	                }
	            	
	                loggerInfo.localHandler = addLocalHandler(loggerInfo.logger);
	                loggerInfo.remoteHandler = addRemoteHandler(loggerInfo.logger); // may be null
	                loggerInfo.loggerOwnerType = loggerOwnerType;
	                
	                loggers.put(loggerName, loggerInfo);
	            }
                if (DEBUG) {
                    System.out.println("created remote logger '" + loggerName + "' (level " + loggerInfo.logger.getLevel() + ") and separate local handler.");
                }
            }
        } catch (Throwable thr) {
            System.err.println("failed to create logger '" + loggerName + "'.");
        }
        return loggerInfo.logger;
    }
   

    
	/**
	 * Enables loggers to send log records to the central ACS logger service.
     * Tries to connect to the log service using the supplied ACS manager.
     * As long as this connection fails, this method can sleep for 10 seconds and then try to connect again,
     * if the parameter <code>retry</code> is <code>true</code>. Total retries are limited to 5,
     * to detect a permanent problem before the log queue overflows.
     * <p>
     * Execution time can be significant, so consider calling this method in a separate thread 
     * (which has no negative effect on the logging since all log records are cached and automatically sent off
     * once the log service is available). 
     * Use a daemon thread to avoid shutdown problems if this method still hangs in the login loop.
     * <p>
     * When the log service is obtained, the log queue used for remote logging will be flushed periodically 
     * to the log service unless an otherwise triggered flush has done this already. 
     * The default period is 10 seconds, but can be overridden in the CDB. <br>
     * 
	 * @param orb  the ORB used in this JVM
	 * @param manager  the ACS manager
	 * @param managerHandle  handle assigned by the ACS Manager for this client
     * @param retry  if true, a failing connection to the log service will trigger up to 5 other attempts, every 10 seconds.
     * @return true if remote logging was initialized successfully
     * @see #shutdown(boolean)
	 */
	public boolean initRemoteLogging(ORB orb, Manager manager, int managerHandle, boolean retry)
	{
        if (logDispatcher != null) {
            System.err.println("Ignoring call to ClientLogManager#init: already initialized!");
            // todo: or is there a case where we want to retrieve the log service again? 
            return false;
        }
        
        if (orb == null) {
            System.err.println("Given ORB is null.");
            return false;
        }
		if (manager == null || managerHandle <= 0) {
            System.err.println("can't connect to log service: manager is null, or invalid handle " + managerHandle);
            return false;
		}
        AcsLogServiceOperations logService = null;
        int count = 0;
        String errMsg;
        do {
            errMsg = null;
            count++;
            try {
                // normally there will be a remote handler and log queue already, which has captured all log records produced so far.
                // However, if suppressRemoteLogging was called, we need to set up remote logging from scratch. 
                prepareRemoteLogging();
                logService = getLogService(manager, managerHandle);
                if (logService == null) {
                    errMsg = "Failed to obtain central log service '" + logServiceName + "': reference is 'null'. ";
                }
                else { 
                	logQueueLock.lock(); // we keep the above get_service call outside this locked section in order to not block shutdown() too long
//System.out.println("ClientLogManager#initRemoteLogging got the logQueue lock");
                	try {
                    	if (logQueue == null) {
                    		// this can happen if shutdown or suppressRemoteLogging is called concurrently
                    		System.out.println("Will abort ClientLogManager#initRemoteLogging because remote logging seems no longer needed.");
                    		return false;
                    	}
	                    if (count > 1) {
	                        // all is fine, but we report the difficulty
	                        m_internalLogger.info("Connected to central log service after initial failure. ");
	                    }
	                    // make log service available to our dispatcher, and flush the records collected so far
	                    if (LOG_BIN_TYPE){
	                        logDispatcher = new RemoteLogDispatcher(orb, logService, new AcsBinLogFormatter());
	                    } 
	                    else {
	                        logDispatcher = new RemoteLogDispatcher(orb, logService, new AcsXMLLogFormatter());
	                    }
	                    
	                    logQueue.setRemoteLogDispatcher(logDispatcher);
	                    logQueue.flushAllAndWait();
	                    logQueue.setPeriodicFlushing(flushPeriodSeconds * 1000);
                	}
	                finally {
	                	logQueueLock.unlock();
//	                	System.out.println("ClientLogManager#initRemoteLogging released the logQueue lock");
	                }
                }
            }
            catch (Throwable thr) {
                errMsg = "Failed to connect to central log service with exception " + thr.toString();
                // as this message must go repeatedly to the command line output regardless of local log level, 
                // we don't want to print the multi-line exception stack, but just the original location.
            	StackTraceElement[] trace = thr.getStackTrace();
            	if (trace != null && trace.length > 0) {
            		StackTraceElement traceOrigin = trace[0];
            		errMsg += " in file " + traceOrigin.getFileName() + ", line " + traceOrigin.getLineNumber();
            	}
                errMsg += ". ";
            }
            if (errMsg != null) {
            	// can't use the loggers, so println is ok here
            	if (retry) {
            		System.err.println(errMsg + "Will try again in 10 seconds.");
            		try {
	                    Thread.sleep(10000);
	                } catch (InterruptedException e) {
	                	System.err.println("Abandoning ClientLogManager#initRemoteLogging retries because of thread interruption.");
	                    retry = false; 
	                }
            	}
            	else {
            		System.err.println(errMsg);
            	}
            }
        } while (retry && count <= 5 && errMsg != null);
        
        return (errMsg == null);
	}

	/**
	 * This method is broken out from {@link #initRemoteLogging(ORB, Manager, int, boolean)} to allow mock implementation by tests
	 * without a functional manager object. Note that module acsjlog comes before jmanager.
	 * <p>
	 * Since ACS 8.0.1 we use an ACS-specific subtype of {@link LogOperations} to avoid the marshalling to Corba Any.
	 */
	protected AcsLogServiceOperations getLogService(Manager manager, int managerHandle) throws ComponentNotAlreadyActivatedEx, CannotGetComponentEx, NoPermissionEx, ComponentConfigurationNotFoundEx {
		return AcsLogServiceHelper.narrow(manager.get_service(managerHandle, logServiceName, true));
	}

	
	/**
	 * Suppresses remote logging. If log messages destined for remote logging have not been sent to the central log
	 * service yet (e.g. because {@link #initRemoteLogging(ORB, Manager, int, boolean) initRemoteLogging} has not been
	 * called, or because of sending failures), these log messages will be lost for remote logging. Log messages
	 * produced after this call will not even be queued for remote logging.
	 * <p>
	 * This method should only be called by special ALMA applications such as the Observation Preparation tool, which
	 * runs stand-alone, with all containers, managers, etc. in one JVM. In this case, no central logger is available,
	 * and all loggers which normally send their output to the central logger should be limited to local logging
	 * (stdout).
	 * <p>
	 * It is possible (probably not useful in real life) to re-enable remote logging later, by calling
	 * <code>initRemoteLogging</code>.
	 */
	public void suppressRemoteLogging() {
		logQueueLock.lock();
		try {
			System.out.println("suppressRemoteLogging called");
			disableRemoteLogging();
			logQueue = null;
		} finally {
			logQueueLock.unlock();
		}
	}

    /**
	 * Allows to suppress remote logging of Corba/ORB logger(s). Internally this suppression is handled using log level
	 * changes that cannot be undone by other log level changes. Generally remote logging remains enabled though, which
	 * makes this method quite different from {@linkplain #suppressRemoteLogging()}.
	 * <p>
	 * <strong>Application code such as components must not call this method!<strong>
	 */
    public void suppressCorbaRemoteLogging() {
    	suppressCorbaRemoteLogging = true;
    	
    	synchronized (loggers) {
	    	for (AcsLoggerInfo loggerInfo : loggers.values()) {
	    		if (loggerInfo.loggerOwnerType == LoggerOwnerType.OrbLogger) {
	    			sharedLogConfig.setAndLockMinLogLevel(AcsLogLevelDefinition.OFF, loggerInfo.logger.getLoggerName());
	    		}
			}
    	}
    }


	/**
	 * Gets a logger to be used by ORB and POA classes, or by hibernate. 
	 * The logger is connected to the central ACS logger.
	 * <p>
	 * @TODO rename this method to accommodate non-corba frameworks into which we insert ACS loggers, such as hibernate,
	 *       see {@link org.slf4j.impl.ACSLoggerFactory}.
	 * <p>
	 * For hibernate loggers, the logger automatically receives an initial custom log level configuration, 
	 * to avoid jamming the log system with hibernate logs. 
	 * The applied custom log level is the maximum of the default log level and WARNING.
	 * Note that the hibernate logger can still be set to a more verbose level by giving it a custom log config
	 * in the CDB, or dynamically using logLevelGUI etc.
	 * <p>
	 * @TODO Instead of this hard coded and probably confusing application of a custom log level,
	 * the CDB should offer a central configuration option for all jacorb, hibernate etc loggers,
	 * independently of the process (container or manager etc).
	 * <p>
	 * @param corbaName
	 *            e.g. <code>jacorb</code>. @TODO rename it.
	 * @param autoConfigureContextName
	 *            if true, the context (e.g. container name) will be appended to this logger's name as soon as it is
	 *            available, changing the logger name to something like <code>jacorb@frodoContainer</code>.
	 */
	public AcsLogger getLoggerForCorba(String corbaName, boolean autoConfigureContextName) {

		String loggerName = corbaName;
		AcsLogger corbaLogger = null;

		processNameLock.lock();
		try {
			// if the process name is not known yet (e.g. during startup), then we need to schedule its update
			if (autoConfigureContextName && processName != null) {
				// if the process name is already known, we can even use it for the regular logger name instead of using a later workaround
				loggerName += "@" + processName;
			}

			corbaLogger = getAcsLogger(loggerName, LoggerOwnerType.OrbLogger);
			// Suppress logs inside the call to the Log service, which could happen e.g. when policies are set and jacorb-debug is enabled.
			// As of ACS 8, that trashy log message would be "get_policy_overrides returns 1 policies"
			corbaLogger.addIgnoreLogs("org.omg.DsLogAdmin._LogStub", "write_records");
			corbaLogger.addIgnoreLogs("alma.Logging._AcsLogServiceStub", "write_records");
			corbaLogger.addIgnoreLogs("alma.Logging._AcsLogServiceStub", "writeRecords");

			if (autoConfigureContextName && processName == null) {
				// mark this logger for process name update
				AcsLoggerInfo loggerInfo = loggers.get(loggerName);
				loggerInfo.needsProcessNameUpdate = true;
			}
		} 
		finally {
			processNameLock.unlock();
		}

		// fix levels if we suppress corba remote logging
		if (suppressCorbaRemoteLogging) {
			sharedLogConfig.setAndLockMinLogLevel(AcsLogLevelDefinition.OFF, loggerName);
		}
		else if (!sharedLogConfig.hasCustomConfig(loggerName)) {
			// In the absence of their own custom logger config, some very verbose loggers 
			// get a minimum log level applied to ensure that a carelessly set low default log level
			// does not swamp the system with log messages.
			if (corbaName.startsWith(ACSLoggerFactory.HIBERNATE_LOGGER_NAME_PREFIX) ||
				corbaName.startsWith(ACSLoggerFactory.HIBERNATE_SQL_LOGGER_NAME_PREFIX)) {

				AcsLogLevelDefinition minCustomLevel = AcsLogLevelDefinition.WARNING;
				AcsLogLevelDefinition customLevel = 
					( minCustomLevel.compareTo(sharedLogConfig.getDefaultMinLogLevel()) > 0 
							? minCustomLevel 
							: sharedLogConfig.getDefaultMinLogLevel() );
				sharedLogConfig.setMinLogLevel(customLevel, loggerName);
				
				AcsLogLevelDefinition customLevelLocal = 
					( minCustomLevel.compareTo(sharedLogConfig.getDefaultMinLogLevelLocal()) > 0 
							? minCustomLevel 
							: sharedLogConfig.getDefaultMinLogLevelLocal() );
				sharedLogConfig.setMinLogLevelLocal(customLevelLocal, loggerName);
				
				m_internalLogger.info("Logger " + loggerName + " created with custom log levels local=" + customLevelLocal.name + 
						", remote=" + customLevel.name + " to avoid hibernate log jams due to careless default log level settings.");
			}
		}

		return corbaLogger;
	}


	/**
	 * Gets a logger to be used by the Java container classes. The logger is connected to the central ACS logger.
	 */
	public AcsLogger getLoggerForContainer(String containerName) {
		setProcessName(containerName);
		return getAcsLogger(containerName, LoggerOwnerType.ContainerLogger);
	}

    /**
	 * Gets a logger object to be used by a component.
	 * <p>
	 * This method is not supposed to be accessed directly in the component implementation.
	 * Instead, the implementation of <code>alma.acs.container.ContainerServices</code>
	 * should call this method.
	 * 
	 * @param componentName  component name (sufficiently qualified to be unique in the system or log domain)
	 * @return AcsLogger
	 */
	public AcsLogger getLoggerForComponent(String componentName) {
        return getAcsLogger(componentName, LoggerOwnerType.ComponentLogger);
	}
    

    /**
     * Gets a logger for an application (which is not an ACS component itself), e.g. a GUI application using the ACS ComponentClient. 
     * @param loggerName  the logger name, should identify the application or the particular logger that gets requested.
     * @param enableRemoteLogging  if true (generally recommended), the returned logger is set up to send the logs to the remote log service,
     *                             as it always happens for container and component loggers. 
     *                             <emph>This will only work if {@link #initRemoteLogging(ORB, Manager, int, boolean)} is also called,
     *                             which happens automatically in <code>ComponentClient</code>. 
     *                             For a standalone application that is not ACS-aware (no manager known etc), remote logging is not available 
     *                             even with enableRemoteLogging == true.</emph> 
	 * @return a configured Logger
	 */
    public AcsLogger getLoggerForApplication(String loggerName, boolean enableRemoteLogging)
    {
        if (loggerName == null || loggerName.trim().length() == 0) {
        	loggerName = "unknownClientApplication";
        }
        else {
            setProcessName(loggerName);
        }
        AcsLogger logger = null;
        if (enableRemoteLogging) {
            logger = getAcsLogger(loggerName, LoggerOwnerType.OtherLogger);
        }
        else {
            logger = AcsLogger.createUnconfiguredLogger(loggerName, null);
            logger.setUseParentHandlers(false);
            addLocalHandler(logger);
            logger.configureLevels(sharedLogConfig.getNamedLoggerConfig(loggerName));
        }
        return logger;
    }
    
    
    /**
     * Takes the process name and overwrites previous names,
     * and updates all Loggers which are waiting to get their overly simple name enriched.
     * The new name will be the old name + @ + processName.
     * <p>
     * For example, the Java container first uses a logger from which the process name "AcsContainerRunner"
     * is derived. Only later when the the real container name has been parsed out, this gets renamed.
     * <p>
     * The update mechanism ensures that the process name will eventually be set also on loggers
     * which were created before the process name was known, e.g. component logger created before container logger.
     * @TODO check if we still need the process name appended to the logger name, now that we have a separate field for it in AcsLogger.
     * 
     * @param name
     */
    private void setProcessName(String newProcessName) {
		if (newProcessName == null) {
			// just ignore this call
			return;
		}
		if (!newProcessName.equals(this.processName)) {
			processNameLock.lock();
			try {
				String oldProcessName = this.processName;
				this.processName = newProcessName;
				synchronized (loggers) {
					for (String oldLoggerName : new ArrayList<String>(loggers.keySet())) { // iterate over copy to avoid ConcurrentModif.Ex 
						AcsLoggerInfo loggerInfo = loggers.get(oldLoggerName);
						if (loggerInfo.needsProcessNameUpdate) {
							String newLoggerName = oldLoggerName + "@" + newProcessName;
							loggerInfo.logger.setLoggerName(newLoggerName);
							loggerInfo.logger.setProcessName(newProcessName);
							loggerInfo.needsProcessNameUpdate = false;
							AcsLoggerInfo gonner = loggers.put(newLoggerName, loggerInfo);
							if (gonner != null) {
								m_internalLogger.info("Process name update on logger '" + newLoggerName + "' overwrote an existing logger. This should be reported to the ACS developers.");
							}
							loggers.remove(oldLoggerName);
						}
					}
				}
				m_internalLogger.finer("Changed processName='" + oldProcessName + "' to '" + newProcessName + "' and updated logger names.");
			} finally {
				processNameLock.unlock();
			}
		}
	}

	/**
	 * Because of build order problems (acsjlog before jcont), we cannot pass <code>ContainerServices</code> or some
	 * other alarm API to <code>ClientLogManager</code>. Instead we define the alarm methods here and let the user
	 * delegate to the underlying alarm API.
	 * @see ClientLogManager#enableLoggingAlarms(LogAlarmHandler)
	 */
	public static interface LogAlarmHandler
	{
		public void raiseAlarm(String faultFamily, String faultMember, int faultCode) throws AcsJCouldntPerformActionEx;

		public void clearAlarm(String faultFamily, String faultMember, int faultCode) throws AcsJCouldntPerformActionEx;
	}

	/**
	 * @since ACS 9.1
	 */
	public void enableLoggingAlarms(LogAlarmHandler logAlarmHandler) {
		this.logAlarmHandler = logAlarmHandler;
	}

	/**
	 * Shuts down remote ACS logging and nulls the singleton instance.
	 * The loggers can still be used, but will only log locally. 
	 * @param wait
	 */
	public void shutdown(boolean wait) {

		if (DEBUG) {
			System.out.println("ClientLogManager#shutdown(" + wait + ") called.");
		}

		// clean up remote logging, if it's still active
		logQueueLock.lock();
		try {
			if (logQueue != null) {
				// stop adding more log records for remote logging
				disableRemoteLogging();

				if (wait) {
					flushAll();
				} else {
					// trigger one last flush, which may itself attempt to trigger more flushes, 
					// but only until shutDown prohibits further scheduling.
					Future<Boolean> flushFuture = logQueue.flush();
					// wait at most 200 milliseconds, to give the flush a chance to reach the central log service.
					// we don't care about the result.
					try {
						flushFuture.get(200, TimeUnit.MILLISECONDS);
					} catch (Exception e) {
						e.printStackTrace();
					}
				}
				logQueue.shutDown();
				logQueue = null;
			}

			// junit classloaders don't manage to reload this class between tests, so we explicitly null the instance
			s_instance = null;

			// todo: check if we should release the Log service with the manager 
			// (probably not since currently it doesn't even require us to be logged in to access the Log service).
		} finally {
			logQueueLock.unlock();
		}
	}
    
    
	/**
	 * Flushes the remote log queue completely and returns only when it's done. 
	 */
	public void flushAll()
	{
        if (logQueue != null) {
            logQueue.flushAllAndWait();
        }
    }

	/**
	 * Callback that gets notified about throttle actions and raises/clears throttle alarms,
	 * using {@link ClientLogManager#logAlarmHandler}.
	 * <p>
	 * @TODO: Ensure that alarm gets cleared when container (or other throttled process) is shut down.
	 * Not clear yet if we want to do the same for crashes (though rare in Java).
	 * Thus maybe enough to add a shutdown method here, which gets called from {@link ClientLogManager#shutdown(boolean)},
	 * which clears the alarm and removes log throttling.
	 */
	private class ThrottleCallback implements LogThrottle.ThrottleCallback {
		public static final String throttleAlarmFF = "Logging";
		/**
		 * The FM is the container name (=process name), which becomes available to ClientLogManager only later on.
		 */
		private String throttleAlarmFM;
		public static final int throttleAlarmFC = 10;
		
		private final AtomicLong suppressedLocalLogCount = new AtomicLong();
		private final AtomicLong suppressedRemoteLogCount = new AtomicLong();
		
		public void suppressedLog(boolean remoteLog) {
			boolean firstSuppressed = (suppressedLocalLogCount.get() + suppressedRemoteLogCount.get() == 0);
			if (remoteLog) {
				suppressedRemoteLogCount.incrementAndGet();
			}
			else {
				suppressedLocalLogCount.incrementAndGet();
			}
			if (firstSuppressed) {
				// raise alarm
				if (logAlarmHandler != null && processName != null) {
					// the processName should not change any more once it is != null, but just for safety, this copy will ensure 
					// that we later clear the alarm using the same FM, even if the process name changes in between calls.
					throttleAlarmFM = processName;
					try {
						logAlarmHandler.raiseAlarm(throttleAlarmFF, throttleAlarmFM, throttleAlarmFC);
						m_internalLogger.fine("Log throttle kicked in, suppressing logs. Alarm has been raised."); // TODO type-safe log
					} catch (AcsJCouldntPerformActionEx ex) {
						m_internalLogger.severe("Failed to publish alarm about dropping logs in the log throttle.");
					}
				}
				else {
					m_internalLogger.warning("Cannot raise alarm about log throttle action because alarm callback or process name is not yet available.");
				}
			}
		}
		public void clearedLogSuppression() {
			// @TODO: somehow say (alarm property, or only log?) how many logs were suppressed by the throttle,
			//        based on suppressedLocalLogCount, suppressedRemoteLogCount
			suppressedLocalLogCount.set(0);
			suppressedRemoteLogCount.set(0);
			if (logAlarmHandler != null) {
				try {
					// this method will not be called more than once a second because only a new time interval can "free" the logs
					// (see LogThrottle.LogStreamThrottle#intervalLengthMillis),
					// so that we don't have to worry about raising/clearing this alarm on a very short time scale.
					logAlarmHandler.clearAlarm(throttleAlarmFF, throttleAlarmFM, throttleAlarmFC);
					m_internalLogger.fine("Log throttle disengaged. Alarm has been cleared."); // TODO type-safe log
				} catch (AcsJCouldntPerformActionEx ex) {
					m_internalLogger.severe("Failed to clear alarm about dropping logs in the log throttle."); // TODO type-safe log
				}
			}
		}
	}

}
