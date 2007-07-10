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
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.logging.Handler;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import org.omg.CORBA.ORB;
import org.omg.DsLogAdmin.Log;
import org.omg.DsLogAdmin.LogHelper;

import si.ijs.maci.Manager;

import alma.acs.logging.config.LogConfig;
import alma.acs.logging.config.LogConfigException;
import alma.acs.logging.config.LogConfigSubscriber;
import alma.acs.logging.formatters.AcsXMLLogFormatter;
import alma.acs.logging.formatters.AcsBinLogFormatter;
//TODO: CARLI fix this to match the other formatters
import alma.acs.logging.formatters.ConsoleLogFormatter;

import alma.maci.loggingconfig.LoggingConfig;


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
 * 
 * @author hsommer
 */
public class ClientLogManager implements LogConfigSubscriber
{
	/** logger namespace for CORBA classes (ORB, POAs, etc) */
	public static final String NS_CORBA = "alma.acs.corba";

	/** logger namespace for container classes during operation */
	public static final String NS_CONTAINER = "alma.acs.container";

	/** parent of logger namespaces for application components */
    public static final String NS_COMPONENT = "alma.component";

    /** logger namespace for logger classes from this module */
    private static final String NS_LOGGER = "alma.acs.logger";

    /** instance of a singleton */
	private volatile static ClientLogManager s_instance;

	
    /** The logging configurator shared by various classes of this package */  
    private final LogConfig logConfig;
    
	/** The (CORBA-) name of the remote logging service to which logs will be sent */
	private volatile String logServiceName;

	/** The time in seconds between the periodic log queue flushings */
	private volatile int flushPeriodSeconds;


    /** parent of all other remote loggers, so that its log handler gets shared */ 
    private Logger parentRemoteLogger;
    
    /** logging handler which feeds the queue for remote logging */ 
    private AcsLoggingHandler sharedRemoteHandler;

    protected DispatchingLogQueue logQueue;
    private RemoteLogDispatcher logDispatcher;

    /** used for local logging */ 
    private StdOutConsoleHandler sharedLocalHandler;

    /** logger used by the classes in this logging package */
	private final Logger m_internalLogger;

	/** for testing */
	private boolean DEBUG = Boolean.getBoolean("alma.acs.logging.verbose");
    private boolean LOG_BIN_TYPE = Boolean.getBoolean("ACS.loggingBin");

	/** the process name to be prepended to the name for the Corba logger, so that different ORB instances in the system can be distinguished */ 
    private String processName;

    private List<AcsLogger> loggersNeedingProcessNameUpdate = new ArrayList<AcsLogger>();
    
	/**
	 * Singleton accessor.
     * <p>
     * TODO: rename, because now that we have class {@link AcsLogManager}, this method name is confusing.
	 * @return ClientLogManager
	 */
	public static synchronized ClientLogManager getAcsLogManager()
	{
		if (s_instance == null)
		{
			s_instance = new ClientLogManager();
		}
		return s_instance;
	}

	protected ClientLogManager()
	{
        logConfig = new LogConfig();
        logConfig.addSubscriber(this);
        try {
            logConfig.initialize(); // will call configureLogging, using default values
        } catch (LogConfigException ex) {
            System.err.println("Failed to configure logging: " + ex.toString());
        }
        
        // parentRemoteLogger is not removed in method disableRemoteLogging, and thus does not need to be
        // (re-) created in method prepareRemoteLogging (unlike the queue and the handler)
        parentRemoteLogger = Logger.getLogger("AcsRemoteLogger");
        parentRemoteLogger.setUseParentHandlers(false); // otherwise the default log handler on the root logger will dump all logs to stderr
        prepareRemoteLogging();

        sharedLocalHandler = new StdOutConsoleHandler(logConfig);
        
        sharedLocalHandler.setFormatter(new ConsoleLogFormatter());
        
        m_internalLogger = createRemoteLogger(NS_LOGGER);
        logConfig.setInternalLogger(m_internalLogger);
        if (DEBUG){
            m_internalLogger.fine("ClientLogManager instance is created.");
        }        
	}

    /**
     * @see alma.acs.logging.config.LogConfigSubscriber#configureLogging(alma.acs.logging.LogConfig)
     */
    public void configureLogging(LogConfig logConfig) {
    	LoggingConfig loggingConfig = logConfig.getLoggingConfig();
        if (!loggingConfig.getCentralizedLogger().equals(logServiceName)) {
        	if (logServiceName == null) {
        		logServiceName = loggingConfig.getCentralizedLogger();
        	}
        	else {
        		m_internalLogger.warning("Dynamic switching of Log service not yet supported!");
        		// TODO: switch connection to new log service
        	}
        }
        
        if (loggingConfig.getFlushPeriodSeconds() != flushPeriodSeconds) {
        	flushPeriodSeconds = loggingConfig.getFlushPeriodSeconds();
        	if (logQueue != null) {
        		logQueue.setPeriodicFlushing(loggingConfig.getFlushPeriodSeconds());
        	}
        }
    }

    public LogConfig getLogConfig() {
        return logConfig;
    }
    
    /**
     * If not done already, sets up a remote handler with queue, and adds that handler 
     * to the shared (parent) remote logger.
     */
    protected void prepareRemoteLogging() {
        if (logQueue == null) {
            logQueue = new DispatchingLogQueue();
        }
        if (sharedRemoteHandler == null) {
            sharedRemoteHandler = new AcsLoggingHandler(logQueue, logConfig);
        }
        else {
            Handler[] handlers = parentRemoteLogger.getHandlers();
            for (int i = 0; i < handlers.length; i++) {
                if (sharedRemoteHandler == handlers[i]) {
                    if (DEBUG) {
                        System.err.println("attempt to add remote handler twice to logger " + parentRemoteLogger.getName());
                    }
                    return;
                }
            }
        }
        parentRemoteLogger.addHandler(sharedRemoteHandler);
    }

    
    /**
     * Removes, closes, and nulls the remote handler,
     * so that no more records are put into the queue, and both queue and handler can be garbage collected.
     * <p>
     * GC can be an advantage when logs have been queued for remote sending,
     * but instead of connecting to the remote logger, we get a call
     * to {@link #suppressRemoteLogging()}. All messages have been logged locally anyway,
     * so in this case we want to clean up all these <code>LogRecord</code>s.
     */
    protected void disableRemoteLogging() {
        if (sharedRemoteHandler != null) {
            logConfig.removeSubscriber(sharedRemoteHandler);
            parentRemoteLogger.removeHandler(sharedRemoteHandler);
            sharedRemoteHandler.close();
            sharedRemoteHandler = null;
        }
    }

    
    /**
     * Adds the local logging handler to the provided logger.
     * Note that local handlers are not attached to the parent logger, 
     * which is why this method must be called for every remote logger which also should output locally.
     * @param logger  the remote logger to be set up also for local logging.
     */
    private void addLocalHandler(Logger logger) {
        Handler[] handlers = logger.getHandlers();
        for (int i = 0; i < handlers.length; i++) {
            if (sharedLocalHandler == handlers[i]) {
                if (DEBUG) {
                    System.err.println("attempt to add local handler twice to logger " + logger.getName());
                }
                return;
            }
        }
        logger.addHandler(sharedLocalHandler);
    }

    
    private AcsLogger createRemoteLogger(String loggerNamespace) {
        if (loggerNamespace == null) {
            loggerNamespace = "unknown";
            System.err.println("illegal namespace 'null' in ClientLogManager#createRemoteLogger turned to 'unknown'.");
        }
        
        AcsLogger logger = null;
        
        // just to make sure we never throw an exception
        try {
            LogManager manager = LogManager.getLogManager();
            logger = (AcsLogger) manager.getLogger(loggerNamespace);
            if (logger == null) {
            	// not yet in cache, so we create the logger
                AcsLogger acsLogger = new AcsLogger(loggerNamespace, null, logConfig);
                manager.addLogger(acsLogger);
                
                // get it from the cache
                logger = (AcsLogger) manager.getLogger(loggerNamespace);
            
                logger.setParent(parentRemoteLogger);
                logger.setUseParentHandlers(true);
        
                logger.setProcessName(this.processName);
                if (loggerNamespace.startsWith(NS_COMPONENT)) {
                        logger.setSourceObject(loggerNamespace.substring(NS_COMPONENT.length()+1));
                }
                else if (loggerNamespace.startsWith(NS_CORBA)){
                        logger.setSourceObject(loggerNamespace.substring(NS_CORBA.length()+1));
                }
                else {
                        logger.setSourceObject(this.processName);
                }
                // currently all remote loggers should also log locally
                addLocalHandler(logger);
                
                if (DEBUG) {
                    System.out.println("created remote logger '" + loggerNamespace + "' (level " + logger.getLevel() + ") and separate local handler.");
                }
            }
        } catch (Throwable thr) {
            System.err.println("failed to create logger '" + loggerNamespace + "'.");
        }
        return logger;
    }
   

    
	/**
	 * Enables loggers to send log records to the central ACS logger service.
     * Tries to connect to the log service using the supplied ACS manager.
     * As long as this connection fails, this method can sleep for 10 seconds and then try to connect again,
     * if the parameter <code>retry</code> is <code>true</code>.
     * <p>
     * Execution time can be significant, so consider calling this method in a separate thread 
     * (which has no negative effect on the logging since all log records are cached and automatically sent off
     * once the log service is available). 
     * Use a daemon thread to avoid shutdown problems if this method still hangs in the login loop.
     * <p>
     * When the log service is obtained, the log queue used for remote logging will be flushed periodically 
     * to the log service every 10 seconds unless an otherwise triggered flush has done this already. <br>
     * TODO: check if this fixed automatic flush period should be overridden by a value in the CDB (attribute TBD). 
     * 
	 * @param orb  the ORB used in this JVM
	 * @param manager  the ACS manager
	 * @param managerHandle  handle assigned by the ACS Manager for this client
     * @param retry  if true, a failing connection to the log service will trigger another attempt 10 seconds later.
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
                
        Log logService = null;
        int count = 0;
        String errMsg;
        do {
            errMsg = null;
            count++;
            try {
                logService = LogHelper.narrow(manager.get_service(managerHandle, logServiceName, true));
                if (logService == null) {
                    errMsg = "Failed to obtain central log service '" + logServiceName + "': reference is 'null'. ";
                }
                else { 
                    if (count > 1) {
                        // all is fine, but we report the difficulty
                        m_internalLogger.info("Connected to central log service after initial failure. ");
                    }
                    // make log service available to our dispatcher, and flush the records collected so far
                    if(LOG_BIN_TYPE){
                        logDispatcher = new RemoteLogDispatcher(orb, logService, new AcsBinLogFormatter());
                    }else{
                        logDispatcher = new RemoteLogDispatcher(orb, logService, new AcsXMLLogFormatter());
                    }
                    logQueue.setRemoteLogDispatcher(logDispatcher);
                    logQueue.flushAllAndWait();
                    logQueue.setPeriodicFlushing(flushPeriodSeconds * 1000);
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
                System.err.println(errMsg + "Will try again in 10 seconds.");
                try {
                    Thread.sleep(10000);
                } catch (InterruptedException e) {
                    break;
                }
            }
        } while (retry && (errMsg != null));
        
        if (errMsg == null) {
            // normally there will be a remote handler already, which has captured all log records produced so far.
            // However, if suppressRemoteLogging was called, we need to set up remote logging from scratch. 
            prepareRemoteLogging();
            return true;
        }
        return false;
	}

    
    /**
     * Suppresses remote logging.
     * If log messages destined for remote logging have not been sent to the central log service yet
     * (e.g. because {@link #initRemoteLogging(ORB, Manager, int, boolean) initRemoteLogging} has not been called,
     * or because of sending failures), these log messages will be lost for remote logging.
     * Log messages produced after this call will not even be queued for remote logging.
     * <p>
     * This method should only be called by special ALMA applications such as the Observation Preparation tool,
     * which runs stand-alone, with all containers, managers, etc. in one JVM. 
     * In this case, no central logger is available, and all loggers which normally send their output to the central 
     * logger should be limited to local logging (stdout). 
     * <p>
     * It is possible (probably not useful in real life) to re-enable remote logging later,
     * by calling <code>initRemoteLogging</code>. 
     */
    public void suppressRemoteLogging() {
        disableRemoteLogging();
        logQueue = null;
    }

    
	/**
	 * Gets a logger to be used by ORB and POA classes.
	 * The logger is connected to the central ACS logger.
	 * <p>
	 * @param autoConfigureContextName  if true, the context (e.g. container name) will be appended to this logger's name as soon as it is available.
	 */
	public AcsLogger getLoggerForCorba(String corbaName, boolean autoConfigureContextName) {
        String ns = NS_CORBA;
        if (corbaName != null) {
        	corbaName = corbaName.trim();
            if (corbaName.length() > 0) {
                ns = ns + '.' + corbaName;
            }
        }
        boolean needsProcessNameUpdate = false; // use boolean instead of another processName!=null check to avoid sync issues
        if (autoConfigureContextName) {
        	if (processName != null) {
	        	// if the process name is already known, we can even use it for the regular logger name instead of using a later workaround 
	        	ns += "@" + processName;
		    }
        	else {
        		needsProcessNameUpdate = true;
        	}
        }
        AcsLogger logger = createRemoteLogger(ns);
        if (needsProcessNameUpdate) {
        	loggersNeedingProcessNameUpdate.add(logger);
        }
        return logger;
	}
	
	/**
	 * Gets a logger to be used by the Java container classes.
	 * The logger is connected to the central ACS logger.
	 */
	public Logger getLoggerForContainer(String containerName) {
        String ns = NS_CONTAINER;
        if (containerName != null) {
            setProcessName(containerName);
            containerName = containerName.trim();
            if (containerName.length() > 0) {
                ns = ns + '.' + containerName;
            }
        }
        return createRemoteLogger(ns);
	}

    /**
	 * Gets a logger object to be used by a component.
     * The logger namespace will be <code>alma.component</code> with the value of <code>subNamespace</code> appended.
	 * <p>
	 * This method is not supposed to be accessed directly in the component implementation.
	 * Instead, the implementation of <code>alma.acs.container.ContainerServices</code>
	 * should call this method.
	 *
	 * @param subNamespace	 optional logger namespace postfix; same kind as used by the JDK (<code>java.util.logging</code>).
	 * @return Logger
	 *
	 */
	public Logger getLoggerForComponent(String subNamespace)
	{
		String ns = NS_COMPONENT;
		if (subNamespace != null)
		{
			subNamespace = subNamespace.trim();
			if (subNamespace.length() > 0)
			{
				if (subNamespace.charAt(0) != '.')
				{
					subNamespace = '.' + subNamespace;
				}
				ns += subNamespace;
			}
		}
        return createRemoteLogger(ns);
	}
    

    /**
     * Gets a logger for an application (which is not an ACS component itself), e.g. a GUI application using the ACS ComponentClient. 
     * @param namespace  the logger namespace, should identify the application or the particular logger that gets requested.
     * @param enableRemoteLogging  if true (generally recommended), the returned logger is set up to send the logs to the remote log service,
     *                             as it always happens for container and component loggers. 
     *                             <emph>This will only work if {@link #initRemoteLogging(ORB, Manager, int, boolean)} is also called,
     *                             which happens automatically in <code>ComponentClient</code>. 
     *                             For a standalone application that is not ACS-aware (no manager known etc), remote logging is not available 
     *                             even with enableRemoteLogging == true.</emph> 
     * @return a configured Logger  
     */
    public Logger getLoggerForApplication(String namespace, boolean enableRemoteLogging)
    {
        if (namespace == null || namespace.trim().length() == 0) {
            namespace = "unknownClientApplication";
        }
        else {
           	setProcessName(namespace);
        }
        Logger logger = null;
        if (enableRemoteLogging) {
            logger = createRemoteLogger(namespace);
        }
        else {
            logger = Logger.getLogger(namespace);
            logger.setUseParentHandlers(false);
            addLocalHandler(logger);
            // this is a bit dirty: the local-only loggers are of the plain JDK Logger type which does not implement LogConfigSubscriber
            // and thus can't be configured in the normal way.
            // Nonetheless we want to configure its log level, for which we reuse code from AcsLogger
            AcsLogger.configureJDKLogger(logger, logConfig.getLoggingConfig());            
        }
        return logger;
    }
    
    
    /**
     * Takes the process name and overwrites previous names,
     * and updates all Loggers which are waiting to get their overly simple name enriched.
     * The new name will be the old name + @ + processName.
     * <p>
     * The update mechanism ensures that the process name will eventually be set also on loggers
     * which were created before the process name was known, e.g. component logger created before container logger.
     * @TODO check if we still need the process name appended to the logger name, now that we have a separate field for it in AcsLogger.
     * 
     * @param name
     */
    private void setProcessName(String processName) {
		if (processName != null) {
			this.processName = processName;
			for (Iterator<AcsLogger> iter = loggersNeedingProcessNameUpdate.iterator(); iter.hasNext();) {
				AcsLogger logger = iter.next();
				logger.setLoggerName(logger.getName() + "@" + processName);
				logger.setProcessName(processName);
			}
		}
	}

	/**
     * Shuts down remote ACS logging. 
     * The loggers can still be used, but will only log locally. 
     * @param wait
     */
    public void shutdown(boolean wait) {
        
        if (DEBUG) {
            System.out.println("ClientLogManager#shutdown(" + wait + ") called.");
        }
        
        // clean up remote logging, if it's still active
        if (logQueue != null) {
            // stop adding more log records for remote logging
            disableRemoteLogging();             
            
            if (wait) {
                flushAll();
            }
            else {
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
	 * Strips the prepended constants {@link #NS_CORBA}, {@link #NS_CONTAINER}, {@link #NS_COMPONENT} etc from the logger namespace.
	 * This allows for a short, but possibly not unique display of the logger name.   
	 */
	public static String stripKnownLoggerNamespacePrefix(String loggerName) {
		if (loggerName != null) {
            // try to strip off fixed prefix from logger namespace
            try {
                if (loggerName.startsWith(NS_COMPONENT)) {
                	loggerName = loggerName.substring(NS_COMPONENT.length()+1);
                }
                else if (loggerName.startsWith(NS_CONTAINER)) {
                	loggerName = loggerName.substring(NS_CONTAINER.length()+1);
                }
                else if (loggerName.startsWith(NS_CORBA)) {
                	loggerName = loggerName.substring(NS_CORBA.length()+1);
                }
            } catch (Exception e) {
                // fallback: use logger namespace
            }            
        }
		return loggerName;		
	}
}
