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

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import org.omg.CORBA.ORB;
import org.omg.DsLogAdmin.Log;
import org.omg.DsLogAdmin.LogHelper;

import si.ijs.maci.Manager;

import alma.acs.logging.formatters.AcsXMLLogFormatter;
import alma.acs.logging.formatters.ConsoleLogFormatter;


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
 * Each logger has a property .level (see almalogging.properties) used to select
 * from among the incoming log messages only the ones of interest to the user.
 * Additionally, there is a global property .level.
 * 
 * @author hsommer, radi
 */
public class ClientLogManager
{
	private static final String DEFAULT_CENTRALIZED_LOGGER = "Log";

	/**
	 * TODO: get the service name from the CDB
	 */
	private String loggerContextName = DEFAULT_CENTRALIZED_LOGGER;

	/** logger namespace for container classes during operation */
	public static final String NS_CONTAINER = "alma.acs.container";

	/** parent of logger namespaces for application components */
    public static final String NS_COMPONENT = "alma.component";

    /** logger namespace for logger classes from this module */
    private static final String NS_LOGGER = "alma.acs.logger";

	/** property file with default logger settings to be found on the classpath */
	private static final String DEFAULT_LOG_PROPERTY_FILE = "almalogging.properties";

	/** property whose value is the property file with user settings */
	private static final String USER_LOG_PROPERTY_FILE_PROPERTY = "java.util.logging.config.file";

	/** instance of a singleton */
	private volatile static ClientLogManager s_instance;
    
    /** parent of all other remote loggers, so that its log handler gets shared */ 
    private Logger parentRemoteLogger;
    
    /** logging handler which feeds the queue for remote logging */ 
    private AcsLoggingHandler sharedRemoteHandler;

    protected DispatchingLogQueue logQueue;
    private RemoteLogDispatcher logDispatcher;

    /** used for local logging */ 
    private StdOutConsoleHandler sharedLocalHandler;

    /** logger used by the classes in this logging package */
	private Logger m_internalLogger;

	/** for testing */
	private boolean DEBUG = Boolean.getBoolean("alma.acs.logging.verbose");
    
	private LogManager m_logManager;

    
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
        m_logManager = LogManager.getLogManager();
        // todo: update or throw away the property file base logger configuration. 
        // currently it comes before the hard coded handler configuration, to not mess up anything. 
        setDefaultLogConfiguration();
        setUserLogConfiguration();

        // parentRemoteLogger is not removed in method disableRemoteLogging, and thus does not need to be
        // (re-) created in method prepareRemoteLogging (unlike the queue and the handler)
        parentRemoteLogger = Logger.getLogger("AcsRemoteLogger");
        prepareRemoteLogging();

        sharedLocalHandler = new StdOutConsoleHandler();
        
        sharedLocalHandler.setFormatter(new ConsoleLogFormatter());
        
        m_internalLogger = getInternalLogger();
        if (DEBUG){
            m_internalLogger.fine("ClientLogManager instance is created.");    
        }        
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
            sharedRemoteHandler = new AcsLoggingHandler(logQueue);
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

    
    private Logger createRemoteLogger(String loggerNamespace) {
        if (loggerNamespace == null) {
            loggerNamespace = "unknown";
            System.err.println("illegal namespace 'null' in ClientLogManager#createRemoteLogger turned to 'unknown'.");
        }
        
        Logger logger = null;
        
        // just to make sure we never throw an exception
        try {
            LogManager manager = LogManager.getLogManager();
            logger = manager.getLogger(loggerNamespace);
            if (logger == null) {
                logger = new AcsLogger(loggerNamespace, null);
                manager.addLogger(logger);
                logger = manager.getLogger(loggerNamespace);                
            
                logger.setParent(parentRemoteLogger);
                logger.setUseParentHandlers(true);
        
                Level loggerLevel = getLoggerLevel(loggerNamespace);
                logger.setLevel(loggerLevel);
                
                // currently all remote loggers should also log locally
                addLocalHandler(logger);
                
                if (DEBUG) {
                    System.out.println("created remote logger '" + loggerNamespace + "' (level " + loggerLevel + ") and separate local handler.");
                }
            }
        } catch (Throwable thr) {
            System.err.println("failed to create logger '" + loggerNamespace + "'.");
        }
        return logger;
    }
   
   
	/**
	 * Method getLoggerLevel. Needed for setting the level of each logger
	 * if it has been defined in the properties file.
	 * @param ns namespace
	 * @return Level
	 */
	public Level getLoggerLevel(String ns)
	{
		String lev = m_logManager.getProperty(ns + ".level");
		if (lev == null)
		{
			return Level.ALL;
		}
		
		if (lev.indexOf(".") == -1)
		{
			String startName = lev.substring(0, 1);
			String name = startName + lev.substring(1).toUpperCase();
			return Level.parse(name);
		}
		else if (lev.startsWith("Level."))
		{
			int start = lev.indexOf('.');
			String lvl = lev.substring(start);
			String name = lvl.substring(1).toUpperCase();
			return Level.parse(name);
		}
		else
		{
			System.err.println("Please set the logger's level according to the Java Logging API!");
			return Level.parse("OFF");
		}
	}
    
    
	/**
	 * Creates a logger for this module.
	 * @return Logger
	 */
	Logger getInternalLogger() {
        return createRemoteLogger(NS_LOGGER);
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
                
		org.omg.CORBA.IntHolder holder = new org.omg.CORBA.IntHolder();
        Log logService = null;
        int count = 0;
        String errMsg;
        do {
            errMsg = null;
            count++;
            holder.value = -1;
            try {
                logService = LogHelper.narrow(manager.get_service(managerHandle, loggerContextName, true, holder));
                if (logService == null) {
                    errMsg = "Failed to obtain central log service: reference 'null'. ";
                }
                else if (holder.value != 0) {
                    errMsg = "Obtained central log service, but with error code '" + holder.value + "'. ";
                }
                else { 
                    if (count > 1) {
                        // all is fine, but we report the difficulty
                        m_internalLogger.info("Connected to central log service after initial failure. ");
                    }
                    // make log service available to our dispatcher, and flush the records collected so far
                    logDispatcher = new RemoteLogDispatcher(orb, logService, new AcsXMLLogFormatter());
                    logQueue.setRemoteLogDispatcher(logDispatcher);
                    logQueue.flushAllAndWait();
                    logQueue.setPeriodicFlushing(10000);                    
                }
            }
            catch (Throwable thr) {                
                errMsg = "Failed to connect to central log service with exception " + thr.toString();
            }
            if (errMsg != null) {
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
	 * Sets the default logging configuration.
	 * It is taken from the file almalogging.properties that comes with ACS.
	 */
	private void setDefaultLogConfiguration()
	{
		InputStream is = null;

		try
		{
            // Matej: Here we use getClass().getResourceAsStream() and
            // NOT  ClassLoader.getSystemResourceAsStream()
            // This method is not compatible with WebStart
			// '/' means from the root (e.g. jar file)
			is = getClass().getResourceAsStream("/"+DEFAULT_LOG_PROPERTY_FILE);
			m_logManager.readConfiguration(is);
		}
		catch (Exception e)
		{
				System.err.println("failed to read default log configuration");
				e.printStackTrace(System.err);
		}
	}
    
	/**
	 * Sets the user-specific logging configuration.
	 */
	void setUserLogConfiguration()
	{
		String userConfigFile = System.getProperty(USER_LOG_PROPERTY_FILE_PROPERTY);
		if (userConfigFile != null)
		{
			try
			{
				InputStream in = new FileInputStream(userConfigFile);
				BufferedInputStream bin = new BufferedInputStream(in);
				m_logManager.readConfiguration(bin);
//				System.out.println("configured logging from user-supplied properties file " + userConfigFile);
			}
			catch (Exception e)
			{
				System.err.println("failed to read user log configuration file '" + userConfigFile + "': ");
				e.printStackTrace(System.err);
			}
		}
	}
    
	/**
	 * Gets a logger to be used by the Java container classes.
	 * The logger is connected to the central ACS logger.
	 * <p>
	 * Tries to find <code>almalogging.properties</code> on the classpath and
	 * feed it to <code>java.util.logging.LogManager</code>.
	 * <p>
	 * It will override settings of <code>java.util.logging.LogManager</code>,
	 * including those from a user-supplied logging properties file
	 * given in <code>java.util.logging.config.file</code>.
	 * Therefore, user settings must be re-set.
	 * <p>
	 * TODO (perhaps) retrieve logger settings from CDB
	 * 		(currently the property file should be found in acsjlog.jar)
	 */
	public Logger getLoggerForContainer(String containerName) {
        String ns = NS_CONTAINER;
        if (containerName != null) {
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
     * @param namespace  the logger namespace, should identify the application.
     * @param enableRemoteLogging  if true (generally recommended), log messages will be sent to the remote log service,
     *                             as it always happens for container and component loggers. 
     * @return a configured Logger  
     */
    public Logger getLoggerForApplication(String namespace, boolean enableRemoteLogging)
    {
        if (namespace == null || namespace.trim().length() == 0) {
            namespace = "unknownClientApplication";
        }
        Logger logger = null;
        if (enableRemoteLogging) {
            logger = createRemoteLogger(namespace);
        }
        else {
            logger = Logger.getLogger(namespace);
            addLocalHandler(logger);
        }
        return logger;
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
}
