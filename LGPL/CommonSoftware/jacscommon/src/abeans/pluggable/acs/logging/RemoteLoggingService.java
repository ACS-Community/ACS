/*
 * @@COPYRIGHT@@
 */
 
package abeans.pluggable.acs.logging;

import java.util.ArrayList;
import java.util.logging.Level;

import javax.naming.Context;

import org.omg.CORBA.Any;
import org.omg.CORBA.ORB;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContext;
import org.omg.DsLogAdmin.Log;
import org.omg.DsLogAdmin.LogHelper;

import abeans.core.ComponentDescriptor;
import abeans.core.ComponentInitializationException;
import abeans.core.ComponentManager;
import abeans.core.ComponentSupport;
import abeans.core.Identifier;
import abeans.core.IllegalComponentStateException;
import abeans.core.InitializationException;
import abeans.core.Root;
import abeans.core.UnableToInstallComponentException;
import abeans.core.defaults.AbeansProperties;
import abeans.core.defaults.Configurable;
import abeans.core.defaults.ConfigurationService;
import abeans.core.defaults.DefaultThreadPoolService;
import abeans.core.defaults.MessageLogEntry;
import abeans.core.defaults.ThreadPoolService;
import abeans.pluggable.RemoteDirectory;
import abeans.pluggable.RemoteService;
import abeans.pluggable.acs.CORBAService;
import abeans.pluggable.acs.DefaultCORBAService;
import abeans.pluggable.acs.NamingServiceRemoteDirectory;

/**
 * This service implements ACS Remote Logging Service.
 * It caches logs and sends them to the centralized logger.
 * 
 * It depends on:
 * - CORBAService (defaults to DefaultCORBAService)
 * - RemoteDirectory (defaults to NamingServiceRemoteDirectory)
 * - ThreadPoolService (defaults to DefaultThreadPoolService)
 * 
 * This implementation best effort is to minimize bad/broken connection
 * impact to application execution performance (it should not block at all).
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class RemoteLoggingService
	extends ComponentSupport
	implements RemoteService, Configurable, Runnable
{

	/**
	 */
	private class SendTask implements Runnable
	{
		
		Any[] anys = null;
		
		/**
		 * Method SendTask.
		 * @param anys
		 */
		public SendTask(Any[] anys)
		{
			assert (anys != null);
			this.anys = anys;
		}

		/**
		 * @see java.lang.Runnable#run()
		 */
		public void run()
		{
			sendLogs(anys);

			synchronized (this)
			{
				threads--;
			}
		}
		
	}

	/**
	 * The constant denoting the name of the configuration resource (file).
	 */
	private static final String RLS_RESOURCE_LOC = "RemoteLoggingService";


	/**
	 * The constant denoting the value of the configuration key.
	 * A stringified IOR of the centralized logging object.
	 * The IOR is expected to denote a persistent object which implements the Telecom Log
	 * Service�s Log interface, in particular the <code>write_records</code> method
	 */
	private static final String CENTRALIZED_LOGGER = "CentralizedLogger";	// Log

	/**
	 * The constant denoting the value of the configuration key.
	 * Log entries whose priority is below (smaller than) the one specified
	 * with this property will be ignored (neither cached nor submitted to
	 * the centralized logging). In release version of the system, this will be
	 * set to LM_INFO (3), ignoring LM_TRACE and LM_DEBUG log entries.
	 * Debug version of the system will set this to LM_DEBUG (2).
	 * During development, it will be set to LM_TRACE (1).
	 */
	private static final String MIN_CACHE_PRIORITY = "MinCachePriority";		// 0

	/**
	 * The constant denoting the value of the configuration key.
	 * Log entries whose priority exceeds (is greater than) the one specified
	 * with this property will be directly transmitted to the centralized logging,
	 * bypassing the local cache. If this is less than MinCachePriority,
	 * the local cache feature is disabled.
	 */
	private static final String MAX_CACHE_PRIORITY = "MaxCachePriority";		// 31

	/**
	 * The constant denoting the value of the configuration key.
	 * The number of log entries that can be kept in the local cache.
	 * When this number is reached, all log entries are transferred to the centralized
	 * logging. If network connection is not available, the local cache continues to grow,
	 * and every submitting of a log entry will attempt to flush the cache to
	 * the centralized logging.
	 */
	private static final String CACHE_SIZE = "CacheSize";

	/**
	 * The constant denoting the value of the configuration key.
	 * The maximum number of log entries (also used for internal purposes) that can be kept in the local cache.
	 * When this number is reached, all log entries are lost (implementation to save them to file can be added).
	 */
	private static final String MAX_CACHE_SIZE = "MaxCacheSize";

	/**
	 * The constant denoting the value of the configuration key.
	 * The maximum number of threads taken from thread pool by this service.
	 * NOTE: this value should be less than number of threads in the thread pool.
	 */
	private static final String MAX_THREADS = "MaxThreads";

	/**
	 * The constant denoting the default Centralized Logger reference.
	 */
	private static final String DEFAULT_CENTRALIZED_LOGGER = "Log";

	/**
	 * The constant denoting the value of the configuration key.
	 * Switch to disable remote directory lookup,
	 * reference should be provided manually via <code>setRemoteDirectory</code> method.
	 */
	private static final String DISABLE_REMOTE_DIRECTORY = "DisableRemoteDirectoryLookup";

	/**
	 * The constant denoting the default min. cache priority.
	 */
	private static final int DEFAULT_MIN_CACHE_PRIORITY = 0;

	/**
	 * The constant denoting the default max. cache priority.
	 */
	private static final int DEFAULT_MAX_CACHE_PRIORITY = 31;

	/**
	 * The constant denoting the default cache size.
	 */
	private static final int DEFAULT_CACHE_SIZE = 10;

	/**
	 * The constant denoting the default max. cache size.
	 */
	private static final int DEFAULT_MAX_CACHE_SIZE = 500;

	/**
	 * The constant denoting the default max. number of threads.
	 */
	private static final int DEFAULT_MAX_THREADS = 10;

	/**
	 * Context name of the Centralized Logger.
	 */
	private String loggerContextName = DEFAULT_CENTRALIZED_LOGGER;

	/**
	 * The constant denoting the value of the configuration key.
	 * Log entries whose priority is below (smaller than) the one specified
	 * with this property will be ignored (neither cached nor submitted to
	 * the centralized logging). In release version of the system, this will be
	 * set to LM_INFO (3), ignoring LM_TRACE and LM_DEBUG log entries.
	 * Debug version of the system will set this to LM_DEBUG (2).
	 * During development, it will be set to LM_TRACE (1).
	 */
	private int minCachePriority = DEFAULT_MIN_CACHE_PRIORITY;

	/**
	 * The constant denoting the value of the configuration key.
	 * Log entries whose priority exceeds (is greater than) the one specified
	 * with this property will be directly transmitted to the centralized logging,
	 * bypassing the local cache. If this is less than MinCachePriority,
	 * the local cache feature is disabled.
	 */
	private int maxCachePriority = DEFAULT_MAX_CACHE_PRIORITY;

	/**
	 * The constant denoting the value of the configuration key.
	 * The number of log entries that can be kept in the local cache.
	 * When this number is reached, all log entries are transferred to the centralized
	 * logging. If network connection is not available, the local cache continues to grow,
	 * and every submitting of a log entry will attempt to flush the cache to
	 * the centralized logging.
	 */
	private int cacheSize = DEFAULT_CACHE_SIZE;

	/**
	 * The maximum number of log entries (also used for internal purposes) that can be kept in the local cache.
	 * When this number is reached, all log entries are lost (implementation to save them to file can be added).
	 */
	private int maxCacheSize = DEFAULT_MAX_CACHE_SIZE;

	/**
	 * The maximum number of threads taken from thread pool by this service.
	 */
	private int maxThreads = DEFAULT_MAX_THREADS;
	
	/**
	 * Properties of the service.
	 */
	private AbeansProperties serviceConfig = null;

	/**
	 * Component description of this plug.
	 */
	private final transient ComponentDescriptor descriptor =
		new ComponentDescriptor(getClass(), RemoteLoggingService.class, 1, "ACS Remote Logging Service", false, true, null);

	/**
	 * Cache.
	 * Note that <code>java.util.ArrayList</code> is not synchornized.
	 */
	private ArrayList cache = null;

	/**
	 * Caching policy.
	 */
	private boolean cacheEnabled = false;

	/**
	 * Thread Pool Service.
	 */
	private ThreadPoolService threadPool = null;

	/**
	 * CORBA ORB.
	 */
	private ORB orb = null;

	/**
	 * Destroy phase status.
	 */
	private boolean destroyPhase = false;

	/**
	 * Number of pending log messages to be sent.
	 */
	private int pending = 0;

	/**
	 * Number of threads used.
	 */
	private int threads = 0;

	/**
	 * Switch to disable remote directory lookups.
	 */
	private boolean disableRemoteDirectoryLookup = false;

	/**
	 * Remote Directory Service (used to lookup for cenralized logger reference).
	 */
	private RemoteDirectory remoteDirectory = null;

	/**
	 * CORBA Remote Directory Service (used to lookup for cenralized logger reference).
	 * Additional RD, if <code>remoteDirectory</code> is not present.
	 */
	private NamingContext corbaRemoteDirectory = null;

	/**
	 * Centralized Logger remote object reference.
	 */
	private Log logger = null;

	/**
	 * CORBA service component;
	 */
	protected CORBAService corbaService = null;
	
	/**
	 * Constructor for RemoteLoggingService.
	 */
	public RemoteLoggingService()
	{
		super("RemoteLoggingService", "RmtLogSvc", Identifier.PLUG);
	}

	/**
	 * @see abeans.core.Component#getComponentDescriptor()
	 */
	public ComponentDescriptor getComponentDescriptor()
	{
		return descriptor;
	}

	/**
	 * Initializes the component by placing it into the hierarchy.
	 *
	 * @param	manager		the parent of this component, non-<code>null</code>
	 * @param 	state 		must be <code>null</code>
	 * @param 	cdesc 		must be <code>null</code>
	 * @throws IllegalComponentStateException 
	 * 						when the <code>cdesc</code> is not <code>null</code>
	 * @throws ComponentInitializationException 
	 * 						when the manager already contains an authentication service
	 * 						instance
	 * @see abeans.core.Component#initialize(ComponentManager, Object, ComponentDescriptor)
	 */
	public void initialize(
		ComponentManager manager,
		Object state,
		ComponentDescriptor cdesc)
		throws IllegalComponentStateException, ComponentInitializationException
	{
		if (manager == null)
			throw new ComponentInitializationException(this, "Parameter 'manager' passed to initialize() was null.");
			
		if (cdesc != null)
		{
			IllegalComponentStateException icse = new IllegalComponentStateException(this, "Cannot interpret a non-null component state.");
			icse.putValue("state", state);
			icse.putValue("cdesc", cdesc);
			throw icse;
		}
		
		//
		// Configuration
		//

		ConfigurationService configurationService = (ConfigurationService)Root.getComponentManager().getComponent(ConfigurationService.class);
		if (configurationService == null) 
		{
			ComponentInitializationException cie = new ComponentInitializationException(this, "There is no Configuration service installed.");
			throw cie;
		}

		AbeansProperties ap = null;
		try
		{
			// load configuration (this has to be done before internalInitialize method is called
			ap = configurationService.getConfiguration(this, getConfigurationName());
			setConfiguration(ap);
		}
		catch (InitializationException ie)
		{
			ComponentInitializationException cie = new ComponentInitializationException(this, "Unable to initialize service.", ie);
			cie.putValue("ap", ap);
			throw cie;
		}

		//
		// CORBAService
		//

		// try to install Default CORBA Service, if none installed yet
		corbaService = (CORBAService)Root.getComponentManager().getComponent(CORBAService.class);
		if (corbaService == null) 
		{
			try
			{
				Root.getComponentManager().installComponent(DefaultCORBAService.class);
			}
			catch (UnableToInstallComponentException uice)
			{
				ComponentInitializationException cie = new ComponentInitializationException(this, "Unable to install Default CORBA Service.", uice);
				throw cie;
			}
			corbaService = (CORBAService)Root.getComponentManager().getComponent(CORBAService.class);
		}
		
		if (corbaService == null) 
		{
			ComponentInitializationException cie = new ComponentInitializationException(this, "There is no CORBA Service installed.");
			throw cie;
		}

		orb = corbaService.getORB();
		assert(orb != null);
		
		if (!disableRemoteDirectoryLookup)
		{		
			//
			// RemoteDirectory
			//
	
			// try to install RemoteDirectory, if none installed yet
			remoteDirectory = (RemoteDirectory)Root.getComponentManager().getComponent(RemoteDirectory.class);
			if (remoteDirectory == null) 
			{
				try
				{
					Root.getComponentManager().installComponent(NamingServiceRemoteDirectory.class);
				}
				catch (UnableToInstallComponentException uice)
				{
					ComponentInitializationException cie = new ComponentInitializationException(this, "Unable to install CORBA Naming Remote Directory Service.", uice);
					throw cie;
				}
				remoteDirectory = (RemoteDirectory)Root.getComponentManager().getComponent(RemoteDirectory.class);
			}
			
			if (remoteDirectory == null) 
			{
				ComponentInitializationException cie = new ComponentInitializationException(this, "There is no Remote Directory Service installed.");
				throw cie;
			}
		}		

		//
		// ThreadPoolService
		//

		// get thread pool or install one, if it is not available
		threadPool = (ThreadPoolService)Root.getComponentManager().getComponent(ThreadPoolService.class);
		if (threadPool == null) 
		{
			try
			{
				Root.getComponentManager().installComponent(DefaultThreadPoolService.class);
			} catch (UnableToInstallComponentException utice)
			{
				throw new ComponentInitializationException(this, "Cannot access the 'ThreadPoolService' component.", utice);
			}
			threadPool = (ThreadPoolService)Root.getComponentManager().getComponent(ThreadPoolService.class);
		}

		if (threadPool == null) 
		{
			ComponentInitializationException cie = new ComponentInitializationException(this, "There is no Thread Pool Service installed.");
			throw cie;
		}

		// max. threads should never exceed number of threads in the pool service (-3)
		maxThreads = Math.min(maxThreads, threadPool.getMaximumPoolSize()-3);

		setParent(manager);

		if (!disableRemoteDirectoryLookup)
		{		
			connectLogger(true);
			if (logger == null)
				invalidateLogger(true);
		}
		
		if (isDebug())
			new MessageLogEntry(this, "initialize", "ACS Remote Logging Service successfully initialized.", Level.INFO).dispatch();
	}


	/**
	 * Sync. method which connects to the CL
	 * @throws ComponentInitializationException
	 */
	private void connectLogger(boolean allowLogging) throws ComponentInitializationException
	{
		assert (loggerContextName != null);

		// check if ORB is still available 		
		if (corbaService == null || corbaService.getORB() == null) return;
		
		if (remoteDirectory != null)
		{		
			try
			{
				Context ctx = remoteDirectory.getContext();
	
				logger = LogHelper.narrow((org.omg.CORBA.Object)ctx.lookup(loggerContextName));
	
				// down code does not work with JacORB
				//logger = (Log)ctx.lookup(loggerContextName);
				
				if (allowLogging && isDebug())
					new MessageLogEntry(this, "connectLogger", "Successfully obtained Centralized Logger reference.", Level.INFO).dispatch();
			}
			catch (Exception ex)
			{
				// do not throw exception, retry...
				if (allowLogging)
					new MessageLogEntry(this, "connectLogger", "Failed to resolve Centralized Logger using name '" + loggerContextName + "', will retry later...", ex, Level.WARNING).dispatch();
			}
		}
		else if (corbaRemoteDirectory != null)
		{
			try
			{
				NameComponent[] nc = new NameComponent[1];
				nc[0] = new NameComponent(loggerContextName, "");
				
				logger = LogHelper.narrow((org.omg.CORBA.Object)corbaRemoteDirectory.resolve(nc));
	
				if (allowLogging && isDebug())
					new MessageLogEntry(this, "connectLogger", "Successfully obtained Centralized Logger reference.", Level.INFO).dispatch();
			}
			catch (Exception ex)
			{
				// do not throw exception, retry...
				if (allowLogging)
					new MessageLogEntry(this, "connectLogger", "Failed to resolve Centralized Logger using name '" + loggerContextName + "', will retry later...", ex, Level.WARNING).dispatch();
			}
		}
	}
	
	/**
	 * Silently tries to connect to the logger.
	 */
	private void reconnectLogger() 
	{
		try
		{
			connectLogger(false);
		}
		catch (Exception ex)
		{
			// NOP
		}
		
		if (logger != null)
		{
			cacheCheck(false);
			new MessageLogEntry(this, "reconnectLogger", "Reconnected to the Centralized Logger.", Level.INFO).dispatch();
		}

	}

	/**
	 * Returns the descriptors for this service. 
	 * 
	 * @return an array of configuration descriptions
	 * @see abeans.core.defaults.Configurable#getConfigurationDescriptions()
	 */
	public String[][] getConfigurationDescriptions()
	{
		String[][] retVal = new String[8][2];
		retVal[0][0] = CENTRALIZED_LOGGER;
		retVal[0][1] = "Context name of the Centralized Logger, defaults to '" + DEFAULT_CENTRALIZED_LOGGER + "'.";
		retVal[1][0] = CACHE_SIZE;
		retVal[1][1] = "The number of log entries that can be kept in the local cache. When this number is reached, all log entries are transferred to the centralized logging, defaults to '" + DEFAULT_CACHE_SIZE + "'.";
		retVal[2][0] = MAX_CACHE_SIZE;
		retVal[2][1] = "The maximum number of log entries (also for internal purposes) that can be kept in the local cache. When this number is reached, all log entries are lost, defaults to '" + DEFAULT_MAX_CACHE_SIZE + "'.";
		retVal[3][0] = MIN_CACHE_PRIORITY;
		retVal[3][1] = "Log entries whose priority is below (smaller than) the one specified with this property will be ignored (neither cached nor submitted to the centralized logging), defaults to '" + DEFAULT_MIN_CACHE_PRIORITY + "'.";
		retVal[4][0] = MAX_CACHE_PRIORITY;
		retVal[4][1] = "Log entries whose priority exceeds (is greater than) the one specified with this property will be directly transmitted to the centralized logging, bypassing the local cache. If this is less than MinCachePriority, the local cache feature is disabled, defaults to '" + DEFAULT_MAX_CACHE_PRIORITY + "'.";
		retVal[5][0] = MAX_THREADS;
		retVal[5][1] = "The maximum number of threads taken from thread pool by this service, defaults to '" + DEFAULT_MAX_THREADS + "' (minimal 2).";
		retVal[6][0] = DISABLE_REMOTE_DIRECTORY;
		retVal[6][1] = "Switch to disable remote directory lookup, defaults to 'false'.";
		retVal[7][0] = DEBUG;
		retVal[7][1] = "Can take values 'true' or 'false'. If 'true', plug will print out additional debug information.";
		return retVal;
	}

	/**
	 * Returns name of the plug, the configuration name of this plug.
	 * 
	 * @return configuration name
	 * @see 	abeans.core.defaults.Configurable#getConfigurationName()
	 */
	public String getConfigurationName()
	{
		return RLS_RESOURCE_LOC;
	}

	/**
	 * Extracts Method extractIntProperty.
	 * @param name
	 * @param defaultValue
	 * @return int
	 */
	private int extractIntProperty(AbeansProperties props, String name, int defaultValue)
	{
		int retVal = defaultValue;

		String strVal = props.getProperty(name, null);
		if (strVal != null)
		{
			try
			{
				retVal = Integer.parseInt(strVal);
			}
			catch (Exception ex) {
				new MessageLogEntry(this, "extractIntProperty", "Failed to parse '"+name+"' property, '"+strVal+"' is not an integer value.  Using '"+defaultValue+"',", ex, Level.WARNING).dispatch(); 
			}
		}
		return	retVal;
	}

	/**
	 * Interprets the configuration delivered by Abeans configuration service.
	 * 
	 * @param 	prop		the configuration, if <code>null</code>, the method returns NOP
	 * @throws InitializationException 
	 * 						when the configuration cannot be interpreted
	 * @see				abeans.core.defaults.Configurable#setConfiguration(AbeansProperties)
	 */
	public void setConfiguration(AbeansProperties props) throws InitializationException
	{
		if (props == null || serviceConfig != null) return;
		serviceConfig = props;

		// set reference
		loggerContextName = props.getProperty(CENTRALIZED_LOGGER, DEFAULT_CENTRALIZED_LOGGER);

		// cache size
		cacheSize = Math.max(0, extractIntProperty(props, CACHE_SIZE, DEFAULT_CACHE_SIZE));

		// max. cache size
		maxCacheSize = Math.max(0, extractIntProperty(props, MAX_CACHE_SIZE, DEFAULT_MAX_CACHE_SIZE));

		// min. cache priority
		minCachePriority = extractIntProperty(props, MIN_CACHE_PRIORITY, DEFAULT_MIN_CACHE_PRIORITY);

		// max. cache priority
		maxCachePriority = extractIntProperty(props, MAX_CACHE_PRIORITY, DEFAULT_MAX_CACHE_PRIORITY);

		// max. threads
		maxThreads = Math.max(2, extractIntProperty(props, MAX_THREADS, DEFAULT_MAX_THREADS));
		
		// if maxCachePriority is less than minCachePriority, the local cache feature is disabled.
		if (maxCachePriority < minCachePriority)
			cacheSize = 0;

		// fix cache size
		if (cacheSize > maxCacheSize)
			cacheSize = maxCacheSize;
			
		// disable remote directory lookup
		disableRemoteDirectoryLookup = Boolean.valueOf(props.getProperty(DISABLE_REMOTE_DIRECTORY, "false")).booleanValue();

		// debug 
		boolean debug = Boolean.valueOf(props.getProperty(DEBUG, "false")).booleanValue(); 
		setDebug(debug);
		
		// reinitialize cache
		initializeCache();
	}

	/**
	 * Initialize cache buffer.
	 */
	private void initializeCache()
	{
		if (cacheSize <= 0)
		{
			cache = new ArrayList(100);
			cacheEnabled = false;
		}
		else
		{
			// some additional space
			int size = (int)(cacheSize*1.2);
			
			if (cache == null)
				cache = new ArrayList(size);	
			else
				cache.ensureCapacity(size);
				
			cacheEnabled = true;
		}
	}

	/**
	 * Checks if cache has reached its full capacity
	 * and sends the logs if necessary.
	 * @param 	forceSend		forces lofs to be sent
	 */
	private synchronized void cacheCheck(boolean forceSend)
	{
		if (cache != null)
		{

			// maximum cache size reached
			if (cache.size() > maxCacheSize)
			{
				int lost = cache.size();
				
				// clear cache
				cache.clear();
				
				// log a message, but only when cache is big enough (not to cause a cycle)
				if (isDebug() && cacheSize >= 5 && maxCacheSize >= 5)
					new MessageLogEntry(this, "cacheCheck", "Lost " + lost + " message logs.", Level.WARNING).dispatch(); 
			}

			if (logger == null ||
				(!forceSend && threads > maxThreads))
				return;

			// should we send
			if (cache.size() >= cacheSize || forceSend)
			{
				synchronized (this)
				{
					pending += cache.size();
				}

				Any[] anys = new Any[cache.size()];
				cache.toArray(anys);

				// run SendTask in separate thread
				try
				{

					threadPool.execute(new SendTask(anys));

					// thread spawned
					synchronized (this)
					{
						threads++;
					}
					cache.clear();	
					return;
					
				} catch (InterruptedException ie)
				{
					// do nothing, will be processed later
				}
				
			}
		}
	}


	/**
	 * Stores log into cache.
	 * If cache is disabled, log will be lost.
	 * @param log		log message
	 */
	private synchronized void cache(Any log)
	{
		if (cache != null)
		{
			// add to cache
			cache.add(log);
			
			cacheCheck(false);
		}
	}
	
	/**
	 * Stores logs into cache.
	 * If cache is disabled, logs will be lost.
	 * @param log		log message
	 */
	private synchronized void cache(Any[] logs)
	{
		if (cache != null)
		{
			// add to cache
			for (int i=0; i<logs.length; i++)
				cache.add(logs[i]);
			
			cacheCheck(false);
		}
	}

	/**
	 * Sends log to the centralized logger.
	 * If it fails, the log will be put back into cache.
	 * @param log		log message
	 */
	private void pushLog(Any log)
	{

		// send if you have log, otherwise cache
		if (logger != null && threads <= maxThreads)
		{
			synchronized (this)
			{
				pending++;
			}
			
			Any[] anys = new Any[] { log };
	
			try
			{
				
				threadPool.execute(new SendTask(anys));

				synchronized (this)
				{
					threads++;
				}
				return;
				
			} catch (InterruptedException ie)
			{
				// do nothing, will be processed later
			}
		}
		
		// if failed to spawn thread, cache the log
		cache(log);
	}


	/**
	 * Method run.
	 */
	public void run()
	{
		final int DELAY_IN_MS = 3000;


		while (!destroyPhase && logger == null)
		{
			reconnectLogger();

			if (logger == null)
			{
				try
				{
					Thread.sleep(DELAY_IN_MS);
				}
				catch (InterruptedException ie)
				{
				}
			}
		}

		synchronized (this)
		{
			threads--;
		}

	}

	/**
	 * Connect task
	 */
	private void invalidateLogger(boolean force)
	{
		final int DELAY_IN_MS = 1000;
		
		if  (logger == null && !force) return;
		
		logger = null;

		// thread count limit should not affect connection task

		// kep trying until connect task is spawned
		while (!destroyPhase && logger == null &&
				((remoteDirectory != null) || (corbaRemoteDirectory != null)))
		{

			try
			{

				threadPool.execute(this);

				synchronized (this)
				{
					threads++;
				}
				return;

			} catch (InterruptedException ie)
			{
				// do nothing, will be processed later
			}
	
			// sleep for a while
			if (DELAY_IN_MS > 0)
			{			
				try
				{
					Thread.sleep(DELAY_IN_MS);
				}
				catch (InterruptedException ie) { }
			}

		}
		
	}

	/**
	 * This is the method which actually sends the logs
	 * to the centralized logger (should be called in separate thread).
	 * If it fails, logs will be put back into cache.
	 * @param logs		log messages
	 */
	private void sendLogs(Any[] logs)
	{
		final int DELAY_IN_MS = 0;	//1000;
		final int MAX_RETRIES = 1; //3;

		for (int retry = 0; retry < MAX_RETRIES && logger != null; retry++)
		{
			Log logCopy = logger;

			// if we have logger, try to send
			if (logCopy != null)
			{
				try
				{

					logCopy.write_records(logs);

					synchronized (this)
					{
						pending -= logs.length;
					}

					return;
				} 
				catch (Exception ex)
				{
				}
			}

			// sleep for a while
			if (logger != null && DELAY_IN_MS > 0)
			{			
				try
				{
					Thread.sleep(DELAY_IN_MS);
				}
				catch (InterruptedException ie) { }
	
			}
			
		}

		invalidateLogger(false);
		
		// put back to cache
		cache(logs);
	}

	/**
	 * Log entry method.
	 * @param priority priority of log message
	 * @param log		log message
	 * @see java.util.logging.Handler#publish(LogRecord)
	 */
	public void log(int priority, String log)
	{
		// below logging priority check
		if (priority < minCachePriority || destroyPhase)
			return;
		
		// convert to Any
		Any anyLog = orb.create_any();
		anyLog.insert_string(log);

		// above logging priority check or cache is disabled
		if (priority > maxCachePriority || !cacheEnabled)
			// send to CL
			pushLog(anyLog);
		else
			// put to cache
			cache(anyLog);
	}

	/**
	 * Flushes all logs from cache.
	 * @see java.util.logging.Handler#flush()
	 */
	public void flush()
	{
		cacheCheck(true);
	}

	/**
	 * Overloads the destroy to first perform a clean flush.
	 */
	public void destroy()
	{
		// set destroy phase
		destroyPhase = true;
		
		// if there is way to flush
		if (!(remoteDirectory == null && corbaRemoteDirectory == null && logger == null))
		{
			flush();

			final int DELAY_IN_MS = 1000;
			final int MAX_RETRIES = 3;
	
			for (int retry = 0; retry < MAX_RETRIES && pending > 0; retry++)
			{
				try
				{
					Thread.sleep(DELAY_IN_MS);
				}
				catch (InterruptedException ie) {}
			}
		}
		
		super.destroy();
		
		int lost = pending+cache.size();
		if (isDebug() && lost > 0)
			new MessageLogEntry(this, "destroy", "Lost " + lost + " message logs.", Level.WARNING).dispatch();

	}
	

	/**
	 * Set remote directory used to lookup for centralized logger reference.
	 * @param directory	remote directory.
	 */
	public void setRemoteDirectory(RemoteDirectory directory)
	{
		remoteDirectory = directory;
		invalidateLogger(true);
	}

	/**
	 * Set remote directory used to lookup for centralized logger reference.
	 * @param directory	remote directory.
	 */
	public void setCORBARemoteDirectory(NamingContext nc)
	{
		corbaRemoteDirectory = nc;
		invalidateLogger(true);
	}

}
