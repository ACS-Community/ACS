/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci.manager;

import java.io.IOException;
import java.io.ObjectStreamField;
import java.io.Serializable;
import java.lang.reflect.Constructor;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.Timer;
import java.util.TimerTask;
import java.util.logging.Level;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.Name;
import javax.naming.NameAlreadyBoundException;
import javax.naming.NameNotFoundException;
import javax.naming.NameParser;
import javax.naming.NamingException;

import org.prevayler.Command;
import org.prevayler.Prevayler;
import org.prevayler.implementation.AbstractPrevalentSystem;
import org.prevayler.implementation.SnapshotPrevayler;

import EDU.oswego.cs.dl.util.concurrent.LinkedQueue;
import EDU.oswego.cs.dl.util.concurrent.Mutex;
import EDU.oswego.cs.dl.util.concurrent.PooledExecutor;
import EDU.oswego.cs.dl.util.concurrent.ReadWriteLock;
import EDU.oswego.cs.dl.util.concurrent.ReaderPreferenceReadWriteLock;
import EDU.oswego.cs.dl.util.concurrent.Sync;
import EDU.oswego.cs.dl.util.concurrent.SynchronizedBoolean;
import EDU.oswego.cs.dl.util.concurrent.SynchronizedInt;
import abeans.core.AssertionFailed;
import abeans.core.CoreException;
import abeans.core.Identifiable;
import abeans.core.Identifier;
import abeans.core.IdentifierSupport;
import abeans.core.InitializationException;
import abeans.core.defaults.ExceptionIgnorer;
import abeans.core.defaults.MessageLogEntry;
import abeans.framework.ApplicationContext;
import abeans.pluggable.RemoteException;
import abeans.pluggable.acs.logging.LoggingLevel;

import com.cosylab.acs.cdb.CDBAccess;
import com.cosylab.acs.cdb.DAOProxy;
import com.cosylab.acs.cdb.DAOProxyConnectionListener;
import com.cosylab.acs.maci.AccessRights;
import com.cosylab.acs.maci.ComponentSpec;
import com.cosylab.acs.maci.ComponentSpecIncompatibleWithActiveComponentException;
import com.cosylab.acs.maci.Container;
import com.cosylab.acs.maci.ContainerInfo;
import com.cosylab.acs.maci.Administrator;
import com.cosylab.acs.maci.BadParametersException;
import com.cosylab.acs.maci.Component;
import com.cosylab.acs.maci.ComponentInfo;
import com.cosylab.acs.maci.ComponentStatus;
import com.cosylab.acs.maci.Client;
import com.cosylab.acs.maci.ClientInfo;
import com.cosylab.acs.maci.Daemon;
import com.cosylab.acs.maci.HandleConstants;
import com.cosylab.acs.maci.HandleHelper;
import com.cosylab.acs.maci.IncompleteComponentSpecException;
import com.cosylab.acs.maci.IntArray;
import com.cosylab.acs.maci.InvalidComponentSpecException;
import com.cosylab.acs.maci.Manager;
import com.cosylab.acs.maci.MessageType;
import com.cosylab.acs.maci.NoDefaultComponentException;
import com.cosylab.acs.maci.NoResourcesException;
import com.cosylab.acs.maci.StatusHolder;
import com.cosylab.acs.maci.StatusSeqHolder;
import com.cosylab.acs.maci.Transport;
import com.cosylab.acs.maci.loadbalancing.LoadBalancingStrategy;
import com.cosylab.acs.maci.manager.recovery.ComponentInfoCommandComponentAdd;
import com.cosylab.acs.maci.manager.recovery.ComponentInfoCommandComponentRemove;
import com.cosylab.acs.maci.manager.recovery.ContainerCommandAllocate;
import com.cosylab.acs.maci.manager.recovery.ContainerCommandDeallocate;
import com.cosylab.acs.maci.manager.recovery.ContainerCommandSet;
import com.cosylab.acs.maci.manager.recovery.ContainerCommandUpdate;
import com.cosylab.acs.maci.manager.recovery.ContainerInfoCommandComponentAdd;
import com.cosylab.acs.maci.manager.recovery.ContainerInfoCommandComponentRemove;
import com.cosylab.acs.maci.manager.recovery.AdministratorCommandAllocate;
import com.cosylab.acs.maci.manager.recovery.AdministratorCommandDeallocate;
import com.cosylab.acs.maci.manager.recovery.AdministratorCommandSet;
import com.cosylab.acs.maci.manager.recovery.ComponentCommandAckAlloc;
import com.cosylab.acs.maci.manager.recovery.ComponentCommandAllocateHandle;
import com.cosylab.acs.maci.manager.recovery.ComponentCommandClientAdd;
import com.cosylab.acs.maci.manager.recovery.ComponentCommandDeallocate;
import com.cosylab.acs.maci.manager.recovery.ComponentCommandSet;
import com.cosylab.acs.maci.manager.recovery.ComponentCommandAllocate;
import com.cosylab.acs.maci.manager.recovery.ComponentCommandPreallocate;
import com.cosylab.acs.maci.manager.recovery.ComponentCommandClientRemove;
import com.cosylab.acs.maci.manager.recovery.ClientCommandAllocate;
import com.cosylab.acs.maci.manager.recovery.ClientCommandDeallocate;
import com.cosylab.acs.maci.manager.recovery.ClientCommandSet;
import com.cosylab.acs.maci.manager.recovery.ClientInfoCommandComponentAdd;
import com.cosylab.acs.maci.manager.recovery.ClientInfoCommandComponentRemove;
import com.cosylab.acs.maci.manager.recovery.DefaultComponentCommandPut;
import com.cosylab.acs.maci.manager.recovery.UnavailableComponentCommandPut;
import com.cosylab.acs.maci.manager.recovery.UnavailableComponentCommandRemove;
import com.cosylab.acs.maci.plug.ManagerProxy;
import com.cosylab.util.WildcharMatcher;

import alma.maciErrType.wrappers.AcsJNoPermissionEx;

/**
 * This class is an implementation of MACI com.cosylab.acs.maci.Manager.
 * It provides the actual internal implementation of the Manager
 * functionality in a way independent from the ACS maci Manager IDL interface.
 *
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 * @see		Manager
 */
public class ManagerImpl extends AbstractPrevalentSystem implements Manager, HandleConstants, Identifiable
{

	/**
	 * Synchronization helper class used for activation/deactivation synchronization.
	 * This class enforces <code>attempt</code> method of acquiring the locks to prevent deadlocks.
	 * Class also offers reference counting.
	 * (NOTE: automatic lock counting was not implemented due to imperfect usage.)
	 *
	 * Example of usage:
	 * <code>
	 *		ReferenceCountingLock lock;
	 * 		if (lock.acquire(3*Sync.ONE_MINUTE))
	 * 		{
	 * 			try
	 * 			{
	 * 				// critical section here
	 * 			}
	 * 			finally
	 * 			{
	 * 				lock.release();
	 * 			}
	 * 		}
	 * 		else
	 * 		{
	 * 			throw new TimoutException("Deadlock detected...");
	 * 		}
	 *
	 * </code>
	 */
	class ReferenceCountingLock
	{
		/**
		 * Number of current locks.
		 */
		private SynchronizedInt references = new SynchronizedInt(1);

		/**
		 * Synchronization mutex.
		 */
		private Sync lock = new Mutex();

		/**
		 * Constructor of <code>ReferenceCountingLock</code>.
		 * After construction lock is free and reference count equals <code>1</code>.
		 */
		public ReferenceCountingLock()
		{
			// no-op.
		}

		/**
		 * Attempt to acquire lock.
		 *
		 * @param	msecs	the number of milleseconds to wait.
		 * 					An argument less than or equal to zero means not to wait at all.
		 * @return	<code>true</code> if acquired, <code>false</code> othwerwise.
		 */
		public boolean acquire(long msecs)
		{
			try
			{
				return lock.attempt(msecs);
			}
			catch (InterruptedException ie)
			{
				return false;
			}
		}

		/**
		 * Release previously acquired lock.
		 */
		public void release()
		{
			lock.release();
		}

		/**
		 * Get number of references.
		 *
		 * @return number of references.
		 */
		public int referenceCount()
		{
			return references.get();
		}

		/**
		 * Increment number of references.
		 *
		 * @return number of references.
		 */
		public int increment()
		{
			return references.increment();
		}

		/**
		 * Decrement number of references.
		 *
		 * @return number of references.
		 */
		public int decrement()
		{
			return references.decrement();
		}

	}

	/**
	 * Task thats invokes <code>internalRequestComponent</code> method.
	 */
	class RequestComponentTask implements Runnable
	{
		private int h;
		private URI[] curls;

		public RequestComponentTask(int h, URI curl)
		{
			assert (curl != null);

			this.h = h;
			this.curls = new URI[] { curl };
		}

		public RequestComponentTask(int h, URI[] curls)
		{
			assert (curls != null);

			this.h = h;
			this.curls = curls;
		}

		public void run()
		{
			StatusHolder status = new StatusHolder();
			for (int i = 0; i < curls.length; i++)
			{
				try
				{
					internalRequestComponent(h, curls[i], status);

					if (status.getStatus() != ComponentStatus.COMPONENT_ACTIVATED)
						new MessageLogEntry(ManagerImpl.this, "ManagerImpl.RequestComponentTask", "Failed to activate requested component '"+curls[i]+"', reason: '"+status.getStatus()+"'.", LoggingLevel.DEBUG).dispatch();
				}
				catch (Throwable ex)
				{
					CoreException ce = new CoreException(ManagerImpl.this, "Failed to request component '"+curls[i]+"'.", ex);
					ce.caughtIn(this, "ManagerImpl.RequestComponentTask");
					ce.putValue("curl",  curls[i]);
					// exception service will handle this
				}
			}
		}
	}

	/**
	 * Task thats invokes <code>internalReleaseComponent</code> method.
	 */
	class ReleaseComponentTask implements Runnable
	{
		private int h;
		private int[] handles;

		public ReleaseComponentTask(int h, int handle)
		{
			this.h = h;
			this.handles = new int[] { handle };
		}

		public ReleaseComponentTask(int h, int[] handles)
		{
			assert (handles != null);

			this.h = h;
			this.handles = handles;
		}

		public void run()
		{
			for (int i = 0; i < handles.length; i++)
			{
				try
				{
					internalReleaseComponent(h, handles[i], false);
				}
				catch (Throwable ex)
				{
					CoreException ce = new CoreException(ManagerImpl.this, "Failed to release component with handle '"+handles[i]+"'.", ex);
					ce.caughtIn(this, "ManagerImpl.ReleaseComponentTask");
					ce.putValue("handle",  new Integer(handles[i]));
					// exception service will handle this
				}
			}
		}
	}

	/**
	 * Task thats invokes <code>internalDeactivateComponent</code> method.
	 */
	class DeactivateComponentTask extends TimerTask
	{
		private String name;

		public DeactivateComponentTask(String name)
		{
			super();
			this.name = name;
		}

		public void run()
		{
			try
			{
				internalDeactivateComponent(name);
			}
			catch (Throwable th)
			{
				CoreException ce = new CoreException(ManagerImpl.this, "Failed to deactivate component '"+name+"'.", th);
				ce.caughtIn(this, "ManagerImpl.DeactivateComponentTask");
				ce.putValue("name",  name);
				// exception service will handle this
			}
		}
	}

	/**
	 * Task thats invokes <code>shutdownContainer</code> method.
	 */
	class ShutdownContainerTask extends TimerTask
	{
		private String containerName;

		public ShutdownContainerTask(String containerName)
		{
			super();
			this.containerName = containerName;
		}

		public void run()
		{
			try
			{
				// shutdown only if component does not host any component
				ContainerInfo containerInfo = getContainerInfo(containerName);
				if (containerInfo == null)
					return;
				else
				{
					synchronized (containerInfo.getComponents())
					{
						if (containerInfo.getComponents().size() > 0)
							return;
					}
				}
				final int SHUTDOWN_CONTAINER_ACTION = 2 << 8;
				shutdownContainer(ManagerImpl.this.getHandle(), containerName, SHUTDOWN_CONTAINER_ACTION);
			}
			catch (Throwable th)
			{
				CoreException ce = new CoreException(ManagerImpl.this, "Failed to shutdown container '"+containerName+"'.", th);
				ce.caughtIn(this, "ManagerImpl.ShutdownContainerTask");
				ce.putValue("containerName",  containerName);
				// exception service will handle this
			}
		}
	}

	/**
	 * Implementation of <code>Component</code> interface for services.
	 */
	class ServiceComponent implements Component
	{
		/**
		 * Service representing this Component.
		 */
		private Object object;

		/**
		 * Construct service Component.
		 * @param	object	service itself, non-<code>null</code>.
		 */
		public ServiceComponent(Object object)
		{
			assert (object != null);

			this.object = object;
		}

		/**
		 * @see com.cosylab.acs.maci.Component#construct()
		 */
		public void construct() throws RemoteException
		{
		}

		/**
		 * @see com.cosylab.acs.maci.Component#destruct()
		 */
		public void destruct() throws RemoteException
		{
		}

		/**
		 * @see com.cosylab.acs.maci.Component#getObject()
		 */
		public Object getObject()
		{
			return object;
		}

		/**
		 * @see com.cosylab.acs.maci.Component#implementedInterfaces()
		 */
		public String[] implementedInterfaces()
		{
			return new String[] { object.getClass().getName() };
		}

		/**
		 * @see com.cosylab.acs.maci.Component#doesImplement(String)
		 */
		public boolean doesImplement(String type)
		{
			return false;
		}

	}

	/**
	 * All fields that represent state of the Manager must be listed here.
	 * Those fields will be written to and read from persistence storage
	 */
    private static final ObjectStreamField[] serialPersistentFields
                      = {
                      	new ObjectStreamField("domain", String.class),
                      	new ObjectStreamField("handle", Integer.TYPE),
                      	new ObjectStreamField("clients", HandleDataStore.class),
                      	new ObjectStreamField("administrators", HandleDataStore.class),
                      	new ObjectStreamField("containers", HandleDataStore.class),
                      	new ObjectStreamField("components", HandleDataStore.class),
                      	new ObjectStreamField("unavailableComponents", Map.class),
						new ObjectStreamField("defaultComponents", Map.class),
						new ObjectStreamField("domains", HashSet.class)};

	/**
	 * Interdomain manager handle mask.
	 */
    // TODO MF tmp
	private static final int INTERDOMAIN_MANAGER_HANDLE = 0x05555555;

	/**
	 * Manager domain name.
	 * @serial
	 */
	private String domain = DEFAULT_DOMAIN;

	/**
	 * Manager handle.
	 * @serial
	 */
	private int handle = MANAGER_MASK;

	/**
	 * Clients data.
	 * @serial
	 */
	private HandleDataStore clients = new HandleDataStore(16, HANDLE_MASK);

	/**
	 * Administrators data.
	 * @serial
	 */
	private HandleDataStore administrators = new HandleDataStore(16, HANDLE_MASK);

	/**
	 * Containers data.
	 * @serial
	 */
	private HandleDataStore containers = new HandleDataStore(32, HANDLE_MASK);

	/**
	 * Components data.
	 * @serial
	 */
	private HandleDataStore components = new HandleDataStore(128, HANDLE_MASK);

	/**
	 * List of all unavailable components.
	 * @serial
	 */
	private Map unavailableComponents = new HashMap();

	/**
	 * List of all pending activations.
	 * Needed for cyclid dependency checks, since non-fuly-activasted components
	 * are not accessible via HandleDataStore iterator.
	 */
	private transient Map pendingActivations = null;

	/**
	 * List of all pending container shutdowns.
	 */
	private transient Set pendingContainerShutdown = null;

	/**
	 * New container logged in notification.
	 */
	private transient Object containerLoggedInMonitor = null;

	/**
	 * Map of default components (set via getDynamicComponent) overriding CDB state.
	 * Entry is: (String type, String name).
	 * @serial
	 */
	private Map defaultComponents = new HashMap();

	/**
	 * Manager domain name.
	 */
	private transient Random random = null;

	/**
	 * Heartbeat timer.
	 */
	private transient Timer heartbeatTask = null;

	/**
	 * Delatyed release timer.
	 */
	private transient Timer delayedDeactivationTask = null;

	/**
	 * Root context of the remote directory.
	 */
	private transient Context remoteDirectory = null;

	/**
	 * Manager Component reference.
	 */
	private transient Object managerComponentReference = null;

	/**
	 * Remote Directory Component reference.
	 */
	private transient Object remoteDirectoryComponentReference = null;

	/**
	 * Activation/deactivation synchronization mechanism.
	 */
	private transient Map activationSynchronization;

	/**
	 * Activation/deactivation synchronization mechanism.
	 * Reader lock is acquired when activation/deactivation
	 * is pending (i.e. until completed) and writer lock by
	 * processes which require not to be run until
	 * activation/deactivation is in progress.
	 */
	private transient ReadWriteLock activationPendingRWLock;

	/**
	 * Shutdown status.
	 */
	private transient SynchronizedBoolean shutdown;

	/**
	 * Thread pool (guarantees order of execution).
	 */
	private transient PooledExecutor threadPool;

	/**
	 * Identifier.
	 */
	private transient Identifier id = null;

	/**
	 * Default manager domain.
	 */
	private static final String DEFAULT_DOMAIN = "";

	/**
	 * Number of threads in thread pool (guarantees order of execution).
	 */
	private static final int POOL_THREADS = 10;

	/*
	 * Lock timeout (deadlock detection time) in ms.
	 */
	private static long lockTimeout = 3 * Sync.ONE_MINUTE;	// 3 minutes

	/**
	 * Client ping interval.
	 */
	private static final int	CLIENT_PING_INTERVAL = 60000;		// 60 secs

	/**
	 * Administrator ping interval.
	 */
	private static final int	ADMINISTRATOR_PING_INTERVAL = 45000;		// 45 secs

	/**
	 * Container ping interval.
	 */
	private static final int	CONTAINER_PING_INTERVAL = 30000;		// 30 secs

	/**
	 * Container rights.
	 */
	private static final int	CONTAINER_RIGHTS = AccessRights.NONE;

	/**
	 * Release immediately keep-alive time.
	 */
	private static final int	RELEASE_IMMEDIATELY = 0;		// 0 secs

	/**
	 * Never release (immortal) immediately keep-alive time.
	 */
	private static final int	RELEASE_NEVER = -1;		// < 0 secs

	/**
	 * (Internal) undefined keep-alive time.
	 */
	private static final int	RELEASE_TIME_UNDEFINED = Integer.MIN_VALUE + 2;

	/**
	 * Shutdown implementation.
	 */
	private transient ManagerShutdown shutdownImplementation = null;

	/**
	 * Abeans application context where Manager lives in (used to access CDB).
	 */
	private transient ApplicationContext applicationContext = null;

	/**
	 * CDB access.
	 */
	private transient CDBAccess cdbAccess = null;

	/**
	 * Manager DAO dao (access to the CDB).
	 */
	private transient DAOProxy managerDAO = null;

	/**
	 * Components DAO dao (access to the CDB).
	 */
	private transient DAOProxy componentsDAO = null;

	/**
	 * Containers DAO dao (access to the CDB).
	 */
	private transient DAOProxy containersDAO = null;

	/**
	 * Cached list of all component entries in the CDB.
	 */
	private transient String[] componentListCache = null;

	/**
	 * CDB component specification system property name.
	 */
	private static final String NAME_CDB_COMPONENTSPEC = "ACS.CDBComponentSpec";

	/**
	 * CDB component specification.
	 * If non-<code>null</code> CDB will be automatically activated on container login.
	 */
	private transient ComponentSpec cdbActivation = null;

	/**
	 * Load balancing strategy system property name.
	 */
	private static final String NAME_LOAD_BALANCING_STRATEGY = "ACS.LoadBalancingStrategy";

	/**
	 * Load balancing strategy.
	 */
	private transient LoadBalancingStrategy loadBalancingStrategy = null;

	/**
	 * CDB disable system property name.
	 */
	private static final String NAME_CDB_DISABLE = "ACS.disableCDB";

	/**
	 * CURL URI schema
	 */
	private static final String CURL_URI_SCHEMA = "curl://";

	/**
	 * Manager federation domain list property name.
	 */
	private static final String NAME_DOMAIN_LIST = "ACS.domains";

	/**
	 * Manager federation central naming service.
	 */
	private static final String NAME_DOMAIN_DIRECTORY = "ACS.federationDirectory";

	/**
	 * Federation enabled flag.
	 */
	private transient boolean federationEnabled = false;

	/**
	 * Root context of the federation directory.
	 */
	private transient Context federationDirectory = null;

	/**
	 * List of domains to manage.
	 */
	private HashSet domains = new HashSet();

	/**
	 * Cache of non-local (federated) managers.
	 */
	private transient Map managerCache = null;

	/**
	 * Implementation of prevayler system.
	 */
	private transient Prevayler prevayler = null;

	/**
	 * Implementation of transport helper.
	 */
	private transient Transport transport = null;

	/**
	 * Component info topology sort manager.
	 */
	private transient ComponentInfoTopologicalSortManager topologySortManager;

	/**
	 * Initializes Manager.
	 * @param	prevayler			implementation of prevayler system
	 * @param	applicationContext	abeans application context where Manager lives in
	 * @param	context				remote directory implementation
	 */
	public void initialize(Prevayler prevayler, ApplicationContext applicationContext,
						   CDBAccess cdbAccess, Context context)
		throws InitializationException
	{
		this.prevayler = prevayler;
		this.remoteDirectory = context;

		if (applicationContext != null)
			setApplicationContext(applicationContext);

		if (cdbAccess != null)
			setCDBAccess(cdbAccess);

		random = new Random();
		heartbeatTask = new Timer(true);
		delayedDeactivationTask = new Timer(true);

		containerLoggedInMonitor = new Object();

		activationSynchronization = new HashMap();
		activationPendingRWLock = new ReaderPreferenceReadWriteLock();
		shutdown = new SynchronizedBoolean(false);
		threadPool = new PooledExecutor(new LinkedQueue());
		managerCache = new HashMap();
                pendingActivations = new HashMap();
                pendingContainerShutdown = Collections.synchronizedSet(new HashSet());

		// create threads
   		threadPool.setMinimumPoolSize(POOL_THREADS);
		threadPool.createThreads(POOL_THREADS);

		// read CDB startup
		try
		{
			String componentSpec = System.getProperty(NAME_CDB_COMPONENTSPEC);
			if (componentSpec != null)
			{
				cdbActivation = new ComponentSpec(componentSpec);
				new MessageLogEntry(this, "initialize", "Using CDB component specification: '" + cdbActivation + "'.", LoggingLevel.INFO).dispatch();
			}
		}
		catch (Throwable t)
		{
			new MessageLogEntry(this, "initialize", "Failed to parse '" + NAME_CDB_COMPONENTSPEC + "' variable, " + t.getMessage(), t, LoggingLevel.WARNING).dispatch();
		}

		// check load balancing strategy
		checkLoadBalancingStrategy();

		// register ping tasks
		initializePingTasks();

		// start topology sort manager
		topologySortManager = new ComponentInfoTopologicalSortManager(
				components, containers, activationPendingRWLock,
				pendingContainerShutdown, threadPool);
	}

	/**
	 * Initialized (registers) all ping tasks (to completely recover).
	 */
	private void initializePingTasks()
	{
		// go through all the containers, clients, administrators and register ping tasks,
		// reference of classes are already in TimerTaskContainerInfo/TimerTaskClientInfo

		// TODO some admin references can be null !!!

		// containers
		TimerTaskContainerInfo containerInfo = null;
		int h = containers.first();
		while (h != 0)
	    {
	    	containerInfo = (TimerTaskContainerInfo)containers.get(h);
			h = containers.next(h);

			// if deserialization failed, logout container
			ClientInfo clientInfo = containerInfo.createClientInfo();

			// register container to the heartbeat manager
			PingTimerTask task = new PingTimerTask(this, clientInfo);
			containerInfo.setTask(task);
			heartbeatTask.schedule(task, 0, CONTAINER_PING_INTERVAL);
	    }

	    // administrators
		TimerTaskClientInfo adminInfo = null;
		h = administrators.first();
		while (h != 0)
	    {
	    	adminInfo = (TimerTaskClientInfo)administrators.get(h);
			h = administrators.next(h);

			// register administrator to the heartbeat manager
			PingTimerTask task = new PingTimerTask(this, adminInfo);
			adminInfo.setTask(task);
			heartbeatTask.schedule(task, 0, ADMINISTRATOR_PING_INTERVAL);
	    }

	    // clients
		TimerTaskClientInfo clientInfo = null;
		h = clients.first();
		while (h != 0)
	    {
	    	clientInfo = (TimerTaskClientInfo)clients.get(h);
			h = clients.next(h);

			// register client to the heartbeat manager
			PingTimerTask task = new PingTimerTask(this, clientInfo);
			clientInfo.setTask(task);
			heartbeatTask.schedule(task, 0, CLIENT_PING_INTERVAL);
	    }
	}

	/**
	 * Checks and registers load balancing strategy.
	 * Load balancing strategy is defined as Java JVM system property named
	 * <code>NAME_LOAD_BALANCING_STRATEGY</code> contaning class name of the
	 * <code>LoadBalancingStrategy</code> implementation.
	 */
	private void checkLoadBalancingStrategy() {
		try
		{
			String loadBalancingStrategyClassName = System.getProperty(NAME_LOAD_BALANCING_STRATEGY);
			if (loadBalancingStrategyClassName != null)
			{
				// load class
				Class strategyClass = Class.forName(loadBalancingStrategyClassName);

				// create component implementation
				Constructor constructor = strategyClass.getConstructor((Class[])null);
				if (constructor == null)
					throw new IllegalArgumentException("Class '" + strategyClass.getName() + "' does have required default constructor.");
				Object strategyObject = constructor.newInstance((Class[])null);
				if (!(strategyObject instanceof LoadBalancingStrategy))
					throw new IllegalArgumentException("Class '" + strategyClass.getName() + "' does not implement '" + LoadBalancingStrategy.class.getName() + "' interface.");
				loadBalancingStrategy = (LoadBalancingStrategy)strategyObject;

				new MessageLogEntry(this, "checkLoadBalancingStrategy", "Using load balancing strategy: '" + strategyClass.getName() + "'.", LoggingLevel.INFO).dispatch();
			}
		}
		catch (Throwable t)
		{
			new MessageLogEntry(this, "checkLoadBalancingStrategy", "Failed to register '" + NAME_LOAD_BALANCING_STRATEGY + "' load balancing strategy: " + t.getMessage(), t, LoggingLevel.WARNING).dispatch();
		}
	}

	/**
	 * Returns the handle of the Manager.
	 * @return int	handle of the Manager.
	 */
	public int getHandle()
	{
		return handle;
	}

	/**
	 * Set name of the domain, which this manager will handle.
	 *
	 * @param	domain	name of the domain, which this manager will handle, non-<code>null</code>
	 * @see #getDomain
	 */
	public void setDomain(String domain)
	{
		assert (domain != null);

		this.domain = domain;
	}

	/**
	 * @see com.cosylab.acs.maci.Manager#getDomain()
	 */
	public String getDomain()
	{
		return domain;
	}

	/**
	 * @see com.cosylab.acs.maci.Manager#getContainerInfo(int, int[], String)
	 */
	public ContainerInfo[] getContainerInfo(int id, int[] handles, String name_wc)
		throws AcsJNoPermissionEx
	{
		if (isDebug())
			new MessageLogEntry(this, "getContainerInfo", new Object[] { new Integer(id), handles, name_wc }).dispatch();

		if (handles == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException(this, "Non-null 'handles' sequence expected.");
			af.caughtIn(this, "getContainerInfo");
			af.putValue("id", new Integer(id));
			af.putValue("handles", handles);
			af.putValue("name_wc", name_wc);
			throw af;
		}
		else if (handles.length == 0 && name_wc == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException(this, "Non-null 'names_wc' sequence expected.");
			af.caughtIn(this, "getContainerInfo");
			af.putValue("id", new Integer(id));
			af.putValue("handles", handles);
			af.putValue("name_wc", name_wc);
			throw af;
		}

/*
		Pattern pattern = null;
		if (handles.length == 0 && name_wc != null)
		{
			// test wildcard patten (try to compile it)
			try
			{
				pattern = Pattern.compile(name_wc);
			}
			catch (Exception ex)
			{
				// BAD_PARAM
				BadParametersException af = new BadParametersException(this, "Failed to compile 'names_wc' reqular expression string '"+name_wc+"'.");
				af.caughtIn(this, "getContainerInfo");
				af.putValue("name_wc", name_wc);
				throw af;
			}
		}
*/
		/****************************************************************/

		// info to be returned
		ContainerInfo[] info = null;

		// requesting info. about itself
		if (handles.length == 1 && handles[0] == id)
		{
			// check handle, no special rights for own info
			securityCheck(id, 0);

			info = new ContainerInfo[1];
			info[0] = getContainerInfo(id);
		}
		// get info of requested handles
		else if (handles.length > 0)
		{
			// check handle, INTROSPECT_MANAGER rights needed
			securityCheck(id, AccessRights.INTROSPECT_MANAGER);

			info = new ContainerInfo[handles.length];
			for (int i = 0; i < handles.length; i++)
				info[i] = getContainerInfo(handles[i]);
		}
		// get info requested using name wildcard
		else
		{
			// check handle, INTROSPECT_MANAGER rights needed
			securityCheck(id, AccessRights.INTROSPECT_MANAGER);

			// list of client matching search pattern
			ArrayList list = new ArrayList();

			// check clients
			synchronized (containers)
			{
				int h = containers.first();
				while (h != 0)
			    {
			    	ContainerInfo containerInfo = (ContainerInfo)containers.get(h);
					/*Matcher m = pattern.matcher(containerInfo.getName());
					if (m.matches())*/
					if (WildcharMatcher.match(name_wc, containerInfo.getName()))
						list.add(containerInfo);

					h = containers.next(h);
			    }
			}

			// copy to array
			info = new ContainerInfo[list.size()];
			list.toArray(info);
		}

		/****************************************************************/

		if (isDebug())
			new MessageLogEntry(this, "getContainerInfo", "Exiting.", Level.FINEST).dispatch();

		return info;
	}

	/**
	 * @see com.cosylab.acs.maci.Manager#getClientInfo(int, int[], String)
	 */
	public ClientInfo[] getClientInfo(int id, int[] handles, String name_wc)
		throws AcsJNoPermissionEx
	{
		if (isDebug())
			new MessageLogEntry(this, "getClientInfo", new Object[] { new Integer(id), handles, name_wc }).dispatch();

		if (handles == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException(this, "Non-null 'handles' sequence expected.");
			af.caughtIn(this, "getClientInfo");
			af.putValue("id", new Integer(id));
			af.putValue("handles", handles);
			af.putValue("name_wc", name_wc);
			throw af;
		}
		else if (handles.length == 0 && name_wc == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException(this, "Non-null 'names_wc' sequence expected.");
			af.caughtIn(this, "getClientInfo");
			af.putValue("id", new Integer(id));
			af.putValue("handles", handles);
			af.putValue("name_wc", name_wc);
			throw af;
		}

/*
		Pattern pattern = null;
		if (handles.length == 0 && name_wc != null)
		{
			// test wildcard patten (try to compile it)
			try
			{
				pattern = Pattern.compile(name_wc);
			}
			catch (Exception ex)
			{
				// BAD_PARAM
				BadParametersException af = new BadParametersException(this, "Failed to compile 'names_wc' reqular expression string '"+name_wc+"'.");
				af.caughtIn(this, "getClientInfo");
				af.putValue("name_wc", name_wc);
				throw af;
			}
		}
*/

		/****************************************************************/

		// info to be returned
		ClientInfo[] info = null;

		// requesting info. about itself
		if (handles.length == 1 && handles[0] == id)
		{
			// check handle, no special rights for own info
			securityCheck(id, 0);

			info = new ClientInfo[1];
			info[0] = getClientInfo(id);
		}
		// get info of requested handles
		else if (handles.length > 0)
		{
			// check handle, INTROSPECT_MANAGER rights needed
			securityCheck(id, AccessRights.INTROSPECT_MANAGER);

			info = new ClientInfo[handles.length];
			for (int i = 0; i < handles.length; i++)
				info[i] = getClientInfo(handles[i]);
		}
		// get info requested using name wildcard
		else
		{
			// check handle, INTROSPECT_MANAGER rights needed
			securityCheck(id, AccessRights.INTROSPECT_MANAGER);

			// list of clients matching search pattern
			ArrayList list = new ArrayList();

			// check clients
			synchronized (clients)
			{
				int h = clients.first();
				while (h != 0)
			    {
			    	ClientInfo clientInfo = (ClientInfo)clients.get(h);
			    	/*
					Matcher m = pattern.matcher(clientInfo.getName());
					if (m.matches())
					*/
					if (WildcharMatcher.match(name_wc, clientInfo.getName()))
						list.add(clientInfo);

					h = clients.next(h);
			    }
			}

			// check administrators
			synchronized (administrators)
			{
				int h = administrators.first();
				while (h != 0)
			    {
			    	ClientInfo clientInfo = (ClientInfo)administrators.get(h);
			    	/*
					Matcher m = pattern.matcher(clientInfo.getName());
					if (m.matches())
					*/
					if (WildcharMatcher.match(name_wc, clientInfo.getName()))
						list.add(clientInfo);

					h = administrators.next(h);
			    }
			}

			// copy to array
			info = new ClientInfo[list.size()];
			list.toArray(info);
		}

		/****************************************************************/

		if (isDebug())
			new MessageLogEntry(this, "getClientInfo", "Exiting.", Level.FINEST).dispatch();

		return info;
	}

	/**
	 * @see com.cosylab.acs.maci.Manager#getComponentInfo(int, int[], String, String, boolean)
	 */
	// TODO MF all (using wildchars match for domain names) interdomain queries
	public ComponentInfo[] getComponentInfo(int id, int[] handles, String name_wc, String type_wc, boolean activeOnly)
		throws AcsJNoPermissionEx
	{

		if (isDebug())
			new MessageLogEntry(this, "getComponentInfo", new Object[] { new Integer(id), handles, name_wc,
																	type_wc, new Boolean(activeOnly) }).dispatch();

		if (handles == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException(this, "Non-null 'handles' sequence expected.");
			af.caughtIn(this, "getComponentInfo");
			af.putValue("id", new Integer(id));
			af.putValue("handles", handles);
			af.putValue("name_wc", name_wc);
			af.putValue("type_wc", type_wc);
			throw af;
		}
		else if (handles.length == 0 && (name_wc == null || type_wc == null))
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException(this, "Non-null 'names_wc' or' type_wc' sequence expected.");
			af.caughtIn(this, "getComponentInfo");
			af.putValue("id", new Integer(id));
			af.putValue("handles", handles);
			af.putValue("name_wc", name_wc);
			af.putValue("type_wc", type_wc);
			throw af;
		}

		/****************************************************************/

		// the caller must have INTROSPECT_MANAGER access rights,
		// or it must have adequate privileges to access the component (the same as with the get_component method).
		securityCheck(id, AccessRights.NONE);

		// info to be returned
		ComponentInfo[] info = null;

		// get info of requested handles
		if (handles.length > 0)
		{
			info = new ComponentInfo[handles.length];
			for (int i = 0; i < handles.length; i++)
			{
				// access rights to be checked here...

				info[i] = getComponentInfo(handles[i]);

				// filter out unavailable
				if (info[i] != null && info[i].getComponent() == null)
					info[i] = null;
			}
		}
		// use name_wc and type_wc as search criteria
		else
		{
		    // check for inter-domain search
		    if (name_wc.startsWith(CURL_URI_SCHEMA))
		    {
		        URI curl = null;

		        try
                {
                    curl = CURLHelper.createURI(name_wc);
                    if (curl.getAuthority() != null && curl.getAuthority().indexOf('*') >= 0)
                        throw new IllegalArgumentException("Wildchars not supported in domain names.");
                } catch (URISyntaxException e) {
        			// BAD_PARAM
        			BadParametersException af = new BadParametersException(this, "Invalid CURL syntax in 'names_wc'.");
        			af.caughtIn(this, "getComponentInfo");
        			af.putValue("id", new Integer(id));
        			af.putValue("handles", handles);
        			af.putValue("name_wc", name_wc);
        			af.putValue("type_wc", type_wc);
        			throw af;
                }

                name_wc = extractName(curl);

                Manager remoteManager = null;

                // if not local do inter-domain query
                if (name_wc.startsWith(CURL_URI_SCHEMA))
                {
                    // TODO MF do the login?
        		    try
        		    {
        			    String domainName = curl.getAuthority();
        			    remoteManager = getManagerForDomain(domainName);
        			    if (remoteManager == null)
        			        throw new AssertionFailed(this, "Failed to obtain manager for domain '" + domainName + "'.");
        		    } catch (Throwable th) {
        				new MessageLogEntry(this, "getComponentInfo", "Failed to obtain non-local manager required by CURL '"+curl+"'.", th, LoggingLevel.WARNING).dispatch();
        				return null;
        		    }
                }

    			try
    			{
				    // local name to be used
				    String localName = curl.getPath();
					if (localName.charAt(0) == '/')
					    localName = localName.substring(1);
    			    ComponentInfo[] infos = remoteManager.getComponentInfo(INTERDOMAIN_MANAGER_HANDLE, handles, localName, type_wc, false);
    			    if (infos != null)
    			    {
    			        // prefix names
    			        final String prefix = CURL_URI_SCHEMA + curl.getAuthority() + "/";
    			        for (int i = 0; i < infos.length; i++)
    			        {
    			            if (!infos[i].getName().startsWith(CURL_URI_SCHEMA))
    			                infos[i].setName(prefix + infos[i].getName());
    			            String containerName = infos[i].getContainerName();
    			            if (containerName != null && !containerName.startsWith(CURL_URI_SCHEMA))
    			                infos[i].setContainerName(prefix + containerName);
    			        }
    			    }
    			    return infos;
    			}
    			catch (Exception ex)
    			{
    				RemoteException re = new RemoteException(this, "Failed to obtain component infos for CURL '"+curl+"' from remote manager.", ex);
    				re.caughtIn(this, "getComponentInfo");
    				re.putValue("id", new Integer(id));
    				re.putValue("handles", handles);
    				re.putValue("name_wc", name_wc);
    				re.putValue("type_wc", type_wc);
    				// exception service will handle this
    				new MessageLogEntry(this, "getComponentInfo", re.getMessage(), LoggingLevel.ERROR).dispatch();
    				return null;
    			}
		    }



			// map of components to be returned
			Map map = new HashMap();

			// read active/registered components
			synchronized (components)
			{
				int h = components.first();
				while (h != 0)
				{
					ComponentInfo componentInfo = (ComponentInfo)components.get(h);
					if (componentInfo.getComponent() != null &&
						WildcharMatcher.match(name_wc, componentInfo.getName()) &&
						WildcharMatcher.match(type_wc, componentInfo.getType()))
						{
							// access rights to be checked here...

							// found the match, add existing info to list
							map.put(componentInfo.getName(), componentInfo);
						}

					h = components.next(h);
				}
			}

			// add also non-active, if requested
			if (!activeOnly)
			{

				DAOProxy componentsDAO = getComponentsDAOProxy();
				if (componentsDAO != null)
				{

					try
					{
						// get names of all components
						/*String[] ids =*/ componentsDAO.get_field_data(""); /// @TODO here to check if CDB is available
					    String[] ids = getComponentsList();

						// test names
						for (int i = 0; i < ids.length; i++)
						{
							// read name
							String name = ids[i]; //readStringCharacteristics(componentsDAO, ids[i]+"/Name");
							if (name == null)
							{
								new MessageLogEntry(this, "getComponentInfo", "Misconfigured CDB, there is no name of component '"+ids[i]+"' defined.", LoggingLevel.WARNING).dispatch();
								continue;
							}

							// add if not already added and matches criteria
							if (!map.containsKey(name) &&
								//!name.equals(ComponentSpec.COMPSPEC_ANY) &&
							    name.indexOf(ComponentSpec.COMPSPEC_ANY) != 0 &&
								WildcharMatcher.match(name_wc, name))
							{

								// read type
								String type = readStringCharacteristics(componentsDAO, ids[i]+"/Type");
								if (type == null)
								{
									new MessageLogEntry(this, "getComponentInfo", "Misconfigured CDB, there is no type of component '"+name+"' defined.", LoggingLevel.WARNING).dispatch();
									continue;
								}

								// test type
								if (!type.equals(ComponentSpec.COMPSPEC_ANY) &&
									WildcharMatcher.match(type_wc, type))
								{
									// read code
									String code = readStringCharacteristics(componentsDAO, ids[i]+"/Code");
									if (code == null)
									{
										new MessageLogEntry(this, "getComponentInfo", "Misconfigured CDB, there is no code of component '"+name+"' defined.", LoggingLevel.WARNING).dispatch();
										continue;
									}

									// test code
									if (code.equals(ComponentSpec.COMPSPEC_ANY))
										continue;


									// read container
									String container = readStringCharacteristics(componentsDAO, ids[i]+"/Container");
									if (container == null)
									{
										new MessageLogEntry(this, "getComponentInfo", "Misconfigured CDB, there is no container name of component '"+name+"' defined.", LoggingLevel.WARNING).dispatch();
										continue;
									}

									// test container
									if (container.equals(ComponentSpec.COMPSPEC_ANY))
										continue;

									// got the match

									// access rights to be checked here...

									// create info and put it into list
									ComponentInfo retInfo = new ComponentInfo(0, name, type, code, null);
									retInfo.setContainerName(container);
									map.put(name, retInfo);

								}
							}
						}
					}
					catch (Exception ex)
					{
						CoreException ce = new CoreException(this, "Failed to obtain component data from the CDB.", ex);
						ce.caughtIn(this, "getComponentInfo");
						// exception service will handle this
						// new MessageLogEntry(this, "getComponentInfo", ce.getMessage(), ex, LoggingLevel.WARNING).dispatch();
					}

				}


			}

			// copy to array
			info = new ComponentInfo[map.size()];
			map.values().toArray(info);
		}

		/****************************************************************/

		if (isDebug())
			new MessageLogEntry(this, "getComponentInfo", "Exiting.", Level.FINEST).dispatch();

		return info;
	}


	/**
	 * @see com.cosylab.acs.maci.Manager#getService(int, java.net.URI, boolean, StatusHolder)
	 */
	public Component getService(int id,	URI curl, boolean activate,	StatusHolder status)
		throws AcsJNoPermissionEx
	{
		return getComponent(id, curl, activate, status, true);
	}

	/**
	 * @see com.cosylab.acs.maci.Manager#getServices(int, java.net.URI[], boolean, StatusSeqHolder)
	 */
	public Component[] getServices(int id, URI[] curls,	boolean activate, StatusSeqHolder statuses)
		throws AcsJNoPermissionEx
	{
		return getComponents(id, curls, activate, statuses, true);
	}

	/**
	 * @see com.cosylab.acs.maci.Manager#getComponent(int, URI, boolean, StatusHolder)
	 */
	public Component getComponent(int id, URI curl, boolean activate, StatusHolder status)
		throws AcsJNoPermissionEx
	{
		return getComponent(id, curl, activate, status, false);
	}

	/**
	 * @see com.cosylab.acs.maci.Manager#getComponents(int, URI[], boolean, StatusSeqHolder)
	 * @deprecated
	 */
	public Component[] getComponents(int id, URI[] curls, boolean activate, StatusSeqHolder statuses)
		throws AcsJNoPermissionEx
	{
		return getComponents(id, curls, activate, statuses, false);
	}

	/**
	 * @see #getComponent
	 */
	private Component getComponent(int id, URI curl, boolean activate, StatusHolder status, boolean allowServices)
		throws AcsJNoPermissionEx
	{
	
		if (isDebug())
			new MessageLogEntry(this, "getComponent", new Object[] { new Integer(id), curl,
										new Boolean(activate), status, new Boolean(allowServices) }).dispatch();
	
		// check if null
		checkCURL(curl);
	
		if (status == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException(this, "Non-null 'status' expected.");
			af.caughtIn(this, "getComponent");
			af.putValue("status", status);
			throw af;
		}

		/****************************************************************/
	
		// extract name
		String name = extractName(curl);
	
		// log info
		String requestorName = null;
	
		if (id != 0)
		{
			requestorName = getRequestorName(id);
			new MessageLogEntry(this, "getComponent", "'" + requestorName + "' requested component '" + curl + "'.", LoggingLevel.INFO).dispatch();
		}
		else
			new MessageLogEntry(this, "getComponent", "Request for component '" + curl + "' issued.", LoggingLevel.INFO).dispatch();
	
	
		// no login required for predefined objects (services)
	
		Component component = null;
	
		// "Manager" is a special service Component
		if (allowServices && name.equals("Manager"))
		{
			if (managerComponentReference != null)
				status.setStatus(ComponentStatus.COMPONENT_ACTIVATED);
			else
				status.setStatus(ComponentStatus.COMPONENT_DOES_NO_EXIST);

			component = new ServiceComponent(managerComponentReference);
		}
		// "NameService" is also a special service Component
		else if (allowServices && name.equals("NameService"))
		{
			if (remoteDirectoryComponentReference != null)
				status.setStatus(ComponentStatus.COMPONENT_ACTIVATED);
			else
				status.setStatus(ComponentStatus.COMPONENT_DOES_NO_EXIST);
	
			component = new ServiceComponent(remoteDirectoryComponentReference);
		}
		else if (allowServices
		         && !name.startsWith(CURL_URI_SCHEMA)
		         && isServiceComponent(name))
		{
			Object obj = lookup(name, null);
	
			// set status
			if (obj != null)
				status.setStatus(ComponentStatus.COMPONENT_ACTIVATED);
			else
				status.setStatus(ComponentStatus.COMPONENT_DOES_NO_EXIST);
	
			component = new ServiceComponent(obj);
		}
		else
		{
			// check handle and NONE permissions
			securityCheck(id, AccessRights.NONE);
	
			component = internalRequestComponent(id, curl, status, activate);
		}
	
		// log info
		if (component != null && component.getObject() != null)
		{
			if (requestorName != null)
				new MessageLogEntry(this, "getComponent", "Component '" + curl + "' provided to '" + requestorName + "'.", LoggingLevel.INFO).dispatch();
			else
				new MessageLogEntry(this, "getComponent", "Component '" + curl + "' provided.", LoggingLevel.INFO).dispatch();
		}
		else if (status.getStatus() != ComponentStatus.COMPONENT_NOT_ACTIVATED)
		{
			if (requestorName != null)
				new MessageLogEntry(this, "getComponent", "Request from '" + requestorName + "' for component '" + curl + "' completed sucessfully, but component not activated.", LoggingLevel.INFO).dispatch();
			else
				new MessageLogEntry(this, "getComponent", "Request for component '" + curl + "' completed sucessfully, but component not activated.", LoggingLevel.INFO).dispatch();
		}
		else
		{
			if (requestorName != null)
				new MessageLogEntry(this, "getComponent", "Failed to provide component '" + curl + "' to '" + requestorName + "'.", LoggingLevel.INFO).dispatch();
			else
				new MessageLogEntry(this, "getComponent", "Failed to provide component '" + curl + "'.", LoggingLevel.INFO).dispatch();
		}
	
		/****************************************************************/
	
		if (isDebug())
			new MessageLogEntry(this, "getComponent", "Exiting.", Level.FINEST).dispatch();
	
		return component;
	
	}

	/**
	 * @see #getComponents
	 * @deprecated
	 */
	private Component[] getComponents(int id, URI[] curls, boolean activate, StatusSeqHolder statuses, boolean allowServices)
		throws AcsJNoPermissionEx
	{
	
		if (isDebug())
			new MessageLogEntry(this, "getComponents", new Object[] { new Integer(id), curls,
											new Boolean(activate), statuses, new Boolean(allowServices) }).dispatch();
	
		// check if null
		if (curls == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException(this, "Non-null CURLs expected.");
			af.caughtIn(this, "getComponents");
			af.putValue("curls", curls);
			throw af;
		}
	
		if (statuses == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException(this, "Non-null 'statuses' expected.");
			af.caughtIn(this, "getComponents");
			af.putValue("statuses", statuses);
			throw af;
		}
	
		// check handle and NONE permissions
		securityCheck(id, AccessRights.NONE);
	
		/****************************************************************/
	
		int obtained = 0;
	
		Component[] components = new Component[curls.length];
		ComponentStatus[] componentStatuses = new ComponentStatus[curls.length];
		statuses.setStatus(componentStatuses);
	
		for (int i = 0; i < curls.length; i++)
		{
			StatusHolder status = new StatusHolder();
			try
			{
				components[i] = getComponent(id, curls[i], activate, status, allowServices);
				componentStatuses[i] = status.getStatus();
				obtained++;
			}
			catch (Exception ex)
			{
				components[i] = null;
				componentStatuses[i] = ComponentStatus.COMPONENT_NOT_ACTIVATED;
	
				CoreException ce = new CoreException(this, "Failed to get component '"+curls[i]+"'.", ex);
				ce.caughtIn(this, "getComponents");
				ce.putValue("curl", curls[i]);
				// exception service will handle this
			}
		}
	
		new MessageLogEntry(this, "getComponents", obtained + " of " + curls.length +" components obtained.", LoggingLevel.INFO).dispatch();
	
		/****************************************************************/
	
		if (isDebug())
			new MessageLogEntry(this, "getComponents", "Exiting.", Level.FINEST).dispatch();
	
		return components;
	
	}

	/**
	 * @see com.cosylab.acs.maci.Manager#getComponentNonSticky(int id, URI curl)
	 */
	public Component getComponentNonSticky(int id, URI curl) 
		throws AcsJNoPermissionEx
	{
		if (isDebug())
			new MessageLogEntry(this, "getComponentNonSticky", new Object[] { new Integer(id), curl }).dispatch();
	
		// check if null
		checkCURL(curl);

		/****************************************************************/
	
		// check handle and NONE permissions
		securityCheck(id, AccessRights.NONE);

		// extract name
		String name = extractName(curl);
	
		// log info
		String requestorName = getRequestorName(id);
		new MessageLogEntry(this, "getComponentNonSticky", "'" + requestorName + "' requested non-sticky component '" + curl + "'.", LoggingLevel.INFO).dispatch();
	
		Component component = null;
		synchronized (components)
		{
			int h = components.first();
			while (h != 0)
		    {
		    	ComponentInfo ci = (ComponentInfo)components.get(h);
				if (ci.getName().equals(name))
				{
					component = ci.getComponent();
					break;
				}
				h = components.next(h);
		    }
		}
	
		// log info
		if (component != null && component.getObject() != null)
			new MessageLogEntry(this, "getComponentNonSticky", "Non-sticky component '" + curl + "' provided to '" + requestorName + "'.", LoggingLevel.INFO).dispatch();
		else
			new MessageLogEntry(this, "getComponentNonSticky", "Failed to provide non-sticky component '" + curl + "' to '" + requestorName + "'.", LoggingLevel.INFO).dispatch();
	
		/****************************************************************/
	
		if (isDebug())
			new MessageLogEntry(this, "getComponentNonSticky", "Exiting.", Level.FINEST).dispatch();
	
		return component;
	
	}

	/**
	 * @see com.cosylab.acs.maci.Manager#makeComponentImmortal(int, java.net.URI, boolean)
	 */
	public void makeComponentImmortal(int id, URI curl, boolean immortalState) throws AcsJNoPermissionEx
	{
		if (isDebug())
			new MessageLogEntry(this, "makeComponentImmortal", new Object[] { new Integer(id), curl, new Boolean(immortalState) }).dispatch();

		// check if null
		checkCURL(curl);

		// check handle and NONE permissions
		securityCheck(id, AccessRights.NONE);

		/****************************************************************/

		// extract name
		String name = extractName(curl);

		int h;
		ComponentInfo componentInfo = null;

		synchronized (components)
		{
			h = components.first();
			while (h != 0)
		    {
		    	componentInfo = (ComponentInfo)components.get(h);
				if (componentInfo.getName().equals(name))
				{
					h = componentInfo.getHandle();
					break;
				}
				h = components.next(h);
		    }

			// component not yet activated check
			if (h == 0)
			{
				NoResourcesException af = new NoResourcesException(this, "Component not activated.");
				af.caughtIn(this, "makeComponentImmortal");
				af.putValue("id", new Integer(id));
				af.putValue("curl", curl);
				throw af;
			}

			// if not an owner of the component, check administrator rights
			if (!componentInfo.getClients().contains(id))
			{
				securityCheck(id, AccessRights.INTROSPECT_MANAGER);
			}

			if (immortalState)
			{
				// finally, add manager as an owner
				if (!componentInfo.getClients().contains(this.getHandle()))
				{
					// ACID - !!!
					executeCommand(new ComponentCommandClientAdd(componentInfo.getHandle() & HANDLE_MASK, this.getHandle()));
					//componentInfo.getClients().add(this.getHandle());
				}
				new MessageLogEntry(this, "makeComponentImmortal", "Component " + name + " was made immortal.", LoggingLevel.INFO).dispatch();
			}
		}

		// this must be done outside component sync. block
		if (!immortalState)
		{
			new MessageLogEntry(this, "makeComponentImmortal", "Component " + name + " was made mortal.", LoggingLevel.INFO).dispatch();

			// finally, can happen that the manager is the only owner
			// so release could be necessary
			internalReleaseComponent(this.getHandle(), h, false);
		}

		/****************************************************************/

		if (isDebug())
			new MessageLogEntry(this, "makeComponentImmortal", "Exiting.", Level.FINEST).dispatch();

	}

	/**
	 * @see com.cosylab.acs.maci.Manager#login(Client)
	 */
	public ClientInfo login(Client reference) throws AcsJNoPermissionEx
	{

		// NOTE: do not log references - prevents GC to finalize and terminate connection thread (JacORB)
		if (isDebug())
			new MessageLogEntry(this, "login", new Object[] { reference == null ? "null" : reference.toString() }).dispatch();

		// check if already shutdown
		if (shutdown.get())
		{
			// already shutdown
			AcsJNoPermissionEx npe = new AcsJNoPermissionEx();
			npe.setReason("Manager in shutdown state.");
			throw npe;
		}

		if (reference == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException(this, "Non-null 'reference' expected.");
			af.caughtIn(this, "login");
			throw af;
		}

		/****************************************************************/

		ClientInfo info = null;

		try
		{
			String reply = reference.authenticate("Identify yourself");

			if (reply == null)
			{
				// BAD_PARAM
				BadParametersException af = new BadParametersException(this, "Invalid response to 'Client::authenticate()' method - non-null string expected.");
				af.caughtIn(this, "login");
				af.putValue("reply", reply);
				throw af;
			}

			if (reply.length() > 0 &&
				(reply.charAt(0) != 'A' &&
				 reply.charAt(0) != 'C' &&
				 reply.charAt(0) != 'S'))
				{
					// NO_PERMISSION
					AcsJNoPermissionEx npe = new AcsJNoPermissionEx();
					npe.setReason("Invalid response to 'Client::authenticate()' method. [A,C,S] are expected as first characters.");
					throw npe;
				}

			// get client's name
			String name = reference.name();

			if (name == null)
			{
				// BAD_PARAM
				BadParametersException af = new BadParametersException(this, "Invalid response to 'Client::name()' method - non-null string expected.");
				af.caughtIn(this, "login");
				af.putValue("name", name);
				throw af;
			}

			if (isDebug())
				new MessageLogEntry(this, "login", "'"+name+"' is logging in.", LoggingLevel.DEBUG).dispatch();

			// delegate
			switch (reply.charAt(0))
			{
				// container
				case 'A':
					if (reference instanceof Container)
					{
						info = containerLogin(name, reply, (Container)reference);
					}
					else
					{
						// NO_PERMISSION
						AcsJNoPermissionEx npe = new AcsJNoPermissionEx();
						npe.setReason("Given reply to 'Client::authenticate()' method indicated container login, but given reference does not implement 'maci::Container' interface.");
						throw npe;
					}
					break;

				// client
				case 'C':
					info = clientLogin(name, reply, reference);
					break;

				// supervisor (administrator)
				case 'S':
					if (reference instanceof Administrator)
					{
						info = administratorLogin(name, reply, (Administrator)reference);
					}
					else
					{
						// NO_PERMISSION
						AcsJNoPermissionEx npe = new AcsJNoPermissionEx();
						npe.setReason("Given reply to 'Client::authenticate()' method indicated administrator login, but given reference does not implement 'maci::Administrator' interface.");
						throw npe;
					}
					break;

				default:
					assert(false);
			}

		}
		catch (AcsJNoPermissionEx npe)
		{
			throw npe;
		}
		catch (BadParametersException bpe)
		{
			throw bpe;
		}
		catch (NoResourcesException nre)
		{
			throw nre;
		}
		catch (RemoteException re)
		{
			AssertionFailed af = new AssertionFailed(this, "Exception caught while examining the client. Login rejected.", re);
			af.caughtIn(this, "login");
			// NOTE: do not log references - prevents GC to finalize and terminate connection thread (JacORB)
			af.putValue("reference", reference.toString());
			throw af;
		}
		catch (Exception ex)
		{
			AssertionFailed af = new AssertionFailed(this, "Unexpected exception during login. Login rejected.", ex);
			af.caughtIn(this, "login");
			// NOTE: do not log references - prevents GC to finalize and terminate connection thread (JacORB)
			af.putValue("reference", reference.toString());
			throw af;
		}

		/****************************************************************/

		if (isDebug())
			new MessageLogEntry(this, "login", "Exiting.", Level.FINEST).dispatch();

		return info;
	}

	/**
	 * @see com.cosylab.acs.maci.Manager#logout(int)
	 */
	public void logout(int id) throws AcsJNoPermissionEx
	{
		if (isDebug())
			new MessageLogEntry(this, "logout", new Object[] { new Integer(id) }).dispatch();

		if (isDebug())
			new MessageLogEntry(this, "logout", "Client with handle '" + HandleHelper.toString(id) + "' is logging out.", LoggingLevel.DEBUG).dispatch();

		// check handle, no special rights needed for logout
                // AcsJNoPermissionEx flies directly up from securityCheck()
		securityCheck(id, 0);

		/****************************************************************/

		switch	(id & TYPE_MASK)
		{
			case CONTAINER_MASK:
				containerLogout(id);
				break;
			case CLIENT_MASK:
				clientLogout(id);
				break;
			case ADMINISTRATOR_MASK:
				administratorLogout(id);
				break;
		}

		/****************************************************************/

		if (isDebug())
			new MessageLogEntry(this, "logout", "Exiting.", Level.FINEST).dispatch();
	}

	/**
	 * @see com.cosylab.acs.maci.Manager#registerComponent(int, URI, String, Component)
	 */
	public int registerComponent(int id, URI curl, String type, Component component)
		throws AcsJNoPermissionEx
	{

		// NOTE: do not log references - prevents GC to finalize and terminate connection thread (JacORB)
		if (isDebug())
			new MessageLogEntry(this, "registerComponent", new Object[] { new Integer(id), curl, type, component == null ? "null" : component.toString() }).dispatch();

		// check for null
		if (curl == null || type == null || component == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException(this, "Non-null parameters expected.");
			af.caughtIn(this, "registerComponent");
			af.putValue("id", new Integer(id));
			af.putValue("curl", curl);
			af.putValue("type", type);
			// NOTE: do not log references - prevents GC to finalize and terminate connection thread (JacORB)
			af.putValue("component", component == null ? "null" : component.toString());
			throw af;
		}

		// checks CURL, reject non-local domain curls
		checkCURL(curl, false);

		// check handle and REGISTER_COMPONENT permissions
		securityCheck(id, AccessRights.REGISTER_COMPONENT);

		/****************************************************************/

		// extract name
		String name = extractName(curl);

		int h = 0;

		synchronized (components)
		{

			// check if Component is already registred
			// if it is, return existing info
			h = components.first();
			while (h != 0)
		    {
		    	ComponentInfo registeredComponentInfo = (ComponentInfo)components.get(h);
				if (registeredComponentInfo.getName().equals(name))
				{
					if (registeredComponentInfo.getType().equals(type))
					{
						// it is already activated, add manager as an owner and return handle
						if (!registeredComponentInfo.getClients().contains(this.getHandle()))
						{
							// ACID - !!!
							executeCommand(new ComponentCommandClientAdd(registeredComponentInfo.getHandle() & HANDLE_MASK, this.getHandle()));
							//registredComponentInfo.getClients().add(this.getHandle());
						}

						return registeredComponentInfo.getHandle();
					}
					else
					{
						AcsJNoPermissionEx npe = new AcsJNoPermissionEx();
						npe.setReason("Component with name '"+name+"' but different type already registered.");
						throw npe;
					}

				}

				h = components.next(h);
		    }

			// allocate new handle
			// !!! ACID 2
			int handle = ((Integer)executeCommand(new ComponentCommandAllocate())).intValue();
			//int handle = components.allocate();
			if (handle == 0)
			{
				NoResourcesException af = new NoResourcesException(this, "Generation of new handle failed, too many components registred.");
				af.caughtIn(this, "registerComponent");
				af.putValue("curl", curl);
				throw af;
			}

			// generate external handle
			h = handle | COMPONENT_MASK;

			// add generated key
		    h |= (random.nextInt(0x100)) << 16;

			// create new component info
			ComponentInfo componentInfo = new ComponentInfo(h, name, type, null, component);

			// no container
			componentInfo.setContainer(0);
			componentInfo.setContainerName(null);

			// components can register other components
			componentInfo.setAccessRights(AccessRights.REGISTER_COMPONENT);

			// set Manager as client of the Component (to keep it immortal)
			componentInfo.getClients().add(this.getHandle());

			// set interfaces
			// NOTE: this could block since it is a remote call
			componentInfo.setInterfaces(component.implementedInterfaces());

			// !!! ACID - register AddComponentCommand
			executeCommand(new ComponentCommandSet(handle, componentInfo));
			// store info
			//components.set(handle, componentInfo);
		}


		// bind to remote directory
		// NOTE: this could block since it is a remote call
		bind(convertToHiearachical(name), "O", component);

		new MessageLogEntry(this, "registerComponent", "Component '"+name+"' registered.", LoggingLevel.INFO).dispatch();

		/****************************************************************/

		if (isDebug())
			new MessageLogEntry(this, "registerComponent", "Exiting.", Level.FINEST).dispatch();

		return h;
	}

	/**
	 * @see com.cosylab.acs.maci.Manager#unregisterComponent(int, int)
	 */
	public void unregisterComponent(int id, int h) throws AcsJNoPermissionEx
	{

		if (isDebug())
			new MessageLogEntry(this, "unregisterComponent", new Object[] { new Integer(id), new Integer(handle) }).dispatch();

		// check handle and REGISTER_COMPONENT permissions
		securityCheck(id, AccessRights.REGISTER_COMPONENT);

		/****************************************************************/

		internalReleaseComponent(this.getHandle(), h, false);

		new MessageLogEntry(this, "unregisterComponent", "Component with handle '"+HandleHelper.toString(h)+"' unregistered.", LoggingLevel.INFO).dispatch();

		/****************************************************************/

		if (isDebug())
			new MessageLogEntry(this, "unregisterComponent", "Exiting.", Level.FINEST).dispatch();

	}

	/**
	 * @see com.cosylab.acs.maci.Manager#restartComponent(int, URI)
	 */
	public Component restartComponent(int id, URI curl) throws AcsJNoPermissionEx
	{

		if (isDebug())
			new MessageLogEntry(this, "restartComponent", new Object[] { new Integer(id), curl }).dispatch();

		// checks CURL
		// TODO MF tmp, reject non-local domains
		checkCURL(curl, false);

		// check handle and NONE permissions
		securityCheck(id, AccessRights.NONE);

		/****************************************************************/

		Component component = internalRestartComponent(id, curl);

		if (component != null)
			new MessageLogEntry(this, "restartComponent", "Component '"+curl+"' restarted.", LoggingLevel.INFO).dispatch();
		else
			new MessageLogEntry(this, "restartComponent", "Failed to restart component '"+curl+"'.", LoggingLevel.INFO).dispatch();

		/****************************************************************/

		if (isDebug())
			new MessageLogEntry(this, "restartComponent", "Exiting.", Level.FINEST).dispatch();

		return component;

	}

	/**
	 * @see com.cosylab.acs.maci.Manager#releaseComponent(int, URI)
	 */
	public int releaseComponent(int id, URI curl) throws AcsJNoPermissionEx
	{

		if (isDebug())
			new MessageLogEntry(this, "releaseComponent", new Object[] { new Integer(id), curl }).dispatch();

		// checks CURL
		checkCURL(curl);

		// check handle and NONE permissions
		securityCheck(id, AccessRights.NONE);

		/****************************************************************/

		// log info
		String requestorName = getRequestorName(id);
		new MessageLogEntry(this, "releaseComponent", "'" + requestorName + "' requested release of component '" + curl + "'.", LoggingLevel.INFO).dispatch();

		int owners = internalReleaseComponent(id, curl, id == this.getHandle());

		new MessageLogEntry(this, "releaseComponent", "Component '" + curl + "' released by '" + requestorName + "'.", LoggingLevel.INFO).dispatch();

		/****************************************************************/

		if (isDebug())
			new MessageLogEntry(this, "releaseComponent", "Exiting.", Level.FINEST).dispatch();

		return owners;

	}

	/**
	 * @see com.cosylab.acs.maci.Manager#forceReleaseComponent(int, URI)
	 */
	public int forceReleaseComponent(int id, URI curl) throws AcsJNoPermissionEx
	{

		if (isDebug())
			new MessageLogEntry(this, "forceReleaseComponent", new Object[] { new Integer(id), curl }).dispatch();

		// checks CURL
		checkCURL(curl);

		// check handle and INTROSPECT_MANAGER permissions
		// TODO not clean !!! admin permissions required
		//securityCheck(id, AccessRights.INTROSPECT_MANAGER);

		// hack by HSO 2006-07-19: ACS 5.0.4 temporarily offers the forceReleaseComponent in the AdvancedContainerServices,
		// which will be removed when we have implemented the concept of non-sticky clients whose references must not prevent
		// the unloading of a component.
		// This feature will be used by some master components, which just cannot be admin/supervisors for the manager,
		// therefore we temporarily have to grant access to just any kind of client.
		securityCheck(id, AccessRights.NONE);

		/****************************************************************/

		// log info
		String requestorName = getRequestorName(id);
		new MessageLogEntry(this, "forceReleaseComponent", "'" + requestorName + "' requested forceful release of component '" + curl + "'.", LoggingLevel.INFO).dispatch();

		int owners = internalReleaseComponent(id, curl, true);

		new MessageLogEntry(this, "forceReleaseComponent", "Component '" + curl + "' forcefully released by '" + requestorName + "'.", LoggingLevel.INFO).dispatch();

		/****************************************************************/

		if (isDebug())
			new MessageLogEntry(this, "forceReleaseComponent", "Exiting.", Level.FINEST).dispatch();

		return owners;

	}

	/**
	 * @see com.cosylab.acs.maci.Manager#releaseComponents(int, URI[])
	 */
	public void releaseComponents(int id, URI[] curls) throws AcsJNoPermissionEx
	{

		if (isDebug())
			new MessageLogEntry(this, "releaseComponents", new Object[] { new Integer(id), curls }).dispatch();

		// check if null
		if (curls == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException(this, "Non-null CURLs expected.");
			af.caughtIn(this, "releaseComponents");
			af.putValue("curls", curls);
			throw af;
		}

		// check handle and NONE permissions
		securityCheck(id, AccessRights.NONE);

		/****************************************************************/

		int released = 0;

		for (int i = 0; i < curls.length; i++)
		{
			try
			{
				releaseComponent(id, curls[i]);
				released++;
			}
			catch (Exception ex)
			{
				CoreException ce = new CoreException(this, "Failed to release component '"+curls[i]+"'.", ex);
				ce.caughtIn(this, "releaseComponents");
				ce.putValue("curl", curls[i]);
				// exception service will handle this
			}
		}

		new MessageLogEntry(this, "releaseComponents", released + " of " + curls.length +" components released.", LoggingLevel.INFO).dispatch();

		/****************************************************************/

		if (isDebug())
			new MessageLogEntry(this, "releaseComponents", "Exiting.", Level.FINEST).dispatch();

	}

	/**
	 * Checks if container's state (e.g. not in shutdown state).
	 * @param	containerInfo	container's info to be checked.
	 */
	private void checkContainerShutdownState(ContainerInfo containerInfo) throws NoResourcesException
	{
		String containerName = containerInfo.getName();
		if (pendingContainerShutdown.contains(containerName))
		{
			// NO_RESOURCES
			NoResourcesException nre = new NoResourcesException(this, "Container '" + containerName + "' is being shutdown.");
			nre.caughtIn(this, "checkContainerState");
			nre.putValue("containerName", containerName);
			throw nre;
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Manager#shutdownContainer(int, java.lang.String, int)
	 */
	public void shutdownContainer(int id, String containerName, int action)
			throws AcsJNoPermissionEx {

		if (isDebug())
			new MessageLogEntry(this, "shutdownContainer", new Object[] { new Integer(id), containerName, new Integer(action) }).dispatch();

		// check if null
		if (containerName == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException(this, "Non-null 'containerName' expected.");
			af.caughtIn(this, "shutdownContainer");
			af.putValue("containerName", containerName);
			throw af;
		}

		/****************************************************************/

		// the caller must have SHUTDOWN_SYSTEM access rights,
		securityCheck(id, AccessRights.SHUTDOWN_SYSTEM);

		Container container;
		ContainerInfo containerInfo = getContainerInfo(containerName);
		if (containerInfo == null || (container = containerInfo.getContainer()) == null)
		{
			// NO_RESOURCES
			NoResourcesException nre = new NoResourcesException(this, "Container '" + containerName + "' not logged in.");
			nre.caughtIn(this, "shutdownContainer");
			nre.putValue("containerName", containerName);
			nre.putValue("containerInfo", containerInfo);
			throw nre;
		}

		pendingContainerShutdown.add(containerInfo.getName());
		try
		{
			// release components
			try
			{
				// get shutdown order
				ComponentInfo[] infos = topologySortManager.getComponentShutdownOrder(containerInfo);
				releaseComponents(infos);
			}
			catch (Throwable th)
			{
				CoreException ce = new CoreException(this, "Failed to release components on container '" + containerName + "'.", th);
				ce.caughtIn(this, "shutdownContainer");
				ce.putValue("containerInfo", containerInfo);
				// exception handler service will handle this
			}

			// shutdown (or disconnect)
			try
			{
				if (action == 0)
					container.disconnect();
				else
					container.shutdown(action);
			}
			catch (Throwable th)
			{
				// NO_RESOURCES
				NoResourcesException nre = new NoResourcesException(this, "Failed to shutdown container '" + containerName + "'.", th);
				nre.caughtIn(this, "shutdownContainer");
				nre.putValue("containerInfo", containerInfo);
				throw nre;
			}

		}
		finally
		{
			pendingContainerShutdown.remove(containerInfo.getName());
		}

		/****************************************************************/
		if (isDebug())
			new MessageLogEntry(this, "shutdownContainer", "Exiting.", Level.FINEST).dispatch();
	}

	/**
	 * Release components (using manager handle)
	 * @param infos	components to release
	 */
	private void releaseComponents(ComponentInfo[] infos) throws AcsJNoPermissionEx {
		if (infos.length > 0)
		{
			// map strings to CURLs
			URI[] uris = new URI[infos.length];
				for (int i = 0; i < infos.length; i++)
				{
					try
					{
						uris[i] = CURLHelper.createURI(infos[i].getName());
					}
					catch (URISyntaxException usi)
					{
						BadParametersException hbpe = new BadParametersException(this, usi.getMessage(), usi);
						hbpe.caughtIn(this, "releaseComponents");
						hbpe.putValue("infos[i]", infos[i]);
						// exception service will handle this
					}
				}

			// release components
			releaseComponents(getHandle(), uris);
		}
	}

	/*
	 * Used to store original values and then restored after called via shutdownImplementation.
	 * Default values are values when called via shutdownImplemention.
	 */
	private volatile int originalId = MANAGER_MASK;
	private volatile int originalContainers = 0;

	/**
	 * @see com.cosylab.acs.maci.Manager#shutdown(int, int)
	 */
	public void shutdown(int id, int containers) throws AcsJNoPermissionEx
	{
		if (isDebug())
			new MessageLogEntry(this, "shutdown", new Object[] { new Integer(id), new Integer(containers) }).dispatch();

		// check handle and SHUTDOWN_SYSTEM permissions
		securityCheck(id, AccessRights.SHUTDOWN_SYSTEM);

		if (id == MANAGER_MASK)
			id = originalId;
		else
			originalId = id;

		if (containers == 0)
			containers = originalContainers;
		else
			originalContainers = containers;

		/****************************************************************/

		// always destroy through destroy implementation
		// if application is not destroying already, destroy it
		if (shutdownImplementation != null && !shutdownImplementation.isShutdownInProgress())
		{
			// spawn another thread
			new Thread(new Runnable()
			{
				public void run()
				{
					// fire destroy application
					if (!shutdownImplementation.isShutdownInProgress())
						shutdownImplementation.shutdown(false);
				}
			}
			, "ManagerApplicationShutdown").start();

			return;
		}

		/****************************************************************/

		// check if already shutdown
		if (shutdown.set(true))
		{
			// already shutdown
			AcsJNoPermissionEx npe = new AcsJNoPermissionEx();
			npe.setReason("Manager already in shutdown state.");
			throw npe;
		}

		/****************************************************************/

		new MessageLogEntry(this, "shutdown", "Manager is shutting down.", LoggingLevel.INFO).dispatch();


		new MessageLogEntry(this, "shutdown", "Canceling heartbeat task.", LoggingLevel.TRACE).dispatch();
		// cancel hertbeat task
		heartbeatTask.cancel();
		topologySortManager.destroy();

		/*
		 * Are those actions OK? don't we want to have silent (unnoticable) restarts of the manager?!
		 * There probably should be several shutdown modes - from silent to whole system shutdown...
		 * For the time being sileny mode is implemented.
		 *
		new MessageLogEntry(this, "shutdown", "Releasing immortal components.", LoggingLevel.TRACE).dispatch();
		/// !!! TBD


		new MessageLogEntry(this, "shutdown", "Notifying clients and administrators to disconnect.", LoggingLevel.TRACE).dispatch();
		notifyClientDisconnectShutdown();

		*/

		// if not "silent" shutdown
		if (containers != 0)
		{
			new MessageLogEntry(this, "shutdown", "Releasing all components in the system.", LoggingLevel.TRACE).dispatch();
			try
			{
				topologySortManager.requestTopologicalSort();
				releaseComponents(topologySortManager.getComponentShutdownOrder(null));
			}
			catch (Throwable th)
			{
				CoreException ce = new CoreException(this, "Failed to release all components in the system.", th);
				ce.caughtIn(this, "shutdown");
				// exception handler service will handle this
			}
		}

		new MessageLogEntry(this, "shutdown", "Notifying containers to disconnect or shutdown.", LoggingLevel.TRACE).dispatch();
		notifyContainerDisconnectShutdown(containers);

		// finalizeFearation
		finalizeFederation();

		// process tasks in thread poll
		// !!! NOTE: this could block (for a long time)
		new MessageLogEntry(this, "shutdown", "Waiting tasks in thread poll to complete...", LoggingLevel.TRACE).dispatch();
		threadPool.shutdownAfterProcessingCurrentlyQueuedTasks();
		try
		{
			threadPool.awaitTerminationAfterShutdown(3000);
		}
		catch (InterruptedException ie) {}
		threadPool.interruptAll();

		// unbind Manager
		unbind("Manager", null);

		// remove Abeans DB connection
		setCDBAccess(null);

		setApplicationContext(null);

		// release CDB DAO daos
		destroyComponetsDAOProxy();
		destroyContainersDAOProxy();
		destroyManagerDAOProxy();

		new MessageLogEntry(this, "shutdown", "Manager shutdown completed.", LoggingLevel.INFO).dispatch();

		/****************************************************************/

		if (isDebug())
			new MessageLogEntry(this, "shutdown", "Exiting.", Level.FINEST).dispatch();
	}

	/*****************************************************************************/
	/**************************** [ Login methods ] ******************************/
	/*****************************************************************************/

	/**
	 * Container specific login method.
	 * @param	name	name of the container, non-<code>null</code>.
	 * @param	reply	reply to authenticate method, non-<code>null</code>.
	 * @param	container	container that is logging in, non-<code>null</code>.
	 * @return	ClientInfo	client info. of newly logged container
	 */
	private ClientInfo containerLogin(String name, String reply, Container container) throws AcsJNoPermissionEx
	{
		assert (name != null);
		assert (reply != null);
		assert (container != null);

		// NOTE: do not log references - prevents GC to finalize and terminate connection thread (JacORB)
		if (isDebug())
			new MessageLogEntry(this, "containerLogin", new Object[] { name, container == null ? "null" : container.toString() }).dispatch();

		TimerTaskContainerInfo containerInfo = null;
		ClientInfo clientInfo = null;
		boolean existingLogin = false;

		synchronized (containers)
		{

			// check if container is already logged in,
			// if it is, return existing info
			int h = containers.first();
			while (h != 0)
		    {
		    	ContainerInfo loggedContainerInfo = (ContainerInfo)containers.get(h);
				// containers are persistant and this will not work
				//if (container.equals(loggedContainerInfo.getContainer()))
				if (name.equals(loggedContainerInfo.getName()))
				{

					Container loggedContainer = loggedContainerInfo.getContainer();
					if (loggedContainer != null)
					{
						 if (!loggedContainer.equals(container))
						 	sendMessage(loggedContainer,
						 				"Other container instance was logged in with the same name and taking over this instance. Resolve this problem to avoid potential problems.",
						 				MessageType.MSG_ERROR);
					}

					// !!! ACID 2
					// new reference is considered as better
					executeCommand(new ContainerCommandUpdate(h, container));
					//loggedContainerInfo.setContainer(container);

					existingLogin = true;

					// generate ClientInfo
					containerInfo = (TimerTaskContainerInfo)loggedContainerInfo;
					clientInfo = containerInfo.createClientInfo();
					break;
				}

				h = containers.next(h);
		    }

			// new container
			if (h == 0)
			{
				// allocate new handle
				// !!! ACID 2
				int handle = ((Integer)executeCommand(new ContainerCommandAllocate())).intValue();
				//int handle = containers.allocate();

				if (handle == 0)
				{
					NoResourcesException af = new NoResourcesException(this, "Generation of new handle failed, too many containers logged in.");
					af.caughtIn(this, "containerLogin");
					af.putValue("name", name);
					throw af;
				}

				// generate external handle
				h = handle | CONTAINER_MASK;

				// add generated key
			    h |= (random.nextInt(0x100)) << 16;

				// create new container info
				containerInfo = new TimerTaskContainerInfo(h, name, container);

				clientInfo = containerInfo.createClientInfo();

				// register container to the heartbeat manager
				PingTimerTask task = new PingTimerTask(this, clientInfo);
				containerInfo.setTask(task);
				heartbeatTask.schedule(task, CONTAINER_PING_INTERVAL, CONTAINER_PING_INTERVAL);

				// !!! ACID - register AddContainerCommand
				executeCommand(new ContainerCommandSet(handle, containerInfo));
				// store info
				//containers.set(handle, containerInfo);
			}
		}

		final boolean recoverContainer = (reply.length() >= 2 && reply.charAt(1) == 'R');

		if (existingLogin)
		{
			// merge container's and manager's internal state
			containerInternalStateMerge(containerInfo, recoverContainer);
		}

		// notify administrators about the login
		notifyContainerLogin(containerInfo);

		// do container post login activation in separate thread
		final ContainerInfo finalInfo = containerInfo;
		try
		{
			threadPool.execute(
				new Runnable() {
					public void run()
					{
						containerPostLoginActivation(finalInfo, recoverContainer);
					}
				});
		}
		catch (InterruptedException ie)
		{
			new MessageLogEntry(ManagerImpl.this, "containerLogin",
								"Interrupted execution of port activation task for container '"+containerInfo.getName()+"'.", ie, LoggingLevel.ERROR).dispatch();
		}

		new MessageLogEntry(this, "containerLogin", "Container '" + name + "' logged in.", LoggingLevel.INFO).dispatch();

		if (isDebug())
			new MessageLogEntry(this, "containerLogin", "Exiting.", Level.FINEST).dispatch();

		return clientInfo;
	}

	/**
	 * Container post login activation, activate startup and unavailable components.
	 * NOTE: to be run in separate thread.
	 *
	 * @param	containerInfo	container info for which to perform post login activation.
	 * @param	recoverContainer	recovery mode flag.
	 */
	private void containerPostLoginActivation(final ContainerInfo containerInfo, boolean recoverContainer)
	{
		// give containers some time to fully initialize
		try
		{
			Thread.sleep(3000);
		}
		catch (InterruptedException ie)
		{
		}

		// shutdown check
		if (isShuttingDown())
			return;

		// TODO what if it has already logged out?

		if (isDebug())
			new MessageLogEntry(this, "containerPostLoginActivation", new Object[0]).dispatch();

		// CDB startup
		if (cdbActivation != null && containerInfo.getName().equals(cdbActivation.getContainer()))
		{
			try
			{
				StatusHolder status = new StatusHolder();

				ComponentInfo cdbInfo = internalRequestComponent(this.getHandle(),
										 cdbActivation.getName(), cdbActivation.getType(),
										 cdbActivation.getCode(), cdbActivation.getContainer(), RELEASE_IMMEDIATELY, status, true);

				if (status.getStatus() != ComponentStatus.COMPONENT_ACTIVATED)
					new MessageLogEntry(ManagerImpl.this, "containerPostLoginActivation", "Failed to activate CDB, reason: '"+status.getStatus()+"'.", LoggingLevel.SEVERE).dispatch();
				else if (cdbInfo == null || cdbInfo.getHandle() == 0 || cdbInfo.getComponent() == null)
					new MessageLogEntry(ManagerImpl.this, "containerPostLoginActivation", "Failed to activate CDB, invalid ComponentInfo returned: '"+cdbInfo+"'.", LoggingLevel.SEVERE).dispatch();
				else
				{
					new MessageLogEntry(ManagerImpl.this, "containerPostLoginActivation", "CDB activated on container '" + containerInfo.getName() + "'.", LoggingLevel.INFO).dispatch();
				}
			}
			catch (Exception ex)
			{
				CoreException ce = new CoreException(this, "Failed to activate CDB on container '" + containerInfo.getName() + "'.", ex);
				ce.caughtIn(this, "containerPostLoginActivation");
				// exception service will handle this
				// new MessageLogEntry(this, "containerPostLoginActivation", ce.getMessage(), ex, LoggingLevel.SEVERE).dispatch();
				new MessageLogEntry(ManagerImpl.this, "containerPostLoginActivation", "Failed to activate CDB, reason: '"+ex.getMessage()+"'.", ex, LoggingLevel.SEVERE).dispatch();
			}
		}


		// used for fast lookups
		Map activationRequests = new HashMap();
		// order is important, preserve it
		ArrayList activationRequestsList = new ArrayList();

		// get CDB access daos
		DAOProxy dao = getManagerDAOProxy();
		DAOProxy componentsDAO = getComponentsDAOProxy();

		//
		// autostart components (Manager.Startup array) - TODO to be removed (left for backward compatibility)
		//
		if (dao != null && componentsDAO != null)
		{

			try
			{
				// query startup components
				String[] startup = dao.get_string_seq("Startup");

				final Integer managerHandle = new Integer(this.getHandle());

				for (int i = 0; i < startup.length; i++)
				{
					// TODO simulator test workaround
					if (startup[i].length() == 0)
						continue;

					// read container field
					String containerName = readStringCharacteristics(componentsDAO, startup[i]+"/Container");
					if (containerName == null)
					{
						new MessageLogEntry(this, "containerPostLoginActivation", "Misconfigured CDB, there is no container of component '"+startup[i]+"' defined.", LoggingLevel.WARNING).dispatch();
						continue;
					}

					// if container name matches, add activation request
					if (containerInfo.getName().equals(containerName))
					{
						activationRequests.put(startup[i], managerHandle);
						try
						{
							URI curl = CURLHelper.createURI(startup[i]);

							// check CURL
							checkCURL(curl);

							activationRequestsList.add(curl);
						}
						catch (URISyntaxException usi)
						{
							CoreException ce = new CoreException(this, "Failed to create URI from component name '"+startup[i]+"'.", usi);
							ce.caughtIn(this, "containerPostLoginActivation");
							ce.putValue("startup[i]", startup[i]);
							// exception handler service will take care of this exception
							new MessageLogEntry(this, "containerPostLoginActivation", ce.getMessage(), ce, LoggingLevel.WARNING).dispatch();
						}
					}
				}
			}
			catch (Throwable th)
			{
				CoreException ce = new CoreException(this, "Failed to retrieve list of startup components.", th);
				ce.caughtIn(this, "containerPostLoginActivation");
				// exception handler service will take care of this exception
				new MessageLogEntry(this, "containerPostLoginActivation", ce.getMessage(), ce, LoggingLevel.DEBUG).dispatch();
			}

		}

		//
		// autostart components (<component>.Autostart attribute)
		//
		final String TRUE_STRING = "true";
		if (componentsDAO != null)
		{

			try
			{
				final Integer managerHandle = new Integer(this.getHandle());

				// get names of all components
				/*String[] ids =*/ componentsDAO.get_field_data("");  // TODO here to check if CDB is available
			    String[] ids = getComponentsList();

				// test names
				for (int i = 0; i < ids.length; i++)
				{
					// read name
					String name = ids[i]; //readStringCharacteristics(componentsDAO, ids[i]+"/Name");
					if (name == null)
					{
						new MessageLogEntry(this, "getComponentInfo", "Misconfigured CDB, there is no name of component '"+ids[i]+"' defined.", LoggingLevel.WARNING).dispatch();
						continue;
					}

					// read autostart silently
					String autostart = readStringCharacteristics(componentsDAO, ids[i]+"/Autostart", true);
					if (autostart == null)
					{
						new MessageLogEntry(this, "getComponentInfo", "Misconfigured CDB, there is no autostart attribute of component '"+ids[i]+"' defined.", LoggingLevel.WARNING).dispatch();
						continue;
					}
					else if (autostart.equalsIgnoreCase(TRUE_STRING) && !activationRequests.containsKey(name) /* TODO to be removed */ )
					{
						// read container silently
						String componentContainer = readStringCharacteristics(componentsDAO, ids[i]+"/Container", true);
						if (componentContainer == null)
						{
							new MessageLogEntry(this, "getComponentInfo", "Misconfigured CDB, there is no container attribute of component '"+ids[i]+"' defined.", LoggingLevel.WARNING).dispatch();
							continue;
						}
						else if (!containerInfo.getName().equals(componentContainer))
							continue;

						activationRequests.put(name, managerHandle);
						try
						{
							URI curl = CURLHelper.createURI(name);

							// check CURL, no non-local curls
							checkCURL(curl, false);

							activationRequestsList.add(curl);
						}
						catch (URISyntaxException usi)
						{
							CoreException ce = new CoreException(this, "Failed to create URI from component name '"+name+"'.", usi);
							ce.caughtIn(this, "containerPostLoginActivation");
							ce.putValue("name", name);
							// exception handler service will take care of this exception
							new MessageLogEntry(this, "containerPostLoginActivation", ce.getMessage(), ce, LoggingLevel.WARNING).dispatch();
						}
					}
				}
			}
			catch (Exception ex)
			{
				CoreException ce = new CoreException(this, "Failed to retrieve list of components.", ex);
				ce.caughtIn(this, "containerPostLoginActivation");
				// exception handler service will take care of this exception
				new MessageLogEntry(this, "containerPostLoginActivation", ce.getMessage(), ce, LoggingLevel.DEBUG).dispatch();
			}

		}

		// list of componentInfo to be cleaned up (cannot be immediately, due to lock)
		ArrayList cleanupList = new ArrayList();

		//
		// check unavailable components
		if (unavailableComponents.size() > 0)
		{
			if (componentsDAO != null)
			{

				try
				{
					final Integer reactivateHandle = new Integer(0);

					synchronized (unavailableComponents)
					{
						Iterator iterator = unavailableComponents.keySet().iterator();
						while (iterator.hasNext())
						{
							String name = (String)iterator.next();

							boolean reactivate = false;

							// dynamic component check
							ComponentInfo componentInfo = (ComponentInfo)unavailableComponents.get(name);
							if (componentInfo != null && componentInfo.isDynamic() &&
							 	componentInfo.getDynamicContainerName() != null &&
								componentInfo.getDynamicContainerName().equals(containerInfo.getName()))
							{
								// reactivate dynamic component
								reactivate = true;
							}
							else
							{
								// read container field
								String containerName = readStringCharacteristics(componentsDAO, name+"/Container");
								if (containerName == null)
								{
									new MessageLogEntry(this, "containerPostLoginActivation", "Misconfigured CDB, there is no container of component '"+name+"' defined.", LoggingLevel.WARNING).dispatch();
									//continue;
								}
								// if container name matches, add (override) activation request
								else if (containerInfo.getName().equals(containerName))
								{
									reactivate = true;
								}
							}

							if (reactivate)
							{
								// clean up if in non-recovery mode
								if (!recoverContainer)
								{
									// discard all component information

									// and if not already in activation list (startup component)
									if (!activationRequests.containsKey(name))
									{
										cleanupList.add(componentInfo);
										continue;
									}
								}

								// this Component needs reactivation
								if (activationRequests.containsKey(name))
								{
									activationRequests.put(name, reactivateHandle);
								}
								// put to activation list
								else
								{
									activationRequests.put(name, reactivateHandle);
									try
									{
										activationRequestsList.add(CURLHelper.createURI(name));
									}
									catch (URISyntaxException usi)
									{
										CoreException ce = new CoreException(this, "Failed to create URI from component name '"+name+"'.", usi);
										ce.caughtIn(this, "containerPostLoginActivation");
										ce.putValue("name", name);
										// exception handler service will take care of this exception
										new MessageLogEntry(this, "containerPostLoginActivation", ce.getMessage(), ce, LoggingLevel.WARNING).dispatch();
									}
								}
							}
						}
					}


				}
				catch (Exception ex)
				{
					CoreException ce = new CoreException(this, "Failed to obtain component data from the CDB.", ex);
					ce.caughtIn(this, "containerPostLoginActivation");
					// exception service will handle this
					// new MessageLogEntry(this, "containerPostLoginActivation", ce.getMessage(), ex, LoggingLevel.WARNING).dispatch();
				}
			}

		}

		if (cleanupList.size() > 0)
		{
			Iterator iter = cleanupList.iterator();
			while (iter.hasNext())
			{
				ComponentInfo componentInfo = (ComponentInfo)iter.next();

				synchronized (components)
				{
					// remove from its owners list ...
					int[] owners = componentInfo.getClients().toArray();
					for (int j = 0; j < owners.length; j++)
						removeComponentOwner(componentInfo.getHandle(), owners[j]);

					// ... and deallocate
					executeCommand(new ComponentCommandDeallocate(componentInfo.getHandle() & HANDLE_MASK));
					executeCommand(new UnavailableComponentCommandRemove(componentInfo.getName()));

					// remove component from container component list
					synchronized (containerInfo.getComponents())
					{
						if (containerInfo.getComponents().contains(componentInfo.getHandle()))
							executeCommand(new ContainerInfoCommandComponentRemove(containerInfo.getHandle() & HANDLE_MASK, componentInfo.getHandle()));
					}
				}

				// unbind from remote directory
				unbind(convertToHiearachical(componentInfo.getName()), "O");
			}

		}

		new MessageLogEntry(this, "containerPostLoginActivation", "Container '"+containerInfo.getName()+"' startup statistics: " +
							activationRequestsList.size() + " components queued to be activated.", LoggingLevel.INFO).dispatch();

		// send message to the container
		sendMessage(containerInfo.getContainer(), "Startup statistics: " + activationRequestsList.size() +
					" components queued to be activated.", MessageType.MSG_INFORMATION);

		// activate startup components
		int activated = 0;
		StatusHolder status = new StatusHolder();
		Iterator iterator = activationRequestsList.iterator();
		while (iterator.hasNext())
		{
			URI uri = (URI)iterator.next();
			try
			{
				String name = extractName(uri);

				int requestor = ((Integer)activationRequests.get(name)).intValue();

				internalRequestComponent(requestor, uri, status);

				if (status.getStatus() != ComponentStatus.COMPONENT_ACTIVATED)
					new MessageLogEntry(ManagerImpl.this, "containerPostLoginActivation", "Failed to reactivate requested component '"+uri+"', reason: '"+status.getStatus()+"'.", LoggingLevel.DEBUG).dispatch();
				else
					activated++;
			}
			catch (Exception ex)
			{
				CoreException ce = new CoreException(ManagerImpl.this, "Failed to request component '"+uri+"'.", ex);
				ce.caughtIn(this, "containerPostLoginActivation");
				ce.putValue("uri",  uri);
				// exception service will handle this
			}
		}

		new MessageLogEntry(this, "containerPostLoginActivation", "Container '"+containerInfo.getName()+"' startup statistics: " +
							activated + " of " + activationRequestsList.size() + " components activated.", LoggingLevel.INFO).dispatch();

		// send message to the container
		sendMessage(containerInfo.getContainer(), "Startup statistics: " + activated + " of " +
					activationRequestsList.size() + " components activated.", MessageType.MSG_INFORMATION);

		// notify about new container login
		synchronized (containerLoggedInMonitor)
		{
			containerLoggedInMonitor.notifyAll();
		}

		if (isDebug())
			new MessageLogEntry(this, "containerPostLoginActivation", "Exiting.", Level.FINEST).dispatch();

	}


	/**
	 * Retrieve container's internal state and merge it with manager's.
	 * NOTE: this method should not be run in separate thread since states
	 * should be merged synchronously.
	 * Merge is split to two parts:
	 *
	 * <h2>Container -> Manager</h2>
	 *
	 * If container component handle is also allocated in manager state and components information match,
	 * then we have a perfect fit and no action is required. If they do not match, container is rejected.
	 * If container component handle is not allocated in the manager state and no component with
	 * same name is found in manager state, component information is transferred to the manager,
	 * owtherwise container is rejected.
	 * <p>
	 * NOTE: The second option allows components without owners to be activated.
	 * </p>
	 * <p>
	 * NOTE: Container is rejected due to its state inconsistency which will probably cause
	 * problems in the future. Container has to be restarted.<br/>
	 * (A more sophisticated algorithm could give manager "the power to shape up" container state.)
	 * </p>
	 * <p>
	 * NOTE: Container -> Manager has to work in the way transactions do, i.e. in case of rejection
	 * manager state should no be affected.
	 * </p>
	 *
	 * <h2>Manager -> Container</h2>
	 *
	 * If <code>recoverContainer</code> is <code>true</code>, all components known to the manager to
	 * be activated on this particular container and listed as activated by the container
	 * will be marked as unavailable to be reactivated later by
	 * <code>containerPostLoginActivation</code> method.
	 * If <code>recoverContainer</code> is <code>false</code>, all there information will be discared
	 * (components removed from container component list, deallocated, and removed from their owners list).
	 *
	 * @param	containerInfo	container info for which to perform merge, non-<code>null</code>.
	 * @param	recoverContainer	if <code>true</code> manager state will be 'transferred' to container.
	 */
	private void containerInternalStateMerge(ContainerInfo containerInfo, boolean recoverContainer) throws AcsJNoPermissionEx
	{
		assert(containerInfo != null);

		// NOTE: do not log references - prevents GC to finalize and terminate connection thread (JacORB)
		if (isDebug())
			new MessageLogEntry(this, "containerInternalStateMerge", new Object[] { containerInfo == null ? "null" : containerInfo.toString() , new Boolean(recoverContainer) }).dispatch();

		// firstly, query containers state
		ComponentInfo[] infos = null;
		try
		{
			infos = containerInfo.getContainer().get_component_info(new int[0]);
		}
		catch (Exception ex)
		{
			RemoteException re = new RemoteException(this, "Failed to query state of container '"+containerInfo.getName()+"'.", ex);
			re.caughtIn(this, "containerInternalStateMerge");
			// exception service will handle this
			new MessageLogEntry(this, "containerInternalStateMerge", re.getMessage(), LoggingLevel.ERROR).dispatch();
		}

		boolean requireTopologySort = false;

		// copy elements
		IntArray managerContainersComponents;
		synchronized (containerInfo.getComponents())
		{
			 managerContainersComponents = new IntArray(containerInfo.getComponents().toArray());
		}

		//
		// Container -> Manager
		//

		if (infos != null && infos.length > 0)
		{
			synchronized (components)
			{
				// iterate through all handles
				// phase 1: check for state consistency
				for (int i = 0; i < infos.length; i++)
				{

					// check if info is valid
					if (infos[i].getHandle() == 0 ||
						 (infos[i].getHandle() & COMPONENT_MASK) != COMPONENT_MASK ||
						 infos[i].getName() == null ||
						 infos[i].getType() == null ||
						 infos[i].getCode() == null ||
						 infos[i].getComponent() == null ||
						 infos[i].getContainer() == 0 ||
						 infos[i].getInterfaces() == null)
						 {
						 	// bad info
							// NO_PERMISSION
						   	AcsJNoPermissionEx npe = new AcsJNoPermissionEx();
						   	npe.setReason("Inconsistent container state - component information is not valid, rejecting container.");
						   	throw npe;
						 }

					int handle = infos[i].getHandle() & HANDLE_MASK;
					if (components.isAllocated(handle))
					{
						// handle is allocated
						// check if components information match
						ComponentInfo componentInfo = (ComponentInfo)components.get(handle);
						if (componentInfo == null ||
							componentInfo.getHandle() != infos[i].getHandle() ||
							!componentInfo.getName().equals(infos[i].getName()) ||
							 (componentInfo.getContainer() != 0 &&
							  componentInfo.getContainer() != infos[i].getContainer()))
							{
								// information does not match, reject container
								// NO_PERMISSION
						   		AcsJNoPermissionEx npe = new AcsJNoPermissionEx();
						   		npe.setReason("Inconsistent container state - components information do not match, rejecting container.");
						   		throw npe;
							}

					}
					else
					{
						// handle is not allocated
						// check if component with same name is already registered
						int h = components.first();
						while (h != 0)
					    {
					    	ComponentInfo componentInfo = (ComponentInfo)components.get(h);
							if (componentInfo.getName().equals(infos[i].getName()))
							{
								// yes it does, reject container
								// NO_PERMISSION
							   	AcsJNoPermissionEx npe = new AcsJNoPermissionEx();
							   	npe.setReason("Inconsistent container state - component with name '" +
										componentInfo.getName() + "' already registered with different handle, rejecting container.");
							   	throw npe;
							}
							h = components.next(h);
					    }
					}
				}

				// states are consistent
				// phase 2: do the transfer
				for (int i = 0; i < infos.length; i++)
				{

					int handle = infos[i].getHandle() & HANDLE_MASK;

					if (components.isAllocated(handle))
					{
						// handle is allocated and info match
						// just in case component is not persistent, update the reference
						// is always non-null - checked already above
				    	ComponentInfo componentInfo = (ComponentInfo)components.get(handle);
				    	componentInfo.setComponent(infos[i].getComponent());
				    	componentInfo.setContainer(infos[i].getContainer());
				    	componentInfo.setContainerName(infos[i].getContainerName());

				    	// remove if unavailable and notify
				    	synchronized (unavailableComponents)
				    	{
				    		// !!! ACID 3
				    		executeCommand(new UnavailableComponentCommandRemove(componentInfo.getName()));
				    		//unavailableComponents.remove(componentInfo.getName());
				    	}
						int clients[] = componentInfo.getClients().toArray();
						notifyComponentAvailable(0, clients, new ComponentInfo[] { componentInfo });
					}
					else
					{
						// handle is not allocated
						// transfer component

						// create new ComponentInfo (we do not trust containers)
						ComponentInfo info = new ComponentInfo(infos[i].getHandle(),
													infos[i].getName(),
													infos[i].getType(),
													infos[i].getCode(),
													infos[i].getComponent());
						info.setContainer(infos[i].getContainer());
						info.setContainerName(infos[i].getContainerName());
						info.setInterfaces(infos[i].getInterfaces());
						info.setAccessRights(0);

						// determine if component is dynamic
						// if it has a CDB entry or if there is no CDB available
						if (!hasCDBEntry(info))
						{
							info.setDynamic(true);
							info.setDynamicContainerName(containerInfo.getName());
						}

						// !!! ACID 3
						// allocate and store
						executeCommand(new ComponentCommandAllocateHandle(handle));
						//components.allocate(handle);
						executeCommand(new ComponentCommandSet(handle, info));
						//components.set(handle, info);

						requireTopologySort = true;
					}

					// we handled this component
					managerContainersComponents.remove(infos[i].getHandle());

					// add to container component list
					synchronized (containerInfo.getComponents())
					{
						// !!! ACID 3
						if (!containerInfo.getComponents().contains(infos[i].getHandle()))
							executeCommand(new ContainerInfoCommandComponentAdd(containerInfo.getHandle() & HANDLE_MASK, infos[i].getHandle()));
							//containerInfo.getComponents().add(infos[i].getHandle());
					}

				}

			}
		}

		//
		// Manager -> Container
		//

		// managerContainersComponents contains component handles not handled yet
		for (int i = 0; i < managerContainersComponents.size(); i++)
		{
			int componentHandle = managerContainersComponents.get(i);

			// remove component handle from container component list
			// will be added when actually activated
			// !!! ACID 3
			executeCommand(new ContainerInfoCommandComponentRemove(containerInfo.getHandle() & HANDLE_MASK, componentHandle));
			//containerInfo.getComponents().remove(componentHandle);

			// marked as unavailable to be reactivated later (or discarded)
			synchronized (components)
			{
				int handle = componentHandle & HANDLE_MASK;

				if (components.isAllocated(handle))
				{
					ComponentInfo componentInfo = (ComponentInfo)components.get(handle);
					// what if null (very not likely to happen, but possible)
					if (componentInfo == null)
					{
						// internal error, this should not happen
						new MessageLogEntry(this, "containerInternalStateMerge", "Internal state is not consistent (no ComponentInfo).", LoggingLevel.CRITICAL).dispatch();
						continue;
					}


					// notify component owners about Component deactivation
					makeUnavailable(componentInfo);


					if (!recoverContainer)
					{
						// discard all Component information

						// remove from its owners list ...
						int[] owners = componentInfo.getClients().toArray();
						for (int j = 0; j < owners.length; j++)
							removeComponentOwner(componentHandle, owners[j]);

						// !!! ACID 3
						// ... and deallocate
						executeCommand(new ComponentCommandDeallocate(handle));
						//components.deallocate(handle);

						requireTopologySort = true;
					}

				}
				else
				{
					// internal error, this should not happen
					new MessageLogEntry(this, "containerInternalStateMerge", "Internal state is not consistent.", LoggingLevel.CRITICAL).dispatch();
				}

			}


		}

		if (requireTopologySort)
			topologySortManager.notifyTopologyChange(containerInfo.getHandle());

		if (isDebug())
			new MessageLogEntry(this, "containerInternalStateMerge", "Exiting.", Level.FINEST).dispatch();
	}

	/**
	 * Search for CDB entry of given component.
	 * @param	componentInfo	component info of CDB entry to be found, non-<code>null</code>.
	 * @return	if CDB entry of has been found <code>true</code>, or if it does not exist or
	 * 		     there is no CDB available <code>false</code>.
	 */
	private boolean hasCDBEntry(ComponentInfo componentInfo)
	{
		assert (componentInfo != null);

		try
		{
			// check component entry
			DAOProxy dao = getComponentsDAOProxy();
			if (dao == null || readStringCharacteristics(dao, componentInfo.getName(), true) == null)
				return false;

			// check type
			String type = readStringCharacteristics(dao, componentInfo.getName()+"/Type", true);
			if (type == null || !type.equals(componentInfo.getType()))
				return false;

			// check code
			String code = readStringCharacteristics(dao, componentInfo.getName()+"/Code", true);
			if (code == null || !code.equals(componentInfo.getCode()))
				return false;

			// check container
			String container = readStringCharacteristics(dao, componentInfo.getName()+"/Container", true);
			if (container == null)
				return false;
			else
			{
				// dynamic component case
				if (componentInfo.isDynamic())
				{
					if (componentInfo.getDynamicContainerName() != null &&
						!container.equals(componentInfo.getDynamicContainerName()))
						return false;
				}
				// static component case
				else
				{
					ContainerInfo containerInfo = (ContainerInfo)getContainerInfo(componentInfo.getContainer());
					if (containerInfo != null && !containerInfo.getName().equals(container))
						return false;
				}

			}

			// there is an entry in CDB
			return true;
		}
		catch (Throwable t)
		{
			return false;
		}

	}

	/**
	 * Administrator specific login method.
	 * @param	name	name of the administrator
	 * @param	reply	reply to authenticate method
	 * @param	administrator	administrator that is logging in
	 * @return	ClientInfo	client info. of newly logged administrator
	 */
	private ClientInfo administratorLogin(String name, String reply, Administrator administrator) throws AcsJNoPermissionEx
	{
		assert (name != null);
		assert (administrator != null);

		// NOTE: do not log references - prevents GC to finalize and terminate connection thread (JacORB)
		if (isDebug())
			new MessageLogEntry(this, "administratorLogin", new Object[] { name, administrator == null ? "null" : administrator.toString() }).dispatch();

		TimerTaskClientInfo clientInfo = null;

		synchronized (administrators)
		{

			// check if administrator is already logged in,
			// if it is, return existing info
			int h = administrators.first();
			while (h != 0)
		    {
		    	ClientInfo loggedAdministratorInfo = (ClientInfo)administrators.get(h);
				if (administrator.equals(loggedAdministratorInfo.getClient()))
					return loggedAdministratorInfo;

				h = administrators.next(h);
		    }

			// allocate new handle
			// !!! ACID 2
			int handle = ((Integer)executeCommand(new AdministratorCommandAllocate())).intValue();
			//int handle = administrators.allocate();
			if (handle == 0)
			{
				NoResourcesException af = new NoResourcesException(this, "Generation of new handle failed, too many administrators logged in.");
				af.caughtIn(this, "administratorLogin");
				af.putValue("name", name);
				throw af;
			}

			// generate external handle
			h = handle | ADMINISTRATOR_MASK;

			// add generated key
		    h |= (random.nextInt(0x100)) << 16;

			// create new client info
			clientInfo = new TimerTaskClientInfo(h, name, administrator);
			clientInfo.setAccessRights(AccessRights.REGISTER_COMPONENT |
									   AccessRights.INTROSPECT_MANAGER |
									   AccessRights.SHUTDOWN_SYSTEM);


			// register administrator to the heartbeat manager
			PingTimerTask task = new PingTimerTask(this, clientInfo);
			clientInfo.setTask(task);
			heartbeatTask.schedule(task, ADMINISTRATOR_PING_INTERVAL, ADMINISTRATOR_PING_INTERVAL);

			// !!! ACID - register AddAdministratorCommand
			executeCommand(new AdministratorCommandSet(handle, clientInfo));
			// store info
			//administrators.set(handle, clientInfo);
		}

		// notify administrators about the login
		notifyClientLogin(clientInfo);

		new MessageLogEntry(this, "administratorLogin", "Administrator '" + name + "' logged in.", LoggingLevel.INFO).dispatch();

		if (isDebug())
			new MessageLogEntry(this, "administratorLogin", "Exiting.", Level.FINEST).dispatch();

		return clientInfo;
	}

	/**
	 * Client specific login method.
	 * @param	name	name of the client
	 * @param	reply	reply to authenticate method
	 * @param	client	client that is logging in
	 * @return	ClientInfo	client info. of newly logged client
	 */
	private ClientInfo clientLogin(String name, String reply, Client client) throws AcsJNoPermissionEx
	{
		assert (name != null);
		assert (client != null);

		// NOTE: do not log references - prevents GC to finalize and terminate connection thread (JacORB)
		if (isDebug())
			new MessageLogEntry(this, "clientLogin", new Object[] { name, client == null ? "null" : client.toString() }).dispatch();

		TimerTaskClientInfo clientInfo = null;

		synchronized (clients)
		{

			// check if client is already logged in,
			// if it is, return existing info
			int h = clients.first();
			while (h != 0)
		    {
		    	ClientInfo loggedClientInfo = (ClientInfo)clients.get(h);
				if (client.equals(loggedClientInfo.getClient()))
					return loggedClientInfo;

				h = clients.next(h);
		    }

			// allocate new handle
			// !!! ACID 2
			int handle = ((Integer)executeCommand(new ClientCommandAllocate())).intValue();
			//int handle = clients.allocate();
			if (handle == 0)
			{
				NoResourcesException af = new NoResourcesException(this, "Generation of new handle failed, too many clients logged in.");
				af.caughtIn(this, "clientLogin");
				af.putValue("name", name);
				throw af;
			}

			// generate external handle
			h = handle | CLIENT_MASK;

			// add generated key
		    h |= (random.nextInt(0x100)) << 16;

			// create new client info
			clientInfo = new TimerTaskClientInfo(h, name, client);
			clientInfo.setAccessRights(AccessRights.REGISTER_COMPONENT);


			// register client to the heartbeat manager
			PingTimerTask task = new PingTimerTask(this, clientInfo);
			clientInfo.setTask(task);
			heartbeatTask.schedule(task, CLIENT_PING_INTERVAL, CLIENT_PING_INTERVAL);

			// !!! ACID - register AddClientCommand
			executeCommand(new ClientCommandSet(handle, clientInfo));
			// store info
			//clients.set(handle, clientInfo);
		}

		// notify administrators about the login
		notifyClientLogin(clientInfo);

		new MessageLogEntry(this, "clientLogin", "Client '" + name + "' logged in.", LoggingLevel.INFO).dispatch();

		if (isDebug())
			new MessageLogEntry(this, "clientLogin", "Exiting.", Level.FINEST).dispatch();

		return clientInfo;
	}


	/**
	 * Container specific logout method
	 * @param	id	handle of the container.
	 */
	private void containerLogout(int id)
	{
		if (isDebug())
			new MessageLogEntry(this, "containerLogout", new Object[] { new Integer(id) }).dispatch();

		TimerTaskContainerInfo containerInfo = null;

		synchronized (containers)
		{
			int handle = id & HANDLE_MASK;

			// already logged out
			if (!containers.isAllocated(handle))
				return;

			containerInfo = (TimerTaskContainerInfo)containers.get(handle);

			// !!! ACID - RemoveContainerCommand
			executeCommand(new ContainerCommandDeallocate(handle));
			// remove
			//containers.deallocate(handle);

		}

		// deregister container from the heartbeat manager
		containerInfo.getTask().cancel();

		// make all container components unavailable
		int[] markUnavailable;
		synchronized (containerInfo.getComponents())
		{
			markUnavailable = containerInfo.getComponents().toArray();
		}

		if (markUnavailable.length > 0)
		{
			synchronized (components)
			{
				synchronized (unavailableComponents)
				{
					for (int i = 0; i < markUnavailable.length; i++)
					{
						int handle = markUnavailable[i] & HANDLE_MASK;
						if (components.isAllocated(handle))
						{
							ComponentInfo componentInfo = (ComponentInfo)components.get(handle);
							if (componentInfo != null)
								makeUnavailable(componentInfo);
						}
					}
				}
			}


		}

		// notify administrators about the logout
		notifyContainerLogout(containerInfo);

		new MessageLogEntry(this, "containerLogout", "Container '" + containerInfo.getName() + "' logged out.", LoggingLevel.INFO).dispatch();

		if (isDebug())
			new MessageLogEntry(this, "containerLogout", "Exiting.", Level.FINEST).dispatch();
	}

	/**
	 * Make Component unavailable.
	 *
	 * @param	componentInfo		component to be made unavailable.
	 */
	private void makeUnavailable(ComponentInfo componentInfo)
	{
		synchronized (unavailableComponents)
		{
			// !!! ACID 3
			// add to unavailable component map (override old info)
			boolean notificationNeeded = !unavailableComponents.containsKey(componentInfo.getName());
			executeCommand(new UnavailableComponentCommandPut(componentInfo.getName(), componentInfo));
			//unavailableComponents.put(componentInfo.getName(), componentInfo);

			if (notificationNeeded)
			{
				// inform clients about unavailability (if necessary)
				int[] clients = componentInfo.getClients().toArray();
				notifyComponentUnavailable(0, clients, new String[] { componentInfo.getName() });
			}

			// clear component reference and container
			componentInfo.setComponent(null);
			componentInfo.setContainer(0);
			// leave container name set
		}
	}

	/**
	 * Client specific logout method
	 * @param	id	handle of the client.
	 */
	private void clientLogout(int id)
	{
		if (isDebug())
			new MessageLogEntry(this, "clientLogout", new Object[] { new Integer(id) }).dispatch();

		TimerTaskClientInfo clientInfo = null;
		int[] componentsArray = null;

		synchronized (clients)
		{
			int handle = id & HANDLE_MASK;

			// already logged out
			if (!clients.isAllocated(handle))
				return;

			clientInfo = (TimerTaskClientInfo)clients.get(handle);

			// !!! ACID - RemoveClientCommand
			executeCommand(new ClientCommandDeallocate(handle));
			// remove
			//clients.deallocate(handle);

			componentsArray = clientInfo.getComponents().toArray();
		}

		// deregister client from the heartbeat manager
		clientInfo.getTask().cancel();

		// spawn another task which will release all clientInfo.getComponents()
		try
		{
			threadPool.execute(new ReleaseComponentTask(clientInfo.getHandle(), componentsArray));
		}
		catch (InterruptedException ie)
		{
			new MessageLogEntry(ManagerImpl.this, "clientLogout",
								"Interrupted execution of release task releasing components of client '"+clientInfo.getName()+"'.", ie, LoggingLevel.ERROR).dispatch();
		}


		//internalReleaseComponents(components to be released, owners IntArray)
		//internalReleaseComponents(clientInfo.getComponents(), clientInfo.getComponents())

		// notify administrators about the logout
		notifyClientLogout(clientInfo);

		new MessageLogEntry(this, "clientLogout", "Client '" + clientInfo.getName() + "' logged out.", LoggingLevel.INFO).dispatch();

		if (isDebug())
			new MessageLogEntry(this, "clientLogout", "Exiting.", Level.FINEST).dispatch();
	}

	/**
	 * Administrator specific logout method
	 * @param	id	handle of the administrators.
	 */
	private void administratorLogout(int id)
	{
		if (isDebug())
			new MessageLogEntry(this, "administratorLogout", new Object[] { new Integer(id) }).dispatch();

		TimerTaskClientInfo clientInfo = null;
		int[] componentsArray = null;

		synchronized (administrators)
		{
			int handle = id & HANDLE_MASK;

			// already logged out
			if (!administrators.isAllocated(handle))
				return;

			clientInfo = (TimerTaskClientInfo)administrators.get(handle);

			// !!! ACID - RemoveAdministratorCommand
			executeCommand(new AdministratorCommandDeallocate(handle));
			// remove
			//administrators.deallocate(handle);

			componentsArray = clientInfo.getComponents().toArray();
		}

		// deregister client from the heartbeat manager
		clientInfo.getTask().cancel();

		// spawn another task which will release all clientInfo.getComponents()
		try
		{
			threadPool.execute(new ReleaseComponentTask(clientInfo.getHandle(), componentsArray));
		}
		catch (InterruptedException ie)
		{
			new MessageLogEntry(ManagerImpl.this, "administratorLogout",
								"Interrupted execution of release task releasing components of administrator '"+clientInfo.getName()+"'.", ie, LoggingLevel.ERROR).dispatch();
		}

		// notify administrators about the logout
		notifyClientLogout(clientInfo);

		new MessageLogEntry(this, "administratorLogout", "Administrator '" + clientInfo.getName() + "' logged out.", LoggingLevel.INFO).dispatch();

		if (isDebug())
			new MessageLogEntry(this, "administratorLogout", "Exiting.", Level.FINEST).dispatch();
	}

	/**
	 * Returns array of currently logged administrators.
	 * @param	excludeHandle	handle of administrator not to be included in the array, can be 0.
	 * @return	ClientInfo[]	array of currently logged administrators
	 */
	private ClientInfo[] getAdministrators(int excludeHandle)
	{
		if (isDebug())
			new MessageLogEntry(this, "getAdministrators", new Object[] { new Integer(excludeHandle) }).dispatch();

		// array of administrators to be notified
		ClientInfo[] admins = null;

		// generate array of Administrators to be notified
		synchronized (administrators)
		{
			int len = administrators.size();

			// no administrator to notify
			if (len > 0)
			{
				ArrayList list = new ArrayList();

				int h = administrators.first();
				while (h != 0)
				{
			    	ClientInfo adminInfo = (ClientInfo)administrators.get(h);

					// do not notify administrator itself
			    	if (adminInfo.getHandle() != excludeHandle)
			    		list.add(adminInfo);

					h = administrators.next(h);
				}

				// copy to array
				if (list.size() > 0)
				{
					admins = new ClientInfo[list.size()];
					list.toArray(admins);
				}

			}

		}

		if (isDebug())
			new MessageLogEntry(this, "getAdministrators", "Exiting.", Level.FINEST).dispatch();

		return admins;
	}

	/**
	 * Returns array of currently logged containers.
	 * @return	ContainerInfo[]	array of currently logged containers
	 */
	private ContainerInfo[] getContainersInfo()
	{
		if (isDebug())
			new MessageLogEntry(this, "getContainers", new Object[0] ).dispatch();

		// array of containers to be notified
		ContainerInfo[] acts = null;

		// generate array of containers to be notified
		synchronized (containers)
		{
			int len = containers.size();

			// no containers to notify
			if (len > 0)
			{
				ArrayList list = new ArrayList();

				int h = containers.first();
				while (h != 0)
				{
			    	ContainerInfo actInfo = (ContainerInfo)containers.get(h);
		    		list.add(actInfo);

					h = containers.next(h);
				}

				// copy to array
				if (list.size() > 0)
				{
					acts = new ContainerInfo[list.size()];
					list.toArray(acts);
				}

			}

		}

		if (isDebug())
			new MessageLogEntry(this, "getContainers", "Exiting.", Level.FINEST).dispatch();

		return acts;
	}

    /**
     * Returns array of currently logged clients.
     * @return	ClientInfo[]	array of currently logged clients
     */
    private ClientInfo[] getClientInfo()
    {
    	if (isDebug())
    		new MessageLogEntry(this, "getClientInfo", new Object[0] ).dispatch();

		ArrayList list = new ArrayList();

    	// get clients
    	synchronized (clients)
    	{
			int h = clients.first();
			while (h != 0)
			{
		    	ClientInfo clientInfo = (ClientInfo)clients.get(h);
	    		list.add(clientInfo);

				h = clients.next(h);
			}
    	}

    	// get admins
    	synchronized (administrators)
    	{
			int h = administrators.first();
			while (h != 0)
			{
		    	ClientInfo clientInfo = (ClientInfo)administrators.get(h);
	    		list.add(clientInfo);

				h = clients.next(h);
			}
    	}

	    ClientInfo[] infos = null;
    	if (list.size() > 0)
    	{
			infos = new ClientInfo[list.size()];
			list.toArray(infos);
    	}

    	if (isDebug())
    		new MessageLogEntry(this, "getClientInfo", "Exiting.", Level.FINEST).dispatch();

    	return infos;
    }



	/**
	 * Get client info. for specified handles of <code>Client</code> or <code>Administrator</code>. For <code>Component</code>
	 * handles component's <code>Container</code> is returned.
	 *
	 * @param	excludeHandle	handle of client not to be included in the array, can be 0.
	 * @param	handles			handles of the clients whose info. should be returned, non-<code>null</code>.
	 * @param	returns			requested infos, <code>null</code> if none
	 */
	private ClientInfo[] getClients(int excludeHandle, int[] handles)
	{
		assert (handles != null);

		if (isDebug())
			new MessageLogEntry(this, "getClients", new Object[] { new Integer(excludeHandle), handles }).dispatch();

		// array of clients to be notified
		ClientInfo[] clients = null;

		ArrayList list = new ArrayList();

		for (int i = 0; i < handles.length; i++)
			if (handles[i] != excludeHandle)
			{
				if ((handles[i] & TYPE_MASK) == COMPONENT_MASK)
				{
					ComponentInfo componentInfo = getComponentInfo(handles[i]);
					if (componentInfo != null)
					{
						ContainerInfo containerInfo = getContainerInfo(componentInfo.getContainer());
						if (containerInfo != null)
						{
							ClientInfo info = new ClientInfo(containerInfo.getHandle(), containerInfo.getName(), containerInfo.getContainer());
							list.add(info);
						}
					}
				}
				else
				{
					ClientInfo info = getClientInfo(handles[i]);
					if (info != null)
						list.add(info);
				}
			}

		// copy to array
		if (list.size() > 0)
		{
			clients = new ClientInfo[list.size()];
			list.toArray(clients);
		}

		if (isDebug())
			new MessageLogEntry(this, "getClients", "Exiting.", Level.FINEST).dispatch();

		return clients;
	}

	/**
	 * Remove component handle from the owners list.
	 * @param componentHandle	component handle to be removed from the owners list.
	 * @param owner				owner whom to remove the ownership.
	 */
	private void removeComponentOwner(int componentHandle, int owner)
	{
		if ((owner & TYPE_MASK) == COMPONENT_MASK)
		{
			// remove from owner list
			ComponentInfo componentInfo = getComponentInfo(owner);
			if (componentInfo != null)
			{
				// needed since execute is sync. and action
				// ComponentInfoCommandComponentRemove then sync. componentInfo
				synchronized (components)
				{
					// !!! ACID 3
					executeCommand(new ComponentInfoCommandComponentRemove(owner & HANDLE_MASK, componentHandle));
					//ci.getComponents().remove(componentHandle);
				}
			}
		}
		else
		{
			// remove from owner list
			ClientInfo clientInfo = getClientInfo(owner);
			if (clientInfo != null)
			{
				// needed since execute is sync. and action
				// ClientInfoCommandComponentRemove then sync. clientInfo
				synchronized (clients)
				{
					// !!! ACID 3
					executeCommand(new ClientInfoCommandComponentRemove(owner, componentHandle));
					//clientInfo.getComponents().remove(componentHandle);
				}
			}
		}
	}


	/**
	 * Add component handle to the owners list.
	 * @param componentHandle	component handle to be addd to the owners list.
	 * @param owner				owner whom to add the ownership.
	 */
	private void addComponentOwner(int componentHandle, int owner)
	{
		if ((owner & TYPE_MASK) == COMPONENT_MASK)
		{
			// remove from owner list
			ComponentInfo componentInfo = getComponentInfo(owner);
			if (componentInfo != null)
			{
				// needed since execute is sync. and action
				// ComponentInfoCommandComponentAdd then sync. componentInfo
				synchronized (components)
				{
					// !!! ACID 3
					if (!componentInfo.getComponents().contains(componentHandle))
						executeCommand(new ComponentInfoCommandComponentAdd(owner & HANDLE_MASK, componentHandle));
					//ci.getComponents().add(componentHandle);
				}
			}
		}
		else
		{
			// remove from owner list
			ClientInfo clientInfo = getClientInfo(owner);
			if (clientInfo != null)
			{
				// needed since execute is sync. and action
				// ClientInfoCommandComponentAdd then sync. clientInfo
				synchronized (clients)
				{
					// !!! ACID 3
					if (!clientInfo.getComponents().contains(componentHandle))
						executeCommand(new ClientInfoCommandComponentAdd(owner, componentHandle));
					//clientInfo.getComponents().add(componentHandle);
				}
			}
		}
	}


	/**
	 * Notifies administrators about newly logged client.
	 * @param	clientInfo	newly logged client, non-<code>null</code>
	 */
	private void notifyClientLogin(final ClientInfo clientInfo)
	{
		assert (clientInfo != null);

		// NOTE: do not log references - prevents GC to finalize and terminate connection thread (JacORB)
		if (isDebug())
			new MessageLogEntry(this, "notifyClientLogin", new Object[] { clientInfo == null ? "null" : clientInfo.toString() }).dispatch();

		// array of administrators to be notified
		ClientInfo[] admins = getAdministrators(clientInfo.getHandle());

		if (admins != null)
		{

			/**
			 * Task thats invokes <code>Administrator#clientLoggedIn</code> method.
			 */
			class ClientLoggedInTask implements Runnable
			{
				private ClientInfo administratorInfo;
				private ClientInfo clientInfo;

				public ClientLoggedInTask(ClientInfo administratorInfo, ClientInfo clientInfo)
				{
					this.administratorInfo = administratorInfo;
					this.clientInfo = clientInfo;
				}

				public void run()
				{
					final int MAX_RETRIES = 3;

					for (int retries = 0; retries < MAX_RETRIES; retries++)
					{
						try
						{
							((Administrator)administratorInfo.getClient()).clientLoggedIn(clientInfo);
							break;
						}
						catch (RemoteException re)
						{
							new MessageLogEntry(ManagerImpl.this, "notifyClientLogin.ClientLoggedInTask.run",
												"RemoteException caught while invoking 'Administrator.clientLoggedIn' on "+administratorInfo+".", re, LoggingLevel.ERROR).dispatch();
						}
					}
				}
			}


			// spawn new task which surely does not block
			for (int i = 0; i < admins.length; i++)
			{
				try
				{
					threadPool.execute(new ClientLoggedInTask(admins[i], clientInfo));
				}
				catch (InterruptedException ie)
				{
					new MessageLogEntry(ManagerImpl.this, "notifyClientLogin",
										"Interrupted execution of notification task for admin '"+admins[i]+"'.", ie, LoggingLevel.ERROR).dispatch();
				}
			}

		}

		if (isDebug())
			new MessageLogEntry(this, "notifyClientLogin", "Exiting.", Level.FINEST).dispatch();
	}

	/**
	 * Notifies administrators about newly logged container.
	 * @param	containerInfo	newly logged container, non-<code>null</code>
	 */
	private void notifyContainerLogin(final ContainerInfo containerInfo)
	{
		assert (containerInfo != null);

		// NOTE: do not log references - prevents GC to finalize and terminate connection thread (JacORB)
		if (isDebug())
			new MessageLogEntry(this, "notifyContainerLogin", new Object[] { containerInfo == null ? "null" : containerInfo.toString() }).dispatch();

		// array of administrators to be notified
		ClientInfo[] admins = getAdministrators(containerInfo.getHandle());

		if (admins != null)
		{

			/**
			 * Task thats invokes <code>Administrator#containerLoggedIn</code> method.
			 */
			class ContainerLoggedInTask implements Runnable
			{
				private ClientInfo administratorInfo;
				private ContainerInfo containerInfo;

				public ContainerLoggedInTask(ClientInfo administratorInfo, ContainerInfo containerInfo)
				{
					this.administratorInfo = administratorInfo;
					this.containerInfo = containerInfo;
				}

				public void run()
				{
					final int MAX_RETRIES = 3;

					for (int retries = 0; retries < MAX_RETRIES; retries++)
					{
						try
						{
							((Administrator)administratorInfo.getClient()).containerLoggedIn(containerInfo);
							break;
						}
						catch (RemoteException re)
						{
							new MessageLogEntry(ManagerImpl.this, "notifyContainerLogin.ContainerLoggedInTask.run",
												"RemoteException caught while invoking 'Administrator.containerLoggedIn' on "+administratorInfo+".", re, LoggingLevel.ERROR).dispatch();
						}
					}
				}
			}


			// spawn new task which surely does not block
			for (int i = 0; i < admins.length; i++)
			{
				try
				{
					threadPool.execute(new ContainerLoggedInTask(admins[i], containerInfo));
				}
				catch (InterruptedException ie)
				{
					new MessageLogEntry(ManagerImpl.this, "notifyContainerLogin",
										"Interrupted execution of notification task for administrator '"+admins[i]+"'.", ie, LoggingLevel.ERROR).dispatch();
				}
			}

		}

		if (isDebug())
			new MessageLogEntry(this, "notifyContainerLogin", "Exiting.", Level.FINEST).dispatch();
	}



	/**
	 * Notifies administrators about client logging out.
	 * @param	client	client logging out, non-<code>null</code>
	 */
	private void notifyClientLogout(final ClientInfo clientInfo)
	{
		assert (clientInfo != null);

		// NOTE: do not log references - prevents GC to finalize and terminate connection thread (JacORB)
		if (isDebug())
			new MessageLogEntry(this, "notifyClientLogout", new Object[] { clientInfo == null ? "null" : clientInfo.toString() }).dispatch();

		// array of administrators to be notified
		ClientInfo[] admins = getAdministrators(clientInfo.getHandle());

		if (admins != null)
		{

			/**
			 * Task thats invokes <code>Administrator#clientLoggedOut</code> method.
			 */
			class ClientLoggedOutTask implements Runnable
			{
				private ClientInfo administratorInfo;
				private ClientInfo clientInfo;

				public ClientLoggedOutTask(ClientInfo administratorInfo, ClientInfo clientInfo)
				{
					this.administratorInfo = administratorInfo;
					this.clientInfo = clientInfo;
				}

				public void run()
				{
					final int MAX_RETRIES = 3;

					for (int retries = 0; retries < MAX_RETRIES; retries++)
					{
						try
						{
							((Administrator)administratorInfo.getClient()).clientLoggedOut(clientInfo.getHandle());
							break;
						}
						catch (RemoteException re)
						{
							new MessageLogEntry(ManagerImpl.this, "notifyClientLogout.ClientLoggedOutTask.run",
												"RemoteException caught while invoking 'Administrator.clientLoggedOut' on "+administratorInfo+".", re, LoggingLevel.ERROR).dispatch();
						}
					}
				}
			}


			// spawn new task which surely does not block
			for (int i = 0; i < admins.length; i++)
			{
				try
				{
					threadPool.execute(new ClientLoggedOutTask(admins[i], clientInfo));
				}
				catch (InterruptedException ie)
				{
					new MessageLogEntry(ManagerImpl.this, "notifyClientLogout",
										"Interrupted execution of notification task for admin '"+admins[i]+"'.", ie, LoggingLevel.ERROR).dispatch();
				}
			}

		}

		if (isDebug())
			new MessageLogEntry(this, "notifyClientLogout", "Exiting.", Level.FINEST).dispatch();
	}


	/**
	 * Notifies administrators about container logging out.
	 * @param	containerInfo	container logging out, non-<code>null</code>
	 */
	private void notifyContainerLogout(final ContainerInfo containerInfo)
	{
		assert (containerInfo != null);

		// NOTE: do not log references - prevents GC to finalize and terminate connection thread (JacORB)
		if (isDebug())
			new MessageLogEntry(this, "notifyContainerLogout", new Object[] { containerInfo == null ? "null" : containerInfo.toString() }).dispatch();

		// array of administrators to be notified
		ClientInfo[] admins = getAdministrators(containerInfo.getHandle());

		if (admins != null)
		{

			/**
			 * Task thats invokes <code>Administrator#containerLoggedIn</code> method.
			 */
			class ContainerLoggedOutTask implements Runnable
			{
				private ClientInfo administratorInfo;
				private ContainerInfo containerInfo;

				public ContainerLoggedOutTask(ClientInfo administratorInfo, ContainerInfo containerInfo)
				{
					this.administratorInfo = administratorInfo;
					this.containerInfo = containerInfo;
				}

				public void run()
				{
					final int MAX_RETRIES = 3;

					for (int retries = 0; retries < MAX_RETRIES; retries++)
					{
						try
						{
							((Administrator)administratorInfo.getClient()).containerLoggedOut(containerInfo.getHandle());
							break;
						}
						catch (RemoteException re)
						{
							new MessageLogEntry(ManagerImpl.this, "notifyContainerLogout.ContainerLoggedOutTask.run",
												"RemoteException caught while invoking 'Administrator.containerLoggedOut' on "+administratorInfo+".", re, LoggingLevel.ERROR).dispatch();
						}
					}
				}
			}

			// spawn new task which surely does not block
			for (int i = 0; i < admins.length; i++)
			{
				try
				{
					threadPool.execute(new ContainerLoggedOutTask(admins[i], containerInfo));
				}
				catch (InterruptedException ie)
				{
					new MessageLogEntry(ManagerImpl.this, "notifyContainerLogout",
										"Interrupted execution of notification task for admin '"+admins[i]+"'.", ie, LoggingLevel.ERROR).dispatch();
				}
			}

		}

		if (isDebug())
			new MessageLogEntry(this, "notifyContainerLogout", "Exiting.", Level.FINEST).dispatch();
	}

	/**
	 * Notifies containers to disconnect or shutdown.
	 * @param	code	code to be sent to container, if <code>0</code> disconnect method will be called.
	 */
	private void notifyContainerDisconnectShutdown(int code)
	{
		if (isDebug())
			new MessageLogEntry(this, "notifyContainerDisconnectShutdown", new Object[] { new Integer(code) }).dispatch();

		// array of containers to be notified
		ContainerInfo[] acts = getContainersInfo();

		if (acts != null)
		{

			/**
			 * Task thats invokes <code>Container#shutdown</code> method.
			 */
			class ContainerShutdownTask implements Runnable
			{
				private int code;;
				private ContainerInfo containerInfo;

				public ContainerShutdownTask(ContainerInfo containerInfo, int code)
				{
					this.containerInfo = containerInfo;
					this.code = code;
				}

				public void run()
				{
					final int MAX_RETRIES = 3;

					for (int retries = 0; retries < MAX_RETRIES; retries++)
					{
						try
						{
							(containerInfo.getContainer()).shutdown(code);
							break;
						}
						catch (RemoteException re)
						{
							new MessageLogEntry(ManagerImpl.this, "notifyContainerDisconnectShutdown.ContainerShutdownTask.run",
												"RemoteException caught while invoking 'Container.shutdown' on "+containerInfo+".", re, LoggingLevel.ERROR).dispatch();
						}
					}
				}
			}

			/**
			 * Task thats invokes <code>Container#shutdown</code> method.
			 */
			class ContainerDisconnectTask implements Runnable
			{
				private ContainerInfo containerInfo;

				public ContainerDisconnectTask(ContainerInfo containerInfo)
				{
					this.containerInfo = containerInfo;
				}

				public void run()
				{
					final int MAX_RETRIES = 3;

					for (int retries = 0; retries < MAX_RETRIES; retries++)
					{
						try
						{
							(containerInfo.getContainer()).disconnect();
							break;
						}
						catch (RemoteException re)
						{
							new MessageLogEntry(ManagerImpl.this, "notifyContainerDisconnectShutdown.ContainerDisconnectTask.run",
												"RemoteException caught while invoking 'Container.disconnect' on "+containerInfo+".", re, LoggingLevel.ERROR).dispatch();
						}
					}
				}
			}

			// spawn new task which surely does not block
			for (int i = 0; i < acts.length; i++)
			{
				try
				{
					Runnable task;
					if (code == 0)
						task = new ContainerDisconnectTask(acts[i]);
					else
						task = new ContainerShutdownTask(acts[i], code);
					threadPool.execute(task);
				}
				catch (InterruptedException ie)
				{
					new MessageLogEntry(ManagerImpl.this, "notifyContainerDisconnectShutdown",
										"Interrupted execution of notification task for container '"+acts[i]+"'.", ie, LoggingLevel.ERROR).dispatch();
				}
			}

		}

		if (isDebug())
			new MessageLogEntry(this, "notifyContainerDisconnectShutdown", "Exiting.", Level.FINEST).dispatch();
	}

	/**
	 * Informs containers abouts its component shutdown order.
	 * @param	containerInfo	container to inform
	 * @param	handles			ordered list of component handles
	 */
	private void notifyContainerShutdownOrder(ContainerInfo containerInfo, int[] handles)
	{
		if (isDebug())
			new MessageLogEntry(this, "notifyContainerShutdownOrder", new Object[] { containerInfo, handles }).dispatch();

		/**
		 * Task thats invokes <code>Container#shutdown</code> method.
		 */
		class ContainerSetShutdownOrderTask implements Runnable
		{
			private int[] handles;
			private ContainerInfo containerInfo;

			public ContainerSetShutdownOrderTask(ContainerInfo containerInfo, int[] handles)
			{
				this.containerInfo = containerInfo;
				this.handles = handles;
			}

			public void run()
			{
				final int MAX_RETRIES = 3;

				for (int retries = 0; retries < MAX_RETRIES; retries++)
				{
					try
					{
						(containerInfo.getContainer()).set_component_shutdown_order(handles);
						break;
					}
					catch (RemoteException re)
					{
						new MessageLogEntry(ManagerImpl.this, "notifyContainerShutdownOrder.ContainerSetShutdownOrderTask.run",
											"RemoteException caught while invoking 'Container.set_component_shutdown_order' on "+containerInfo+".", re, LoggingLevel.ERROR).dispatch();
					}
				}
			}
		}


		// spawn new task which surely does not block
		try
		{
			threadPool.execute(new ContainerSetShutdownOrderTask(containerInfo, handles));
		}
		catch (InterruptedException ie)
		{
			new MessageLogEntry(ManagerImpl.this, "notifyContainerShutdownOrder",
								"Interrupted execution of notification task for container '"+containerInfo+"'.", ie, LoggingLevel.ERROR).dispatch();
		}

		if (isDebug())
			new MessageLogEntry(this, "notifyContainerShutdownOrder", "Exiting.", Level.FINEST).dispatch();
	}

    /**
     * Notifies clients to disconnect or shutdown.
     */
    private void notifyClientDisconnectShutdown()
    {
    	if (isDebug())
    		new MessageLogEntry(this, "notifyClientDisconnectShutdown", new Object[0]).dispatch();

    	// array of clients to be notified
    	ClientInfo[] clts = getClientInfo();

    	if (clts != null)
    	{

    		/**
    		 * Task thats invokes <code>Client#disconnect</code> method.
    		 */
    		class ClientDisconnectTask implements Runnable
    		{
    			private ClientInfo clientInfo;

    			public ClientDisconnectTask(ClientInfo clientInfo)
    			{
    				this.clientInfo = clientInfo;
    			}

    			public void run()
    			{
    				final int MAX_RETRIES = 3;

    				for (int retries = 0; retries < MAX_RETRIES; retries++)
    				{
    					try
    					{
    						(clientInfo.getClient()).disconnect();
    						break;
    					}
    					catch (RemoteException re)
    					{
    						new MessageLogEntry(ManagerImpl.this, "notifyClientDisconnectShutdown.ClientDisconnectTask.run",
    											"RemoteException caught while invoking 'Client.disconnect' on "+clientInfo+".", re, LoggingLevel.ERROR).dispatch();
    					}
    				}
    			}
    		}

    		// spawn new task which surely does not block
    		for (int i = 0; i < clts.length; i++)
    		{
    			try
    			{
    				Runnable task = new ClientDisconnectTask(clts[i]);
    				threadPool.execute(task);
    			}
    			catch (InterruptedException ie)
    			{
    				new MessageLogEntry(ManagerImpl.this, "notifyClientDisconnectShutdown",
    									"Interrupted execution of notification task for client '"+clts[i]+"'.", ie, LoggingLevel.ERROR).dispatch();
    			}
    		}

    	}

    	if (isDebug())
    		new MessageLogEntry(this, "notifyClientDisconnectShutdown", "Exiting.", Level.FINEST).dispatch();
    }



	/**
	 * Notifies administrators about Component request.
	 * @param	requestors	array of clients requesting the Component, non-<code>null</code>
	 * @param	components		array of requested the components, non-<code>null</code>
	 */
	private void notifyComponentRequested(int[] requestors, int[] components)
	{
		assert (requestors != null);
		assert (components != null);

		if (isDebug())
			new MessageLogEntry(this, "notifyComponentRequested", new Object[] { requestors, components }).dispatch();

		// array of administrators to be notified
		ClientInfo[] admins = getAdministrators(0);

		if (admins != null)
		{

			/**
			 * Task thats invokes <code>Administrator#components_requested</code> method.
			 */
			class ComponentRequestedTask implements Runnable
			{
				private ClientInfo administratorInfo;
				private int[] requestors;
				private int[] components;

				public ComponentRequestedTask(ClientInfo administratorInfo, int[] requestors, int[] components)
				{
					this.administratorInfo = administratorInfo;
					this.requestors = requestors;
					this.components = components;
				}

				public void run()
				{
					final int MAX_RETRIES = 3;

					for (int retries = 0; retries < MAX_RETRIES; retries++)
					{
						try
						{
							((Administrator)administratorInfo.getClient()).components_requested(requestors, components);
							break;
						}
						catch (RemoteException re)
						{
							new MessageLogEntry(ManagerImpl.this, "notifyComponentRequested.ComponentRequestedTask.run",
												"RemoteException caught while invoking 'Administrator.components_requested' on "+administratorInfo+".", re, LoggingLevel.ERROR).dispatch();
						}
					}
				}
			}


			// spawn new task which surely does not block
			for (int i = 0; i < admins.length; i++)
			{
				try
				{
					threadPool.execute(new ComponentRequestedTask(admins[i], requestors, components));
				}
				catch (InterruptedException ie)
				{
					new MessageLogEntry(ManagerImpl.this, "notifyComponentRequested",
										"Interrupted execution of notification task for admin '"+admins[i]+"'.", ie, LoggingLevel.ERROR).dispatch();
				}
			}

		}

		if (isDebug())
			new MessageLogEntry(this, "notifyComponentRequested", "Exiting.", Level.FINEST).dispatch();
	}

	/**
	 * Notifies administrators about Component request.
	 * @param	requestors	array of clients requesting the Component, non-<code>null</code>
	 * @param	components		array of requested the components, non-<code>null</code>
	 */
	private void notifyComponentReleased(int[] requestors, int[] components)
	{
		assert (requestors != null);
		assert (components != null);

		if (isDebug())
			new MessageLogEntry(this, "notifyComponentReleased", new Object[] { requestors, components }).dispatch();

		// array of administrators to be notified
		ClientInfo[] admins = getAdministrators(0);

		if (admins != null)
		{

			/**
			 * Task thats invokes <code>Administrator#components_requested</code> method.
			 */
			class ComponentReleasedTask implements Runnable
			{
				private ClientInfo administratorInfo;
				private int[] requestors;
				private int[] components;

				public ComponentReleasedTask(ClientInfo administratorInfo, int[] requestors, int[] components)
				{
					this.administratorInfo = administratorInfo;
					this.requestors = requestors;
					this.components = components;
				}

				public void run()
				{
					final int MAX_RETRIES = 3;

					for (int retries = 0; retries < MAX_RETRIES; retries++)
					{
						try
						{
							((Administrator)administratorInfo.getClient()).components_released(requestors, components);
							break;
						}
						catch (RemoteException re)
						{
							new MessageLogEntry(ManagerImpl.this, "notifyComponentReleased.ComponentReleasedTask.run",
												"RemoteException caught while invoking 'Administrator.components_released' on "+administratorInfo+".", re, LoggingLevel.ERROR).dispatch();
						}
					}
				}
			}


			// spawn new task which surely does not block
			for (int i = 0; i < admins.length; i++)
			{
				try
				{
					threadPool.execute(new ComponentReleasedTask(admins[i], requestors, components));
				}
				catch (InterruptedException ie)
				{
					new MessageLogEntry(ManagerImpl.this, "notifyComponentReleased",
										"Interrupted execution of notification task for admin '"+admins[i]+"'.", ie, LoggingLevel.ERROR).dispatch();
				}
			}

		}

		if (isDebug())
			new MessageLogEntry(this, "notifyComponentReleased", "Exiting.", Level.FINEST).dispatch();
	}

	/**
	 * Notifies administrators about component request.
	 * @param	excludeClient	client not to be informed, <code>0</code> if none
	 * @param	clientHandles	array of client handles to be informed, non-<code>null</code>
	 * @param	info			component infos to be sent to the clients, non-<code>null</code>
	 */
	private void notifyComponentAvailable(int excludeClient, int[] clientHandles, ComponentInfo[] info)
	{
		assert (clients != null);
		assert (info != null);

		// NOTE: do not log references - prevents GC to finalize and terminate connection thread (JacORB)
		if (isDebug())
			new MessageLogEntry(this, "notifyComponentAvailable", new Object[] { new Integer(excludeClient), clientHandles, info == null ? "null" : "non-null ComponentInfo[] array"}).dispatch();

		// array of clients to be notified
		ClientInfo[] clients = getClients(excludeClient, clientHandles);

		if (clients != null)
		{

			/**
			 * Task thats invokes <code>Client#components_available</code> method.
			 */
			class ComponentAvailableTask implements Runnable
			{
				private ClientInfo clientInfo;
				private ComponentInfo[] info;

				public ComponentAvailableTask(ClientInfo clientInfo, ComponentInfo[] info)
				{
					this.clientInfo = clientInfo;
					this.info = info;
				}

				public void run()
				{
					final int MAX_RETRIES = 3;

					for (int retries = 0; retries < MAX_RETRIES; retries++)
					{
						try
						{
							clientInfo.getClient().components_available(info);
							break;
						}
						catch (RemoteException re)
						{
							new MessageLogEntry(ManagerImpl.this, "notifyComponentAvailable.ComponentAvailableTask.run",
												"RemoteException caught while invoking 'Client.components_available' on "+clientInfo+".", re, LoggingLevel.ERROR).dispatch();
						}
					}
				}
			}


			// spawn new task which surely does not block
			for (int i = 0; i < clients.length; i++)
			{
				try
				{
					threadPool.execute(new ComponentAvailableTask(clients[i], info));
				}
				catch (InterruptedException ie)
				{
					new MessageLogEntry(ManagerImpl.this, "notifyComponentAvailable",
										"Interrupted execution of notification task for client '"+clients[i]+"'.", ie, LoggingLevel.ERROR).dispatch();
				}
			}

		}

		if (isDebug())
			new MessageLogEntry(this, "notifyComponentAvailable", "Exiting.", Level.FINEST).dispatch();
	}

	/**
	 * Notifies administrators about Component request.
	 * @param	excludeClient	client not to be informed, <code>0</code> if none
	 * @param	clientHandles	array of client handles to be informed, non-<code>null</code>
	 * @param	names			component names to be sent to the clients, non-<code>null</code>
	 */
	private void notifyComponentUnavailable(int excludeClient, int[] clientHandles, String[] names)
	{
		assert (clients != null);
		assert (names != null);

		if (isDebug())
			new MessageLogEntry(this, "notifyComponentUnavailable", new Object[] { new Integer(excludeClient), clientHandles, names }).dispatch();

		// array of clients to be notified
		ClientInfo[] clients = getClients(excludeClient, clientHandles);

		if (clients != null)
		{

			/**
			 * Task thats invokes <code>Client#components_unavailable</code> method.
			 */
			class ComponentUnavailableTask implements Runnable
			{
				private ClientInfo clientInfo;
				private String[] names;

				public ComponentUnavailableTask(ClientInfo clientInfo, String[] names)
				{
					this.clientInfo = clientInfo;
					this.names = names;
				}

				public void run()
				{
					final int MAX_RETRIES = 3;

					for (int retries = 0; retries < MAX_RETRIES; retries++)
					{
						try
						{
							clientInfo.getClient().components_unavailable(names);
							break;
						}
						catch (RemoteException re)
						{
							new MessageLogEntry(ManagerImpl.this, "notifyComponentUnavailable.ComponentUnavailableTask.run",
												"RemoteException caught while invoking 'Client.components_unavailable' on "+clientInfo+".", re, LoggingLevel.ERROR).dispatch();
						}
					}
				}
			}


			// spawn new task which surely does not block
			for (int i = 0; i < clients.length; i++)
			{
				try
				{
					threadPool.execute(new ComponentUnavailableTask(clients[i], names));
				}
				catch (InterruptedException ie)
				{
					new MessageLogEntry(ManagerImpl.this, "notifyComponentUnavailable",
										"Interrupted execution of notification task for client '"+clients[i]+"'.", ie, LoggingLevel.ERROR).dispatch();
				}
			}

		}

		if (isDebug())
			new MessageLogEntry(this, "notifyComponentUnavailable", "Exiting.", Level.FINEST).dispatch();
	}

	/**
	 * Sends an message to the client.
	 * @param	client		client to receive the message, non-<code>null</code>
	 * @param	message		message to be sent, non-<code>null</code>
	 * @param	messageType	type of the message, non-<code>null</code>
	 */
	private void sendMessage(Client client, String message, MessageType messageType)
	{
		assert (client != null);
		assert (message != null);
		assert (messageType != null);

		// NOTE: do not log references - prevents GC to finalize and terminate connection thread (JacORB)
		if (isDebug())
			new MessageLogEntry(this, "sendMessage", new Object[] { client == null ? "null" : client.toString(), message, messageType }).dispatch();

		/**
		 * Task thats invokes <code>Client#message</code> method.
		 */
		class ClientMessageTask implements Runnable
		{
			private Client client;
			private String message;
			private MessageType messageType;

			public ClientMessageTask(Client client, String message, MessageType messageType)
			{
				this.client = client;
				this.message = message;
				this.messageType = messageType;
			}

			public void run()
			{
				final int MAX_RETRIES = 3;

				for (int retries = 0; retries < MAX_RETRIES; retries++)
				{
					try
					{
						client.message(messageType, message);
						break;
					}
					catch (RemoteException re)
					{
						new MessageLogEntry(ManagerImpl.this, "sendMessage.ClientMessageTask.run",
											"RemoteException caught while invoking 'Client.message' on "+client+".", re, LoggingLevel.ERROR).dispatch();
					}
				}
			}
		}


		// spawn new task which surely does not block
		try
		{
			threadPool.execute(new ClientMessageTask(client, message, messageType));
		}
		catch (InterruptedException ie)
		{
			new MessageLogEntry(ManagerImpl.this, "sendMessage",
								"Interrupted execution of message task for client '"+client+"'.", ie, LoggingLevel.ERROR).dispatch();
		}

		if (isDebug())
			new MessageLogEntry(this, "sendMessage", "Exiting.", Level.FINEST).dispatch();
	}


	/**
	 * Performs security check on given handle and if check if owner has <code>rights</code> permissions granted.
	 *
	 * Validating means checking key part (KEY_MASK) of the handle.
	 *
	 * @param	id	handle to be checked.
	 * @param	rights	checks if owner of the handle has this permissions granted, can be 0.
	 * @throws	AcsJNoPermissionEx	thrown if handle is not valid or handle owner has not enough permissions
	 */
	private void securityCheck(int id, int requiredRights) throws AcsJNoPermissionEx
	{
		if (isDebug())
			new MessageLogEntry(this, "securityCheck", new Object[] { new Integer(id), new Integer(requiredRights) }).dispatch();

		// check if already shutdown
		if (id != this.getHandle() && shutdown.get())
		{
			// already shutdown
			AcsJNoPermissionEx npe = new AcsJNoPermissionEx();
			npe.setReason("Manager in shutdown state.");
			throw npe;
		}

		// parse handle part
		int handle	= id & HANDLE_MASK;

		int grantedRights = 0;
		boolean invalidHandle = true;

		switch	(id & TYPE_MASK)
		{
			case CONTAINER_MASK:
				synchronized (containers)
				{
					if (containers.isAllocated(handle))
					{
						ContainerInfo info = (ContainerInfo)containers.get(handle);
						if (info.getHandle() == id)
							invalidHandle = false;
						grantedRights = CONTAINER_RIGHTS;
					}
				}
				break;

			case CLIENT_MASK:
				synchronized (clients)
				{
					if (clients.isAllocated(handle))
					{
						ClientInfo info = (ClientInfo)clients.get(handle);
						if (info.getHandle() == id)
							invalidHandle = false;
						grantedRights = info.getAccessRights();
					}
				}
				break;

			case ADMINISTRATOR_MASK:
				synchronized (administrators)
				{
					if (administrators.isAllocated(handle))
					{
						ClientInfo info = (ClientInfo)administrators.get(handle);
						if (info.getHandle() == id)
							invalidHandle = false;
						grantedRights = info.getAccessRights();
					}
				}
				break;

			case COMPONENT_MASK:
				synchronized (components)
				{
					if (components.isAllocated(handle))
					{
						ComponentInfo info = (ComponentInfo)components.get(handle);
						if (info != null && info.getHandle() == id)
							invalidHandle = false;
						grantedRights = AccessRights.REGISTER_COMPONENT;
					}
				}
				break;

			case MANAGER_MASK:
				invalidHandle = false;
				grantedRights = AccessRights.REGISTER_COMPONENT |
								AccessRights.SHUTDOWN_SYSTEM |
								AccessRights.INTROSPECT_MANAGER;
				break;
		}

		if (invalidHandle)
		{
			// NO_PERMISSION
			AcsJNoPermissionEx npe = new AcsJNoPermissionEx();
			npe.setReason("Invalid handle.");
			throw npe;
		}

		if ((grantedRights & requiredRights) != requiredRights)
		{
			// NO_PERMISSION
			AcsJNoPermissionEx npe = new AcsJNoPermissionEx();
			npe.setReason("Insufficient rights.");
			throw npe;
		}

		if (isDebug())
			new MessageLogEntry(this, "securityCheck", "Exiting.", Level.FINEST).dispatch();

	}

	/**
	 * Get client info. for specified id of <code>Client</code> or <code>Administrator</code>.
	 *
	 * @param	id	handle of the client whose info. should be returned
	 * @param	returns	requested info, <code>null</code> if client with requested handle does not exits
	 */
	public ClientInfo getClientInfo(int id)
	{
		if (isDebug())
			new MessageLogEntry(this, "getClientInfo", new Object[] { new Integer(id) }).dispatch();

		// parse handle part
		int handle	= id & HANDLE_MASK;

		// info to be returned
		ClientInfo info = null;

		switch	(id & TYPE_MASK)
		{
			case CLIENT_MASK:
				synchronized (clients)
				{
					if (clients.isAllocated(handle))
						info = (ClientInfo)clients.get(handle);
				}
				break;

			case ADMINISTRATOR_MASK:
				synchronized (administrators)
				{
					if (administrators.isAllocated(handle))
						info = (ClientInfo)administrators.get(handle);
				}
				break;
		}

		if (isDebug())
			new MessageLogEntry(this, "getClientInfo", "Exiting.", Level.FINEST).dispatch();

		return info;
	}

	/**
	 * Get container info. for specified id of <code>Container</code>.
	 *
	 * @param	id	handle of the container whose info. should be returned
	 * @param	returns	requested info, <code>null</code> if container with requested handle does not exits
	 */
	private ContainerInfo getContainerInfo(int id)
	{
		if (isDebug())
			new MessageLogEntry(this, "getContainerInfo", new Object[] { new Integer(id) }).dispatch();

		// parse handle part
		int handle	= id & HANDLE_MASK;

		// info to be returned
		ContainerInfo info = null;

		synchronized (containers)
		{
			if (containers.isAllocated(handle))
				info = (ContainerInfo)containers.get(handle);
		}

		if (isDebug())
			new MessageLogEntry(this, "getContainerInfo", "Exiting.", Level.FINEST).dispatch();

		return info;
	}

	/**
	 * Get container info. for specified name of <code>Container</code>.
	 *
	 * @param	name	name of the container whose info. should be returned, non-<code>null</code>
	 * @param	returns	requested info, <code>null</code> if container with requested handle does not exits
	 */
	private ContainerInfo getContainerInfo(String name)
	{
		assert(name != null);

		if (isDebug())
			new MessageLogEntry(this, "getContainerInfo", new Object[] { name }).dispatch();

		// info to be returned
		ContainerInfo info = null;

		synchronized (containers)
		{
			int h = containers.first();
			while (h != 0)
			{
				ContainerInfo containerInfo = (ContainerInfo)containers.get(h);
				if (name.equals(containerInfo.getName()))
				{
					info = containerInfo;
					break;
				}

				h = containers.next(h);
			}
		}

		if (isDebug())
			new MessageLogEntry(this, "getContainerInfo", "Exiting.", Level.FINEST).dispatch();

		return info;
	}

	/**
	 * Get component info. for specified id of <code>Component</code>.
	 *
	 * @param	id	handle of the component whose info. should be returned
	 * @param	returns	requested info, <code>null</code> if component with requested handle does not exits
	 */
	public ComponentInfo getComponentInfo(int id)
	{
		if (isDebug())
			new MessageLogEntry(this, "getComponentInfo", new Object[] { new Integer(id) }).dispatch();

		// parse handle part
		int handle	= id & HANDLE_MASK;

		// info to be returned
		ComponentInfo info = null;

		synchronized (components)
		{
			if (components.isAllocated(handle))
				info = (ComponentInfo)components.get(handle);
		}

		if (isDebug())
			new MessageLogEntry(this, "getComponentInfo", "Exiting.", Level.FINEST).dispatch();

		return info;
	}

	/*****************************************************************************/
	/********************** [ Component (de)activation ] *************************/
	/*****************************************************************************/

	/**
	 * Internal method for requesting components.
	 * @param	requestor	requestor of the component.
	 * @param	curl	curl of the component to be requested.
	 * @param	status	status of the component.
	 * @return	component		retuested component.
	 */
	private Component internalRequestComponent(int requestor, URI curl, StatusHolder status)
	{
		return internalRequestComponent(requestor, curl, status, true);
	}

	/**
	 * Internal method for requesting components.
	 * @param	requestor	requestor of the component.
	 * @param	curl	curl of the component to be requested.
	 * @param	status	status of the component.
	 * @param	activate	<code>true</code> if component has to be activated
	 * @return	component		retuested component.
	 */
	private Component internalRequestComponent(int requestor, URI curl, StatusHolder status, boolean activate)
	{
		if (isDebug())
			new MessageLogEntry(this, "internalRequestComponent", new Object[] { new Integer(requestor), curl, status, new Boolean(activate) }).dispatch();

		// extract unique name
		String name = extractName(curl);

		checkCyclicDependency(requestor, name);

		// try to acquire lock
		boolean lockAcquired = acquireSynchronizationObject(name, getLockTimeout());
		if (lockAcquired)
		{
			boolean releaseRWLock = true;
			try
			{
				// try to acquire activation readers lock first
				// NOTE: the locks are NOT reentrant
				try	{
					activationPendingRWLock.readLock().acquire();
				} catch (InterruptedException ie) {
					releaseRWLock = false;

					NoResourcesException nre = new NoResourcesException(this, "Failed to obtain synchronization lock for component '"+name+"'.");
					nre.caughtIn(this, "internalRequestComponent");
					nre.putValue("curl", curl);
					nre.putValue("requestor", new Integer(requestor));
					throw nre;
				}

				ComponentInfo componentInfo = null;
				try
				{
					componentInfo = internalNoSyncRequestComponent(requestor, name, null, null, null, RELEASE_TIME_UNDEFINED, status, activate);
				}
				catch(ComponentSpecIncompatibleWithActiveComponentException ciwace)
				{
					status.setStatus(ComponentStatus.COMPONENT_NOT_ACTIVATED);
					AssertionFailed af = new AssertionFailed(this, "Failed to request for component.", ciwace);
					af.caughtIn(this, "internalRequestComponent");
					// throw af;
				}

				if (componentInfo != null)
					return componentInfo.getComponent();
				else
					return null;
			}
			finally
			{
				if (releaseRWLock)
					activationPendingRWLock.readLock().release();
				releaseSynchronizationObject(name);
			}
		}
		else
		{
			NoResourcesException nre = new NoResourcesException(this, "Failed to obtain synchronization lock for component '"+name+"', possible deadlock.");
			nre.caughtIn(this, "internalRequestComponent");
			nre.putValue("curl", curl);
			nre.putValue("requestor", new Integer(requestor));
			throw nre;
		}
	}

	/**
	 * Check for cyclic dependency between components, if detected path is returned.
	 * @param requestor	handle of requestor component
	 * @param requested	handle of requested component
	 * @return if cycle is detected then path is returned, otherwise <code>null</code>
	 */
	private ArrayList doCycleCheck(int requestor, int requested)
	{
		synchronized (components)
		{
			ComponentInfo info = (ComponentInfo)components.get(requested & HANDLE_MASK);
			if (info == null)
				return null;

			if (requested == requestor)
			{
				// detected
				ArrayList list = new ArrayList();
				list.add(info);
				return list;
			}

			int[] subcomponents = info.getComponents().toArray();
			for (int i = 0; i < subcomponents.length; i++)
			{
				ArrayList list = doCycleCheck(requestor, subcomponents[i]);
				if (list != null) {
					list.add(info);
					return list;
				}
			}
		}
		return null;
	}

	/**
	 * Check for cyclic dependency between components, if detected <code>NoResourcesException</code> exception is thrown.
	 * @param requestor
	 * @param requestedComponentName
	 */
	private void checkCyclicDependency(int requestor, String requestedComponentName)
	{
		if (isDebug())
			new MessageLogEntry(this, "checkCyclicDependency", new Object[] { new Integer(requestor), requestedComponentName }).dispatch();

		// check only if component is requesting component
		if ((requestor & TYPE_MASK) != COMPONENT_MASK)
			return;

		// check if requested component is already activated (and pending activations)
		ComponentInfo componentInfo = null;
		synchronized (components)
		{
			int h = components.first();
			while (h != 0)
			{
				ComponentInfo info = (ComponentInfo)components.get(h);
				if (info.getName().equals(requestedComponentName))
				{
					// yes, component is already activated
					componentInfo = info;
					break;
				}
				else
					h = components.next(h);
			}

			// check pending activations...
			ComponentInfo pendingComponentInfo;
			synchronized (pendingActivations)
			{
				pendingComponentInfo = (ComponentInfo)pendingActivations.get(requestedComponentName);
			}

			// if component is already completely activated, we allow cyclic dependencies (but their usage is discouraged)
			if (componentInfo != null && pendingComponentInfo == null)
				return;

			// take pending activation...
			if (componentInfo == null)
				componentInfo = pendingComponentInfo;

			// not activated yet, so no cyclic dependency is possible
			if (componentInfo == null)
				return;

			ArrayList pathList = doCycleCheck(requestor, componentInfo.getHandle());
			// no dependency detected
			if (pathList == null)
				return;

			// stringify
			StringBuffer pathBuffer = new StringBuffer();
			for (int i = pathList.size()-1; i >= 0; i--)
				pathBuffer.append(((ComponentInfo)pathList.get(i)).getName()).append(" -> ");
			pathBuffer.append(componentInfo.getName());

			NoResourcesException nre = new NoResourcesException(this, "Cyclic dependency detected: ["+pathBuffer.toString()+"].");
			nre.caughtIn(this, "checkCyclicDependency");
			nre.putValue("requestedComponentName", requestedComponentName);
			nre.putValue("requestor", new Integer(requestor));
			throw nre;
		}

	}

	/**
	 * Internal method for requesting components.
	 * @param	requestor		requestor of the component.
	 * @param	name			name of component to be requested, non-<code>null</code>.
	 * @param	type			type of component to be requested; if <code>null</code> CDB will be queried.
	 * @param	code			code of component to be requested; if <code>null</code> CDB will be queried.
	 * @param	containerName	container name of component to be requested; if <code>null</code> CDB will be queried.
	 * @param	status			returned completion status of the request.
	 * @param	activate		<code>true</code> if component has to be activated
	 * @return	componentInfo	<code>ComponentInfo</code> of requested component.
	 */
	private ComponentInfo internalRequestComponent(
								int requestor,
								String name, String type, String code, String containerName,
								int keepAliveTime,
								StatusHolder status, boolean activate)
		throws ComponentSpecIncompatibleWithActiveComponentException
	{

		assert(name != null);
		assert(status != null);

		if (isDebug())
			new MessageLogEntry(this, "internalRequestComponent",
						new Object[] { new Integer(requestor), name, type, code, containerName, new Integer(keepAliveTime),
									   status, new Boolean(activate) }).dispatch();

		checkCyclicDependency(requestor, name);

		// try to acquire lock
		boolean lockAcquired = acquireSynchronizationObject(name, getLockTimeout());
		if (lockAcquired)
		{
			boolean releaseRWLock = true;
			try
			{
				// try to acquire activation readers lock first
				// NOTE: the locks are NOT reentrant
				try	{
					activationPendingRWLock.readLock().acquire();
				} catch (InterruptedException ie) {
					releaseRWLock = false;

					NoResourcesException nre = new NoResourcesException(this, "Failed to obtain synchronization lock for component '"+name+"'.");
					nre.caughtIn(this, "internalRequestComponent");
					nre.putValue("name", name);
					nre.putValue("requestor", new Integer(requestor));
					throw nre;
				}

				return internalNoSyncRequestComponent(requestor, name, type, code, containerName, keepAliveTime, status, activate);
			}
			finally
			{
				if (releaseRWLock)
					activationPendingRWLock.readLock().release();
				releaseSynchronizationObject(name);
			}
		}
		else
		{
			NoResourcesException nre = new NoResourcesException(this, "Failed to obtain synchronization lock for component '"+name+"', possible deadlock.");
			nre.caughtIn(this, "internalRequestComponent");
			nre.putValue("name", name);
			nre.putValue("requestor", new Integer(requestor));
			throw nre;
		}

	}

	/**
	 * Internal method for requesting components (non sync).
	 * @param	requestor		requestor of the component.
	 * @param	name			name of component to be requested, non-<code>null</code>.
	 * @param	type			type of component to be requested; if <code>null</code> CDB will be queried.
	 * @param	code			code of component to be requested; if <code>null</code> CDB will be queried.
	 * @param	containerName	container name of component to be requested; if <code>null</code> CDB will be queried.
	 * @param	status			returned completion status of the request.
	 * @param	activate		<code>true</code> if component has to be activated
	 * @return	componentInfo	<code>ComponentInfo</code> of requested component.
	 */
	private ComponentInfo internalNoSyncRequestComponent(
								int requestor,
								String name, String type, String code, String containerName,
								int keepAliveTime,
								StatusHolder status, boolean activate)
		throws ComponentSpecIncompatibleWithActiveComponentException
	{

		assert(name != null);
		assert(status != null);

		if (isDebug())
			new MessageLogEntry(this, "internalNoSyncRequestComponent",
						new Object[] { new Integer(requestor), name, type, code, containerName, new Integer(keepAliveTime),
									   status, new Boolean(activate) }).dispatch();

		boolean isOtherDomainComponent = name.startsWith(CURL_URI_SCHEMA);
		boolean isDynamicComponent = isOtherDomainComponent ?
		        						false : (type != null || code != null || containerName != null);

		//
		// check if component is already activated
		//

		int h;

		// if true, component with handle h will be reactivated
		boolean reactivate = false;
		ComponentInfo componentInfo = null;

		synchronized (components)
		{
			h = components.first();
			while (h != 0)
			{
				componentInfo = (ComponentInfo)components.get(h);
				if (componentInfo.getName().equals(name))
				{
					// yes, component is already activated

					// check if component is unavailable
					synchronized (unavailableComponents)
					{
						if (unavailableComponents.containsKey(name))
						{
							// try to reactivate, possible component reallocation
							reactivate = true;
						}
					}


					// check for consistency
					ContainerInfo containerInfo = getContainerInfo(componentInfo.getContainer());

					if ((type != null && !componentInfo.getType().equals(type)) ||
					    (code != null && componentInfo.getCode() != null && !componentInfo.getCode().equals(code)) ||
						(!reactivate && containerInfo != null &&
						 containerName != null && !containerInfo.getName().equals(containerName)))
					{
						ComponentSpec activeComponentSpec = new ComponentSpec(
																	componentInfo.getName(),
																	componentInfo.getType(),
																	componentInfo.getCode() != null ? componentInfo.getCode() : "<unknown>",
																	containerInfo != null ? containerInfo.getName() : "<none>");
						ComponentSpecIncompatibleWithActiveComponentException ciwace =
							new ComponentSpecIncompatibleWithActiveComponentException(this,
								"Requested and already activated component data with the same name are not compatible.",
								activeComponentSpec);

						ciwace.caughtIn(this, "internalNoSyncRequestComponent");
						// NOTE: do not log references - prevents GC to finalize and terminate connection thread (JacORB)
						ciwace.putValue("componentInfo", componentInfo == null ? "null" : componentInfo.toString());
						ciwace.putValue("activeComponentSpec", activeComponentSpec);
						throw ciwace;
					}


					if (activate)
					{

						// bail out and reactivate
						if (reactivate)
							break;


						// add client/component as an owner (if requestor is not 'reactivation')
						if (requestor != 0)
						{
							// !!! ACID
							if (!componentInfo.getClients().contains(requestor))
								executeCommand(new ComponentCommandClientAdd(componentInfo.getHandle() & HANDLE_MASK, requestor));
								//componentInfo.getClients().add(requestor);
						}

						// add component to client component list (if requestor is not manager or 'reactivation')
						if (requestor != this.getHandle() && requestor != 0)
							addComponentOwner(componentInfo.getHandle(), requestor);

						// inform administrators about component request
						notifyComponentRequested(new int[] { requestor }, new int[] { componentInfo.getHandle() });

						// notify about the change (only if on the same container)
						// on complete system shutdown sort will be done anyway
						if ((requestor & TYPE_MASK) == COMPONENT_MASK)
						{
							ComponentInfo requestorComponentInfo = getComponentInfo(requestor);
							if (requestorComponentInfo != null &&
								requestorComponentInfo.getContainerName() != null &&
								requestorComponentInfo.getContainerName().equals(componentInfo.getContainerName()))
								topologySortManager.notifyTopologyChange(componentInfo.getContainer());
						}

						// return info
						status.setStatus(ComponentStatus.COMPONENT_ACTIVATED);
						return componentInfo;
					}
					else
					{
						if (reactivate)
							status.setStatus(ComponentStatus.COMPONENT_NOT_ACTIVATED);
						else
							status.setStatus(ComponentStatus.COMPONENT_ACTIVATED);

						return componentInfo;
					}


				}
				h = components.next(h);
			}
		}

		// if we have to reactivate a dynamic component,
		// then reread info from existing component info
		// and do not touch CDB
		if (reactivate && componentInfo.isDynamic())
		{
			if (componentInfo.getType() == null || componentInfo.getCode() == null ||
				componentInfo.getDynamicContainerName() == null)
			{
				// failed
				new MessageLogEntry(this, "internalNoSyncRequestComponent", "Failed to reactivate dynamic component '"+componentInfo+"'.", LoggingLevel.ERROR).dispatch();
				status.setStatus(ComponentStatus.COMPONENT_DOES_NO_EXIST);
				return null;
			}
			else
			{
				// reread info
				type = componentInfo.getType();
				code = componentInfo.getCode();
				containerName = componentInfo.getDynamicContainerName();
			}
		}
		// is CDB lookup needed
		else if (!isOtherDomainComponent &&
				(type == null || code == null || containerName == null || keepAliveTime == RELEASE_TIME_UNDEFINED))
		{

			//
			// read component info from CDB / remote directory lookup
			//

			DAOProxy dao = getComponentsDAOProxy();
			if (dao == null || readStringCharacteristics(dao, name, true) == null)
			{
				// component with this name does not exists,
				// make a remote directory lookup
				Object ref = lookup(name, null);
				if (ref != null)
				{
					// found
					status.setStatus(ComponentStatus.COMPONENT_ACTIVATED);
					return new ComponentInfo(0, name, null, null, new ServiceComponent(ref));
				}
				else
				{
					// not found
					status.setStatus(ComponentStatus.COMPONENT_DOES_NO_EXIST);
					return null;
				}

			}

			if (code == null)
			{
				code = readStringCharacteristics(dao, name+"/Code");
				if (code == null)
				{
					new MessageLogEntry(this, "internalNoSyncRequestComponent", "Misconfigured CDB, there is no code of component '"+name+"' defined.", LoggingLevel.WARNING).dispatch();
					status.setStatus(ComponentStatus.COMPONENT_DOES_NO_EXIST);
					return null;
				}
			}

			if (type == null)
			{
				type = readStringCharacteristics(dao, name+"/Type");
				if (type == null)
				{
					new MessageLogEntry(this, "internalNoSyncRequestComponent", "Misconfigured CDB, there is no type of component '"+name+"' defined.", LoggingLevel.WARNING).dispatch();
					status.setStatus(ComponentStatus.COMPONENT_DOES_NO_EXIST);
					return null;
				}
			}

			if (containerName == null)
			{
				containerName = readStringCharacteristics(dao, name+"/Container");
				if (containerName == null)
				{
					new MessageLogEntry(this, "internalNoSyncRequestComponent", "Misconfigured CDB, there is no container of component '"+name+"' defined.", LoggingLevel.WARNING).dispatch();
					status.setStatus(ComponentStatus.COMPONENT_DOES_NO_EXIST);
					return null;
				}
			}

			if (keepAliveTime == RELEASE_TIME_UNDEFINED)
			{
				// defaults to 0 == RELEASE_IMMEDIATELY
				keepAliveTime = readLongCharacteristics(dao, name+"/KeepAliveTime", RELEASE_IMMEDIATELY, true);
			}

		}


		// if requestor did not request activation we are finished
		if (!activate)
		{
			status.setStatus(ComponentStatus.COMPONENT_NOT_ACTIVATED);
			return null;
		}

		/****************** component activation ******************/

		//
		// get container/remote manager
		//

		Container container = null;
		ContainerInfo containerInfo = null;
		Manager remoteManager = null;

		if (isOtherDomainComponent)
		{
			// @todo MF do the login?
		    try
		    {
			    String domainName = CURLHelper.createURI(name).getAuthority();
			    remoteManager = getManagerForDomain(domainName);
			    if (remoteManager == null)
			        throw new AssertionFailed(this, "Failed to obtain manager for domain '" + domainName + "'.");
		    } catch (Throwable th) {
				new MessageLogEntry(this, "internalNoSyncRequestComponent", "Failed to obtain non-local manager required by component '"+name+"'.", th, LoggingLevel.WARNING).dispatch();
				status.setStatus(ComponentStatus.COMPONENT_NOT_ACTIVATED);
				return null;
		    }
		}
		else
		{
			// search for container by its name
			containerInfo = getContainerInfo(containerName);


			// try to start-up container
			if (containerInfo == null)
				containerInfo = startUpContainer(containerName);

			// check state and get container
			if (containerInfo != null) {
				checkContainerShutdownState(containerInfo);
				container = containerInfo.getContainer();
			}

			// required container is not logged in
			if (container == null)
			{
				new MessageLogEntry(this, "internalNoSyncRequestComponent", "Container '"+containerName+"' required by component '"+name+"' is not logged in.", LoggingLevel.WARNING).dispatch();
				status.setStatus(ComponentStatus.COMPONENT_NOT_ACTIVATED);
				return null;
			}
		}

		//
		// get handle
		//

		// obtain handle
		synchronized (components)
		{
			// only preallocate (if necessary)
			if (!reactivate)
				// !!! ACID 2
				h = ((Integer)executeCommand(new ComponentCommandPreallocate())).intValue();
				//h = components.preallocate();

			// failed to obtain handle
			if (h == 0)
			{
				NoResourcesException af = new NoResourcesException(this, "Preallocation of new handle failed, too many registered components.");
				af.caughtIn(this, "internalNonSyncRequestComponent");
				af.putValue("h", new Integer(h));
				af.putValue("name", name);
				af.putValue("requestor", new Integer (requestor));
				throw af;
			}

			// create temporary ComponentInfo - to allow hierarchical components
			if (!reactivate)
			{
				ComponentInfo data = new ComponentInfo(h | COMPONENT_MASK, name, type, code, null);
				// !!! ACID
				executeCommand(new ComponentCommandSet(h, data));

				// add to pending activation list
				synchronized (pendingActivations)
				{
					componentInfo = (ComponentInfo)pendingActivations.put(name, data);
				}

				// add component to client component list to allow dependency checks
				if ((requestor & TYPE_MASK) == COMPONENT_MASK)
					addComponentOwner(data.getHandle(), requestor);
			}

		}

		//
		// invoke get_component
		//

		componentInfo = null;

		if (isOtherDomainComponent)
		{
			//
			// invoke get_component on remote manager
			//

			try
			{
			    URI curlName = CURLHelper.createURI(name);
			    StatusHolder statusHolder = new StatusHolder();
			    // @todo MF tmp (handle)
			    remoteManager.getComponent(INTERDOMAIN_MANAGER_HANDLE, curlName, true, statusHolder);

			    if (statusHolder.getStatus() == ComponentStatus.COMPONENT_ACTIVATED)
			    {
				    // local name to be used
				    String localName = curlName.getPath();
					if (localName.charAt(0) == '/')
					    localName = localName.substring(1);
				    /// @TODO MF tmp (handle)
				    ComponentInfo[] infos = remoteManager.getComponentInfo(INTERDOMAIN_MANAGER_HANDLE, new int[0], localName, "*", true);
				    if (infos != null && infos.length == 1)
				    {
				        componentInfo = infos[0];
				        // fix container name
					    componentInfo.setContainerName(CURL_URI_SCHEMA + curlName.getAuthority() + "/" + componentInfo.getContainerName());
				    }
				    //else
				    //    throw new RemoteException(this, "Failed to obtain component info for '"+name+"' from remote manager.");
			    }
			    //else
			    //    throw new RemoteException(this, "Failed to obtain component '"+name+"' from remote manager, status: " + statusHolder.getStatus() + ".");
			}
			catch (Exception ex)
			{
				RemoteException re = new RemoteException(this, "Failed to obtain component '"+name+"' from remote manager.", ex);
				re.caughtIn(this, "internalNonSyncRequestComponent");
				re.putValue("h", new Integer(h));
				re.putValue("name", name);
				re.putValue("requestor", new Integer (requestor));
				// exception service will handle this
				new MessageLogEntry(this, "internalNoSyncRequestComponent", re.getMessage(), LoggingLevel.ERROR).dispatch();
			}
		}
		else
		{
			//
			// invoke get_component on container
			//

			// log info
			new MessageLogEntry(this, "internalNoSyncRequestComponent", "Activating component '"+name+"' on container '" + containerInfo.getName() + "'.", LoggingLevel.INFO).dispatch();

			try
			{
				componentInfo = container.activate_component(h | COMPONENT_MASK, name, code, type);
			}
			catch (Exception ex)
			{
				RemoteException re = new RemoteException(this, "Failed to activate component '"+name+"' on container '"+containerName+"'.", ex);
				re.caughtIn(this, "internalNonSyncRequestComponent");
				re.putValue("h", new Integer(h));
				re.putValue("name", name);
				re.putValue("requestor", new Integer (requestor));
				// exception service will handle this
				new MessageLogEntry(this, "internalNoSyncRequestComponent", re.getMessage(), LoggingLevel.ERROR).dispatch();
			}
		}

		// remove component from client component list, will be added later (first lots of checks has to be done)
		if ((requestor & TYPE_MASK) == COMPONENT_MASK)
			removeComponentOwner(h | COMPONENT_MASK, requestor);

		// remove from pending activation list
		synchronized (pendingActivations)
		{
			pendingActivations.remove(name);
		}

		// failed to activate
		if (componentInfo == null || componentInfo.getHandle() == 0 || componentInfo.getComponent() == null)
		{
			new MessageLogEntry(this, "internalNoSyncRequestComponent", "Failed to activate component '"+name+"'.", LoggingLevel.ERROR).dispatch();

			synchronized (components)
			{
				// !!! ACID 3
				if (!reactivate)
					executeCommand(new ComponentCommandDeallocate(h, true));
					//components.deallocate(h, true);
			}

			status.setStatus(ComponentStatus.COMPONENT_NOT_ACTIVATED);
			return null;
		}

		// log info
		new MessageLogEntry(this, "internalNoSyncRequestComponent", "Component '"+name+"' activated successfully.", LoggingLevel.INFO).dispatch();

		//
		// check type consistency
		//
		if (!isOtherDomainComponent && !componentInfo.getComponent().doesImplement(type))
		{
			// just output SEVERE message
			new MessageLogEntry(this, "internalNoSyncRequestComponent", "Activated component '" + name + "' does not implement specified type '" + type + "'.", LoggingLevel.SEVERE).dispatch();
		}

		// @todo MF do the component handle mapping here (remember map and fix componentInfo),
		// component info (to get type and code, container - prefix name)
		if (isOtherDomainComponent)
		{
		    // @todo MF tmp (for testing)
		    componentInfo.setHandle(h | COMPONENT_MASK);
		    componentInfo.setClients(new IntArray());
		    componentInfo.setComponents(new IntArray(0));
		    componentInfo.setContainer(0);

		    // set variables
		    type = componentInfo.getType();
		    code = componentInfo.getCode();
		}

		int clients[];
		synchronized (components)
		{

			//
			// check if returned handle matches ours (given)
			//
			if (componentInfo.getHandle() != (h | COMPONENT_MASK))
			{
				// container returned different handle
				// (it seems it has already activated this Component)

				// check if we can accept it
				int componentHandle = componentInfo.getHandle() & HANDLE_MASK;
				if (components.isAllocated(componentHandle))
				{
					// handle is already allocated, we have to reject returned handle

					// !!! ACID 3
					// cancel preallocation
					if (!reactivate)
						executeCommand(new ComponentCommandDeallocate(h, true));
						//components.deallocate(h, true);

					new MessageLogEntry(this, "internalNoSyncRequestComponent", "Container returned another handle than given, failed to fix handle since returned handle is already allocated.", LoggingLevel.ERROR).dispatch();

					status.setStatus(ComponentStatus.COMPONENT_ACTIVATED);		// component is activated, but cannot be managed by the Manager
					return null;
				}
				else
				{
					// handle is free, relocate handle

					ComponentInfo existingData = null;

					// !!! ACID 3
					// deallocate old
					if (!reactivate)
						executeCommand(new ComponentCommandDeallocate(h, true));
						//components.deallocate(h, true);
					else
					{
						// !!! ACID 3
						existingData = (ComponentInfo)components.get(h);
						executeCommand(new ComponentCommandDeallocate(h));
						//components.deallocate(h);
					}

					// !!! ACID 3
					// preallocate new
					h = ((Integer)executeCommand(new ComponentCommandAllocateHandle(componentHandle, true))).intValue();
					//h = components.allocate(componentHandle, true);
					if (h == 0)
					{
						// failed to allocate new
						new MessageLogEntry(this, "internalNoSyncRequestComponent", "Container returned another handle than given, failed to fix handle due to handle relocation failure.", LoggingLevel.ERROR).dispatch();
						status.setStatus(ComponentStatus.COMPONENT_ACTIVATED);		// Component is activated, but cannot be managed by the Manager
						return null;
					}
					// !!! ACID 3
					else if (existingData != null)
						executeCommand(new ComponentCommandSet(h, existingData));
						//components.set(h, existingData);

					new MessageLogEntry(this, "internalNoSyncRequestComponent", "Container returned another handle than given, handle fixed.", LoggingLevel.WARNING).dispatch();
				}

			}


			//
			// pre-store data
			//

			ComponentInfo existingData = (ComponentInfo)components.get(h);

			// create a new ComponentInfo - do not trust containers
			ComponentInfo data = new ComponentInfo(componentInfo.getHandle(), name, type, code, componentInfo.getComponent());
			if (existingData != null)
			{
					data.setClients(existingData.getClients());
					data.setComponents(existingData.getComponents());
			}

			if (requestor != 0)
				if (!data.getClients().contains(requestor))		// hierarchical components need this check
					data.getClients().add(requestor);

			if (keepAliveTime <= RELEASE_NEVER)
				if (!data.getClients().contains(this.getHandle()))		// make component immortal
					data.getClients().add(this.getHandle());


			if (isOtherDomainComponent)
			{
			    data.setContainer(0);
				data.setContainerName(componentInfo.getContainerName());
			}
			else
			{
				data.setContainer(containerInfo.getHandle());
				data.setContainerName(containerInfo.getName());
			}
			data.setAccessRights(0);
			data.setInterfaces(componentInfo.getInterfaces());

			// mark as dynamic component and store its container
			if (isDynamicComponent)
			{
				data.setDynamic(true);
				data.setDynamicContainerName(containerName);
			}

			// !!! ACID
			executeCommand(new ComponentCommandSet(h, data));
			//components.set(h, data);

			// acknowledge allocation
			if (!reactivate)
				// !!! ACID 2
				executeCommand(new ComponentCommandAckAlloc(h));
				//components.ackAllocation(h);

			componentInfo = data;

			clients = componentInfo.getClients().toArray();
		}


		if (!isOtherDomainComponent)
		{
			// add component to client component list to allow dependency checks
			if ((requestor & TYPE_MASK) == COMPONENT_MASK)
				addComponentOwner(componentInfo.getHandle(), requestor);

			//
			// call construct
			//
			boolean constructed = false;
			try
			{
				componentInfo.getComponent().construct();
				constructed = true;
			}
			catch (Exception ex)
			{
				RemoteException re = new RemoteException(this, "Failed to construct component '"+name+"', exception caught when invoking 'construct()' method.", ex);
				re.caughtIn(this, "internalNonSyncRequestComponent");
				re.putValue("h", new Integer(h));
				re.putValue("name", name);
				re.putValue("requestor", new Integer (requestor));
				re.putValue("componentInfo", componentInfo.toString());
				// exception service will handle this
				new MessageLogEntry(this, "internalNoSyncRequestComponent", re.getMessage(), LoggingLevel.ERROR).dispatch();

			}

			// remove component from client component list, will be added later
			if ((requestor & TYPE_MASK) == COMPONENT_MASK)
				removeComponentOwner(componentInfo.getHandle(), requestor);

			if (!constructed)
			{
				// release Component
				synchronized (components)
				{
					// !!! ACID 3
					if (!reactivate)
						executeCommand(new ComponentCommandDeallocate(h));
						//components.deallocate(h);

					// deactivate
					try
					{
						container.deactivate_components(new int[] { componentInfo.getHandle() });
					}
					catch (Exception ex)
					{
						RemoteException re = new RemoteException(this, "Failed to deactivate component '"+name+"' on container '"+containerName+"'.", ex);
						re.caughtIn(this, "internalNonSyncRequestComponent");
						re.putValue("h", new Integer(h));
						re.putValue("name", name);
						re.putValue("requestor", new Integer (requestor));
						re.putValue("componentInfo", componentInfo.toString());
						// exception service will handle this
						new MessageLogEntry(this, "internalNoSyncRequestComponent", re.getMessage(), LoggingLevel.ERROR).dispatch();
					}

					status.setStatus(ComponentStatus.COMPONENT_NOT_ACTIVATED);
					return null;
				}
			}
		}

		// for surely activated
		// add component to client component list
		if (requestor != this.getHandle() && requestor != 0)
			addComponentOwner(componentInfo.getHandle(), requestor);

		// add component to container component list
		if (!isOtherDomainComponent)
			synchronized (containerInfo.getComponents())
			{
				/// !!! ACID

				if (!containerInfo.getComponents().contains(componentInfo.getHandle()))
					executeCommand(new ContainerInfoCommandComponentAdd(containerInfo.getHandle() & HANDLE_MASK, componentInfo.getHandle()));
					//containerInfo.getComponents().add(componentInfo.getHandle());
			}

		// remove component from unavailable list
		if (reactivate)
		{
			// !!! ACID

			synchronized (unavailableComponents)
			{
				executeCommand(new UnavailableComponentCommandRemove(name));
				//unavailableComponents.remove(name);
			}
		}

		// take snapshot of manager state
		if( prevayler != null )
		{
			try {
				((SnapshotPrevayler)prevayler).takeSnapshot();
			}
			catch (IOException e) {
				e.printStackTrace();
			}
		}

		//
		// bind to remote directory
		//
		bind(convertToHiearachical(name), "O", componentInfo.getComponent().getObject());


		//
		// inform clients about availability
		//
		if (reactivate)
		{
			notifyComponentAvailable(requestor, clients, new ComponentInfo[] { componentInfo });
		}

		//
		// notify administrators about the activation
		//
		notifyComponentRequested(new int[] { requestor }, new int[] { componentInfo.getHandle() });

		if (isDebug())
		{
			if (reactivate)
				new MessageLogEntry(this, "internalNoSyncRequestComponent", "Component '"+name+"' reactivated.", LoggingLevel.DEBUG).dispatch();
			else
				new MessageLogEntry(this, "internalNoSyncRequestComponent", "Component '"+name+"' activated.", LoggingLevel.DEBUG).dispatch();
		}

		// notify about the change (only this-domain container which activated the component)...
		if (containerInfo != null)
			topologySortManager.notifyTopologyChange(containerInfo.getHandle());

		if (isDebug())
			new MessageLogEntry(this, "internalNonSyncRequestComponent", "Exiting.", Level.FINEST).dispatch();

		status.setStatus(ComponentStatus.COMPONENT_ACTIVATED);
		return componentInfo;
	}

	/**
	 * Internal method for releasing components.
	 *
	 * @param	owner	owner of the component, if manager's own handle then deactivation will be forced
	 * @param	curl	CURL of the component to be released.
	 * @param	force	force deactivate, if still has owners then component will be made unavailable.
	 * @return			Number of clients that are still using the component after the operation completed.
	 */
	private int internalReleaseComponent(int owner, URI curl, boolean force)
	   throws AcsJNoPermissionEx 
	{
		if (isDebug())
			new MessageLogEntry(this, "internalReleaseComponent", new Object[] { new Integer(owner), curl, new Boolean(force) }).dispatch();

		// resolve handle from curl
		int h = 0;

		String name = extractName(curl);

		synchronized (components)
		{
			h = components.first();
			while (h != 0)
		    {
		    	ComponentInfo componentInfo = (ComponentInfo)components.get(h);
				if (componentInfo.getName().equals(name))
				{
					h = componentInfo.getHandle();
					break;
				}
				h = components.next(h);
		    }
		}

		// if found, delegate operation, otherwise do nothing
		if (h != 0)
			return internalReleaseComponent(owner, h, force);
		else
			return 0;
	}

	/**
	 * Internal method for deactivating components.
	 *
	 * @param	name	name of the component to be released.
	 */
	private void internalDeactivateComponent(String name)
	{
		if (isDebug())
			new MessageLogEntry(this, "internalDeactivateComponent", new Object[] { name }).dispatch();


		// try to acquire lock
		boolean lockAcquired = acquireSynchronizationObject(name, 3*Sync.ONE_MINUTE);
		if (lockAcquired)
		{
			boolean releaseRWLock = false;
			try
			{
				// resolve componentInfo from curl
				ComponentInfo componentInfo = null;
				synchronized (components)
				{
					int h = components.first();
					while (h != 0)
				    {
				    	ComponentInfo ci = (ComponentInfo)components.get(h);
						if (ci.getName().equals(name))
						{
							// a new owner detected, leave component activated
							if (ci.getClients().size() > 0)
								return;
							componentInfo = ci;
							break;
						}
						h = components.next(h);
				    }
				}

				// component is already gone, nothing to do
				if (componentInfo == null)
					return;

				// try to acquire activation readers lock first
				// NOTE: the locks are NOT reentrant
				releaseRWLock = true;
				try	{
					activationPendingRWLock.readLock().acquire();
				} catch (InterruptedException ie) {
					releaseRWLock = false;

					NoResourcesException nre = new NoResourcesException(this, "Failed to obtain synchronization lock for component '"+name+"'.");
					nre.caughtIn(this, "internalDeactivateComponent");
					nre.putValue("name", name);
					throw nre;
				}

				internalNoSyncDeactivateComponent(componentInfo);
			}
			finally
			{
				if (releaseRWLock)
					activationPendingRWLock.readLock().release();
				releaseSynchronizationObject(name);
			}
		}
		else
		{
			NoResourcesException nre = new NoResourcesException(this, "Failed to obtain synchronization lock for component '"+name+"', possible deadlock.");
			nre.caughtIn(this, "internalDeactivateComponent");
			nre.putValue("name", name);
			throw nre;
		}
	}

	/**
	 * Internal method for releasing components.
	 *
	 * @param	owner	owner of the component.
	 * @param	h		handle of the component to be released.
	 * @param	force	force deactivate, if still has owners then component will be made unavailable.
	 * @return			Number of clients that are still using the component after the operation completed.
	 */
	private int internalReleaseComponent(int owner, int h, boolean force)
		   throws AcsJNoPermissionEx 
	{
		if (isDebug())
			new MessageLogEntry(this, "internalReleaseComponent", new Object[] { new Integer(owner), new Integer(h), new Boolean(force) }).dispatch();

		// extract name
		String name = null;
		synchronized (components)
		{
			int handle = h & HANDLE_MASK;
			ComponentInfo componentInfo = null;
			if (components.isAllocated(handle))
				componentInfo = (ComponentInfo)components.get(handle);

			if (componentInfo == null || componentInfo.getHandle() != h)
			{
				// invalid Component handle
				BadParametersException af = new BadParametersException(this, "Invalid component handle.");
				af.caughtIn(this, "internalReleaseComponent");
				af.putValue("h", new Integer(h));
				af.putValue("owner", new Integer(owner));
				throw af;
			}

			name = componentInfo.getName();

		}

		// try to acquire lock
		boolean lockAcquired = acquireSynchronizationObject(name, 3*Sync.ONE_MINUTE);
		if (lockAcquired)
		{
			boolean releaseRWLock = true;
			try
			{
				// try to acquire activation readers lock first
				// NOTE: the locks are NOT reentrant
				try	{
					activationPendingRWLock.readLock().acquire();
				} catch (InterruptedException ie) {
					releaseRWLock = false;

					NoResourcesException nre = new NoResourcesException(this, "Failed to obtain synchronization lock for component '"+name+"'.");
					nre.caughtIn(this, "internalReleaseComponent");
					nre.putValue("owner", new Integer(owner));
					nre.putValue("h", new Integer(h));
					throw nre;
				}

				return internalNoSyncReleaseComponent(owner, h, force);
			}
			finally
			{
				if (releaseRWLock)
					activationPendingRWLock.readLock().release();
				releaseSynchronizationObject(name);
			}
		}
		else
		{
			NoResourcesException nre = new NoResourcesException(this, "Failed to obtain synchronization lock for component '"+name+"', possible deadlock.");
			nre.caughtIn(this, "internalReleaseComponent");
			nre.putValue("owner", new Integer(owner));
			nre.putValue("h", new Integer(h));
			throw nre;
		}

	}

	/**
	 * Internal method for releasing components.
	 *
	 * @param	owner	owner of the component.
	 * @param	h		handle of the component to be released.
	 * @param	force	force deactivate, if still has owners then component will be made unavailable.
	 * @return			Number of clients that are still using the component after the operation completed.
	 */
	private int internalNoSyncReleaseComponent(int owner, int h, boolean force)
	     throws AcsJNoPermissionEx 
	{

		if (isDebug())
			new MessageLogEntry(this, "internalNoSyncReleaseComponent", new Object[] { new Integer(owner), new Integer(h), new Boolean(force) }).dispatch();

		int handle = h & HANDLE_MASK;
		int owners = 0;

		ComponentInfo componentInfo = null;
		synchronized (components)
		{
			if (components.isAllocated(handle))
				componentInfo = (ComponentInfo)components.get(handle);

			if (componentInfo == null || componentInfo.getHandle() != h)
			{
				// invalid component handle
				BadParametersException af = new BadParametersException(this, "Invalid component handle.");
				af.caughtIn(this, "internalNoSyncReleaseComponent");
				af.putValue("h", new Integer(h));
				af.putValue("owner", new Integer(owner));
				throw af;
			}

			// remove ownership of the component
			if (!componentInfo.getClients().contains(owner))
			{
				if (!force)
				{
					// not an owner
					AcsJNoPermissionEx npe = new AcsJNoPermissionEx();
					npe.setReason("Unregistering component that client does not own.");
					throw npe;
				}
			}
			else
			{

				// ACID - !!!

				// remove client/component as an owner
				executeCommand(new ComponentCommandClientRemove(componentInfo.getHandle() & HANDLE_MASK, owner));
				//componentInfo.getClients().remove(owner);

				// remove component from client component list
				if (owner != this.getHandle())
					removeComponentOwner(componentInfo.getHandle(), owner);
			}

			owners = componentInfo.getClients().size();

			if (owners == 0)
			{
				// !!! ACID

				// this should not be done here (this will throw no permission exception to the component releasing its subcomponents)
				// deallocate Component
				//executeCommand(new ComponentCommandDeallocate(handle));
				////components.deallocate(handle);


				// remove from unavailable list
				// there is not owner to be unavailable for
				synchronized (unavailableComponents)
				{
					if (unavailableComponents.containsKey(componentInfo.getName()))
					{
						// !!! ACID
						executeCommand(new UnavailableComponentCommandRemove(componentInfo.getName()));
						//unavailableComponents.remove(componentInfo.getName());
					}
				}

			}
		}

		/****************** component deactivation ******************/

		// there is no owners of the component, deactivate it
		if (force)
		{
			internalNoSyncDeactivateComponent(componentInfo);
		}
		else if (owners == 0)
		{
			int keepAliveTime = RELEASE_IMMEDIATELY;
			String name = componentInfo.getName();
			boolean isOtherDomainComponent = name.startsWith(CURL_URI_SCHEMA);
			if (!isOtherDomainComponent)
			{
				// @todo or not: what about dynamic components - it is not possible
				// to specify it using ComponentSpec, also not fully recoverable
				// when info is passed from the container
				DAOProxy dao = getComponentsDAOProxy();
				if (dao != null)
					keepAliveTime = readLongCharacteristics(dao, name+"/KeepAliveTime", keepAliveTime, true);
			}

			if (keepAliveTime == 0)
				internalNoSyncDeactivateComponent(componentInfo);
			else if (keepAliveTime > 0)
				delayedDeactivationTask.schedule(new DeactivateComponentTask(name), keepAliveTime * 1000);

			// negative means immortal, however this could not happen since immortal
			// components have manager as an owner
		}

		// notify administrators about the release request
		notifyComponentReleased(new int[] { owner }, new int[] { h });

		if (isDebug())
			new MessageLogEntry(this, "internalNoSyncReleaseComponent", "Component '"+componentInfo.getName()+"' released.", LoggingLevel.DEBUG).dispatch();

		if (isDebug())
			new MessageLogEntry(this, "internalNoSyncReleaseComponent", "Exiting.", Level.FINEST).dispatch();

		// component deactivated
		if (owners == 0 || force)
		{
			topologySortManager.notifyTopologyChange(componentInfo.getContainer());
		}
		else if ((owner & TYPE_MASK) == COMPONENT_MASK)
		{
			// component dependency changed...
			// notify about the change (only if on the same container)...
			// on complete system shutdown sort will be done anyway
			ComponentInfo ownerComponentInfo = getComponentInfo(owner);
			if (ownerComponentInfo != null && ownerComponentInfo.getContainerName() != null &&
				ownerComponentInfo.getContainerName().equals(componentInfo.getContainerName()))
				topologySortManager.notifyTopologyChange(componentInfo.getContainer());
		}

		return owners;
	}


	/**
	 * Deactivate component, issue deactivate reeust to container (or other manager).
	 * @param componentInfo	info about component to be deactivated.
	 */
	private void internalNoSyncDeactivateComponent(ComponentInfo componentInfo)
	{
		// unbind from remote directory
		unbind(convertToHiearachical(componentInfo.getName()), "O");

		int handle = componentInfo.getHandle() & HANDLE_MASK;
		int owners = componentInfo.getClients().size();

		try
		{
			//
			// get container/remote manager
			//

			String name = componentInfo.getName();
			boolean isOtherDomainComponent = name.startsWith(CURL_URI_SCHEMA);

			if (isOtherDomainComponent)
			{
				Manager remoteManager = null;

				// @todo MF do the login?
			    try
			    {
				    String domainName = CURLHelper.createURI(name).getAuthority();
				    remoteManager = getManagerForDomain(domainName);
				    if (remoteManager == null)
				        throw new AssertionFailed(this, "Failed to obtain manager for domain '" + domainName + "'.");
			    } catch (Throwable th) {
					new MessageLogEntry(this, "internalNoSyncDeactivateComponent", "Failed to obtain non-local manager required by component '"+name+"'.", th, LoggingLevel.WARNING).dispatch();
					return;
			    }

				// @todo MF call release_component on other manager (logout?)
			    // release component
				try
				{
				    URI curlName = CURLHelper.createURI(name);
				    // @todo MF tmp (handle)
				    remoteManager.releaseComponent(INTERDOMAIN_MANAGER_HANDLE, curlName);
				}
				catch (Exception ex)
				{
					RemoteException re = new RemoteException(this, "Failed to release component '"+componentInfo.getName()+"' on remote manager.'", ex);
					re.caughtIn(this, "internalNoSyncDeactivateComponent");
					re.putValue("componentInfo", componentInfo.toString());
					// exception service will handle this
					new MessageLogEntry(this, "internalNoSyncDeactivateComponent", re.getMessage(), LoggingLevel.ERROR).dispatch();
				}

			}
			else
			{
				//
				// search for container by its name
				//
				Container container = null;
				ContainerInfo containerInfo = null;
				int containerHandle = componentInfo.getContainer();
				// if containerHandle equals 0, we have unavailable or registered component
				if (containerHandle != 0)
				{
					containerInfo = getContainerInfo(containerHandle);
					if (containerInfo != null)
					{

						// remove component from container component list
						synchronized (containerInfo.getComponents())
						{
							// !!! ACID
							if (containerInfo.getComponents().contains(componentInfo.getHandle()))
								executeCommand(new ContainerInfoCommandComponentRemove(containerInfo.getHandle() & HANDLE_MASK, componentInfo.getHandle()));
								//containerInfo.getComponents().remove(componentInfo.getHandle());
						}

						// we allow this (since releasing components is part of container shutdown procedure)
						//checkContainerState(containerInfo);

						container = containerInfo.getContainer();
					}

					// required container is not logged in
					if (container == null)
					{
						// then simply do not do the deactivation
						String containerName;
						if (containerInfo != null)
							containerName = containerInfo.getName();
						else
							containerName = HandleHelper.toString(componentInfo.getContainer());
						new MessageLogEntry(this, "internalNoSyncDeactivateComponent", "Container '"+containerName+"' required by component '"+componentInfo.getName()+"' is not logged in.", LoggingLevel.WARNING).dispatch();
					}

				}

				if (container != null)
				{

					// log info
					new MessageLogEntry(this, "internalNoSyncDeactivateComponent", "Deactivating component '"+componentInfo.getName()+"' on container '" + containerInfo.getName() + "'.", LoggingLevel.INFO).dispatch();

					// destruct
					try
					{
						componentInfo.getComponent().destruct();
					}
					catch (Exception ex)
					{
						RemoteException re = new RemoteException(this, "Failed to destruct component '"+componentInfo.getName()+"', exception caught when invoking 'destruct()' method.", ex);
						re.caughtIn(this, "internalNoSyncDeactivateComponent");
						re.putValue("componentInfo", componentInfo.toString());
						// exception service will handle this
						new MessageLogEntry(this, "internalNoSyncDeactivateComponent", re.getMessage(), LoggingLevel.ERROR).dispatch();
					}

					// deactivate component in anycase
					try
					{
						container.deactivate_components(new int[] { componentInfo.getHandle() });
					}
					catch (Exception ex)
					{
						RemoteException re = new RemoteException(this, "Failed to deactivate component '"+componentInfo.getName()+"' on container '"+containerInfo.getName()+"'.", ex);
						re.caughtIn(this, "internalNoSyncDeactivateComponent");
						re.putValue("componentInfo", componentInfo.toString());
						// exception service will handle this
						new MessageLogEntry(this, "internalNoSyncDeactivateComponent", re.getMessage(), LoggingLevel.ERROR).dispatch();
					}

					// shutdown container if required (and necessary)
					conditionalShutdownContainer(containerInfo);

				}

			}

		} finally {
			if (owners == 0)
			{
				// deallocate Component
				synchronized (components)
				{
					executeCommand(new ComponentCommandDeallocate(handle));
					//components.deallocate(handle);
				}
			}
		}

		// log info
		new MessageLogEntry(this, "internalNoSyncDeactivateComponent", "Component '"+componentInfo.getName()+"' deactivated.", LoggingLevel.INFO).dispatch();

		// release all subcomponents (just like client logoff)
		// component should have already done this by itself, but take care of clean cleanup
		// what about that: if subcomponent becomes unavailable, does component also becomes?!
		// no, it is notified and it handles situation by its own way (e.g. changes component state).
		// Just like it already handles activation (manager does not care for dependecy trees).
		int [] subcomponents = null;
		// no not hold the lock
		synchronized (componentInfo.getComponents())
		{
		    if (componentInfo.getComponents().size() > 0) {
		        IntArray toCleanupList = new IntArray();

		        IntArray comps = componentInfo.getComponents();
		        for (int i = 0; i < comps.size(); i++)
		            if (components.isAllocated(comps.get(i) & HANDLE_MASK))
		                toCleanupList.add(comps.get(i));

		        if (toCleanupList.size() > 0)
		            subcomponents = toCleanupList.toArray();
		    }

	        //subcomponents = componentInfo.getComponents().toArray();
		}

		if (subcomponents != null && subcomponents.length > 0)
		    new ReleaseComponentTask(componentInfo.getHandle(), subcomponents).run();

		// make unavailable (deactivation was forced)
		if (owners > 0)
			makeUnavailable(componentInfo);

	}

	/**
	 * Start-up container (if it has a deploy info).
	 * @param containerName	name of the container to start up.
	 * @return container info of container, <code>null</code> if failed to start.
	 */
	private ContainerInfo startUpContainer(String containerName)
	{

		DAOProxy dao = getContainersDAOProxy();
		if (dao == null)
			return null;

		//
		// read DeployInfo and initiate start-up
		//

		String type = readStringCharacteristics(dao, containerName + "/DeployInfo/Type", true);
		if (type == null)
			return null;

		String host = readStringCharacteristics(dao, containerName + "/DeployInfo/Host", true);
		if (host == null)
			return null;

		String flags = readStringCharacteristics(dao, containerName + "/DeployInfo/Flags", true);
		if (flags == null)
			flags = "";

		// add itself as manager reference
		flags += " -m " + transport.getManagerReference();

		long instance = readLongCharacteristics(dao, containerName + "/DeployInfo/Instance", 0, true);

		try
		{
			Daemon daemon = transport.getDaemon(host);
			if (daemon != null)
				daemon.startContainer(type, containerName, (short)instance, flags);
			else
				throw new RuntimeException("Failed to get daemon.");

		} catch (Throwable th)
		{
			RemoteException re = new RemoteException(this, "Failed to connect to ACS daemon on host '"+host+"'.", th);
			re.caughtIn(this, "startUpContainer");
			re.putValue("host", host);
			// exception service will handle this
			new MessageLogEntry(this, "startUpContainer", re.getMessage(), LoggingLevel.WARNING).dispatch();
			return null;
		}


		//
		// wait for login
		//

		final int CONTAINER_STARTUP_TIMEOUT = 15000; // 15 seconds

		// notify about new container login
		synchronized (containerLoggedInMonitor)
		{
			int waitTime = CONTAINER_STARTUP_TIMEOUT;
			while (waitTime > 0)
			{
				long start = System.currentTimeMillis();

				try {
					containerLoggedInMonitor.wait(waitTime);
				} catch (InterruptedException e) {
					return null;
				}

				// check if container has logged in
				ContainerInfo info = getContainerInfo(containerName);
				if (info != null)
					return info;

				waitTime = waitTime - (int)(System.currentTimeMillis() - start);
			}

			// container did not logged in within CONTAINER_STARTUP_TIMEOUT ms
			return null;
		}

	}

	/**
	 * Conditionally (if has no components and is not immortal container) shutdown container.
	 * @param containerInfo	container to shutdown.
	 */
	private void conditionalShutdownContainer(ContainerInfo containerInfo)
	{
		int componentsCount;
		synchronized (containerInfo.getComponents())
		{
			componentsCount = containerInfo.getComponents().size();
		}

		// noop if there are components activated by this container
		if (componentsCount > 0)
			return;

		// obtain keepAliveTime
		int keepAliveTime = RELEASE_NEVER;
		DAOProxy dao = getContainersDAOProxy();
		if (dao != null)
		{
			// defaults to RELEASE_NEVER
 			keepAliveTime = readLongCharacteristics(dao, containerInfo.getName()+"/DeployInfo/KeepAliveTime", keepAliveTime, true);
		}

		// always do shutdown in separate thread
		if (keepAliveTime >= 0)
			delayedDeactivationTask.schedule(new ShutdownContainerTask(containerInfo.getName()), keepAliveTime * 1000);
		// negative means immortal
	}

	/**
	 * Internal method for restarting components.
	 *
	 * @param	owner	owner of the component.
	 * @param	curl	CURL of the component to be restarted.
	 * @return			Newly restarted component, <code>null</code> if failed.
	 */
	private Component internalRestartComponent(int owner, URI curl)
	throws AcsJNoPermissionEx 
	{
		if (isDebug())
			new MessageLogEntry(this, "internalRestartComponent", new Object[] { new Integer(owner), curl }).dispatch();

		// resolve handle from curl
		int h = 0;

		String name = extractName(curl);

		synchronized (components)
		{
			h = components.first();
			while (h != 0)
			{
				ComponentInfo componentInfo = (ComponentInfo)components.get(h);
				if (componentInfo.getName().equals(name))
				{
					h = componentInfo.getHandle();
					break;
				}
				h = components.next(h);
			}
		}

		// if found, delegate operation, otherwise do nothing
		if (h != 0)
			return internalRestartComponent(owner, h);
		else
			return null;
	}

	/**
	 * Internal method for restarting components.
	 *
	 * @param	owner	owner of the component.
	 * @param	h		handle of the component to be restarted.
	 * @return			Newly restarted component, <code>null</code> if failed.
	 */
	private Component internalRestartComponent(int owner, int h)
	throws AcsJNoPermissionEx 
	{
		if (isDebug())
			new MessageLogEntry(this, "internalRestartComponent", new Object[] { new Integer(owner), new Integer(h) }).dispatch();

		// extract name
		String name = null;
		synchronized (components)
		{
			int handle = h & HANDLE_MASK;
			ComponentInfo componentInfo = null;
			if (components.isAllocated(handle))
				componentInfo = (ComponentInfo)components.get(handle);

			if (componentInfo == null || componentInfo.getHandle() != h)
			{
				// invalid Component handle
				BadParametersException af = new BadParametersException(this, "Invalid component handle.");
				af.caughtIn(this, "internalRestartComponent");
				af.putValue("h", new Integer(h));
				af.putValue("owner", new Integer(owner));
				throw af;
			}

			name = componentInfo.getName();

		}

		// try to acquire lock
		boolean lockAcquired = acquireSynchronizationObject(name, 3*Sync.ONE_MINUTE);
		if (lockAcquired)
		{
			boolean releaseRWLock = true;
			try
			{
				// try to acquire activation readers lock first
				// NOTE: the locks are NOT reentrant
				try	{
					activationPendingRWLock.readLock().acquire();
				} catch (InterruptedException ie) {
					releaseRWLock = false;

					NoResourcesException nre = new NoResourcesException(this, "Failed to obtain synchronization lock for component '"+name+"'.");
					nre.caughtIn(this, "internalRestartComponent");
					nre.putValue("owner", new Integer(owner));
					nre.putValue("h", new Integer(h));
					throw nre;
				}

				return internalNoSyncRestartComponent(owner, h);
			}
			finally
			{
				if (releaseRWLock)
					activationPendingRWLock.readLock().release();
				releaseSynchronizationObject(name);
			}
		}
		else
		{
			NoResourcesException nre = new NoResourcesException(this, "Failed to obtain synchronization lock for component '"+name+"', possible deadlock.");
			nre.caughtIn(this, "internalRestartComponent");
			nre.putValue("owner", new Integer(owner));
			nre.putValue("h", new Integer(h));
			throw nre;
		}

	}

	/**
	 * Internal method for restarting components.
	 *
	 * @param	owner	owner of the component.
	 * @param	h		handle of the component to be restarting.
	 * @return			Newly restarted component, <code>null</code> if failed.
	 */
	// @todo MF not supported
	private Component internalNoSyncRestartComponent(int owner, int h)
	throws AcsJNoPermissionEx 
	{

		if (isDebug())
			new MessageLogEntry(this, "internalNoSyncRestartComponent", new Object[] { new Integer(owner), new Integer(h) }).dispatch();

		int handle = h & HANDLE_MASK;

		ComponentInfo componentInfo = null;
		synchronized (components)
		{
			if (components.isAllocated(handle))
				componentInfo = (ComponentInfo)components.get(handle);

			if (componentInfo == null || componentInfo.getHandle() != h)
			{
				// invalid component handle
				BadParametersException af = new BadParametersException(this, "Invalid component handle.");
				af.caughtIn(this, "internalNoSyncRestartComponent");
				af.putValue("h", new Integer(h));
				af.putValue("owner", new Integer(owner));
				throw af;
			}

			// remove ownership of the component
			if (!componentInfo.getClients().contains(owner))
			{
				// not an owner
				AcsJNoPermissionEx npe = new AcsJNoPermissionEx();
				npe.setReason("Restarting component that client does not own.");
				throw npe;
			}

		}

		/****************** restart component ******************/

		//
		// get container
		//

		// search for container by its name
		Container container = null;
		ContainerInfo containerInfo = null;
		int containerHandle = componentInfo.getContainer();
		// if containerHandle equals 0, we have unavailable or registered component
		if (containerHandle != 0)
		{
			containerInfo = getContainerInfo(containerHandle);
			if (containerInfo != null) {
				checkContainerShutdownState(containerInfo);
				container = containerInfo.getContainer();
			}

			// required container is not logged in
			if (container == null)
			{
				// then simply do not do the restart
				String containerName;
				if (containerInfo != null)
					containerName = containerInfo.getName();
				else
					containerName = HandleHelper.toString(componentInfo.getContainer());
				new MessageLogEntry(this, "internalNoSyncRestartComponent", "Container '"+containerName+"' required by component '"+componentInfo.getName()+"' is not logged in.", LoggingLevel.WARNING).dispatch();
			}

		}

		// return value
		Component component = null;

		if (container != null)
		{
			// @todo what about notifying clients, marking component as unavailable...

			// restart component
			try
			{
				component = container.restart_component(componentInfo.getHandle());
				if (component == null)
				{
					RemoteException re = new RemoteException(this, "Failed to restart component '"+componentInfo.getName()+"', 'null' returned.");
					re.caughtIn(this, "internalNoSyncRestartComponent");
					re.putValue("h", new Integer(h));
					re.putValue("owner", new Integer(owner));
					throw re;
				}

				// @todo what about notifying clients, marking component as available, updating reference...

			}
			catch (Exception ex)
			{
				RemoteException re = new RemoteException(this, "Failed to restart component '"+componentInfo.getName()+"' on container '"+containerInfo.getName()+"'.", ex);
				re.caughtIn(this, "internalNoSyncRestartComponent");
				re.putValue("h", new Integer(h));
				re.putValue("owner", new Integer(owner));
				// exception service will handle this
				new MessageLogEntry(this, "internalNoSyncRestartComponent", re.getMessage(), LoggingLevel.ERROR).dispatch();
			}
		}

		if (isDebug())
			new MessageLogEntry(this, "internalNoSyncRestartComponent", "Component '"+componentInfo.getName()+"' restarted.", LoggingLevel.DEBUG).dispatch();

		if (isDebug())
			new MessageLogEntry(this, "internalNoSyncRestartComponent", "Exiting.", Level.FINEST).dispatch();

		return component;
	}

	/*****************************************************************************/
	/*********************** [ Dynamic (de)activation ] **************************/
	/*****************************************************************************/

	/**
	 * @see com.cosylab.acs.maci.Manager#getDefaultComponent(int, java.lang.String)
	 */
	// @todo MF not supported
	public ComponentInfo getDefaultComponent(int id, String type)
		throws AcsJNoPermissionEx, NoDefaultComponentException
	{
		if (isDebug())
			new MessageLogEntry(this, "getDefaultComponent", new Object[] { new Integer(id), type }).dispatch();

		if (type == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException(this, "Non-null 'type' expected.");
			af.caughtIn(this, "getDefaultComponent");
			af.putValue("type", type);
			throw af;
		}

		/****************************************************************/

		// check handle and NONE permissions
		securityCheck(id, AccessRights.NONE);

		new MessageLogEntry(this, "getDefaultComponent", "Getting default component for type '" + type + "'.", LoggingLevel.INFO).dispatch();

		ComponentInfo componentInfo = internalRequestDefaultComponent(id, type);

		/****************************************************************/

		if (isDebug())
			new MessageLogEntry(this, "getDefaultComponent", "Exiting.", Level.FINEST).dispatch();

		return componentInfo;

	}

	/**
	 * Internal method for requesting default components.
	 * @param	requestor	requestor of the component.
	 * @param	typr		type of the component
	 * @return	componentInfo	<code>ComponentInfo</code> of requested default component.
	 */
	private ComponentInfo internalRequestDefaultComponent(int requestor, String type) throws NoDefaultComponentException
	{
		if (isDebug())
			new MessageLogEntry(this, "internalRequestDefaultComponent", new Object[] { new Integer(requestor), type }).dispatch();

		String defaultComponentName = null;
		ComponentInfo defaultComponentInfo = null;

		// first check default components table
		synchronized (defaultComponents)
		{
			defaultComponentInfo = (ComponentInfo)defaultComponents.get(type);
		}

		if (defaultComponentInfo != null)
			defaultComponentName = defaultComponentInfo.getName();

		// if not found, search for the default component in the CDB
		if (defaultComponentName == null)
		{
			DAOProxy componentsDAO = getComponentsDAOProxy();
			if (componentsDAO != null)
			{
				try
				{
					// get names of all components
					/*String[] ids =*/ componentsDAO.get_field_data("");  // @todo here to check if CDB is available
				    String[] ids = getComponentsList();

					// test names
					for (int i = 0; i < ids.length; i++)
					{
						// read name
						String name = ids[i]; //readStringCharacteristics(componentsDAO, ids[i]+"/Name");
						if (name == null)
						{
							new MessageLogEntry(this, "internalRequestDefaultComponent", "Misconfigured CDB, there is no type of component '"+ids[i]+"' defined.", LoggingLevel.WARNING).dispatch();
							continue;
						}

						// do not search dynamic components (they cannot be marked as default in CDB anyway)
						if (!name.equals(ComponentSpec.COMPSPEC_ANY))
						{

							// read type
							String componentType = readStringCharacteristics(componentsDAO, ids[i]+"/Type");
							if (type == null)
							{
								new MessageLogEntry(this, "internalRequestDefaultComponent", "Misconfigured CDB, there is no type of component '"+name+"' defined.", LoggingLevel.WARNING).dispatch();
								continue;
							}

							// test type
							final String TRUE_STRING = "true";
							if (type.equals(componentType))
							{
								// check if it is default, read silently
								String isDefault = readStringCharacteristics(componentsDAO, ids[i]+"/Default", true);
								if (isDefault == null || !isDefault.equalsIgnoreCase(TRUE_STRING))
									continue;

								// got the match
								defaultComponentName = name;
								break;
							}

						}
					}

				}
				catch (Exception ex)
				{
					CoreException ce = new CoreException(this, "Failed to obtain component data from the CDB.", ex);
					ce.caughtIn(this, "internalRequestDefaultComponent");
					// exception service will handle this
					// new MessageLogEntry(this, "internalRequestDefaultComponent", ce.getMessage(), ex, LoggingLevel.WARNING).dispatch();
				}
			}
		}


		// @todo MF if non-local do not make this check!!! type should be given for inter-domain...!!! do not bail in..
		// if found get the component
		if (defaultComponentInfo != null)
		{
			try
			{
				StatusHolder status = new StatusHolder();

				ContainerInfo containerInfo = getContainerInfo(defaultComponentInfo.getContainer());
				if (containerInfo == null)
				{
					AssertionFailed huse = new AssertionFailed(this, "Failed to return default component: '" + defaultComponentName + "', container with '" +
																		HandleHelper.toString(defaultComponentInfo.getContainer()) + "' not logged in.");
					huse.caughtIn(this, "internalRequestDefaultComponent");
					huse.putValue("defaultComponentInfo", defaultComponentInfo.toString());
					return null;
				}

				ComponentInfo componentInfo = internalRequestComponent(requestor,
																	   defaultComponentInfo.getName(), defaultComponentInfo.getType(),
																	   defaultComponentInfo.getCode(),	containerInfo.getName(), RELEASE_IMMEDIATELY, status, true);
				if (componentInfo == null || status.getStatus() != ComponentStatus.COMPONENT_ACTIVATED)
				{
					AssertionFailed huse = new AssertionFailed(this, "Failed to obtain default component: '" + defaultComponentName + "'.");
					huse.caughtIn(this, "internalRequestDefaultComponent");
					huse.putValue("defaultComponentInfo", defaultComponentInfo.toString());
					// NOTE: do not log references - prevents GC to finalize and terminate connection thread (JacORB)
					huse.putValue("componentInfo", componentInfo == null ? "null" : componentInfo.toString());
					huse.putValue("status", status.getStatus());
					// no error handling...
					return null;
				}

				return componentInfo;

			}
			catch (Throwable t)
			{
				AssertionFailed huse = new AssertionFailed(this, "Failed to return default component: '" + defaultComponentName + "'.", t);
				huse.caughtIn(this, "internalRequestDefaultComponent");
				// NOTE: do not log references - prevents GC to finalize and terminate connection thread (JacORB)
				huse.putValue("defaultComponentInfo", defaultComponentInfo == null ? "null" : defaultComponentInfo.toString());
				return null;
			}

		}
		else if (defaultComponentName != null)
		{
			// create CURL
			URI curl = null;
			try
			{
				curl = CURLHelper.createURI(defaultComponentName);
			}
			catch (URISyntaxException use)
			{
				AssertionFailed huse = new AssertionFailed(this, "Failed to create CURL from default component name: '" + defaultComponentName + "'.", use);
				huse.caughtIn(this, "internalRequestDefaultComponent");
				huse.putValue("defaultComponentName", defaultComponentName);
				// no error handling...
				return null;
			}

			try
			{
				// request for component
				StatusHolder status = new StatusHolder();

				Component component = internalRequestComponent(requestor, curl, status, true);
				if (component == null || status.getStatus() != ComponentStatus.COMPONENT_ACTIVATED)
				{
					AssertionFailed huse = new AssertionFailed(this, "Failed to obtain default component: '" + defaultComponentName + "'.");
					huse.caughtIn(this, "internalRequestDefaultComponent");
					huse.putValue("defaultComponentName", defaultComponentName);
					// NOTE: do not log references - prevents GC to finalize and terminate connection thread (JacORB)
					huse.putValue("component", component == null ? "null" : component.toString());
					huse.putValue("status", status.getStatus());
					// no error handling...
					return null;
				}

				// return component info
				ComponentInfo[] componentInfo = getComponentInfo(requestor, new int[0], defaultComponentName, type, true);
				if (componentInfo == null || componentInfo.length != 1)
				{
					AssertionFailed huse = new AssertionFailed(this, "Failed to obtain activated default component ComponentInfo: '" + defaultComponentName + "'.");
					huse.caughtIn(this, "internalRequestDefaultComponent");
					// NOTE: do not log references - prevents GC to finalize and terminate connection thread (JacORB)
					huse.putValue("componentInfo", componentInfo == null ? "null" : componentInfo.toString());
					// no error handling...
					return null;
				}
				else
					return componentInfo[0];

			}
			catch (Throwable t)
			{
				AssertionFailed huse = new AssertionFailed(this, "Failed to return default component: '" + defaultComponentName + "'.", t);
				huse.caughtIn(this, "internalRequestDefaultComponent");
				huse.putValue("defaultComponentName", defaultComponentName);
				return null;
			}
		}

		// not found
		NoDefaultComponentException ndce = new NoDefaultComponentException(this, "No default component for type '" + type + "' found.");
		ndce.caughtIn(this, "internalRequestDefaultComponent");
		ndce.putValue("type", type);
		throw ndce;
	}


	/**
	 * @see com.cosylab.acs.maci.Manager#getDynamicComponent(int, com.cosylab.acs.maci.ComponentSpec, boolean)
	 */
	// TODO MF not supported
	public ComponentInfo getDynamicComponent(int id, ComponentSpec componentSpec, boolean markAsDefault)
		throws AcsJNoPermissionEx, IncompleteComponentSpecException,
			   InvalidComponentSpecException, ComponentSpecIncompatibleWithActiveComponentException
	{
		if (isDebug())
			new MessageLogEntry(this, "getDynamicComponent", new Object[] { new Integer(id), componentSpec, new Boolean(markAsDefault) }).dispatch();

		// check if null
		if (componentSpec == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException(this, "Non-null 'componentSpec' expected.");
			af.caughtIn(this, "getDynamicComponent");
			af.putValue("componentSpec", componentSpec);
			throw af;
		}

		// check componentSpec components are null
		if (componentSpec.getName() == null || componentSpec.getType() == null ||
			componentSpec.getCode() == null || componentSpec.getContainer() == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException(this, "Non-null 'componentSpec' fields expected.");
			af.caughtIn(this, "getDynamicComponent");
			af.putValue("componentSpec", componentSpec);
			throw af;
		}

		// check for empty componentSpec.name
		if (componentSpec.getName().length() == 0)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException(this, "Non-empty 'componentSpec.name' field expected.");
			af.caughtIn(this, "getDynamicComponent");
			af.putValue("componentSpec", componentSpec);
			throw af;
		}

		// check handle and NONE permissions
		securityCheck(id, AccessRights.NONE);

		/****************************************************************/

		ComponentInfo componentInfo = internalRequestDynamicComponent(id, componentSpec);

		// update default components table
		if (componentInfo != null && markAsDefault)
		{
			synchronized (defaultComponents)
			{
				// !!! ACID 3
				executeCommand(new DefaultComponentCommandPut(componentInfo.getType(), componentInfo));
				//defaultComponents.put(componentInfo.getType(), componentInfo.getName());
			}

			new MessageLogEntry(this, "getDynamicComponent", "'" + componentInfo.getName() +"' has been marked as a default component of type '" +
																   componentInfo.getType() +"'.", LoggingLevel.INFO).dispatch();
		}

		/****************************************************************/

		if (isDebug())
			new MessageLogEntry(this, "getDynamicComponent", "Exiting.", Level.FINEST).dispatch();

		return componentInfo;
	}

	/**
	 * @see com.cosylab.acs.maci.Manager#getDynamicComponents(int, com.cosylab.acs.maci.ComponentSpec[])
	 */
	public ComponentInfo[] getDynamicComponents(int id,	ComponentSpec[] components)
		throws AcsJNoPermissionEx
	{
		if (isDebug())
			new MessageLogEntry(this, "getDynamicComponents", new Object[] { new Integer(id), components }).dispatch();

		// check if null
		if (components == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException(this, "Non-null 'components' expected.");
			af.caughtIn(this, "getDynamicComponents");
			throw af;
		}

		// check handle and NONE permissions
		securityCheck(id, AccessRights.NONE);

		/****************************************************************/

		int obtained = 0;

		ComponentInfo[] componentInfos = new ComponentInfo[components.length];
		for (int i = 0; i < components.length; i++)
		{
			try
			{
				componentInfos[i] = getDynamicComponent(id, components[i], false);
				obtained++;
			}
			catch (Exception ex)
			{
				componentInfos[i] = null;

				CoreException ce = new CoreException(this, "Failed to get dynamic component '"+components[i]+"'.", ex);
				ce.caughtIn(this, "getDynamicComponents");
				ce.putValue("components[i]", components[i]);
				// exception service will handle this
			}
		}

		new MessageLogEntry(this, "getDynamicComponents", obtained + " of " + components.length +" dynamic components obtained.", LoggingLevel.INFO).dispatch();

		/****************************************************************/

		if (isDebug())
			new MessageLogEntry(this, "getDynamicComponents", "Exiting.", Level.FINEST).dispatch();

		return componentInfos;

	}

	/**
	 * @see com.cosylab.acs.maci.Manager#getCollocatedComponent(int, com.cosylab.acs.maci.ComponentSpec, boolean, URI)
	 */
	/// @todo MF not supported
	public ComponentInfo getCollocatedComponent(int id, ComponentSpec componentSpec,
			boolean markAsDefault, URI targetComponentURI)
		throws AcsJNoPermissionEx, IncompleteComponentSpecException,
			   InvalidComponentSpecException, ComponentSpecIncompatibleWithActiveComponentException
	{
		if (isDebug())
			new MessageLogEntry(this, "getCollocatedComponent", new Object[] { new Integer(id), componentSpec, new Boolean(markAsDefault), targetComponentURI }).dispatch();

		// check if null
		if (componentSpec == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException(this, "Non-null 'componentSpec' expected.");
			af.caughtIn(this, "getCollocatedComponent");
			af.putValue("componentSpec", componentSpec);
			throw af;
		}

		// check componentSpec components are null
		if (componentSpec.getName() == null || componentSpec.getType() == null ||
			componentSpec.getCode() == null || componentSpec.getContainer() == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException(this, "Non-null 'componentSpec' fields expected.");
			af.caughtIn(this, "getCollocatedComponent");
			af.putValue("componentSpec", componentSpec);
			throw af;
		}

		// check for empty componentSpec.name
		if (componentSpec.getName().length() == 0)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException(this, "Non-empty 'componentSpec.name' field expected.");
			af.caughtIn(this, "getCollocatedComponent");
			af.putValue("componentSpec", componentSpec);
			throw af;
		}

		// check if null
		if (targetComponentURI == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException(this, "Non-null 'targetComponentURI' expected.");
			af.caughtIn(this, "getCollocatedComponent");
			af.putValue("targetComponentURI", targetComponentURI);
			throw af;
		}

		if (!componentSpec.getContainer().equals(ComponentSpec.COMPSPEC_ANY))
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException(this, "'" + ComponentSpec.COMPSPEC_ANY + "' value for 'componentSpec.container' field expected.");
			af.caughtIn(this, "getCollocatedComponent");
			af.putValue("componentSpec", componentSpec);
			throw af;
		}

		// check handle and NONE permissions
		securityCheck(id, AccessRights.NONE);

		/****************************************************************/

		/// @todo temporary quick implementation (does not look in the CDB if component is not activated)
		String name = extractName(targetComponentURI);

		int h = 0;
		ComponentInfo targetComponentInfo = null;
		synchronized (components)
		{
			h = components.first();
			while (h != 0)
		    {
				ComponentInfo componentInfo = (ComponentInfo)components.get(h);
				if (componentInfo.getName().equals(name))
				{
					targetComponentInfo = componentInfo;
					break;
				}
				h = components.next(h);
		    }
		}

		// if not found, check the CDB
		if (targetComponentInfo == null)
		{
			DAOProxy componentsDAO = getComponentsDAOProxy();
			if (componentsDAO != null)
			{
				// read container name
				String containerName = readStringCharacteristics(componentsDAO, name+"/Container", true);
				if (containerName != null)
					componentSpec.setContainer(containerName);
			}
		}
		else
			componentSpec.setContainer(targetComponentInfo.getContainerName());

		// failed to detemine a target container
		if (componentSpec.getContainer().equals(ComponentSpec.COMPSPEC_ANY))
		{
			IncompleteComponentSpecException icse = new IncompleteComponentSpecException(this,
					"Failed to detemine a target container (determined by component '" + name + "').", componentSpec);
			icse.caughtIn(this, "internalRequestDynamicComponent");
			icse.putValue("getCollocatedComponent", componentSpec);
			throw icse;
		}

		// request for component
		ComponentInfo componentInfo = internalRequestDynamicComponent(id, componentSpec);

		// update default components table
		if (componentInfo != null && markAsDefault)
		{
			synchronized (defaultComponents)
			{
				// !!! ACID 3
				executeCommand(new DefaultComponentCommandPut(componentInfo.getType(), componentInfo));
				//defaultComponents.put(componentInfo.getType(), componentInfo.getName());
			}

			new MessageLogEntry(this, "getCollocatedComponent", "'" + componentInfo.getName() +"' has been marked as a default component of type '" +
																      componentInfo.getType() +"'.", LoggingLevel.INFO).dispatch();
		}

		/****************************************************************/

		if (isDebug())
			new MessageLogEntry(this, "getCollocatedComponent", "Exiting.", Level.FINEST).dispatch();

		return componentInfo;
	}

	/**
	 * Searches for the best match in Components entry in the CDB.
	 *
	 * @param fieldNames		array of fields names to be searched.
	 * @param requiredValues	required values of fields, if <code>ComponentSpec.COMPSPEC_ANY</code> any value is accepted.
	 * @param equalityRequired	<code>true</code> if <code>requiredValues[i]</code> and <code>fieldNames[i]<code> value values must match,
	 * 							i.e. String.equals(String) is used.
	 * @param equalityPoints	array of points to be given to each field whose value is equal to the CDB value,
	 * 							used to determine best match.
	 * @return					best match found in the CDB, <code>null</code> on failure or if not found.
	 */
	private String[] searchDynamicComponent(String[] fieldNames, String[] requiredValues, boolean[] equalityRequired, int[] equalityPoints)
	{
		assert(fieldNames != null);
		assert(equalityRequired != null);
		assert(equalityPoints != null);
		assert(fieldNames.length != equalityRequired.length);
		assert(equalityRequired.length != equalityPoints.length);

		DAOProxy componentsDAO = getComponentsDAOProxy();
		if (componentsDAO == null)
			return null;

		// get field IDs for all components
		String[] fieldIDs = null;
		try
		{
			/*fieldIDs =*/ componentsDAO.get_field_data("");  /// @todo here to check if CDB is available
			fieldIDs = getComponentsList();
		}
		catch (Exception ex)
		{
			AssertionFailed af = new AssertionFailed(this, "Failed to retrieve data from CDB.", ex);
			af.caughtIn(this, "searchDynamicComponent");
			return null;
		}

		int len = fieldNames.length;
		int maxPoints = Integer.MIN_VALUE;
		String[] bestMatch = null;
		String[] currentMatch = new String[len];

		// for each entry
		for (int fi = 0; fi < fieldIDs.length; fi++)
		{
			// for each field
			int i = 0; int points = 0;
			for (; i < len; i++)
			{
				/// @todo not nice way, but necessary to have hierarchical names
				//String fieldValue = readStringCharacteristics(componentsDAO, fieldIDs[fi]+"/"+fieldNames[i], true);
			    boolean processingNameField = "Name".equals(fieldNames[i]);
				String fieldValue = null;
				if (processingNameField)
				{
				    // problems are multiple "*" (or other duplicated names) -> "*<number>", so reading CDB is necessary
				    if (Character.isDigit(fieldIDs[fi].charAt(fieldIDs[fi].length()-1)))
				    {
						String readFieldValue = readStringCharacteristics(componentsDAO, fieldIDs[fi]+"/Name", true);
						fieldValue = fieldIDs[fi].substring(0, fieldIDs[fi].indexOf(readFieldValue)+readFieldValue.length());
				    }
				    else
				        fieldValue = fieldIDs[fi];
				}
				else
					fieldValue = readStringCharacteristics(componentsDAO, fieldIDs[fi]+"/"+fieldNames[i], true);
				if (fieldValue == null)
					break;

				boolean equals = requiredValues[i].equals(fieldValue);
				// required equality
				if (equalityRequired[i])
				{
					// reject entry
					if (!equals)
						break;

					currentMatch[i] = fieldValue;
				}
				// optional equality brings points
				else
				{
					/* override
					// first check required value condition
					if (!equals &&
						!(requiredValues[i].equals(ComponentSpec.COMPSPEC_ANY) ||
						  fieldValue.equals(ComponentSpec.COMPSPEC_ANY)))
						break;
					*/

					currentMatch[i] = fieldValue;

					if (equals)
						points += equalityPoints[i];
					// special case for name; disallow entries where names do not match
					// note that name cannot be overriden
					else if (processingNameField &&
					         (fieldValue.indexOf(ComponentSpec.COMPSPEC_ANY) == -1 ||
					         !WildcharMatcher.match(fieldValue, requiredValues[i])))
						break;

				}
			}

			// if not rejected and best
			if (i == len && points > maxPoints)
			{
				maxPoints = points;
				if (bestMatch == null)
					bestMatch = new String[len];
				System.arraycopy(currentMatch, 0, bestMatch, 0, len);
			}
		}

		return bestMatch;
	}


	/**
	 * Internal method for requesting dynamic components.
	 *
	 * Resolution:
	 * <code>component_name</code> and <code>component_type</code> can be considered as "determinator" fields,
	 * they play important role in search algorithm.
	 * <code>component_code</code> and <code>container_name</code> can be considered as "override" fields,
	 * they only help to find closest match.
	 * Rule: unspecified <code>component_name</code> case implies that a new component will be activated.
	 * Search points (8,4,2,1): <code>component_name</code>, <code>component_type</code>, <code>component_code</code>, <code>container_name</code>.
	 * <pre>
	 *
	 *  name | type | search criteria
	 * -----------------------------
	 *    *  |   *  | throw IncompleteComponentSpecException
	 *    X  |   *  | (equals, wildcard)
	 *    *  |   X  | (equals, equals) w/ name generation
	 *    X  |   X  | (wildcard, equals) - overriding type is not allowed
	 *
	 * </pre>
	 * 'name' can be also something like "ANT1/*" (ends with) and is threated just like "*".
	 *
	 * @param	requestor	requestor of the component.
	 * @param	componentSpec	requested component <code>ComponentSpec</code>
	 * @return	componentInfo	<code>ComponentInfo</code> of requested dynamic component.
	 */
	private ComponentInfo internalRequestDynamicComponent(int requestor, ComponentSpec componentSpec)
		throws AcsJNoPermissionEx, IncompleteComponentSpecException, InvalidComponentSpecException,
			   ComponentSpecIncompatibleWithActiveComponentException
	{
		if (isDebug())
			new MessageLogEntry(this, "internalRequestDynamicComponent", new Object[] { new Integer(requestor), componentSpec }).dispatch();

		boolean unspecifiedName = componentSpec.getName().endsWith(ComponentSpec.COMPSPEC_ANY);
		boolean unspecifiedType = componentSpec.getType().equals(ComponentSpec.COMPSPEC_ANY);

		// check completeness of componentSpec
		//   *  |   *  | throw IncompleteComponentSpecException
		if (unspecifiedName && unspecifiedType)
		{
			IncompleteComponentSpecException icse = new IncompleteComponentSpecException(this,
								"'name' and 'type' cannot be both '" + ComponentSpec.COMPSPEC_ANY +"'.",
								componentSpec);
			icse.caughtIn(this, "internalRequestDynamicComponent");
			icse.putValue("componentSpec", componentSpec);
			throw icse;
		}
		// all fields are fully specified, no search needed
		else if (!unspecifiedName && !unspecifiedType &&
			!componentSpec.getCode().equals(ComponentSpec.COMPSPEC_ANY) &&
			!componentSpec.getContainer().equals(ComponentSpec.COMPSPEC_ANY))
		{
			StatusHolder statusHolder = new StatusHolder();
			return internalRequestComponent(requestor, componentSpec.getName(),
							 			    componentSpec.getType(), componentSpec.getCode(),
											componentSpec.getContainer(), RELEASE_IMMEDIATELY, statusHolder, true);
		}


		//
		// prepare search conditions
		//

		final String[] fieldNames = new String[] { "Name", "Type", "Code", "Container" };
		final String[] requiredValues = new String[] { componentSpec.getName(), componentSpec.getType(),
													   componentSpec.getCode(), componentSpec.getContainer() };
		final int[] equalityPoints = new int[] { 8, 4 /* never used */, 2, 1 };

		boolean[] equalityRequired = null;
		boolean allowNameGeneration = false;
		boolean prohibitSearch = false;

		//   X  |   X  | (wildcard, equals)
		if (!unspecifiedName &&	!unspecifiedType)
		{
			equalityRequired = new boolean[] { false, true, false, false };
			allowNameGeneration = true;
		}

		//   X  |   *  | (equals, wildcard)
		else if (!unspecifiedName && unspecifiedType)
		{
			equalityRequired = new boolean[] { true, false, false, false };
		}

		//   *  |   X  | (equals, equals) w/ name generation
		//  prefix* |   X  | (equals, equals) w/ name generation
		else if (unspecifiedName && !unspecifiedType)
		{
			equalityRequired = new boolean[] { true, true, false, false };

			// no search needed case...
			if (!componentSpec.getCode().equals(ComponentSpec.COMPSPEC_ANY) &&
				!componentSpec.getContainer().equals(ComponentSpec.COMPSPEC_ANY))
				prohibitSearch = true;

			allowNameGeneration = true;
		}

		// search
		String[] result = prohibitSearch ? null : searchDynamicComponent(fieldNames, requiredValues, equalityRequired, equalityPoints);

		// none found
		if (result == null)
		{
			boolean failed = true;

			// only name or container not speficied...
			if ((allowNameGeneration || !unspecifiedName) &&
			    !unspecifiedType &&
				!componentSpec.getCode().equals(ComponentSpec.COMPSPEC_ANY))
			{
				// container name already specified, i.e. name is *, which is OK
				if (!componentSpec.getContainer().equals(ComponentSpec.COMPSPEC_ANY))
				{
				    result = new String[] { componentSpec.getName(), componentSpec.getType(),
							    componentSpec.getCode(), componentSpec.getContainer() };

					failed = false;
				}
				// container name is *, use load balancing if available
				else if (loadBalancingStrategy != null)
				{
					String containerName = loadBalancingStrategy.selectContainer(getClientInfo(requestor), getContainersInfo());
					if (containerName != null)
					{
						result = new String[] { componentSpec.getName(), componentSpec.getType(),
									componentSpec.getCode(), containerName };
						failed = false;
					}
				}
			}

			if (failed)
			{
				InvalidComponentSpecException icse = new InvalidComponentSpecException(this,
									"Requested ComponentSpec does not match any entry in the CDB.",
									componentSpec);
				icse.caughtIn(this, "internalRequestDynamicComponent");
				icse.putValue("componentSpec", componentSpec);
				throw icse;
			}
		}

		// override...
		for (int i = 0; i < result.length; i++)
			if (!requiredValues[i].equals(ComponentSpec.COMPSPEC_ANY))
				result[i] = requiredValues[i];

		// check completeness
		int i = 0;
		if (allowNameGeneration) i++;
		for (; i < result.length; i++)
			if (result[i].equals(ComponentSpec.COMPSPEC_ANY))
			{
				// only container not speficied...
				// if load balancing strategy is registered, use it to determine container name
				if (fieldNames[i].equals("Container") &&
					loadBalancingStrategy != null)
				{
					String containerName = loadBalancingStrategy.selectContainer(getClientInfo(requestor), getContainersInfo());
					if (containerName != null)
					{
						result[i] = containerName;
						continue;
					}
				}

				IncompleteComponentSpecException icse = new IncompleteComponentSpecException(this,
									"'" + fieldNames[i] + "' equals '" + ComponentSpec.COMPSPEC_ANY +"'.",
									new ComponentSpec(result[0], result[1], result[2], result[3]));
				icse.caughtIn(this, "internalRequestDynamicComponent");
				icse.putValue("result", result);
				icse.putValue(fieldNames[i], result[i]);
				throw icse;
			}

		// generate name if necessary
		if (allowNameGeneration && result[0].endsWith(ComponentSpec.COMPSPEC_ANY))
		{
			synchronized (this)
			{
				/// @todo not perfect
			    if (result[0].equals(ComponentSpec.COMPSPEC_ANY))
			        result[0] = result[1] + "_" + System.currentTimeMillis();
			    else // ends with case
			        result[0] = result[0].substring(0, result[0].length()-1) + "_" + System.currentTimeMillis();

				// flatten hierarchical name (remove IDL separators)
				if (result[0].indexOf('/') >= 0)
					result[0] = result[0].replaceAll("/", "_");
			}
		}

		StatusHolder statusHolder = new StatusHolder();
		return internalRequestComponent(requestor, result[0], result[1],
									    result[2], result[3], RELEASE_IMMEDIATELY, statusHolder, true);
	}

	/*****************************************************************************/
	/************************** [ Remote Directory ] *****************************/
	/*****************************************************************************/

	/**
	 * Bind object to root of remote directory.
	 *
	 * Use INS syntax specified in the INS specification.
	 * In short, the syntax is that components are left-to-right slash ('/') separated and case-sensitive.
	 * The id and kind of each component are separated by the period character ('.').
	 *
	 * @param	name	name of the object, code non-<code>null</code>
	 * @param	object	object to be binded
	 */
	private void bind(String name, String type, Object object)
	{
	    bind(remoteDirectory, name, type, object);
	}

	/**
	 * Bind object to remote directory.
	 *
	 * Use INS syntax specified in the INS specification.
	 * In short, the syntax is that components are left-to-right slash ('/') separated and case-sensitive.
	 * The id and kind of each component are separated by the period character ('.').
	 *
	 * @param	remoteDirectory	remote directory context to be used.
	 * @param	name	name of the object, code non-<code>null</code>
	 * @param	object	object to be binded
	 */
	private void bind(Context remoteDirectory, String name, String type, Object object)
	{
		assert (name != null);

		// do not bind interdomain names
		if (name.startsWith(CURL_URI_SCHEMA))
		    return;

		// NOTE: do not log references - prevents GC to finalize and terminate connection thread (JacORB)
		if (isDebug())
			new MessageLogEntry(this, "bind", new Object[] { name, object == null ? "null" : object.toString() }).dispatch();

		if (remoteDirectory != null)
		{
		    try
			{
			    // hierarchical name check
			    int pos = name.indexOf('/');
			    if (pos != -1)
			    {
			        if (pos == 0 || pos == name.length())
			            throw new IllegalArgumentException("Invalid hierarchical name '" + name + "'.");

			        String parent = name.substring(0, pos);
			        String child = name.substring(pos + 1);

			        // lookup for subcontext, if not found create one
			        Context parentContext = null;
			        try
			        {
			            parentContext = (Context)remoteDirectory.lookup(parent);
			        }
			        catch (NameNotFoundException nnfe)
			        {
			            // handle this exception
			            new ExceptionIgnorer(nnfe);
			        }

			        if (parentContext == null)
			        {
			            parentContext = remoteDirectory.createSubcontext(parent);
					    /// @todo temp. commented out
			            //generateHiearchyContexts(remoteDirectory, parent, parentContext);
			        }

			        // delegate
			        bind(parentContext, child, type, object);
			        return;
			    }


			    NameParser parser = remoteDirectory.getNameParser("");

				Name n;
				if (type != null)
					n = parser.parse(name+"."+type);
				else
				 	n = parser.parse(name);

				// special case
				if (name.endsWith(".D"))
				{
				    remoteDirectory.rebind(n, object);
				    /// @todo temp. commented out
				    //generateHiearchyContexts(remoteDirectory, name, (Context)remoteDirectory.lookup(name));
				}
				else
				    remoteDirectory.bind(n, object);

			}
			catch (NameAlreadyBoundException nabe)
			{
				rebind(remoteDirectory, name, type, object);
			}
			catch (NamingException ne)
			{
				CoreException ce = new CoreException(this, "Failed to bind name '"+name+"' to the remote directory.", ne);
				ce.caughtIn(this, "bind");
				ce.putValue("name", name);
				ce.putValue("type", type);
				// exception handler service will take care of this exception
				new MessageLogEntry(this, "bind", ce.getMessage(), ce, LoggingLevel.DEBUG).dispatch();
			}
		}

		if (isDebug())
			new MessageLogEntry(this, "bind", "Exiting.", Level.FINEST).dispatch();
	}

	/**
     * @param remoteDirectory	parent context of parentContext.
     * @param parent			name of context to bind.
     * @param parentContext		context to bind.
     * @throws NamingException
     *
    private void generateHiearchyContexts(Context remoteDirectory, String parent, Context parentContext) throws NamingException
    {
        /// @todo CORBA specific
        if (remoteDirectory instanceof com.sun.jndi.cosnaming.CNCtx)
        {
            boolean isParentDomain = remoteDirectory.getNameInNamespace().length() == 0 ||
            						 remoteDirectory.getNameInNamespace().endsWith(".D");

            org.omg.CORBA.Object parentRepresentation = ((com.sun.jndi.cosnaming.CNCtx)remoteDirectory)._nc;
            if (parent.endsWith(".D"))
            {
                // domain binding
                parentContext.rebind("Parent.D", parentRepresentation);
            }
            else if (parent.endsWith(".F"))
            {
                // hierarchical component binding
                if (isParentDomain)
                    parentContext.rebind("Domain.D", parentRepresentation);
                else
                    parentContext.rebind("Domain.D", remoteDirectory.lookup("Domain.D"));
                parentContext.rebind("Parent.F", parentRepresentation);
            }
        }
        else if (remoteDirectory instanceof InitialContext)
            generateHiearchyContexts((Context)((InitialContext)remoteDirectory).lookup(""), parent, parentContext);
        else
            new MessageLogEntry(this, "generateHiearchyContexts", "Unsupported remote directory, class '" + remoteDirectory.getClass().getName() + "'.", Level.WARNING).dispatch();
    }*/

	/**
	 * Rebind object to the root of remote directory.
	 *
	 * Use INS syntax specified in the INS specification.
	 * In short, the syntax is that components are left-to-right slash ('/') separated and case-sensitive.
	 * The id and kind of each component are separated by the period character ('.').
	 * NOTE: Does not support hierarchical names.
	 *
	 * @param	name	name of the object, code non-<code>null</code>
	 * @param	type	type of the object
	 * @param	object	object to be binded
	 *
	private void rebind(String name, String type, Object object)
	{
	    rebind(remoteDirectory, name, type, object);
	}*/

    /**
	 * Rebind object to remote directory.
	 *
	 * Use INS syntax specified in the INS specification.
	 * In short, the syntax is that components are left-to-right slash ('/') separated and case-sensitive.
	 * The id and kind of each component are separated by the period character ('.').
	 * NOTE: Does not support hierarchical names.
	 *
	 * @param	remoteDirectory	remote directory context to be used.
	 * @param	name	name of the object, code non-<code>null</code>
	 * @param	type	type of the object
	 * @param	object	object to be binded
	 */
	private void rebind(Context remoteDirectory, String name, String type, Object object)
	{
		assert (name != null);

		// do not bind interdomain names
		if (name.startsWith(CURL_URI_SCHEMA))
		    return;

		// NOTE: do not log references - prevents GC to finalize and terminate connection thread (JacORB)
		if (isDebug())
			new MessageLogEntry(this, "rebind", new Object[] { name, object == null ? "null" : object.toString() }).dispatch();

		if (remoteDirectory != null)
		{
			try
			{
				NameParser parser = remoteDirectory.getNameParser("");

				Name n;
				if (type != null)
					n = parser.parse(name+"."+type);
				else
				 	n = parser.parse(name);

				remoteDirectory.rebind(n, object);
			}
			catch (NamingException ne)
			{
				CoreException ce = new CoreException(this, "Failed to rebind name '"+name+"' to the remote directory.", ne);
				ce.caughtIn(this, "rebind");
				ce.putValue("name", name);
				ce.putValue("type", type);
				// exception handler service will take care of this exception
				new MessageLogEntry(this, "rebind", ce.getMessage(), ce, LoggingLevel.DEBUG).dispatch();
			}
		}

		if (isDebug())
			new MessageLogEntry(this, "rebind", "Exiting.", Level.FINEST).dispatch();
	}

	/**
	 * Lookups for an object in root of the remote directory.
	 *
	 * Use INS syntax specified in the INS specification.
	 * In short, the syntax is that components are left-to-right slash ('/') separated and case-sensitive.
	 * The id and kind of each component are separated by the period character ('.').
	 *
	 * @param	name	name of the object, code non-<code>null</code>
	 * @param	type	type of the object
	 * @return	object	object found in the remote directory, <code>null<code> if nout found
	 */
	private Object lookup(String name, String type)
	{
	    return lookup(remoteDirectory, name, type);
	}

	/**
	 * Lookups for an object in remote directory.
	 *
	 * Use INS syntax specified in the INS specification.
	 * In short, the syntax is that components are left-to-right slash ('/') separated and case-sensitive.
	 * The id and kind of each component are separated by the period character ('.').
	 *
	 * @param	remoteDirectory	remote directory context to be used.
	 * @param	name	name of the object, code non-<code>null</code>
	 * @param	type	type of the object
	 * @return	object	object found in the remote directory, <code>null<code> if nout found
	 */
	private Object lookup(Context remoteDirectory, String name, String type)
	{
		assert (name != null);

		// do not look for interdomain names
		if (name.startsWith(CURL_URI_SCHEMA))
		    return null;

		if (isDebug())
			new MessageLogEntry(this, "lookup", new Object[] { name, type }).dispatch();

		Object resolved = null;

		if (remoteDirectory != null)
		{
			try
			{
				NameParser parser = remoteDirectory.getNameParser("");

				Name n;
				if (type != null)
					n = parser.parse(name+"."+type);
				else
				 	n = parser.parse(name);

				resolved = remoteDirectory.lookup(n);
			}
			catch (NamingException ne)
			{
				CoreException ce = new CoreException(this, "Failed to lookup name '"+name+"' in the remote directory.", ne);
				ce.caughtIn(this, "lookup");
				ce.putValue("name", name);
				ce.putValue("type", type);
				// exception handler service will take care of this exception
				new MessageLogEntry(this, "lookup", ce.getMessage(), ce, LoggingLevel.DEBUG).dispatch();
			}
		}

		if (isDebug())
			new MessageLogEntry(this, "lookup", "Exiting.", Level.FINEST).dispatch();

		return resolved;
	}

	/**
	 * Unbind object to remote directory.
	 *
	 * Use INS syntax specified in the INS specification.
	 * In short, the syntax is that components are left-to-right slash ('/') separated and case-sensitive.
	 * The id and kind of each component are separated by the period character ('.').
	 *
	 * @param	name	name of the object, code non-<code>null</code>
	 * @param	type	type of the object
	 */
	private void unbind(String name, String type)
	{
	    unbind(remoteDirectory, name, type);
	}

	/**
	 * Unbind object to remote directory.
	 *
	 * Use INS syntax specified in the INS specification.
	 * In short, the syntax is that components are left-to-right slash ('/') separated and case-sensitive.
	 * The id and kind of each component are separated by the period character ('.').
	 *
	 * @param	remoteDirectory	remote directory context to be used.
	 * @param	name	name of the object, code non-<code>null</code>
	 * @param	type	type of the object
	 */
	private void unbind(Context remoteDirectory, String name, String type)
	{
		assert (name != null);

		// do not unbind interdomain names
		if (name.startsWith(CURL_URI_SCHEMA))
		    return;

		if (isDebug())
			new MessageLogEntry(this, "unbind", new Object[] { name, type }).dispatch();

		if (remoteDirectory != null)
		{
			try
			{
				NameParser parser = remoteDirectory.getNameParser("");

				Name n;
				if (type != null)
					n = parser.parse(name+"."+type);
				else
				 	n = parser.parse(name);

				remoteDirectory.unbind(n);

				// NOTE: cleaning up the empty context nodes is not implemented
				// since access NS cannot provide quantum actions (transactions)

				// cleanup only empty ".F" contexts - only local manager
				// should modify local NS
				cleanupEmptyFContext(remoteDirectory, name);
			}
			catch (NamingException ne)
			{
				CoreException ce = new CoreException(this, "Failed to unbind name '"+name+"' from the remote directory.", ne);
				ce.caughtIn(this, "unbind");
				ce.putValue("name", name);
				ce.putValue("type", type);
				// exception handler service will take care of this exception
				new MessageLogEntry(this, "unbind", ce.getMessage(), ce, LoggingLevel.DEBUG).dispatch();
			}
		}

		if (isDebug())
			new MessageLogEntry(this, "unbind", "Exiting.", Level.FINEST).dispatch();
	}

	/**
	 * Removes empty ".F" context(s) (recirsive).
	 * @param remoteDirectory	directory root.
	 * @param name	name of the child object being just removed from the potential empty parent context.
	 */
	private void cleanupEmptyFContext(Context remoteDirectory, String name)
	{
	    try
	    {
		    // hierarchical name check
		    int pos = name.lastIndexOf('/');
		    if (pos != -1)
		    {
		        if (pos == 0 || pos == name.length())
		            throw new IllegalArgumentException("Invalid hierarchical name '" + name + "'.");

		        String parent = name.substring(0, pos);

			    if (parent.endsWith(".F") && !remoteDirectory.list(parent).hasMore())
			    {
			        remoteDirectory.unbind(parent);
			        cleanupEmptyFContext(remoteDirectory, parent);
			    }
		    }
	    } catch (Throwable th) {
			CoreException ce = new CoreException(this, "Failed to unbind (potential) empty context for '"+name+"'.", th);
			ce.caughtIn(this, "cleanupEmptyFContext");
			ce.putValue("name", name);
			ce.putValue("remoteDirectory", remoteDirectory);
			// exception handler service will take care of this exception
			new MessageLogEntry(this, "remoteDirectory", ce.getMessage(), ce, LoggingLevel.DEBUG).dispatch();
	    }
	}

 	/*****************************************************************************/
	/*************************** [ Utility methods ] *****************************/
	/*****************************************************************************/

	/**
	 * Returns human-readable and meaningful name of handle.
	 *
	 * @param	id	handle to stringifys
	 * @return	human-readable and meaningful name of handle.
	 */
	private String getRequestorName(int id)
	{
		// parse handle part
		int handle	= id & HANDLE_MASK;

		boolean invalidHandle = true;
		StringBuffer name = new StringBuffer(30);

		switch	(id & TYPE_MASK)
		{
			case CONTAINER_MASK:
				//name.append("Container ");
				synchronized (containers)
				{
					if (containers.isAllocated(handle))
					{
						ContainerInfo info = (ContainerInfo)containers.get(handle);
						if (info.getHandle() == id)
						{
							invalidHandle = false;
							name.append(info.getName());
						}
					}
				}
				break;

			case CLIENT_MASK:
				//name.append("Client ");
				synchronized (clients)
				{
					if (clients.isAllocated(handle))
					{
						ClientInfo info = (ClientInfo)clients.get(handle);
						if (info.getHandle() == id)
						{
							invalidHandle = false;
							name.append(info.getName());
						}
					}
				}
				break;

			case ADMINISTRATOR_MASK:
				//name.append("Administrator ");
				synchronized (administrators)
				{
					if (administrators.isAllocated(handle))
					{
						ClientInfo info = (ClientInfo)administrators.get(handle);
						if (info.getHandle() == id)
						{
							invalidHandle = false;
							name.append(info.getName());
						}
					}
				}
				break;

			case COMPONENT_MASK:
				//name.append("Component ");
				synchronized (components)
				{
					if (components.isAllocated(handle))
					{
						ComponentInfo info = (ComponentInfo)components.get(handle);
						// do additional preallocation check
						if (info != null && info.getHandle() == id)
						{
							invalidHandle = false;
							name.append(info.getName());
						}
					}
				}
				break;

			case MANAGER_MASK:
				//name.append("Manager ");
				name.append("Manager");
				invalidHandle = false;
				break;
		}

		if (invalidHandle)
			name.append("<unknown>");

		//name.append(" [0x").append(Integer.toHexString(id)).append(']');

		return name.toString();
	}

	/**
	 * Verifies URI if it is valid, in CURL format; also checks if it belongs to this domain.
	 * If URI is not valid, <code>BadParametersException</code> exception is thrown.
	 * Allows non-local domains.
	 *
	 * @param	uri		uri to be check to be a valid curl
	 */
	private void checkCURL(URI curl) throws BadParametersException
	{
	    checkCURL(curl, true);
	}

    /**
	 * Verifies URI if it is valid, in CURL format; also checks if it belongs to this domain.
	 * If URI is not valid, <code>BadParametersException</code> exception is thrown.
	 *
	 * @param	uri		uri to be check to be a valid curl
	 * @param	allowNonLocalDomains	allow non-local domains
	 */
	private void checkCURL(URI curl, boolean allowNonLocalDomains) throws BadParametersException
	{
		if (isDebug())
			new MessageLogEntry(this, "checkCURL", new Object[] { curl, new Boolean(allowNonLocalDomains) }).dispatch();

		// check if null
		if (curl == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException(this, "Non-null CURL expected.");
			af.caughtIn(this, "checkCURL");
			af.putValue("curl", curl);
			throw af;
		}

		if (curl.getPath() == null || curl.getPath().length() == 0 ||
			((curl.getScheme() != null && curl.getScheme().startsWith(CURL_URI_SCHEMA))))
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException(this, "Invalid CURL '" + curl.toString() + "'.");
			af.caughtIn(this, "checkCURL");
			af.putValue("curl", curl);
			throw af;
		}

		// check if CURL belongs to this domain
		if (!allowNonLocalDomains && !isLocalDomainCURL(curl))
		{
			// BAD_PARAM
		    String domain = curl.getAuthority();
			BadParametersException af = new BadParametersException(this, "CURL does not belong to this domain ('"+domain+"' not one of '"+domains+"').");
			af.caughtIn(this, "checkCURL");
			af.putValue("domain", domain);
			af.putValue("domains", domains);
			throw af;
		}

		if (isDebug())
			new MessageLogEntry(this, "checkCURL", "Exiting.", Level.FINEST).dispatch();

	}

	/**
	 * Check if CURL belongs to local domain.
	 * @param curl	CURL to be checked.
	 * @return <code>true</code> if CURL belongs to local domain, <code>false</code> otherwise.
	 */
	private boolean isLocalDomainCURL(URI curl)
	{
	    String domain = curl.getAuthority();
	    return (domain == null || domain.length() == 0 || domains.contains(domain));
	}

	/**
	 * Extract component name from the URI.
	 * Only name is returned for local domain, full CURL string for non-local domains.
	 * @param curl	curl from which to extract name.
	 */
	private String extractName(URI curl)
	{
	    if (isLocalDomainCURL(curl))
	    {
		    String name = curl.getPath();
		    // skip trailing slash
			if (name.charAt(0) == '/')
				name = name.substring(1);
			return name;
	    }
	    else
	        return curl.toString();
	}

	/**
	 * Checks if component name is a service component name, list of names is defined in the CDB.
	 *
	 * @param	name	name to be checked, non-<code>null</code>
	 * @returns			<code>true</code> if component name is service component name, <code>false</code> otherwise
	 */
	public boolean isServiceComponent(String name)
	{
		assert (name != null);

		if (isDebug())
			new MessageLogEntry(this, "isServiceComponent", new Object[] { name }).dispatch();

		boolean retVal = false;

		// get CDB access dao
		DAOProxy dao = getManagerDAOProxy();
		if (dao != null)
		{

			try
			{

				// query
				String[] names = dao.get_string_seq("ServiceComponents");

				// find it
				for (int i = 0; i < names.length; i++)
					if (name.equals(names[i]))
					{
						retVal = true;
						break;
					}

			}
			catch (Exception ex)
			{
				CoreException ce = new CoreException(this, "Failed to retrieve list of service components.", ex);
				ce.caughtIn(this, "isServiceComponent");
				ce.putValue("name", name);
				// exception handler service will take care of this exception
				new MessageLogEntry(this, "isServiceComponent", ce.getMessage(), ce, LoggingLevel.DEBUG).dispatch();
			}

		}

		if (isDebug())
			new MessageLogEntry(this, "isServiceComponent", "Exiting.", Level.FINEST).dispatch();

		return retVal;
	}

 	/*****************************************************************************/
	/********************* [ Activation synchronization ] ************************/
	/*****************************************************************************/

	/**
	 * Acquire synchronization lock for named object.
	 * @param	name	name of the object whose lock to acquire.
	 * @param	msecs	the number of milleseconds to wait.
	 * 					An argument less than or equal to zero means not to wait at all.
	 * @return	<code>true</code> if acquired, <code>false</code> othwerwise.
	 */
	private boolean acquireSynchronizationObject(String name, long msec)
	{
		ReferenceCountingLock lock;

		synchronized (activationSynchronization)
		{
			// get synchronization object
			lock = (ReferenceCountingLock)activationSynchronization.get(name);

			// none is found, create and return new one
			// increment references
			if (lock == null)
			{
				lock = new ReferenceCountingLock();
				activationSynchronization.put(name, lock);
			}
			else
				lock.increment();
		}

		boolean success = lock.acquire(msec);

		if (!success)
			releaseSynchronizationObject(name, false);

		return success;
	}

	/**
	 * Release synchronization lock for named object.
	 * @param	name	name of the object whose lock to release.
	 */
	private void releaseSynchronizationObject(String name)
	{
		releaseSynchronizationObject(name, true);
	}

	/**
	 * Release synchronization lock for named object.
	 * @param	name	name of the object whose lock to release.
	 * @param	release	set to <code>false</code> if there is no need to call release
	 * 					on synchronization lock.
	 */
	private void releaseSynchronizationObject(String name, boolean release)
	{
		synchronized (activationSynchronization)
		{
			// get synchronization object
			ReferenceCountingLock lock = (ReferenceCountingLock)activationSynchronization.get(name);

			// release lock
			if (lock != null)
			{
				// if there only one current lock exists
				// remove it from the map
				if (lock.decrement() <= 0)
					activationSynchronization.remove(name);

				// release the lock
				if (release)
					lock.release();
			}
		}
	}

 	/*****************************************************************************/
	/************************* [ CDB access methods ] ****************************/
	/*****************************************************************************/

	/**
	 * Returns, if necessary also creates, Manager DAO (CDB access).
	 *
	 * @return	DAOProxy	Manager DAO (CDB access), otherwise <code>null</code>
	 */
	private synchronized DAOProxy getManagerDAOProxy()
	{
		if (System.getProperties().containsKey(NAME_CDB_DISABLE))
			return null;

		if (isDebug())
			new MessageLogEntry(this, "getManagerDAOProxy", new Object[0] ).dispatch();

		if (managerDAO == null)
			managerDAO = createDAO("MACI/Managers/Manager");

		if (isDebug())
			new MessageLogEntry(this, "getManagerDAOProxy", "Exiting.", Level.FINEST).dispatch();

		return managerDAO;

	}

	/**
	 * Destroys Manager DAO (CDB access).
	 */
	private synchronized void destroyManagerDAOProxy()
	{

		if (isDebug())
			new MessageLogEntry(this, "destroyManagerDAOProxy", new Object[0] ).dispatch();

		if (managerDAO != null)
			destroyDAO(managerDAO);

		managerDAO = null;

		if (isDebug())
			new MessageLogEntry(this, "destroyManagerDAOProxy", "Exiting.", Level.FINEST).dispatch();

	}

	/**
	 * Returns, if necessary also creates, components DAO (CDB access).
	 *
	 * @return	DAOProxy	components DAO (CDB access), otherwise <code>null</code>
	 */
	private synchronized DAOProxy getComponentsDAOProxy()
	{
		if (System.getProperties().containsKey(NAME_CDB_DISABLE))
			return null;

		if (isDebug())
			new MessageLogEntry(this, "getComponentsDAOProxy", new Object[0] ).dispatch();

		if (componentsDAO == null)
		{
			componentsDAO = createDAO("MACI/Components");
			if (componentsDAO != null)
			{
			    // initial refresh
			    componentListCache = refreshComponentsList(componentsDAO);
			    // ... and install link listener (to refresh after reconnect)
			    componentsDAO.addConnectionListener(
			            new DAOProxyConnectionListener()
			            {
			            	public void connected(DAOProxy proxy) {
		                    	componentListCache = refreshComponentsList(proxy);
		                    }
			            	public void disconnected(DAOProxy proxy) { /* noop */ }
			        }
			    );
			}
		}

		if (isDebug())
			new MessageLogEntry(this, "getComponentsDAOProxy", "Exiting.", Level.FINEST).dispatch();

		return componentsDAO;

	}

	/**
	 * Get list of all components.
	 * @return list of all components.
	 */
	private String[] getComponentsList()
	{
	    return componentListCache;
	}

	/**
	 * Searches dao for all potential (nodes containing Name attribute) ComponentInfo nodes.
	 * @param dc	dao to be searched.
	 * @returns list of all potential ComponentInfo nodes.
	 */
	private String[] refreshComponentsList(DAOProxy dc)
    {
        ArrayList componentList = new ArrayList();

        try
        {
            LinkedHashSet nodes = new LinkedHashSet();
            // current
            nodes.add("");
            String[] subnodes = cdbAccess.getSubNodes(dc);
            if (subnodes != null)
                for (int i = 0; i < subnodes.length; i++)
                    nodes.add(subnodes[i]);

            Iterator iter = nodes.iterator();
            while (iter.hasNext())
            {
                String prefix = iter.next().toString();
                if (prefix.length() > 0)
                    prefix += "/";
                String attributes = (String) dc.get_field_data(prefix + "_characteristics");

                // convert into array
                StringTokenizer tokenizer = new StringTokenizer(attributes, ",");
                while (tokenizer.hasMoreTokens())
                {
                    String subname = tokenizer.nextToken().toString();
                    String componentName = prefix + subname;

                    // check if potentially valid ComponentInfo entry - read name
                    /// @todo this could be done better (to check if all attributes exist)
                    if (readStringCharacteristics(dc, componentName + "/Name", true) != null)
                        componentList.add(componentName);
                }
            }

        } catch (Throwable th)
        {
            CoreException ce = new CoreException(this, "Failed to obtain list of all components.", th);
			ce.caughtIn(this, "refreshComponentsList");
			ce.putValue("dc", dc);
			// exception handler service will take care of this exception
			new MessageLogEntry(this, "refreshComponentsList", ce.getMessage(), ce, LoggingLevel.WARNING).dispatch();
        }

        String[] retVal = new String[componentList.size()];
        componentList.toArray(retVal);

		new MessageLogEntry(this, "refreshComponentsList", "Found " + retVal.length + " component entries in the configuration database.", LoggingLevel.INFO).dispatch();

        return retVal;
    }

	/**
	 * Destroys components DAO (CDB access).
	 */
	private synchronized void destroyComponetsDAOProxy()
	{

		if (isDebug())
			new MessageLogEntry(this, "destroyComponetsDAOProxy", new Object[0] ).dispatch();

		if (componentsDAO != null)
			destroyDAO(componentsDAO);

		componentsDAO = null;

		if (isDebug())
			new MessageLogEntry(this, "destroyComponetsDAOProxy", "Exiting.", Level.FINEST).dispatch();

	}

	/**
	 * Returns, if necessary also creates, containers DAO (CDB access).
	 *
	 * @return	DAOProxy	containers DAO (CDB access), otherwise <code>null</code>
	 */
	private synchronized DAOProxy getContainersDAOProxy()
	{
		if (System.getProperties().containsKey(NAME_CDB_DISABLE))
			return null;

		if (isDebug())
			new MessageLogEntry(this, "getContainersDAOProxy", new Object[0] ).dispatch();

		if (containersDAO == null)
			containersDAO = createDAO("MACI/Containers");

		if (isDebug())
			new MessageLogEntry(this, "getContainersDAOProxy", "Exiting.", Level.FINEST).dispatch();

		return containersDAO;

	}

	/**
	 * Destroys containers DAO (CDB access).
	 */
	private synchronized void destroyContainersDAOProxy()
	{

		if (isDebug())
			new MessageLogEntry(this, "destroyContainersDAOProxy", new Object[0] ).dispatch();

		if (containersDAO != null)
			destroyDAO(containersDAO);

		containersDAO = null;

		if (isDebug())
			new MessageLogEntry(this, "destroyContainersDAOProxy", "Exiting.", Level.FINEST).dispatch();

	}

	/**
	 * Creates DAO (CDB access) for requested entity.
	 *
	 * @param	name		name of the entity, non-<code>null</code>.
	 * @return	DAOProxy	DAO (CDB access) for requested entity, <code>null</code> on failure.
	 */
	private DAOProxy createDAO(String entity)
	{
		assert (entity != null);

		if (isDebug())
			new MessageLogEntry(this, "createDAO", new Object[] { entity }).dispatch();

		DAOProxy dao = null;

		// cdbAccess must be given
		if (cdbAccess != null)
		{
			try
			{
				dao = cdbAccess.createDAO(entity);
			}
			catch (Throwable th)
			{
				CoreException ce = new CoreException(this, "Failed to create DAO dao for '"+entity+"'.", th);
				ce.caughtIn(this, "createDAO");
				ce.putValue("entity", entity);
				// exception handler service will take care of this exception
				new MessageLogEntry(this, "createDAO", ce.getMessage(), th, LoggingLevel.DEBUG).dispatch();
			}
		}

		if (isDebug())
			new MessageLogEntry(this, "createDAO", "Exiting.", Level.FINEST).dispatch();

		return dao;

	}

	/**
	 * Reads DAO (CDB access) of string type (uses <code>getStringCharctareistics</code> method).
	 *
	 * @param	path		path to be read non-<code>null</code>.
	 * @param	dao			DAO on which to perform read request.
	 * @return	String		value read, <code>null</code> on failure
	 */
	private String readStringCharacteristics(DAOProxy dao, String path)
	{
		return readStringCharacteristics(dao, path, false);
	}

	/**
	 * Reads DAO (CDB access) of string type.
	 *
	 * @param	path		path to be read non-<code>null</code>.
	 * @param	dao			DAO on which to perform read request.
	 * @param	silent		do not complain, if characteristics not found
	 * @return	String		value read, <code>null</code> on failure
	 */
	private String readStringCharacteristics(DAOProxy dao, String path, boolean silent)
	{
		assert (path != null);

		if (isDebug())
			new MessageLogEntry(this, "readStringCharacteristics", new Object[] { path }).dispatch();

		String retVal = null;

		try
		{
			retVal = dao.get_string(path);
		}
		catch (Throwable th)
		{
			CoreException ce = new CoreException(this, "Failed to read '"+path+"' field on DAO dao '"+dao+"'.", th);
			ce.caughtIn(this, "readStringCharacteristics");
			ce.putValue("path", path);
			if (silent)
				new ExceptionIgnorer(ce);
			// otherwise exception service will handle this
			// new MessageLogEntry(this, "readStringCharacteristics", ce.getMessage(), ex, LoggingLevel.WARNING).dispatch();
		}

		if (isDebug())
			new MessageLogEntry(this, "readStringCharacteristics", "Exiting.", Level.FINEST).dispatch();

		return retVal;

	}

	/**
	 * Reads DAO (CDB access) of long type.
	 *
	 * @param	path		path to be read non-<code>null</code>.
	 * @param	dao			DAO on which to perform read request.
	 * @param	silent		do not complain, if characteristics not found.
	 * @return	int		value read, <code>0</code> on failure.
	 */
	private int readLongCharacteristics(DAOProxy dao, String path, int defaultValue, boolean silent)
	{
		assert (path != null);

		if (isDebug())
			new MessageLogEntry(this, "readLongCharacteristics", new Object[] { path }).dispatch();

		int retVal = defaultValue;

		try
		{
			retVal = dao.get_long(path);
		}
		catch (Throwable th)
		{
			CoreException ce = new CoreException(this, "Failed to read '"+path+"' field on DAO dao '"+dao+"'.", th);
			ce.caughtIn(this, "readLongCharacteristics");
			ce.putValue("path", path);
			if (silent)
				new ExceptionIgnorer(ce);
			// otherwise exception service will handle this
			// new MessageLogEntry(this, "readLongCharacteristics", ce.getMessage(), ex, LoggingLevel.WARNING).dispatch();
		}

		if (isDebug())
			new MessageLogEntry(this, "readLongCharacteristics", "Exiting.", Level.FINEST).dispatch();

		return retVal;

	}

	/**
	 * Destroys DAO (CDB access).
	 *
	 * @param	dao	DAO to be destroyed.
	 */
	private void destroyDAO(DAOProxy dao)
	{

		// NOTE: do not log references - prevents GC to finalize and terminate connection thread (JacORB)
		if (isDebug())
			new MessageLogEntry(this, "destroyDAO", new Object[] { dao == null ? "null" : dao.toString() }).dispatch();

		if (dao != null)
		{
			try
			{
				dao.destroy();
			}
			catch (Throwable th)
			{
				CoreException ce = new CoreException(this, "Failed to destroy DAO dao '"+dao+"'.", th);
				ce.caughtIn(this, "destroyDAO");
				// exception handler service will take care of this exception
				new MessageLogEntry(this, "destroyDAO", ce.getMessage(), th, LoggingLevel.DEBUG).dispatch();
			}

		}

		if (isDebug())
			new MessageLogEntry(this, "destroyDAO", "Exiting.", Level.FINEST).dispatch();

	}

 	/*****************************************************************************/
	/************************* [ Federation methods ] ****************************/
	/*****************************************************************************/

	/**
	 * Initialize manager federation.
	 */
	public void initializeFederation(Hashtable federationDirectoryProperties) throws InitializationException
	{
	    assert(federationDirectoryProperties != null);

	    //
		// read domain list
	    //

	    HashSet givenDomainList = new HashSet();
		try
		{
			String domainList = System.getProperty(NAME_DOMAIN_LIST);
			if (domainList != null)
			{
			    // parse space/comma separated list
				StringTokenizer tokenizer = new StringTokenizer(domainList, ", ");
				while (tokenizer.hasMoreTokens())
				{
				    String domainName = tokenizer.nextToken();
				    // do not allow '/' in domain names
				    if (domainName.indexOf('/') == -1)
				        givenDomainList.add(domainName);
				}
			}

			if (givenDomainList.size() > 0)
			    new MessageLogEntry(this, "initializeFederation", "Using domain list: " + givenDomainList + ".", LoggingLevel.INFO).dispatch();

		}
		catch (Throwable t)
		{
			new MessageLogEntry(this, "initializeFederation", "Failed to parse domain list '" + NAME_DOMAIN_LIST + "' variable, " + t.getMessage(), t, LoggingLevel.WARNING).dispatch();
		}

		// recovery data consistency check
		if (domains.size() != 0 && !domains.equals(givenDomainList))
		{
		    InitializationException ie = new InitializationException(this, "Given domain list is not consistent with recovery data: " + givenDomainList + " != " + domains + ".");
		    ie.caughtIn(this, "initializeFederation");
		    ie.putValue("domains", domains);
		    ie.putValue("givenDomainList", givenDomainList);
		    throw ie;
		}
		else if (domains.size() == 0 && givenDomainList.size() != 0)
		    domains = givenDomainList;


		// no domain to control, no federation
		if (domains.size() == 0)
		{
			new MessageLogEntry(this, "initializeFederation", "No domain list given, manager federation disabled.", LoggingLevel.WARNING).dispatch();
			return;
		}

		//
		// local NS reference check
		//

		if (remoteDirectoryComponentReference == null)
		{
			new MessageLogEntry(this, "initializeFederation", "No valid local domain naming service reference found, manager federation disabled.", LoggingLevel.WARNING).dispatch();
			return;
		}

	    //
		// read federation naming service
	    //

		String domainNS = System.getProperty(NAME_DOMAIN_DIRECTORY);
		if (domainNS == null)
		{
			new MessageLogEntry(this, "initializeFederation", "No federation directory reference given, manager federation disabled.", LoggingLevel.WARNING).dispatch();
			return;
		}
		// set NS address
		federationDirectoryProperties.put(Context.PROVIDER_URL, domainNS);

		if (isDebug())
			new MessageLogEntry(this, "initializeFederation", "Connecting to the federation directory with reference '"+domainNS+"'...", Level.INFO).dispatch();

		try
		{
			federationDirectory = new InitialContext(federationDirectoryProperties);
		}
		catch (Throwable th)
		{
			if (isDebug())
				new MessageLogEntry(this, "initializeFederation", "Failed to connect to the federation directory with reference '"+domainNS+"'...", Level.INFO).dispatch();

			InitializationException cie = new InitializationException(this, "Failed to create initial context.", th);
			cie.putValue("Context.PROVIDER_URL", domainNS);
			// exception service will take care of this exception
			return;
		}

		if (isDebug())
			new MessageLogEntry(this, "initializeFederation", "Connected to the federation directory with reference '"+domainNS+"'.", Level.INFO).dispatch();

		// register domains
		Iterator iter = domains.iterator();
		while (iter.hasNext()) {
		    bind(federationDirectory, dottedToHierarchical(iter.next().toString()),
		            null, remoteDirectoryComponentReference);
		}

		federationEnabled = true;

		new MessageLogEntry(this, "Manager federation enabled.", LoggingLevel.WARNING).dispatch();

		// set domain name string (one name or set of names)
		if (domains.size() == 1)
		    domain = domains.iterator().next().toString();
		else
		    domain = domains.toString();

	}

	/**
	 * Converts dotted name (e.g. "te1.hq.eso.org") to hierachical name (e.g. "org.D/eso.D/hq.D/te1.D").
     * @param dottedName	dotted name to be converted.
     * @return hierarchical name.
     */
    private static String dottedToHierarchical(String dottedName)
    {
        final String kindSuffix = ".D";

        StringTokenizer tokenizer = new StringTokenizer(dottedName, ".");
        if (!tokenizer.hasMoreTokens())
            return dottedName;

        String name = tokenizer.nextToken().toString() + kindSuffix;
        while (tokenizer.hasMoreTokens())
            name = tokenizer.nextToken().toString() + kindSuffix + "/" + name;
        return name;
    }

	/**
	 * Converts component name (e.g. "TOWER1/DOOR1") to hierachical name (e.g. "TOWER1.F/DOOR").
     * @param component name	component name to be converted.
     * @return hierarchical name.
     */
    private static String convertToHiearachical(String componentName)
    {
        if (componentName.indexOf('/') == -1)
            return componentName;

        final String kindSuffix = ".F";

        StringTokenizer tokenizer = new StringTokenizer(componentName, "/");
        if (!tokenizer.hasMoreTokens())
            return componentName;

        StringBuffer name = new StringBuffer(componentName.length()+10);
        name.append(tokenizer.nextToken().toString());
        while (tokenizer.hasMoreTokens())
            name.append(kindSuffix).append('/').append(tokenizer.nextToken().toString());

        return name.toString();
    }

    /**
     * Get manager for given domain.
     * @param domainName	domain name.
     * @return manager for given domain, <code>null</code> if not found.
     */
    private synchronized Manager getManagerForDomain(String domainName)
    {
        // cache lookup
        if (managerCache.containsKey(domainName))
            return (Manager)managerCache.get(domainName);

	    final Object obj = lookup(federationDirectory, dottedToHierarchical(domainName) + "/Manager", null);
	    if (obj == null)
	        return null;

	    /// @todo CORBA specific
	    Manager remoteManager = new ManagerProxy(obj);

	    // store into cache
	    managerCache.put(domainName, remoteManager);

	    return remoteManager;
    }

    /**
	 * Finalize manager federation.
	 */
	private void finalizeFederation()
	{
	    if (!federationEnabled)
	        return;

		// register domains
		Iterator iter = domains.iterator();
		while (iter.hasNext()) {
		    unbind(federationDirectory, dottedToHierarchical(iter.next().toString()), null);
		}
	}

	/*****************************************************************************/
	/*************************** [ Abeans methods ] ******************************/
	/*****************************************************************************/

	/**
	 * @see abeans.core.Identifiable#getIdentifier()
	 */
	public Identifier getIdentifier()
	{
		if (id == null)
			id = new IdentifierSupport("Manager", "Manager", Identifier.PLUG);

		return id;
	}

	/**
	 * @see abeans.core.Identifiable#isDebug()
	 */
	public boolean isDebug()
	{
		return true;
	}

	/**
	 * Returns a single-line rendition of this instance into text.
	 *
	 * @return internal state of this instance
	 */
	public String toString()
	{
		StringBuffer sbuff = new StringBuffer();
		sbuff.append("ManagerImpl = { ");
		sbuff.append("domain = '");
		sbuff.append(domain);
		sbuff.append("' }");
		return new String(sbuff);
	}

 	/*****************************************************************************/
	/************************* [ Prevayler methods ] *****************************/
	/*****************************************************************************/

	/**
	 * @param command
	 * @return
	 */
	private Serializable executeCommand(Command command) /*!!!throws Exception*/
	{
		try
		{
			if (prevayler != null)
				return prevayler.executeCommand(command);
			else
				return command.execute(this);
		}
		catch (Throwable th)
		{
			// no command should throw an exception !!!
			// this has to be handled in a nice way
			th.printStackTrace();
			return null;
		}
	}

	/**
	 * Returns the containers.
	 * @return HandleDataStore
	 */
	public HandleDataStore getContainers()
	{
		return containers;
	}

	/**
	 * Returns the administrators.
	 * @return HandleDataStore
	 */
	public HandleDataStore getAdministrators()
	{
		return administrators;
	}

	/**
	 * Returns the clients.
	 * @return HandleDataStore
	 */
	public HandleDataStore getClients()
	{
		return clients;
	}

	/**
	 * Returns the components.
	 * @return HandleDataStore
	 */
	public HandleDataStore getComponents()
	{
		return components;
	}

	/**
	 * Returns the unavailableComponents.
	 * @return Map
	 */
	public Map getUnavailableComponents()
	{
		return unavailableComponents;
	}

	/**
	 * Returns the defaultComponents.
	 * @return Map
	 */
	public Map getDefaultComponents()
	{
		return defaultComponents;
	}

	/*****************************************************************************/
	/*****************************************************************************/
	/*****************************************************************************/

	/**
	 * Returns the applicationContext.
	 * @return ApplicationContext
	 */
	public ApplicationContext getApplicationContext()
	{
		return applicationContext;
	}

	/**
	 * Returns the remoteDirectory.
	 * @return Context
	 */
	public Context getRemoteDirectory()
	{
		return remoteDirectory;
	}

	/**
	 * Sets the applicationContext.
	 * @param applicationContext The applicationContext to set
	 */
	public void setApplicationContext(ApplicationContext applicationContext)
	{
		this.applicationContext = applicationContext;
	}

	/**
	 * Set the transport.
	 * @param transport
	 */
	public void setTransport(Transport transport) {
		this.transport = transport;
	}

	/**
	 * Sets the remoteDirectory.
	 * @param remoteDirectory The remoteDirectory to set
	 */
	public void setRemoteDirectory(Context remoteDirectory)
	{
		this.remoteDirectory = remoteDirectory;
	}

	/**
	 * Returns the managerComponentReference.
	 * @return Object
	 */
	public Object getManagerComponentReference()
	{
		return managerComponentReference;
	}

	/**
	 * Returns the remoteDirectoryComponentReference.
	 * @return Object
	 */
	public Object getRemoteDirectoryComponentReference()
	{
		return remoteDirectoryComponentReference;
	}

	/**
	 * Sets the managerComponentReference.
	 * @param managerComponentReference The managerComponentReference to set
	 */
	public void setManagerComponentReference(Object managerComponentReference)
	{
		// unbind Manager
		if (this.managerComponentReference != null)
			unbind("Manager", null);

		this.managerComponentReference = managerComponentReference;

		// bind Manager
		if (this.managerComponentReference != null)
			bind("Manager", null, managerComponentReference);

	}

	/**
	 * Sets the remoteDirectoryComponentReference.
	 * @param remoteDirectoryComponentReference The remoteDirectoryComponentReference to set
	 */
	public void setRemoteDirectoryComponentReference(Object remoteDirectoryComponentReference)
	{
		this.remoteDirectoryComponentReference = remoteDirectoryComponentReference;
	}

	/**
	 * Return shutdown status of the Manager.
	 * @return	shutdown status of the Manager
	 */
	public boolean isShuttingDown()
	{
		return shutdown.get();
	}


	/**
	 * Returns the shutdownImplementation.
	 * @return ManagerShutdown
	 */
	public ManagerShutdown getShutdownImplementation()
	{
		return shutdownImplementation;
	}

	/**
	 * Sets the shutdownImplementation.
	 * @param shutdownImplementation The shutdownImplementation to set
	 */
	public void setShutdownImplementation(ManagerShutdown shutdownImplementation)
	{
		this.shutdownImplementation = shutdownImplementation;
	}

	/**
	 * Returns lock timeout (deadlock detection time) in ms.
	 * @return lock timeout (deadlock detection time) in ms
	 */
	public long getLockTimeout()
	{
		return lockTimeout;
	}

	/**
	 * Sets lock timeout (deadlock detection time) in ms.
	 * @param l lock timeout (deadlock detection time) in ms
	 */
	public void setLockTimeout(long l)
	{
		lockTimeout = l;
	}

	/**
	 * Sets CDB access.
	 * @param cdbAccess cdbAccess to set.
	 */
	public void setCDBAccess(CDBAccess cdbAccess) {
		destroyComponetsDAOProxy();
		destroyManagerDAOProxy();

		if (this.cdbAccess != null)
			this.cdbAccess.destroy();

		this.cdbAccess = cdbAccess;

		if (cdbAccess != null) {
			getManagerDAOProxy();
			getComponentsDAOProxy();
		}
	}
}

