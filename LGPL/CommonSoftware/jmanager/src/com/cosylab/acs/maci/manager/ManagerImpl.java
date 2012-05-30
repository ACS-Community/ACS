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
import java.util.Date;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.Name;
import javax.naming.NameAlreadyBoundException;
import javax.naming.NameNotFoundException;
import javax.naming.NameParser;
import javax.naming.NamingException;

import org.omg.CORBA.IntHolder;
import org.prevayler.Command;
import org.prevayler.Prevayler;
import org.prevayler.implementation.AbstractPrevalentSystem;
import org.prevayler.implementation.SnapshotPrevayler;

import com.cosylab.acs.maci.AccessRights;
import com.cosylab.acs.maci.Administrator;
import com.cosylab.acs.maci.AuthenticationData;
import com.cosylab.acs.maci.BadParametersException;
import com.cosylab.acs.maci.Client;
import com.cosylab.acs.maci.ClientInfo;
import com.cosylab.acs.maci.Component;
import com.cosylab.acs.maci.ComponentInfo;
import com.cosylab.acs.maci.ComponentSpec;
import com.cosylab.acs.maci.ComponentStatus;
import com.cosylab.acs.maci.Container;
import com.cosylab.acs.maci.Container.ComponentInfoCompletionCallback;
import com.cosylab.acs.maci.ContainerInfo;
import com.cosylab.acs.maci.CoreException;
import com.cosylab.acs.maci.Daemon;
import com.cosylab.acs.maci.HandleConstants;
import com.cosylab.acs.maci.HandleHelper;
import com.cosylab.acs.maci.ImplLang;
import com.cosylab.acs.maci.IntArray;
import com.cosylab.acs.maci.Manager;
import com.cosylab.acs.maci.MessageType;
import com.cosylab.acs.maci.NoDefaultComponentException;
import com.cosylab.acs.maci.NoResourcesException;
import com.cosylab.acs.maci.RemoteException;
import com.cosylab.acs.maci.RemoteTransientException;
import com.cosylab.acs.maci.ServiceDaemon;
import com.cosylab.acs.maci.StatusHolder;
import com.cosylab.acs.maci.SynchronousAdministrator;
import com.cosylab.acs.maci.TimeoutRemoteException;
import com.cosylab.acs.maci.Transport;
import com.cosylab.acs.maci.loadbalancing.LoadBalancingStrategy;
import com.cosylab.acs.maci.manager.app.ManagerContainerServices;
import com.cosylab.acs.maci.manager.recovery.AdministratorCommandAllocate;
import com.cosylab.acs.maci.manager.recovery.AdministratorCommandDeallocate;
import com.cosylab.acs.maci.manager.recovery.AdministratorCommandSet;
import com.cosylab.acs.maci.manager.recovery.AlarmCleared;
import com.cosylab.acs.maci.manager.recovery.AlarmRaised;
import com.cosylab.acs.maci.manager.recovery.ClientCommandAllocate;
import com.cosylab.acs.maci.manager.recovery.ClientCommandDeallocate;
import com.cosylab.acs.maci.manager.recovery.ClientCommandSet;
import com.cosylab.acs.maci.manager.recovery.ClientInfoCommandComponentAdd;
import com.cosylab.acs.maci.manager.recovery.ClientInfoCommandComponentRemove;
import com.cosylab.acs.maci.manager.recovery.ComponentCommandAckAlloc;
import com.cosylab.acs.maci.manager.recovery.ComponentCommandAllocate;
import com.cosylab.acs.maci.manager.recovery.ComponentCommandAllocateHandle;
import com.cosylab.acs.maci.manager.recovery.ComponentCommandClientAdd;
import com.cosylab.acs.maci.manager.recovery.ComponentCommandClientRemove;
import com.cosylab.acs.maci.manager.recovery.ComponentCommandDeallocate;
import com.cosylab.acs.maci.manager.recovery.ComponentCommandPreallocate;
import com.cosylab.acs.maci.manager.recovery.ComponentCommandSet;
import com.cosylab.acs.maci.manager.recovery.ComponentInfoCommandComponentAdd;
import com.cosylab.acs.maci.manager.recovery.ComponentInfoCommandComponentRemove;
import com.cosylab.acs.maci.manager.recovery.ContainerCommandAllocate;
import com.cosylab.acs.maci.manager.recovery.ContainerCommandDeallocate;
import com.cosylab.acs.maci.manager.recovery.ContainerCommandSet;
import com.cosylab.acs.maci.manager.recovery.ContainerCommandUpdate;
import com.cosylab.acs.maci.manager.recovery.ContainerInfoCommandComponentAdd;
import com.cosylab.acs.maci.manager.recovery.ContainerInfoCommandComponentRemove;
import com.cosylab.acs.maci.manager.recovery.DefaultComponentCommandPut;
import com.cosylab.acs.maci.manager.recovery.UnavailableComponentCommandPut;
import com.cosylab.acs.maci.manager.recovery.UnavailableComponentCommandRemove;
import com.cosylab.acs.maci.plug.ManagerProxy;
import com.cosylab.cdb.client.CDBAccess;
import com.cosylab.cdb.client.DAOProxy;
import com.cosylab.cdb.client.DAOProxyConnectionListener;
import com.cosylab.util.WildcharMatcher;

import si.ijs.maci.ClientOperations;

import alma.ACSErrTypeCommon.wrappers.AcsJBadParameterEx;
import alma.ACSErrTypeCommon.wrappers.AcsJNullPointerEx;
import alma.acs.alarmsystem.source.AlarmSource;
import alma.acs.alarmsystem.source.AlarmSourceImpl;
import alma.acs.concurrent.DaemonThreadFactory;
import alma.acs.exceptions.AcsJException;
import alma.acs.logging.AcsLogLevel;
import alma.acs.util.ACSPorts;
import alma.acs.util.IsoDateFormat;
import alma.jmanagerErrType.wrappers.AcsJCyclicDependencyDetectedEx;
import alma.jmanagerErrType.wrappers.AcsJSyncLockFailedEx;
import alma.maciErrType.wrappers.AcsJCannotGetComponentEx;
import alma.maciErrType.wrappers.AcsJComponentSpecIncompatibleWithActiveComponentEx;
import alma.maciErrType.wrappers.AcsJIncompleteComponentSpecEx;
import alma.maciErrType.wrappers.AcsJInvalidComponentSpecEx;
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
@SuppressWarnings("unchecked")
public class ManagerImpl extends AbstractPrevalentSystem implements Manager, HandleConstants
{
	/**
	 * Serial versionUID
	 */
	private static final long serialVersionUID = 1372046383416324709L;

	// TODO @todo revise...
	private void reportException(Throwable th)
	{
		logger.log(Level.SEVERE, th.getMessage(), th);
	}
	
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
		private AtomicInteger references = new AtomicInteger(1);

		/**
		 * Synchronization mutex.
		 */
		private Lock lock = new ReentrantLock();

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
		 * @param	msecs	the number of milliseconds to wait.
		 * 					An argument less than or equal to zero means not to wait at all.
		 * @return	<code>true</code> if acquired, <code>false</code> othwerwise.
		 */
		public boolean acquire(long msecs)
		{
			try
			{
				return lock.tryLock(msecs, TimeUnit.MILLISECONDS);
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
			lock.unlock();
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
			return references.incrementAndGet();
		}

		/**
		 * Decrement number of references.
		 *
		 * @return number of references.
		 */
		public int decrement()
		{
			return references.decrementAndGet();
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
					    logger.log(Level.FINE,"Failed to activate requested component '"+curls[i]+"', reason: '"+status.getStatus()+"'.");
				}
				catch (Throwable ex)
				{
					CoreException ce = new CoreException("Failed to request component '"+curls[i]+"'.", ex);
					reportException(ce);
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
					CoreException ce = new CoreException("Failed to release component with handle '"+handles[i]+"'.", ex);
					reportException(ce);
				}
			}
		}
	}

	
	private transient Map<Object, GroupedNotifyTask> groupedNotifyTaskMap;

	interface GroupedRunnable extends Runnable {
		public void cancelAll();
		public boolean isCancelAll();
	}
	
	abstract class DefaultGroupedRunnable implements GroupedRunnable {
		private boolean cancelAll = false;
		
		public void cancelAll() { cancelAll = true; }
		public boolean isCancelAll() { return cancelAll; }
	}
	
	protected void registerGroupedNotifyTaks(Object key, GroupedRunnable runnable)
	{
		synchronized (groupedNotifyTaskMap) {
			GroupedNotifyTask gnt = groupedNotifyTaskMap.get(key);
			if (gnt == null)
			{
				gnt = new GroupedNotifyTask(key, runnable);
				groupedNotifyTaskMap.put(key, gnt);
				threadPool.execute(gnt);
			}
			else
				gnt.addTask(runnable);
		}
	}
	
	
	/**
	 * Task thats invokes <code>Runnable</code> method.
	 */
	private class GroupedNotifyTask implements Runnable
	{
		private final Object key;
		// synced to groupedNotifyTaskMap
		private final Deque<GroupedRunnable> tasks = new LinkedList<GroupedRunnable>();

		public GroupedNotifyTask(Object key, GroupedRunnable task)
		{
			this.key = key;
			addTask(task);
		}
		
		// must be synced outside
		public void addTask(GroupedRunnable task)
		{
			tasks.addLast(task);
		}

		public void run()
		{
			while (true)
			{
				GroupedRunnable taskToRun;
				synchronized (groupedNotifyTaskMap) {
					taskToRun = tasks.pollFirst();
					if (taskToRun == null) {
						groupedNotifyTaskMap.remove(key);
						break;
					}
				}
				
				// run
				try {
					taskToRun.run();
				} catch (Throwable th) {
					logger.log(Level.SEVERE, "Unhandeled exception caught.", th);
				}
				
				// checked after
				if (taskToRun.isCancelAll())
				{
					synchronized (groupedNotifyTaskMap) {
						tasks.clear();
						groupedNotifyTaskMap.remove(key);
						break;
					}
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
				CoreException ce = new CoreException("Failed to deactivate component '"+name+"'.", th);
				reportException(ce);
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
				CoreException ce = new CoreException("Failed to shutdown container '"+containerName+"'.", th);
				reportException(ce);
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
                      	new ObjectStreamField("releasedHandles", Map.class),
                      	new ObjectStreamField("unavailableComponents", Map.class),
						new ObjectStreamField("defaultComponents", Map.class),
						new ObjectStreamField("domains", HashSet.class),
    					new ObjectStreamField("activeAlarms", HashSet.class)};

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
	 * Access must be protected using {@link #componentsLock}.
	 * @serial
	 */
	private HandleDataStore components = new HandleDataStore(128, HANDLE_MASK);
	
	/**
	 * This lock was introduced to debug thread contention for http://jira.alma.cl/browse/COMP-6488, 
	 * replacing "synchronized (components)".
	 * Once that problem is resolved, we can either hardcode the ReentrantLock here, or leave in this choice and simply
	 * not define property <code>acs.enableReentrantLockProfiling</code> which would lead to using the ProfilingReentrantLock.
	 */
	private Lock componentsLock = ( ProfilingReentrantLock.isProfilingEnabled 
									? new ProfilingReentrantLock("componentsLock")
									: new ReentrantLock() );

	public enum WhyUnloadedReason { REMOVED, TIMEOUT, DISAPPEARED, REPLACED };
	/**
	 * Monitor entry generated at every handle removal.
	 */
	static class HandleMonitorEntry implements Serializable {
		private static final long serialVersionUID = -5661590007096077942L;
		public final long timestamp;
		public final WhyUnloadedReason reason;
		
		/**
		 * @param timestamp
		 * @param reason
		 */
		public HandleMonitorEntry(long timestamp, WhyUnloadedReason reason) {
			super();
			this.timestamp = timestamp;
			this.reason = reason;
		}
		
	}
	
	/**
	 * Handle data store to monitor released handles.
	 * @serial
	 */
	private Map<Integer, HandleMonitorEntry> releasedHandles = new HashMap<Integer, HandleMonitorEntry>();

	/**
	 * List of all unavailable components.
	 * @serial
	 */
	private Map<String, ComponentInfo> unavailableComponents = new LinkedHashMap<String, ComponentInfo>();

	/**
	 * List of all pending activations.
	 * Needed for cyclic dependency checks, since non-fully-activated components
	 * are not accessible via HandleDataStore iterator.
	 */
	private transient Map<String, ComponentInfo> pendingActivations = null;

	/**
	 * List of all pending container shutdowns.
	 */
	private transient Set<String> pendingContainerShutdown = null;

	/**
	 * New container logged in notification.
	 */
	private transient Object containerLoggedInMonitor = null;

	/**
	 * Map of default components (set via getDynamicComponent) overriding CDB state.
	 * Entry is: (String type, String name).
	 * @serial
	 */
	private Map<String, ComponentInfo> defaultComponents = new HashMap<String, ComponentInfo>();

	/**
	 * Manager domain name.
	 */
	private transient Random random = null;

	/**
	 * Heartbeat timer.
	 */
	private transient Timer heartbeatTask = null;

	/**
	 * Delayed release timer.
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
	private transient Map<String, ReferenceCountingLock> activationSynchronization;

	/**
	 * Activation/deactivation synchronization mechanism.
	 * Reader lock is acquired when activation/deactivation
	 * is pending (i.e. until completed) and writer lock by
	 * processes which require not to be run until
	 * activation/deactivation is in progress.
	 */
	private transient ReaderPreferenceReadWriteLock activationPendingRWLock;
//	private transient ReadWriteLock activationPendingRWLock;
	
	/**
	 * Shutdown status.
	 */
	private transient AtomicBoolean shutdown;

	/**
	 * Thread pool (guarantees order of execution).
	 */
	private transient ThreadPoolExecutor threadPool;

	/**
	 * Default manager domain.
	 */
	private static final String DEFAULT_DOMAIN = "";

	/**
	 * Number of threads in thread pool (guarantees order of execution).
	 */
	private transient int poolThreads;

	/**
	 * Lock timeout (deadlock detection time) in ms.
	 */
	private transient long lockTimeout;

	/**
	 * Client ping interval.
	 */
	private transient int	clientPingInterval;

	/**
	 * Administrator ping interval.
	 */
	private transient int	administratorPingInterval;

	/**
	 * Container ping interval.
	 */
	private transient int	containerPingInterval;

	/**
	 * Container rights.
	 */
	private static final int	CONTAINER_RIGHTS = AccessRights.NONE;

	/**
	 * Shutdown implementation.
	 */
	private transient ManagerShutdown shutdownImplementation = null;

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
	 * Enable handle monitoring property name.
	 */
	private static final String NAME_HANDLE_MONITORING = "manager.debug.rememberOldHandles";

	/**
	 * Handle monitoring flag. 
	 */	
	private transient boolean enableHandleMonitoring;
	
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
	private transient Map<String, Manager> managerCache = null;

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
	 * Logger.
	 */
	private transient Logger logger;

	/**
	 * Alarm System Interface.
	 */
    private transient AlarmSource alarmSource;

	/**
	 * Persistent set of raised (timer task) alarms.
	 */
    private HashSet activeAlarms = new HashSet();

    /**
     * Last execution id;
     */
    private transient long lastExecutionId = 0;

	/**
	 * Queue (per client) for its messages.
	 */
	protected transient Map<Client, LinkedList<ClientMessageTask>> clientMessageQueue;

	/**
	 * Free threads percentage. The manager will reject client logins when this value gets too large
	 * (current threshold is 90).
	 * @see #setConnectionThreadUsage(int)
	 */
	private transient AtomicInteger threadsUsedPercentage;
	
	/**
	 * Use sync. instead of async. activation of components.
	 */
	private static final String NAME_SYNC_ACTIVATE = "manager.sync_activate";

	/**
	 * Allows setting the current percentage of used connection threads, 
	 * which would typically be updated by an ORB profiler.
	 * @param percentage
	 * @see #threadsUsedPercentage
	 */
	public void setConnectionThreadUsage(int percentage)
	{
		threadsUsedPercentage.set(percentage);
	}
	
	/**
	 * Initializes Manager.
	 * @param	prevayler			implementation of prevayler system
	 * @param	context				remote directory implementation
	 */
	public void initialize(Prevayler prevayler, CDBAccess cdbAccess, Context context, Logger logger, ManagerContainerServices managerContainerServices)
	{
		this.prevayler = prevayler;
		this.remoteDirectory = context;
		this.logger = logger;
		
		// needs to be done here, since deserialization is used
		initializeDefaultConfiguration();
		
		if (cdbAccess != null)
			setCDBAccess(cdbAccess);

		readManagerConfiguration();
		
		random = new Random();
		heartbeatTask = new Timer(true);
		delayedDeactivationTask = new Timer(true);

		containerLoggedInMonitor = new Object();

		activationSynchronization = new HashMap<String, ReferenceCountingLock>();
		activationPendingRWLock = new ReaderPreferenceReadWriteLock();
		shutdown = new AtomicBoolean(false);
		
		threadPool = new ThreadPoolExecutor(poolThreads, poolThreads,
				  Long.MAX_VALUE, TimeUnit.NANOSECONDS,
				  new LinkedBlockingQueue(),
				  new DaemonThreadFactory("managerThreadPool"));

		managerCache = new HashMap<String, Manager>();

		pendingActivations = new HashMap<String, ComponentInfo>();
		pendingContainerShutdown = Collections.synchronizedSet(new HashSet<String>());

		clientMessageQueue = new HashMap<Client, LinkedList<ClientMessageTask>>();
		
		groupedNotifyTaskMap = new HashMap<Object, GroupedNotifyTask>();
		
		threadsUsedPercentage = new AtomicInteger(0);
		
		// create threads
		threadPool.prestartAllCoreThreads();

		// read CDB startup
		try
		{
			String componentSpec = System.getProperty(NAME_CDB_COMPONENTSPEC);
			if (componentSpec != null)
			{
				cdbActivation = new ComponentSpec(componentSpec);
				logger.log(Level.INFO,"Using CDB component specification: '" + cdbActivation + "'.");
			}
		}
		catch (Throwable t)
		{
			logger.log(Level.WARNING, "Failed to parse '" + NAME_CDB_COMPONENTSPEC + "' variable, " + t.getMessage(), t);
		}

		// check load balancing strategy
		checkLoadBalancingStrategy();

		// establish connect to the alarm system
		try
		{
			alarmSource = new AlarmSourceImpl(managerContainerServices);
			alarmSource.start();
		}
		catch (Throwable ex)
		{
			logger.log(Level.SEVERE, "Failed to initialize Alarm System Interface " + ex.getMessage(), ex);
			alarmSource = null;
		}
		
		// register ping tasks
		initializePingTasks();

		// start topology sort manager
		topologySortManager = new ComponentInfoTopologicalSortManager(
				components, containers, activationPendingRWLock,
				pendingContainerShutdown, threadPool, logger);
	}

	/**
	 * Initialize manager default configuration.
	 */
	private void initializeDefaultConfiguration()
	{
		poolThreads = 10;
		lockTimeout = 10 * 60000L;	// 10 minutes
		clientPingInterval = 60000;		// 60 secs
		administratorPingInterval = 45000;		// 45 secs
		containerPingInterval = 30000;		// 30 secs
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
			PingTimerTask task = new PingTimerTask(this, logger, clientInfo, alarmSource);
			containerInfo.setTask(task);
			heartbeatTask.schedule(task, 0, containerInfo.getPingInterval());
	    }

	    // administrators
		TimerTaskClientInfo adminInfo = null;
		h = administrators.first();
		while (h != 0)
	    {
	    	adminInfo = (TimerTaskClientInfo)administrators.get(h);
			h = administrators.next(h);

			// register administrator to the heartbeat manager
			PingTimerTask task = new PingTimerTask(this, logger, adminInfo, null);
			adminInfo.setTask(task);
			heartbeatTask.schedule(task, 0, administratorPingInterval);
	    }

	    // clients
		TimerTaskClientInfo clientInfo = null;
		h = clients.first();
		while (h != 0)
	    {
	    	clientInfo = (TimerTaskClientInfo)clients.get(h);
			h = clients.next(h);

			// register client to the heartbeat manager
			PingTimerTask task = new PingTimerTask(this, logger, clientInfo, null);
			clientInfo.setTask(task);
			heartbeatTask.schedule(task, 0, clientPingInterval);
	    }
	}

	/**
	 * Checks and registers load balancing strategy.
	 * Load balancing strategy is defined as Java JVM system property named
	 * <code>NAME_LOAD_BALANCING_STRATEGY</code> containing class name of the
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
				Object strategyObject = constructor.newInstance((Object[])null);
				if (!(strategyObject instanceof LoadBalancingStrategy))
					throw new IllegalArgumentException("Class '" + strategyClass.getName() + "' does not implement '" + LoadBalancingStrategy.class.getName() + "' interface.");
				loadBalancingStrategy = (LoadBalancingStrategy)strategyObject;

				logger.log(Level.INFO,"Using load balancing strategy: '" + strategyClass.getName() + "'.");
			}
		}
		catch (Throwable t)
		{
			logger.log(Level.WARNING, "Failed to register '" + NAME_LOAD_BALANCING_STRATEGY + "' load balancing strategy: " + t.getMessage(), t);
		}
	}

	/**
	 * Called from client code after all manager initialization is done.
	 */
	public void initializationDone()
	{
		threadPool.execute(new Runnable() {
			public void run() {
				initializeServiceDaemons();
				autoStartComponents();
			}
		});
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

	protected synchronized long generateExecutionId()
	{
		final long id = Math.max(System.currentTimeMillis(), lastExecutionId + 1);
		lastExecutionId = id;
		return id;
	}
	
	/**
	 * @see com.cosylab.acs.maci.Manager#getContainerInfo(int, int[], String)
	 */
	public ContainerInfo[] getContainerInfo(int id, int[] handles, String name_wc)
		throws AcsJNoPermissionEx
	{
		if (handles == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException("Non-null 'handles' sequence expected.");
			throw af;
		}
		else if (handles.length == 0 && name_wc == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException("Non-null 'names_wc' sequence expected.");
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
				BadParametersException af = new BadParametersException("Failed to compile 'names_wc' reqular expression string '"+name_wc+"'.");
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
			ArrayList<ContainerInfo> list = new ArrayList<ContainerInfo>();

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

		return info;
	}

	/**
	 * @see com.cosylab.acs.maci.Manager#getClientInfo(int, int[], String)
	 */
	public ClientInfo[] getClientInfo(int id, int[] handles, String name_wc)
		throws AcsJNoPermissionEx
	{

		if (handles == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException("Non-null 'handles' sequence expected.");
			throw af;
		}
		else if (handles.length == 0 && name_wc == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException("Non-null 'names_wc' sequence expected.");
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
				BadParametersException af = new BadParametersException("Failed to compile 'names_wc' reqular expression string '"+name_wc+"'.");
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
			ArrayList<ClientInfo> list = new ArrayList<ClientInfo>();

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

		return info;
	}

	/**
	 * @see com.cosylab.acs.maci.Manager#getComponentInfo(int, int[], String, String, boolean)
	 */
	// TODO MF all (using wildchars match for domain names) interdomain queries
	public ComponentInfo[] getComponentInfo(int id, int[] handles, String name_wc, String type_wc, boolean activeOnly)
		throws AcsJNoPermissionEx
	{

		if (handles == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException("Non-null 'handles' sequence expected.");
			throw af;
		}
		else if (handles.length == 0 && (name_wc == null || type_wc == null))
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException("Non-null 'names_wc' or' type_wc' sequence expected.");
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
        			BadParametersException af = new BadParametersException("Invalid CURL syntax in 'names_wc'.");
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
        			        throw new CoreException("Failed to obtain manager for domain '" + domainName + "'.");
        		    } catch (Throwable th) {
        				logger.log(Level.WARNING, "Failed to obtain non-local manager required by CURL '"+curl+"'.", th);
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
    				RemoteException re = new RemoteException("Failed to obtain component infos for CURL '"+curl+"' from remote manager.", ex);
    				reportException(re);
    				logger.log(Level.SEVERE, re.getMessage(), ex);
    				return null;
    			}
		    }



			// map of components to be returned
			Map<String, ComponentInfo> map = new HashMap<String, ComponentInfo>();

			// read active/registered components
			componentsLock.lock();
			try {
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
			} finally {
				componentsLock.unlock();
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
								logger.log(Level.WARNING,"Misconfigured CDB, there is no name of component '"+ids[i]+"' defined.");
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
									logger.log(Level.WARNING,"Misconfigured CDB, there is no type of component '"+name+"' defined.");
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
										logger.log(Level.WARNING,"Misconfigured CDB, there is no code of component '"+name+"' defined.");
										continue;
									}

									// test code
									if (code.equals(ComponentSpec.COMPSPEC_ANY))
										continue;


									// read container
									String container = readStringCharacteristics(componentsDAO, ids[i]+"/Container");
									if (container == null)
									{
										logger.log(Level.WARNING,"Misconfigured CDB, there is no container name of component '"+name+"' defined.");
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
						CoreException ce = new CoreException("Failed to obtain component data from the CDB.", ex);
						reportException(ce);
					}

				}


			}

			// copy to array
			info = new ComponentInfo[map.size()];
			map.values().toArray(info);
		}

		/****************************************************************/

		return info;
	}


	/**
	 * @see com.cosylab.acs.maci.Manager#getService(int, java.net.URI, boolean, StatusHolder)
	 */
	public Component getService(int id,	URI curl, boolean activate,	StatusHolder status)
		throws AcsJCannotGetComponentEx, AcsJNoPermissionEx
	{
		return getComponent(id, curl, activate, status, true);
	}

	/**
	 * @see com.cosylab.acs.maci.Manager#getComponent(int, URI, boolean, StatusHolder)
	 */
	public Component getComponent(int id, URI curl, boolean activate, StatusHolder status)
		throws AcsJCannotGetComponentEx, AcsJNoPermissionEx
	{
		return getComponent(id, curl, activate, status, false);
	}

	/**
	 * @see #getComponent
	 */
	private Component getComponent(int id, URI curl, boolean activate, StatusHolder status, boolean allowServices)
		throws AcsJCannotGetComponentEx, AcsJNoPermissionEx
	{
		AcsJCannotGetComponentEx ex2 = null;
	
		// extract name
		String name = extractName(curl);

		// check if null
		try {
			checkCURL(curl);
		} catch (AcsJBadParameterEx e) {
			ex2 = new AcsJCannotGetComponentEx(e);
			ex2.setCURL(name);
			throw ex2;
		}
	
		if (status == null)
		{
			AcsJNullPointerEx ex = new AcsJNullPointerEx();
			ex.setVariable("status");
			ex2 = new AcsJCannotGetComponentEx(ex);
			ex2.setCURL(name);
			throw ex2;
		}

		/****************************************************************/
	
		// log info
		String requestorName = null;
	
		if (id != 0)
		{
			requestorName = getRequestorName(id);
			logger.log(Level.INFO,"'" + requestorName + "' requested component '" + curl + "'.");
		}
		else
			logger.log(Level.INFO,"Request for component '" + curl + "' issued.");
	
	
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
			try {
				component = internalRequestComponent(id, curl, status, activate);
			}
			catch (Throwable ce)
			{
				ex2 = new AcsJCannotGetComponentEx(ce);
			}
		}
	
		// log info
		if (component != null && component.getObject() != null)
		{
			if (requestorName != null)
				logger.log(Level.INFO, "Component '" + curl + "' provided to '" + requestorName + "'.");
			else
				logger.log(Level.INFO,"Component '" + curl + "' provided.");
		}
		else if (ex2 == null && !activate && status.getStatus() == ComponentStatus.COMPONENT_NOT_ACTIVATED)
		{
			if (requestorName != null)
				logger.log(Level.INFO,"Request from '" + requestorName + "' for component '" + curl + "' completed sucessfully, but component not activated.");
			else
				logger.log(Level.INFO,"Request for component '" + curl + "' completed sucessfully, but component not activated.");
		}
		/**
		 * @todo GCH 2006.09.25
		 *       This last case should never happen, because 
		 *       there should be and exception thrown instead.
		 */
		else
		{
			if (ex2 == null)
				ex2 = new AcsJCannotGetComponentEx();
			
			if (requestorName != null)
				logger.log(Level.WARNING,"Failed to provide component '" + curl + "' to '" + requestorName + "'.", ex2);
			else
				logger.log(Level.WARNING,"Failed to provide component '" + curl + "'.", ex2);
		}
	
		/****************************************************************/
	
		if(ex2 != null)
		{
			ex2.setCURL(name);
			throw ex2;
			
		}

		return component;
	
	}

	/**
	 * @see com.cosylab.acs.maci.Manager#getComponentNonSticky(int id, URI curl)
	 */
	public Component getComponentNonSticky(int id, URI curl) 
		throws AcsJCannotGetComponentEx, AcsJNoPermissionEx
	{
	
		// extract name
		String name = extractName(curl);
	
		// check if null
		try {
			checkCURL(curl);
		} catch (AcsJBadParameterEx e) {
			AcsJCannotGetComponentEx ex2 = new AcsJCannotGetComponentEx(e);
			ex2.setCURL(name);
			throw ex2;
		}

		/****************************************************************/
	
		// check handle and NONE permissions
		securityCheck(id, AccessRights.NONE);

		// log info
		String requestorName = getRequestorName(id);
		logger.log(Level.FINE,"'" + requestorName + "' requested non-sticky component '" + curl + "'.");
	
		Component component = null;
		componentsLock.lock();
		try {
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
		} finally {
			componentsLock.unlock();
		}
	
		// log info
		if (component != null && component.getObject() != null)
			logger.log(Level.FINE,"Non-sticky component '" + curl + "' provided to '" + requestorName + "'.");
		else
			logger.log(Level.INFO,"Failed to provide non-sticky component '" + curl + "' to '" + requestorName + "'.");
	
		/****************************************************************/
	
		return component;
	
	}

	/**
	 * @see com.cosylab.acs.maci.Manager#makeComponentImmortal(int, java.net.URI, boolean)
	 */
	public void makeComponentImmortal(int id, URI curl, boolean immortalState) 
	    throws AcsJCannotGetComponentEx, AcsJNoPermissionEx, AcsJBadParameterEx
	{

		// extract name
		String name = extractName(curl);

		// check if null
		// let same exception flying up
		try {
			checkCURL(curl);
		} catch (AcsJBadParameterEx e) {
			throw e;
		}

		// check handle and NONE permissions
		securityCheck(id, AccessRights.NONE);

		/****************************************************************/


		int h;
		ComponentInfo componentInfo = null;

		componentsLock.lock();
		try {
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
				NoResourcesException af = new NoResourcesException("Component not activated.");
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
				logger.log(Level.INFO,"Component " + name + " was made immortal.");
			}
		} finally {
			componentsLock.unlock();
		}

		// this must be done outside component sync. block
		if (!immortalState)
		{
			logger.log(Level.INFO,"Component " + name + " was made mortal.");

			// finally, can happen that the manager is the only owner
			// so release could be necessary
			internalReleaseComponent(this.getHandle(), h, false);
		}

		/****************************************************************/

	}

	/**
	 * @see com.cosylab.acs.maci.Manager#login(Client)
	 */
	public ClientInfo login(Client reference) throws AcsJNoPermissionEx
	{

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
			BadParametersException af = new BadParametersException("Non-null 'reference' expected.");
			throw af;
		}

		/****************************************************************/

		ClientInfo info = null;

		try
		{
			long executionId = generateExecutionId();
			AuthenticationData reply = reference.authenticate(executionId, "Identify yourself");

			if (reply == null)
			{
				// BAD_PARAM
				BadParametersException af = new BadParametersException("Invalid response to 'Client::authenticate()' method - non-null structure expected.");
				throw af;
			}
			else if (reply.getClientType() == null)
			{
				// BAD_PARAM
				BadParametersException af = new BadParametersException("Invalid response to 'Client::authenticate()' method - non-null client type expected.");
				throw af;
			}
			else if (reply.getImplLang() == null)
			{
				// BAD_PARAM
				BadParametersException af = new BadParametersException("Invalid response to 'Client::authenticate()' method - no-null implementation language expected.");
				throw af;
			}

			// get client's name
			String name = reference.name();

			if (name == null)
			{
				// BAD_PARAM
				BadParametersException af = new BadParametersException("Invalid response to 'Client::name()' method - non-null string expected.");
				throw af;
			}

			logger.log(Level.FINE,"'"+name+"' is logging in.");

			final long timeStamp = reply.getTimeStamp() > 0 ? reply.getTimeStamp() : System.currentTimeMillis();
			if (reply.getExecutionId() != 0)
				executionId = generateExecutionId();

			// delegate
			switch (reply.getClientType())
			{
				// container
				case CONTAINER:
					if (reference instanceof Container)
					{
						info = containerLogin(name, reply, (Container)reference, timeStamp, executionId);
					}
					else
					{
						// NO_PERMISSION
						AcsJNoPermissionEx npe = new AcsJNoPermissionEx();
						npe.setReason("Given reply to 'Client::authenticate()' method indicated container login, but given reference does not implement 'maci::Container' interface.");
						npe.setID(name);
						throw npe;
					}
					break;

				// client
				case CLIENT:
					info = clientLogin(name, reply, reference, timeStamp, executionId);
					break;

				// supervisor (administrator)
				case ADMINISTRATOR:
					if (reference instanceof Administrator)
					{
						info = administratorLogin(name, reply, (Administrator)reference, timeStamp, executionId);
					}
					else
					{
						// NO_PERMISSION
						AcsJNoPermissionEx npe = new AcsJNoPermissionEx();
						npe.setReason("Given reply to 'Client::authenticate()' method indicated administrator login, but given reference does not implement 'maci::Administrator' interface.");
						npe.setID(name);
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
			// TODO @todo exception
			RuntimeException rt = new RuntimeException("Exception caught while examining the client. Login rejected.", re);
			throw rt;
		}
		catch (Throwable ex)
		{
			// TODO @todo exception
			RuntimeException rt = new RuntimeException("Unexpected exception during login. Login rejected.", ex);
			throw rt;
		}

		/****************************************************************/
		
		logger.log(Level.FINE,"Client with handle '" + HandleHelper.toString(info.getHandle()) + "' has logged in.");
		
		return info;
	}

	/**
	 * @see com.cosylab.acs.maci.Manager#logout(int)
	 */
	public void logout(int id) throws AcsJNoPermissionEx
	{
		logout(id, false);
	}
	
	/**
	 * Logout client.
	 * @param id client handle.
	 * @param pingFailed flag indicating that ping has failed (i.e. reason for logout).
	 * @see com.cosylab.acs.maci.Manager#logout(int)
	 */
	public void logout(int id, boolean pingFailed) throws AcsJNoPermissionEx
	{
		if (pingFailed)
			logger.log(Level.FINE,"Client with handle '" + HandleHelper.toString(id) + "' is being forcefully logged out due to its unresponsiveness.");
		else
			logger.log(Level.FINE,"Client with handle '" + HandleHelper.toString(id) + "' is logging out.");

		// check handle, no special rights needed for logout
                // AcsJNoPermissionEx flies directly up from securityCheck()
		securityCheck(id, 0);

		/****************************************************************/

		switch	(id & TYPE_MASK)
		{
			case CONTAINER_MASK:
				containerLogout(id, pingFailed);
				break;
			case CLIENT_MASK:
				clientLogout(id, pingFailed);
				break;
			case ADMINISTRATOR_MASK:
				administratorLogout(id, pingFailed);
				break;
		}

		/****************************************************************/

	}

	/**
	 * @see com.cosylab.acs.maci.Manager#registerComponent(int, URI, String, Component)
	 */
	public int registerComponent(int id, URI curl, String type, Component component)
		throws AcsJNoPermissionEx, AcsJBadParameterEx
	{

		// check for null
		if (curl == null )
		{
			AcsJBadParameterEx af = new AcsJBadParameterEx();
			af.setParameter("curl");
			af.setParameterValue("null");
			throw af;
		}
		if (type == null)
		{
			AcsJBadParameterEx af = new AcsJBadParameterEx();
			af.setParameter("type");
			af.setParameterValue("null");
			throw af;
		}
		if (component == null)
		{
			AcsJBadParameterEx af = new AcsJBadParameterEx();
			af.setParameter("component");
			af.setParameterValue("null");
			throw af;
		}

		// checks CURL, reject non-local domain curls
        // Just rethrow the exception
		try {
			checkCURL(curl, false);
		} catch (AcsJBadParameterEx e) {
		    throw e;
		}

		// check handle and REGISTER_COMPONENT permissions
		securityCheck(id, AccessRights.REGISTER_COMPONENT);

		/****************************************************************/

		// extract name
		String name = extractName(curl);

		int h = 0;

		componentsLock.lock();
		try {

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
						npe.setID(HandleHelper.toString(id));
						npe.setProtectedResource(name);
						throw npe;
					}

				}

				h = components.next(h);
		    }

			// allocate new handle
			// !!! ACID 2
			Integer objHandle = (Integer)executeCommand(new ComponentCommandAllocate());
			int handle;
			//int handle = components.allocate();
			if (objHandle == null || (handle = objHandle.intValue()) == 0)
			{
				NoResourcesException af = new NoResourcesException("Generation of new handle failed, too many components registred.");
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
		} finally {
			componentsLock.unlock();
		}


		// bind to remote directory
		// NOTE: this could block since it is a remote call
		bind(convertToHiearachical(name), "O", component);

		logger.log(Level.INFO,"Component '"+name+"' registered.");

		/****************************************************************/

		return h;
	}

	/**
	 * @see com.cosylab.acs.maci.Manager#unregisterComponent(int, int)
	 */
	public void unregisterComponent(int id, int h) throws AcsJNoPermissionEx, AcsJBadParameterEx
	{

		// check handle and REGISTER_COMPONENT permissions
		securityCheck(id, AccessRights.REGISTER_COMPONENT);

		/****************************************************************/

		internalReleaseComponent(this.getHandle(), h, false);

		logger.log(Level.INFO,"Component with handle '"+HandleHelper.toString(h)+"' unregistered.");

		/****************************************************************/

	}

	/**
	 * @see com.cosylab.acs.maci.Manager#restartComponent(int, URI)
	 */
	public Component restartComponent(int id, URI curl) throws AcsJNoPermissionEx, AcsJBadParameterEx
	{

		// checks CURL
		// let same exception fly up
		// TODO MF tmp, reject non-local domains
		try {
			checkCURL(curl, false);
		} catch (AcsJBadParameterEx e) {
			throw e;
		}

		// check handle and NONE permissions
		securityCheck(id, AccessRights.NONE);

		/****************************************************************/

		Component component = internalRestartComponent(id, curl);

		if (component != null)
			logger.log(Level.INFO,"Component '"+curl+"' restarted.");
		else
			logger.log(Level.INFO,"Failed to restart component '"+curl+"'.");

		/****************************************************************/

		return component;

	}

	/**
	 * @see com.cosylab.acs.maci.Manager#releaseComponent(int, URI)
	 */
	public int releaseComponent(int id, URI curl) throws AcsJNoPermissionEx, AcsJBadParameterEx
	{

		// checks CURL
		// throw up same exceptions
		try {
			checkCURL(curl);
		} catch (AcsJBadParameterEx e) {
			throw e;
		}

		// check handle and NONE permissions
		securityCheck(id, AccessRights.NONE);

		/****************************************************************/

		// log info
		String requestorName = getRequestorName(id);
		logger.log(Level.INFO,"'" + requestorName + "' requested release of component '" + curl + "'.");

		int owners = internalReleaseComponent(id, curl, id == this.getHandle()).owners;

		logger.log(Level.INFO,"Component '" + curl + "' released by '" + requestorName + "'.");

		/****************************************************************/

		return owners;

	}
	
	/**
	 * @see com.cosylab.acs.maci.Manager#releaseComponentAsync(int, java.net.URI, com.cosylab.acs.maci.Manager.LongCompletionCallback)
	 */
	public void releaseComponentAsync(int id, URI curl,
			LongCompletionCallback callback) throws AcsJNoPermissionEx, AcsJBadParameterEx
	{
		// checks CURL
		// throw up same exceptions
		try {
			checkCURL(curl);
		} catch (AcsJBadParameterEx e) {
			throw e;
		}

		// check handle and NONE permissions
		securityCheck(id, AccessRights.NONE);

		/****************************************************************/

		// log info
		final String requestorName = getRequestorName(id);
		logger.log(Level.INFO,"'" + requestorName + "' requested async release of component '" + curl + "'.");

		final int fid = id;
		final URI fcurl = curl;
		final boolean force = (id == this.getHandle());
		final LongCompletionCallback fcallback = callback;
		threadPool.execute(new Runnable() {

			public void run()
			{
				try
				{
					ReleaseComponentResult rcr = internalReleaseComponent(fid, fcurl, force);
					if (fcallback != null) 
					{
						if (rcr.exception == null) {
							logger.log(Level.INFO,"Component '" + fcurl + "' async released by '" + requestorName + "'.");
							fcallback.done(rcr.owners);
						}
						else {
							logger.log(Level.INFO,"Component '" + fcurl + "' async released by '" + requestorName + " with failure'.");
							fcallback.failed(rcr.owners, rcr.exception);
						}
					}
				} catch (Throwable th) {
						if (fcallback != null) fcallback.failed(-1, th);
				}
			}
				
		});
	}

	/**
	 * @see com.cosylab.acs.maci.Manager#forceReleaseComponent(int, URI)
	 */
	public int forceReleaseComponent(int id, URI curl) throws AcsJNoPermissionEx, AcsJBadParameterEx
	{

		// checks CURL
		// let same exception fly up
		try {
			checkCURL(curl);
		} catch (AcsJBadParameterEx e) {
			throw e;
		}

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
		logger.log(Level.INFO,"'" + requestorName + "' requested forceful release of component '" + curl + "'.");

		int owners = internalReleaseComponent(id, curl, true).owners;

		logger.log(Level.INFO,"Component '" + curl + "' forcefully released by '" + requestorName + "'.");

		/****************************************************************/

		return owners;

	}

	/**
	 * @see com.cosylab.acs.maci.Manager#releaseComponents(int, URI[])
	 */
	public void releaseComponents(int id, URI[] curls) throws AcsJNoPermissionEx
	{

		// check if null
		if (curls == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException("Non-null CURLs expected.");
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
				CoreException ce = new CoreException("Failed to release component '"+curls[i]+"'.", ex);
				reportException(ce);
			}
		}

		logger.log(Level.INFO,released + " of " + curls.length +" components released.");

		/****************************************************************/

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
			NoResourcesException nre = new NoResourcesException("Container '" + containerName + "' is being shutdown.");
			throw nre;
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Manager#shutdownContainer(int, java.lang.String, int)
	 */
	public void shutdownContainer(int id, String containerName, int action)
			throws AcsJNoPermissionEx {

		// check if null
		if (containerName == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException("Non-null 'containerName' expected.");
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
			NoResourcesException nre = new NoResourcesException("Container '" + containerName + "' not logged in.");
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
				CoreException ce = new CoreException("Failed to release components on container '" + containerName + "'.", th);
				reportException(ce);
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
				NoResourcesException nre = new NoResourcesException("Failed to shutdown container '" + containerName + "'.", th);
				throw nre;
			}

		}
		finally
		{
			pendingContainerShutdown.remove(containerInfo.getName());
		}

		/****************************************************************/

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
						BadParametersException hbpe = new BadParametersException(usi.getMessage(), usi);
						reportException(hbpe);
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
		if (shutdown.getAndSet(true))
		{
			// already shutdown
			AcsJNoPermissionEx npe = new AcsJNoPermissionEx();
			npe.setReason("Manager already in shutdown state.");
			npe.setID(HandleHelper.toString(id));
			throw npe;
		}

		/****************************************************************/

		logger.log(Level.INFO,"Manager is shutting down.");


		logger.log(Level.FINER,"Canceling heartbeat task.");
		// cancel hertbeat task
		heartbeatTask.cancel();
		topologySortManager.destroy();

		/*
		 * Are those actions OK? don't we want to have silent (unnoticable) restarts of the manager?!
		 * There probably should be several shutdown modes - from silent to whole system shutdown...
		 * For the time being sileny mode is implemented.
		 *
		logger.finer("Releasing immortal components.");
		/// !!! TBD


		logger.finer("Notifying clients and administrators to disconnect.");
		notifyClientDisconnectShutdown();

		*/

		// if not "silent" shutdown
		if (containers != 0)
		{
			logger.log(Level.FINER,"Releasing all components in the system.");
			try
			{
				topologySortManager.requestTopologicalSort();
				releaseComponents(topologySortManager.getComponentShutdownOrder(null));
			}
			catch (Throwable th)
			{
				CoreException ce = new CoreException("Failed to release all components in the system.", th);
				reportException(ce);
			}
		}

		logger.log(Level.FINER,"Notifying containers to disconnect or shutdown.");
		notifyContainerDisconnectShutdown(containers);

		// finalizeFearation
		finalizeFederation();

		// process tasks in thread pool
		// !!! NOTE: this could block (for a long time)
		logger.log(Level.FINER,"Waiting for tasks in thread pool to complete...");
		threadPool.shutdown();
        try {
            if (!threadPool.awaitTermination(3, TimeUnit.SECONDS))
            	threadPool.shutdownNow();
        } catch (InterruptedException ie) { /* noop */ } 

        if (alarmSource != null) {
            alarmSource.tearDown();
        }
        
		// unbind Manager
		unbind("Manager", null);

		setCDBAccess(null);

		// release CDB DAO daos
		destroyComponetsDAOProxy();
		destroyContainersDAOProxy();
		destroyManagerDAOProxy();

		logger.log(Level.INFO,"Manager shutdown completed.");

		/****************************************************************/

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
	private ClientInfo containerLogin(String name, AuthenticationData reply, Container container, long timeStamp, long executionId) throws AcsJNoPermissionEx
	{
		assert (name != null);
		assert (reply != null);
		assert (container != null);

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
				// containers are persistent and this will not work
				//if (container.equals(loggedContainerInfo.getContainer()))
				if (name.equals(loggedContainerInfo.getName()))
				{

					Container loggedContainer = loggedContainerInfo.getContainer();
					if (loggedContainer != null)
					{
						// if same instance simply recover, if not...
						if (!loggedContainer.equals(container))
						{
							// check if logged container is alive, if it is reject him
							boolean alive = false;
							try
							{
								alive = loggedContainer.ping();
							}
							catch (Throwable th) {
								// noop
							}
							
							if (alive)
							{
								AcsJNoPermissionEx npe = new AcsJNoPermissionEx();
								
								String address = "";
								try
								{
									address = " " + loggedContainer.getRemoteLocation();
								} catch (Throwable th) { /* noop */ }
								npe.setReason("Rejecting container login, container with name '" + name + "'" + address + " already logged in.");
								npe.setID(HandleHelper.toString(loggedContainerInfo.getHandle()));
								npe.setProtectedResource(name);
								throw npe;
							}
							else
								logger.log(Level.FINER, "Container '" + name + "' is no longer functional, new container is taking over.");
						 }
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
				long pingInterval = 0;
                DAOProxy dao = getContainersDAOProxy();
                if (dao != null)
                {
                	String impLang = readStringCharacteristics(dao, name + "/ImplLang", true);
                	ImplLang configuredImplLang = ImplLang.fromString(impLang);
                	if (configuredImplLang != ImplLang.not_specified && configuredImplLang != reply.getImplLang()) {
						AcsJNoPermissionEx npe = new AcsJNoPermissionEx();
						npe.setReason("Rejecting container login, container reported '" + reply.getImplLang() + "' implementation language, but configured '" + configuredImplLang + "'.");
						npe.setProtectedResource(name);
						throw npe;
                	}
                	
                	pingInterval = readLongCharacteristics(dao, name + "/PingInterval", -1, true)*1000;
                }

                // allocate new handle
				// !!! ACID 2
				Integer objHandle = (Integer)executeCommand(new ContainerCommandAllocate());
				int handle;
				//int handle = containers.allocate();

				if (objHandle == null || (handle = objHandle.intValue()) == 0)
				{
					NoResourcesException af = new NoResourcesException("Generation of new handle failed, too many containers logged in.");
					throw af;
				}

				// generate external handle
				h = handle | CONTAINER_MASK;

				// add generated key
			    h |= (random.nextInt(0x100)) << 16;
			    
				// create new container info
				containerInfo = new TimerTaskContainerInfo(h, name, container, containerPingInterval);

				containerInfo.setImplLang(reply.getImplLang());
            	if (pingInterval >= 1000)	// safety limit
            		containerInfo.setPingInterval(pingInterval);

				
				clientInfo = containerInfo.createClientInfo();


				// register container to the heartbeat manager
				PingTimerTask task = new PingTimerTask(this, logger, clientInfo, alarmSource);
				containerInfo.setTask(task);
				heartbeatTask.schedule(task, containerInfo.getPingInterval(), containerInfo.getPingInterval());

				// !!! ACID - register AddContainerCommand
				executeCommand(new ContainerCommandSet(handle, containerInfo));
				// store info
				//containers.set(handle, containerInfo);
			}
		}

		final boolean recoverContainer = reply.isRecover();

		if (existingLogin)
		{
			// merge container's and manager's internal state
			containerInternalStateMerge(containerInfo, recoverContainer);
		}

		// notify administrators about the login
		notifyContainerLogin(containerInfo, timeStamp, executionId);

		// do container post login activation in separate thread
		final ContainerInfo finalInfo = containerInfo;
		threadPool.execute(
				new Runnable() {
					public void run()
					{
						containerPostLoginActivation(finalInfo, recoverContainer);
					}
				});

		logger.log(Level.INFO,"Container '" + name + "' logged in.");

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
        if(containerInfo.getImplLang() == ImplLang.cpp || containerInfo.getImplLang() == ImplLang.not_specified){
            try
            {
                Thread.sleep(3000);
            }
            catch (InterruptedException ie)
            {
            }
        }
		// shutdown check
		if (isShuttingDown())
			return;

		// TODO what if it has already logged out?

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
					logger.log(Level.SEVERE,"Failed to activate CDB, reason: '"+status.getStatus()+"'.");
				else if (cdbInfo == null || cdbInfo.getHandle() == 0 || cdbInfo.getComponent() == null)
					logger.log(Level.SEVERE,"Failed to activate CDB, invalid ComponentInfo returned: '"+cdbInfo+"'.");
				else
				{
				logger.log(Level.INFO,"CDB activated on container '" + containerInfo.getName() + "'.");
				}
			}
			catch (Throwable ex)
			{
				logger.log(Level.SEVERE, "Failed to activate CDB on container '" + containerInfo.getName() + "'.", ex);
			}
		}


		// used for fast lookups
		Map<String, Integer> activationRequests = new HashMap<String, Integer>();
		// order is important, preserve it
		ArrayList<URI> activationRequestsList = new ArrayList<URI>();

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
						logger.log(Level.WARNING,"Misconfigured CDB, there is no container of component '"+startup[i]+"' defined.");
						continue;
					}

					// if container name matches, add activation request
					if (containerInfo.getName().equals(containerName))
					{
						try
						{
							URI curl = CURLHelper.createURI(startup[i]);

							// check CURL
							try {
								checkCURL(curl);
							} catch (RuntimeException e) {
								// @todo Auto-generated catch block
								e.printStackTrace();
							}

							activationRequestsList.add(curl);
							activationRequests.put(startup[i], managerHandle);
						}
						catch (URISyntaxException usi)
						{
							logger.log(Level.WARNING, "Failed to create URI from component name '"+startup[i]+"'.", usi);
						}
					}
				}
			}
			catch (Throwable th)
			{
				logger.log(Level.WARNING, "Failed to retrieve list of startup components.", th);
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
						logger.log(Level.WARNING,"Misconfigured CDB, there is no name of component '"+ids[i]+"' defined.");
						continue;
					}

					// read autostart silently
					String autostart = readStringCharacteristics(componentsDAO, ids[i]+"/Autostart", true);
					if (autostart == null)
					{
						logger.log(Level.WARNING,"Misconfigured CDB, there is no autostart attribute of component '"+ids[i]+"' defined.");
						continue;
					}
					else if (autostart.equalsIgnoreCase(TRUE_STRING) && !activationRequests.containsKey(name) /* TODO to be removed */ )
					{
						// read container silently
						String componentContainer = readStringCharacteristics(componentsDAO, ids[i]+"/Container", true);
						if (componentContainer == null)
						{
							logger.log(Level.WARNING,"Misconfigured CDB, there is no container attribute of component '"+ids[i]+"' defined.");
							continue;
						}
						else if (!containerInfo.getName().equals(componentContainer))
							continue;

						try
						{
							URI curl = CURLHelper.createURI(name);

							// check CURL, no non-local curls
							try {
								checkCURL(curl, false);
							} catch (RuntimeException e) {
								// @todo Auto-generated catch block
								e.printStackTrace();
							}

							activationRequestsList.add(curl);
							activationRequests.put(name, managerHandle);
						}
						catch (URISyntaxException usi)
						{
							logger.log(Level.WARNING, "Failed to create URI from component name '"+name+"'.", usi);
						}
					}
				}
			}
			catch (Throwable ex)
			{
				logger.log(Level.WARNING, "Failed to retrieve list of components.", ex);
			}

		}

		// list of componentInfo to be cleaned up (cannot be immediately, due to lock)
		ArrayList<ComponentInfo> cleanupList = new ArrayList<ComponentInfo>();

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
						String[] names = unavailableComponents.keySet().toArray(new String[unavailableComponents.size()]);
						// reverse
						for (int i = names.length - 1; i >= 0; i--)
						{
							String name = names[i];

							boolean reactivate = false;

							// dynamic component check
							ComponentInfo componentInfo = unavailableComponents.get(name);
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
									logger.log(Level.WARNING,"Misconfigured CDB, there is no container of component '"+name+"' defined.");
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
									try
									{
										activationRequestsList.add(CURLHelper.createURI(name));
										activationRequests.put(name, reactivateHandle);
									}
									catch (URISyntaxException usi)
									{
										logger.log(Level.WARNING, "Failed to create URI from component name '"+name+"'.", usi);
									}
								}
							}
						}
					}


				}
				catch (Throwable ex)
				{
					CoreException ce = new CoreException("Failed to obtain component data from the CDB.", ex);
					reportException(ce);
				}
			}

		}

		if (cleanupList.size() > 0)
		{
			Iterator<ComponentInfo> iter = cleanupList.iterator();
			while (iter.hasNext())
			{
				ComponentInfo componentInfo = iter.next();

				componentsLock.lock();
				try {
					// remove from its owners list ...
					int[] owners = componentInfo.getClients().toArray();
					for (int j = 0; j < owners.length; j++)
						removeComponentOwner(componentInfo.getHandle(), owners[j]);

					// ... and deallocate
					executeCommand(new ComponentCommandDeallocate(componentInfo.getHandle() & HANDLE_MASK, componentInfo.getHandle(), WhyUnloadedReason.REMOVED));
					executeCommand(new UnavailableComponentCommandRemove(componentInfo.getName()));

					// remove component from container component list
					synchronized (containerInfo.getComponents())
					{
						if (containerInfo.getComponents().contains(componentInfo.getHandle()))
							executeCommand(new ContainerInfoCommandComponentRemove(containerInfo.getHandle() & HANDLE_MASK, componentInfo.getHandle()));
					}
				} finally {
					componentsLock.unlock();
				}

				// unbind from remote directory
				unbind(convertToHiearachical(componentInfo.getName()), "O");
			}

		}

		logger.log(Level.INFO,"Container '"+containerInfo.getName()+"' startup statistics: " +
							activationRequestsList.size() + " components queued to be activated.");

		// send message to the container
		sendMessage(containerInfo.getContainer(), "Startup statistics: " + activationRequestsList.size() +
					" components queued to be activated.", MessageType.MSG_INFORMATION, ClientOperations.MSGID_AUTOLOAD_START);

		// activate startup components
		int activated = 0;
		StatusHolder status = new StatusHolder();
		Iterator<URI> iterator = activationRequestsList.iterator();
		while (iterator.hasNext())
		{
			URI uri = iterator.next();
			try
			{
				String name = extractName(uri);

				int requestor = activationRequests.get(name);

				internalRequestComponent(requestor, uri, status);

				if (status.getStatus() != ComponentStatus.COMPONENT_ACTIVATED)
					logger.log(Level.FINE,"Failed to reactivate requested component '"+uri+"', reason: '"+status.getStatus()+"'.");
				else
					activated++;
			}
			catch (Throwable ex)
			{
				CoreException ce = new CoreException("Failed to request component '"+uri+"'.", ex);
				reportException(ce);
			}
		}

		logger.log(Level.INFO,"Container '"+containerInfo.getName()+"' startup statistics: " +
							activated + " of " + activationRequestsList.size() + " components activated.");

		// send message to the container
		sendMessage(containerInfo.getContainer(), "Startup statistics: " + activated + " of " +
					activationRequestsList.size() + " components activated.", MessageType.MSG_INFORMATION, ClientOperations.MSGID_AUTOLOAD_END);

		// notify about new container login
		synchronized (containerLoggedInMonitor)
		{
			containerLoggedInMonitor.notifyAll();
		}

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

		// firstly, query containers state
		ComponentInfo[] infos = null;
		try
		{
			infos = containerInfo.getContainer().get_component_info(new int[0]);
		}
		catch (Throwable ex)
		{
			logger.log(Level.SEVERE, "Failed to query state of container '"+containerInfo.getName()+"'.", ex);
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
			componentsLock.lock();
			try {
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
							npe.setID(containerInfo.getName());
							npe.setProtectedResource(infos[i].getName());
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
								npe.setID(containerInfo.getName());
								if (componentInfo != null) {
									npe.setProtectedResource(componentInfo.getName());
								}
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
								npe.setID(containerInfo.getName());
								npe.setProtectedResource(componentInfo.getName());
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

			} finally {
				componentsLock.unlock();
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
			componentsLock.lock();
			try {
				int handle = componentHandle & HANDLE_MASK;

				if (components.isAllocated(handle))
				{
					ComponentInfo componentInfo = (ComponentInfo)components.get(handle);
					// what if null (very not likely to happen, but possible)
					if (componentInfo == null)
					{
						// internal error, this should not happen
						logger.log(Level.SEVERE,"Internal state is not consistent (no ComponentInfo).");
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
						executeCommand(new ComponentCommandDeallocate(handle, componentInfo.getHandle(), WhyUnloadedReason.REMOVED));
						//components.deallocate(handle);

						requireTopologySort = true;
					}

				}
				else
				{
					// internal error, this should not happen
					logger.log(Level.SEVERE,"Internal state is not consistent.");
				}

			} finally {
				componentsLock.unlock();
			}


		}

		if (requireTopologySort)
			topologySortManager.notifyTopologyChange(containerInfo.getHandle());

	}

	/**
	 * Provides its own address to daemons.
	 * <p>
	 * @TODO: The manager should call this method regularly (e.g. once per minute),
	 *        so that also restarted service daemons get the manager address.
	 *        Or the daemons should resolve the naming service themselves and get it from there,
	 *        with possible problems when using different subnets.
	 */
	private void initializeServiceDaemons()
	{
		// get CDB access daos
		DAOProxy dao = getManagerDAOProxy();

		// no data
		if (dao == null)
			return;

		String[] daemons;
		try
		{
			// query service daemon array
			daemons = dao.get_string_seq("ServiceDaemons");
		}
		catch (Throwable th)
		{
			// parameter is optional
			logger.log(Level.WARNING, "No list of services daemons available in the CDB. " + 
					"In an operational environment using ACS daemons, this is a severe error!! " + 
					"It is OK only if you run the system without using these daemons. ");
			return;
		}

		for (int i = 0; i < daemons.length; i++)
		{
			try
			{
				ServiceDaemon daemon = transport.getServiceDaemon(daemons[i]);
				if (daemon != null)
					daemon.setManagerReference(transport.getManagerReference());
				else
					throw new RuntimeException("Failed to resolve service daemon reference '" + daemons[i] + "'.");

			} catch (Throwable th) {
				// do not make scary logs...
				logger.config("Failed to set manager reference on service daemon on host '"+daemons[i]+"'.");
				//logger.log(Level.CONFIG,"Failed to set manager reference on service daemon on host '"+daemons[i]+"'.", th);
			}
			
		}
	}

	/**
	 * Checks for autostart components that are to be hosed by autostart containers.
	 */
	private void autoStartComponents()
	{
		// order is important, preserve it
		LinkedHashSet<String> activationRequestsList = new LinkedHashSet<String>();

		// get CDB access daos
		DAOProxy dao = getManagerDAOProxy();
		DAOProxy componentsDAO = getComponentsDAOProxy();
		DAOProxy containersDAO = getContainersDAOProxy();

		// no data
		if (componentsDAO == null || containersDAO == null)
			return;

		//
		// autostart components (Manager.Startup array) - TODO to be removed (left for backward compatibility)
		//
		if (dao != null)
		{

			try
			{
				// query startup components and add them to a list of candidates
				String[] startup = dao.get_string_seq("Startup");

				for (int i = 0; i < startup.length; i++)
				{
					// TODO simulator test workaround
					if (startup[i].length() == 0)
						continue;

					activationRequestsList.add(startup[i]);
				}
			}
			catch (Throwable th)
			{
				logger.log(Level.WARNING, "Failed to retrieve list of startup components.", th);
			}

		}

		//
		// autostart components (<component>.Autostart attribute)
		//
		final String TRUE_STRING = "true";
		{

			try
			{
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
						logger.log(Level.WARNING,"Misconfigured CDB, there is no name of component '"+ids[i]+"' defined.");
						continue;
					}

					// read autostart silently
					String autostart = readStringCharacteristics(componentsDAO, ids[i]+"/Autostart", true);
					if (autostart == null)
					{
						logger.log(Level.WARNING,"Misconfigured CDB, there is no autostart attribute of component '"+ids[i]+"' defined.");
						continue;
					}
					else if (autostart.equalsIgnoreCase(TRUE_STRING))
					{
							activationRequestsList.add(name);
					}
				}
			}
			catch (Throwable ex)
			{
				logger.log(Level.WARNING, "Failed to retrieve list of components.", ex);
			}

		}

		// now gather list of all containers to be started up (they will autostart components by default)
		// by filtering out all components that are not hosted by auto-start containers
		// leave only components that have "*" listed as container
		LinkedHashSet startupContainers = new LinkedHashSet();
		Iterator<String> iterator = activationRequestsList.iterator();
		while (iterator.hasNext())
		{
			String name = iterator.next();

			try
			{
				
				// get container
				String containerName = readStringCharacteristics(componentsDAO, name+"/Container", true);
				if (containerName == null)
				{
					iterator.remove();
					continue;
				}
				else if (containerName.equals(ComponentSpec.COMPSPEC_ANY))
				{
					// leave it in the list
					continue;
				}
				
				// get its deploy info
				String host = readStringCharacteristics(containersDAO, containerName + "/DeployInfo/Host", true);
				String startOnDemand = readStringCharacteristics(containersDAO, containerName + "/DeployInfo/StartOnDemand", true);
				if (host != null && startOnDemand != null && startOnDemand.equalsIgnoreCase("TRUE"))
					startupContainers.add(containerName);
	
				// remove (or is it auto-started by starting a container or is it not hosted by auto-start container)
				iterator.remove();
			}
			catch (Throwable ex)
			{
				logger.log(Level.WARNING, "Failed to retrieve list of components.", ex);
			}
		}		
		
		
		int activated = 0;
		
		// autostart containers
		iterator = startupContainers.iterator();
		while (iterator.hasNext())
		{
			String containerName = (String)iterator.next();
			try
			{
				startUpContainer(containerName);
			}
			catch (Throwable ex)
			{
				logger.log(Level.WARNING, "Failed to auto-start container to auto-start components.", ex);
			}
		}
		
		// activate startup components (no container specified)
		StatusHolder status = new StatusHolder();
		iterator = activationRequestsList.iterator();
		while (iterator.hasNext())
		{
			String name = iterator.next();
			try
			{
                URI uri = CURLHelper.createURI(name);

				internalRequestComponent(this.getHandle(), uri, status);

				if (status.getStatus() != ComponentStatus.COMPONENT_ACTIVATED)
					logger.log(Level.FINE, "Failed to auto-activate requested component '"+name+"', reason: '"+status.getStatus()+"'.");
				else
					activated++;
			}
			catch (Throwable ex)
			{
				CoreException ce = new CoreException("Failed to request component '"+name+"'.", ex);
				reportException(ce);
			}
		}
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
	private ClientInfo administratorLogin(String name, AuthenticationData reply, Administrator administrator, long timeStamp, long executionId) throws AcsJNoPermissionEx
	{
		assert (name != null);
		assert (administrator != null);

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
			Integer objHandle = (Integer)executeCommand(new AdministratorCommandAllocate());
			int handle;
			//int handle = administrators.allocate();
			if (objHandle == null || (handle = objHandle.intValue()) == 0)
			{
				NoResourcesException af = new NoResourcesException("Generation of new handle failed, too many administrators logged in.");
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
			PingTimerTask task = new PingTimerTask(this, logger, clientInfo, null);
			clientInfo.setTask(task);
			heartbeatTask.schedule(task, administratorPingInterval, administratorPingInterval);

			// !!! ACID - register AddAdministratorCommand
			executeCommand(new AdministratorCommandSet(handle, clientInfo));
			// store info
			//administrators.set(handle, clientInfo);
		}

		// notify administrators about the login
		notifyClientLogin(clientInfo, timeStamp, executionId);

		logger.log(Level.INFO,"Administrator '" + name + "' logged in.");

		return clientInfo;
	}

	/**
	 * Client specific login method.
	 * @param	name	name of the client
	 * @param	reply	reply to authenticate method
	 * @param	client	client that is logging in
	 * @return	ClientInfo	client info. of newly logged client
	 */
	private ClientInfo clientLogin(String name, AuthenticationData reply, Client client, long timeStamp, long executionId) throws AcsJNoPermissionEx
	{
		assert (name != null);
		assert (client != null);

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
			
			// check thread resources
			int usage = threadsUsedPercentage.get(); 
			if (usage > 90)
			{
				throw new NoResourcesException("Thread usage too high (%" + usage + "), rejecting login.");
			}

			// allocate new handle
			// !!! ACID 2
			Integer objHandle = (Integer)executeCommand(new ClientCommandAllocate());
			int handle;
			//int handle = clients.allocate();
			if (objHandle == null || (handle = objHandle.intValue()) == 0)
			{
				NoResourcesException af = new NoResourcesException("Generation of new handle failed, too many clients logged in.");
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
			PingTimerTask task = new PingTimerTask(this, logger, clientInfo, null);
			clientInfo.setTask(task);
			heartbeatTask.schedule(task, clientPingInterval, clientPingInterval);

			// !!! ACID - register AddClientCommand
			executeCommand(new ClientCommandSet(handle, clientInfo));
			// store info
			//clients.set(handle, clientInfo);
		}

		// notify administrators about the login
		notifyClientLogin(clientInfo, timeStamp, executionId);

		logger.log(Level.INFO,"Client '" + name + "' logged in.");

		return clientInfo;
	}


	/**
	 * Container specific logout method
	 * @param	id	handle of the container.
	 * @param	pingFailed	flag indicating that ping has failed (i.e. is the reason of this logout).
	 */
	private void containerLogout(int id, boolean pingFailed)
	{
		TimerTaskContainerInfo containerInfo = null;

		synchronized (containers)
		{
			int handle = id & HANDLE_MASK;

			// already logged out
			if (!containers.isAllocated(handle))
				return;

			containerInfo = (TimerTaskContainerInfo)containers.get(handle);

			// !!! ACID - RemoveContainerCommand
			executeCommand(new ContainerCommandDeallocate(handle, id,
									pingFailed ? WhyUnloadedReason.DISAPPEARED : WhyUnloadedReason.REMOVED));
			// remove
			//containers.deallocate(handle);

		}

		// deregister container from the heartbeat manager
		containerInfo.getTask().cancel();

		// make all container components unavailable
		markContainersComponentsUnavailable(containerInfo);

/// TODO !!!!!!!!!!!!!! no more handle -> componentInfo data
		// notify administrators about the logout
		notifyContainerLogout(containerInfo, System.currentTimeMillis());
		
		Container container = containerInfo.getContainer();
		if (container != null)
			container.release();

		logger.log(Level.INFO,"Container '" + containerInfo.getName() + "' logged out.");

	}

	private void markContainersComponentsUnavailable(
			TimerTaskContainerInfo containerInfo) {
		// make all container components unavailable
		int[] markUnavailable;
		synchronized (containerInfo.getComponents())
		{
			markUnavailable = containerInfo.getComponents().toArray();
		}

		if (markUnavailable.length > 0)
		{
			componentsLock.lock();
			try {
				synchronized (unavailableComponents)
				{
					// add in reverse order (as deactivation procedure would do)
					for (int i = markUnavailable.length - 1; i >= 0; i--)
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
			} finally {
				componentsLock.unlock();
			}


		}
	}

	/**
	 * Make Component unavailable.
	 *
	 * @param	componentInfo		component to be made unavailable.
	 */
	private void makeUnavailable(ComponentInfo componentInfo)
	{
		final boolean notificationNeeded;
		String componentName = null;
		int[] clients = null;
		synchronized (unavailableComponents)
		{
			// !!! ACID 3
			// add to unavailable component map (override old info)
			notificationNeeded = !unavailableComponents.containsKey(componentInfo.getName());
			executeCommand(new UnavailableComponentCommandPut(componentInfo.getName(), componentInfo));
			//unavailableComponents.put(componentInfo.getName(), componentInfo);

			if (notificationNeeded)
			{
				componentName = componentInfo.getName();
				clients = componentInfo.getClients().toArray();
			}

			// clear component reference and container
			componentInfo.setComponent(null);
			componentInfo.setContainer(0);
			// leave container name set
		}

		// inform clients about unavailability (if necessary)
		if (notificationNeeded)
			notifyComponentUnavailable(0, clients, new String[] { componentName });
	}

	/**
	 * Client specific logout method
	 * @param	id	handle of the client.
	 * @param	pingFailed	flag indicating that ping has failed (i.e. is the reason of this logout).
	 */
	private void clientLogout(int id, boolean pingFailed)
	{

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
			executeCommand(new ClientCommandDeallocate(handle, id,
									pingFailed ? WhyUnloadedReason.DISAPPEARED : WhyUnloadedReason.REMOVED));
			// remove
			//clients.deallocate(handle);

			componentsArray = clientInfo.getComponents().toArray();
		}

		// deregister client from the heartbeat manager
		clientInfo.getTask().cancel();

		// spawn another task which will release all clientInfo.getComponents()
		threadPool.execute(new ReleaseComponentTask(clientInfo.getHandle(), componentsArray));

		//internalReleaseComponents(components to be released, owners IntArray)
		//internalReleaseComponents(clientInfo.getComponents(), clientInfo.getComponents())

/// TODO !!!!!!!!!!!!!! no more handle -> componentInfo data
		// notify administrators about the logout
		notifyClientLogout(clientInfo, System.currentTimeMillis());
		
		Client client = clientInfo.getClient();
		if (client != null)
			client.release();

		logger.log(Level.INFO,"Client '" + clientInfo.getName() + "' logged out.");

	}

	/**
	 * Administrator specific logout method
	 * @param	id	handle of the administrators.
	 * @param	pingFailed	flag indicating that ping has failed (i.e. is the reason of this logout).
	 */
	private void administratorLogout(int id, boolean pingFailed)
	{

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
			executeCommand(new AdministratorCommandDeallocate(handle, id,
									pingFailed ? WhyUnloadedReason.DISAPPEARED : WhyUnloadedReason.REMOVED));
			// remove
			//administrators.deallocate(handle);

			componentsArray = clientInfo.getComponents().toArray();
		}

		// deregister client from the heartbeat manager
		clientInfo.getTask().cancel();

		// spawn another task which will release all clientInfo.getComponents()
		threadPool.execute(new ReleaseComponentTask(clientInfo.getHandle(), componentsArray));

/// TODO !!!!!!!!!!!!!! no more handle -> componentInfo data
		// notify administrators about the logout
		notifyClientLogout(clientInfo, System.currentTimeMillis());

		Client client = clientInfo.getClient();
		if (client != null)
			client.release();

		logger.log(Level.INFO,"Administrator '" + clientInfo.getName() + "' logged out.");

	}

	/**
	 * Returns array of currently logged administrators.
	 * @param	excludeHandle	handle of administrator not to be included in the array, can be 0.
	 * @return	ClientInfo[]	array of currently logged administrators
	 */
	private ClientInfo[] getAdministrators(int excludeHandle)
	{

		// array of administrators to be notified
		ClientInfo[] admins = null;

		// generate array of Administrators to be notified
		synchronized (administrators)
		{
			int len = administrators.size();

			// no administrator to notify
			if (len > 0)
			{
				ArrayList<ClientInfo> list = new ArrayList<ClientInfo>();

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

		return admins;
	}

	/**
	 * Returns array of currently logged containers.
	 * @return	ContainerInfo[]	array of currently logged containers
	 */
	private ContainerInfo[] getContainersInfo()
	{

		// array of containers to be notified
		ContainerInfo[] acts = null;

		// generate array of containers to be notified
		synchronized (containers)
		{
			int len = containers.size();

			// no containers to notify
			if (len > 0)
			{
				ArrayList<ContainerInfo> list = new ArrayList<ContainerInfo>();

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

		return acts;
	}

    /**
     * Returns array of currently logged clients.
     * @return	ClientInfo[]	array of currently logged clients
     */
    private ClientInfo[] getClientInfo()
    {

		ArrayList<ClientInfo> list = new ArrayList<ClientInfo>();

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

		// array of clients to be notified
		ClientInfo[] clients = null;

		ArrayList<ClientInfo> list = new ArrayList<ClientInfo>();

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
				componentsLock.lock();
				try {
					// !!! ACID 3
					executeCommand(new ComponentInfoCommandComponentRemove(owner & HANDLE_MASK, componentHandle));
					//ci.getComponents().remove(componentHandle);
				} finally {
					componentsLock.unlock();
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
				componentsLock.lock();
				try {
					// !!! ACID 3
					if (!componentInfo.getComponents().contains(componentHandle))
						executeCommand(new ComponentInfoCommandComponentAdd(owner & HANDLE_MASK, componentHandle));
					//ci.getComponents().add(componentHandle);
				} finally {
					componentsLock.unlock();
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
	private void notifyClientLogin(final ClientInfo clientInfo, final long timeStamp, final long executionId)
	{
		assert (clientInfo != null);

		// array of administrators to be notified
		ClientInfo[] admins = getAdministrators(clientInfo.getHandle());

		if (admins != null)
		{
			/**
			 * Task thats invokes <code>Administrator#clientLoggedIn</code> method.
			 */
			class ClientLoggedInTask extends DefaultGroupedRunnable
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
					//final int MAX_RETRIES = 3;

					//for (int retries = 0; retries < MAX_RETRIES; retries++)
					{
						try
						{
							((Administrator)administratorInfo.getClient()).clientLoggedIn(clientInfo, timeStamp, executionId);
							//break;
						}
						catch (RemoteTransientException re)
						{
							logger.log(Level.WARNING, "RemoteTransientException caught while invoking 'Administrator.clientLoggedIn' on "+administratorInfo+".", re);
							cancelAll();
						}
						catch (RemoteException re)
						{
							logger.log(Level.WARNING, "RemoteException caught while invoking 'Administrator.clientLoggedIn' on "+administratorInfo+".", re);
						}
					}
				}
			}


			// spawn new task which surely does not block
			for (int i = 0; i < admins.length; i++)
			{
				GroupedRunnable task = new ClientLoggedInTask(admins[i], clientInfo);
				if (admins[i].getClient() instanceof SynchronousAdministrator)
					task.run();
				else
					registerGroupedNotifyTaks(admins[i].getClient(), task);
					//threadPool.execute(task);
			}

		}

	}

	/**
	 * Notifies administrators about newly logged container.
	 * @param	containerInfo	newly logged container, non-<code>null</code>
	 */
	private void notifyContainerLogin(final ContainerInfo containerInfo, final long timeStamp, final long executionId)
	{
		assert (containerInfo != null);

		// array of administrators to be notified
		ClientInfo[] admins = getAdministrators(containerInfo.getHandle());

		if (admins != null)
		{
			/**
			 * Task thats invokes <code>Administrator#containerLoggedIn</code> method.
			 */
			class ContainerLoggedInTask  extends DefaultGroupedRunnable
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
					//final int MAX_RETRIES = 3;

					//for (int retries = 0; retries < MAX_RETRIES; retries++)
					{
						try
						{
							((Administrator)administratorInfo.getClient()).containerLoggedIn(containerInfo, timeStamp, executionId);
							//break;
						}
						catch (RemoteTransientException re)
						{
							logger.log(Level.WARNING, "RemoteTransientException caught while invoking 'Administrator.containerLoggedIn' on "+administratorInfo+".", re);
							cancelAll();
						}
						catch (RemoteException re)
						{
							logger.log(Level.WARNING, "RemoteException caught while invoking 'Administrator.containerLoggedIn' on "+administratorInfo+".", re);
						}
					}
				}
			}


			// spawn new task which surely does not block
			for (int i = 0; i < admins.length; i++)
			{
				GroupedRunnable task = new ContainerLoggedInTask(admins[i], containerInfo);
				if (admins[i].getClient() instanceof SynchronousAdministrator)
					task.run();
				else
					registerGroupedNotifyTaks(admins[i].getClient(), task);
					//threadPool.execute(task);
			}

		}

	}



	/**
	 * Notifies administrators about client logging out.
	 * @param	client	client logging out, non-<code>null</code>
	 */
	private void notifyClientLogout(final ClientInfo clientInfo, final long timeStamp)
	{
		assert (clientInfo != null);

		// array of administrators to be notified
		ClientInfo[] admins = getAdministrators(clientInfo.getHandle());

		if (admins != null)
		{

			/**
			 * Task thats invokes <code>Administrator#clientLoggedOut</code> method.
			 */
			class ClientLoggedOutTask extends DefaultGroupedRunnable
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
					//final int MAX_RETRIES = 3;

					//for (int retries = 0; retries < MAX_RETRIES; retries++)
					{
						try
						{
							((Administrator)administratorInfo.getClient()).clientLoggedOut(clientInfo.getHandle(), timeStamp);
							//break;
						}
						catch (RemoteTransientException re)
						{
							logger.log(Level.WARNING, "RemoteTransientException caught while invoking 'Administrator.clientLoggedOut' on "+administratorInfo+".", re);
							cancelAll();
						}
						catch (RemoteException re)
						{
							logger.log(Level.WARNING, "RemoteException caught while invoking 'Administrator.clientLoggedOut' on "+administratorInfo+".", re);
						}
					}
				}
			}


			// spawn new task which surely does not block
			for (int i = 0; i < admins.length; i++)
			{
				GroupedRunnable task = new ClientLoggedOutTask(admins[i], clientInfo);
				if (admins[i].getClient() instanceof SynchronousAdministrator)
					task.run();
				else
					registerGroupedNotifyTaks(admins[i].getClient(), task);
					//threadPool.execute(task);
			}

		}

	}


	/**
	 * Notifies administrators about container logging out.
	 * @param	containerInfo	container logging out, non-<code>null</code>
	 */
	private void notifyContainerLogout(final ContainerInfo containerInfo, final long timeStamp)
	{
		assert (containerInfo != null);

		// array of administrators to be notified
		ClientInfo[] admins = getAdministrators(containerInfo.getHandle());

		if (admins != null)
		{

			/**
			 * Task thats invokes <code>Administrator#containerLoggedIn</code> method.
			 */
			class ContainerLoggedOutTask extends DefaultGroupedRunnable
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
					//final int MAX_RETRIES = 3;

					//for (int retries = 0; retries < MAX_RETRIES; retries++)
					{
						try
						{
							((Administrator)administratorInfo.getClient()).containerLoggedOut(containerInfo.getHandle(), timeStamp);
							//break;
						}
						catch (RemoteTransientException re)
						{
							logger.log(Level.WARNING, "RemoteTransientException caught while invoking 'Administrator.containerLoggedOut' on "+administratorInfo+".", re);
							cancelAll();
						}
						catch (RemoteException re)
						{
							logger.log(Level.WARNING, "RemoteException caught while invoking 'Administrator.containerLoggedOut' on "+administratorInfo+".", re);
						}
					}
				}
			}

			// spawn new task which surely does not block
			for (int i = 0; i < admins.length; i++)
			{
				GroupedRunnable task = new ContainerLoggedOutTask(admins[i], containerInfo);
				if (admins[i].getClient() instanceof SynchronousAdministrator)
					task.run();
				else
					registerGroupedNotifyTaks(admins[i].getClient(), task);
					//threadPool.execute(task);
			}

		}

	}

	/**
	 * Notifies containers to disconnect or shutdown.
	 * @param	code	code to be sent to container, if <code>0</code> disconnect method will be called.
	 */
	private void notifyContainerDisconnectShutdown(int code)
	{

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
					//final int MAX_RETRIES = 3;

					//for (int retries = 0; retries < MAX_RETRIES; retries++)
					{
						try
						{
							(containerInfo.getContainer()).shutdown(code);
							//break;
						}
						catch (RemoteException re)
						{
							logger.log(Level.WARNING, "RemoteException caught while invoking 'Container.shutdown' on "+containerInfo+".", re);
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
					//final int MAX_RETRIES = 3;

					//for (int retries = 0; retries < MAX_RETRIES; retries++)
					{
						try
						{
							(containerInfo.getContainer()).disconnect();
							//break;
						}
						catch (RemoteException re)
						{
							logger.log(Level.WARNING, "RemoteException caught while invoking 'Container.disconnect' on "+containerInfo+".", re);
						}
					}
				}
			}

			// spawn new task which surely does not block
			for (int i = 0; i < acts.length; i++)
			{
				Runnable task;
				if (code == 0)
					task = new ContainerDisconnectTask(acts[i]);
				else
					task = new ContainerShutdownTask(acts[i], code);
				threadPool.execute(task);
			}

		}

	}

	/**
	 * Informs containers abouts its component shutdown order.
	 * @param	containerInfo	container to inform
	 * @param	handles			ordered list of component handles
	 */
	private void notifyContainerShutdownOrder(ContainerInfo containerInfo, int[] handles)
	{
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
				//final int MAX_RETRIES = 3;

				//for (int retries = 0; retries < MAX_RETRIES; retries++)
				{
					try
					{
						(containerInfo.getContainer()).set_component_shutdown_order(handles);
						//break;
					}
					catch (RemoteException re)
					{
						logger.log(Level.WARNING, "RemoteException caught while invoking 'Container.set_component_shutdown_order' on "+containerInfo+".", re);
					}
				}
			}
		}


		// spawn new task which surely does not block
		threadPool.execute(new ContainerSetShutdownOrderTask(containerInfo, handles));

	}

    /**
     * Notifies clients to disconnect or shutdown.
     */
    private void notifyClientDisconnectShutdown()
    {

    	// array of clients to be notified
    	ClientInfo[] clts = getClientInfo();

    	if (clts != null)
    	{

    		/**
    		 * Task thats invokes <code>Client#disconnect</code> method.
    		 */
    		class ClientDisconnectTask extends DefaultGroupedRunnable
    		{
    			private ClientInfo clientInfo;

    			public ClientDisconnectTask(ClientInfo clientInfo)
    			{
    				this.clientInfo = clientInfo;
    			}

    			public void run()
    			{
    				//final int MAX_RETRIES = 3;

    				//for (int retries = 0; retries < MAX_RETRIES; retries++)
    				{
    					try
    					{
    						(clientInfo.getClient()).disconnect();
    						//break;
    					}
						catch (RemoteTransientException re)
						{
							logger.log(Level.WARNING, "RemoteTransientException caught while invoking 'Client.disconnect' on "+clientInfo+".", re);
							cancelAll();
						}
    					catch (RemoteException re)
    					{
    						logger.log(Level.WARNING, "RemoteException caught while invoking 'Client.disconnect' on "+clientInfo+".", re);
    					}
    				}
    			}
    		}

    		// spawn new task which surely does not block
    		for (int i = 0; i < clts.length; i++)
    		{
   				GroupedRunnable task = new ClientDisconnectTask(clts[i]);
   				registerGroupedNotifyTaks(clts[i].getClient(), task);
   				//threadPool.execute(task);
    		}

    	}

    }



	/**
	 * Notifies administrators about Component request.
	 * @param	requestors	array of clients requesting the Component, non-<code>null</code>
	 * @param	components		array of requested the components, non-<code>null</code>
	 */
	private void notifyComponentRequested(int[] requestors, int[] components, final long timeStamp)
	{
		assert (requestors != null);
		assert (components != null);

		// array of administrators to be notified
		ClientInfo[] admins = getAdministrators(0);

		if (admins != null)
		{

			/**
			 * Task thats invokes <code>Administrator#components_requested</code> method.
			 */
			class ComponentRequestedTask extends DefaultGroupedRunnable
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
					//final int MAX_RETRIES = 3;

					//for (int retries = 0; retries < MAX_RETRIES; retries++)
					{
						try
						{
							((Administrator)administratorInfo.getClient()).components_requested(requestors, components, timeStamp);
							//break;
						}
						catch (RemoteTransientException re)
						{
							logger.log(Level.WARNING, "RemoteTransientException caught while invoking 'Administrator.components_requested' on "+administratorInfo+".", re);
							cancelAll();
						}
						catch (RemoteException re)
						{
							logger.log(Level.WARNING, "RemoteException caught while invoking 'Administrator.components_requested' on "+administratorInfo+".", re);
						}
					}
				}
			}


			// spawn new task which surely does not block
			for (int i = 0; i < admins.length; i++)
			{
				GroupedRunnable task = new ComponentRequestedTask(admins[i], requestors, components);
				if (admins[i].getClient() instanceof SynchronousAdministrator)
					task.run();
				else
					registerGroupedNotifyTaks(admins[i].getClient(), task);
					//threadPool.execute(task);
			}

		}

	}

	/**
	 * Notifies administrators about Component activation.
	 * @param	componentInfo	activated component info, non-<code>null</code>
	 */
	private void notifyComponentActivated(final ComponentInfo componentInfo, final long timeStamp, final long executionId)
	{
		assert (componentInfo != null);

		// array of administrators to be notified
		ClientInfo[] admins = getAdministrators(0);

		if (admins != null)
		{

			/**
			 * Task thats invokes <code>Administrator#component_activated</code> method.
			 */
			class ComponentActivatedTask extends DefaultGroupedRunnable
			{
				private ClientInfo administratorInfo;

				public ComponentActivatedTask(ClientInfo administratorInfo)
				{
					this.administratorInfo = administratorInfo;
				}

				public void run()
				{
					//final int MAX_RETRIES = 3;

					//for (int retries = 0; retries < MAX_RETRIES; retries++)
					{
						try
						{
							((Administrator)administratorInfo.getClient()).component_activated(componentInfo, timeStamp, executionId);
							//break;
						}
						catch (RemoteTransientException re)
						{
							logger.log(Level.WARNING, "RemoteTransientException caught while invoking 'Administrator.component_activated' on "+administratorInfo+".", re);
							cancelAll();
						}
						catch (RemoteException re)
						{
							logger.log(Level.WARNING, "RemoteException caught while invoking 'Administrator.component_activated' on "+administratorInfo+".", re);
						}
					}
				}
			}


			// spawn new task which surely does not block
			for (int i = 0; i < admins.length; i++)
			{
				GroupedRunnable task = new ComponentActivatedTask(admins[i]);
				if (admins[i].getClient() instanceof SynchronousAdministrator)
					task.run();
				else
					registerGroupedNotifyTaks(admins[i].getClient(), task);
					//threadPool.execute(task);
			}
		}

	}

	/**
	 * Notifies administrators about Component deactivation.
	 * @param	component	deactivated component handle, non-<code>0</code>
	 */
	private void notifyComponentDeactivated(final int handle, final long timeStamp)
	{
		assert (handle != 0);

		// array of administrators to be notified
		ClientInfo[] admins = getAdministrators(0);

		if (admins != null)
		{

			/**
			 * Task thats invokes <code>Administrator#component_deactivated</code> method.
			 */
			class ComponentDeactivatedTask extends DefaultGroupedRunnable
			{
				private ClientInfo administratorInfo;

				public ComponentDeactivatedTask(ClientInfo administratorInfo)
				{
					this.administratorInfo = administratorInfo;
				}

				public void run()
				{
					//final int MAX_RETRIES = 3;

					//for (int retries = 0; retries < MAX_RETRIES; retries++)
					{
						try
						{
							((Administrator)administratorInfo.getClient()).component_deactivated(handle, timeStamp);
							//break;
						}
						catch (RemoteTransientException re)
						{
							logger.log(Level.WARNING, "RemoteTransientException caught while invoking 'Administrator.component_deactivated' on "+administratorInfo+".", re);
							cancelAll();
						}
						catch (RemoteException re)
						{
							logger.log(Level.WARNING, "RemoteException caught while invoking 'Administrator.component_deactivated' on "+administratorInfo+".", re);
						}
					}
				}
			}


			// spawn new task which surely does not block
			for (int i = 0; i < admins.length; i++)
			{
				GroupedRunnable task = new ComponentDeactivatedTask(admins[i]);
				if (admins[i].getClient() instanceof SynchronousAdministrator)
					task.run();
				else
					registerGroupedNotifyTaks(admins[i].getClient(), task);
					//threadPool.execute(task);
			}

		}

	}

	/**
	 * Notifies administrators about Component request.
	 * @param	requestors	array of clients requesting the Component, non-<code>null</code>
	 * @param	components		array of requested the components, non-<code>null</code>
	 */
	private void notifyComponentReleased(int[] requestors, int[] components, final long timeStamp)
	{
		assert (requestors != null);
		assert (components != null);

		// array of administrators to be notified
		ClientInfo[] admins = getAdministrators(0);

		if (admins != null)
		{

			/**
			 * Task thats invokes <code>Administrator#components_requested</code> method.
			 */
			class ComponentReleasedTask extends DefaultGroupedRunnable
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
					//final int MAX_RETRIES = 3;

					//for (int retries = 0; retries < MAX_RETRIES; retries++)
					{
						try
						{
							((Administrator)administratorInfo.getClient()).components_released(requestors, components, timeStamp);
							//break;
						}
						catch (RemoteTransientException re)
						{
							logger.log(Level.WARNING, "RemoteTransientException caught while invoking 'Administrator.components_released' on "+administratorInfo+".", re);
							cancelAll();
						}
						catch (RemoteException re)
						{
							logger.log(Level.WARNING, "RemoteException caught while invoking 'Administrator.components_released' on "+administratorInfo+".", re);
						}
					}
				}
			}


			// spawn new task which surely does not block
			for (int i = 0; i < admins.length; i++)
			{
				GroupedRunnable task = new ComponentReleasedTask(admins[i], requestors, components);
				if (admins[i].getClient() instanceof SynchronousAdministrator)
					task.run();
				else
					registerGroupedNotifyTaks(admins[i].getClient(), task);
					//threadPool.execute(task);
			}

		}

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

		// array of clients to be notified
		ClientInfo[] clients = getClients(excludeClient, clientHandles);

		if (clients != null)
		{

			/**
			 * Task thats invokes <code>Client#components_available</code> method.
			 */
			class ComponentAvailableTask extends DefaultGroupedRunnable
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
					//final int MAX_RETRIES = 3;

					//for (int retries = 0; retries < MAX_RETRIES; retries++)
					{
						try
						{
							clientInfo.getClient().components_available(info);
							//break;
						}
						catch (RemoteTransientException re)
						{
							logger.log(Level.WARNING, "RemoteTransientException caught while invoking 'Client.components_available' on "+clientInfo+".", re);
							cancelAll();
						}
						catch (RemoteException re)
						{
							logger.log(Level.WARNING, "RemoteException caught while invoking 'Client.components_available' on "+clientInfo+".", re);
						}
					}
				}
			}


			// spawn new task which surely does not block
			for (int i = 0; i < clients.length; i++)
			{
				GroupedRunnable task = new ComponentAvailableTask(clients[i], info);
				if (clients[i].getClient() instanceof SynchronousAdministrator)
					task.run();
				else
					registerGroupedNotifyTaks(clients[i].getClient(), task);
					//threadPool.execute(task);
			}

		}

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

		// array of clients to be notified
		ClientInfo[] clients = getClients(excludeClient, clientHandles);

		if (clients != null)
		{

			/**
			 * Task thats invokes <code>Client#components_unavailable</code> method.
			 */
			class ComponentUnavailableTask extends DefaultGroupedRunnable
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
					//final int MAX_RETRIES = 3;

					//for (int retries = 0; retries < MAX_RETRIES; retries++)
					{
						try
						{
							clientInfo.getClient().components_unavailable(names);
							//break;
						}
						catch (RemoteTransientException re)
						{
							logger.log(Level.WARNING, "RemoteTransientException caught while invoking 'Client.components_unavailable' on "+clientInfo+".", re);
							cancelAll();
						}
						catch (RemoteException re)
						{
							logger.log(Level.WARNING, "RemoteException caught while invoking 'Client.components_unavailable' on "+clientInfo+".", re);
						}
					}
				}
			}


			// spawn new task which surely does not block
			for (int i = 0; i < clients.length; i++)
			{
				GroupedRunnable task = new ComponentUnavailableTask(clients[i], names);
				if (clients[i].getClient() instanceof SynchronousAdministrator)
					task.run();
				else
					registerGroupedNotifyTaks(clients[i].getClient(), task);
					//threadPool.execute(task);
			}

		}

	}


//	/**
//	 * Sends an message to the client.
//	 * @param	client		client to receive the message, non-<code>null</code>
//	 * @param	message		message to be sent, non-<code>null</code>
//	 * @param	messageType	type of the message, non-<code>null</code>
//	 */
//	private void sendMessage(Client client, String message, MessageType messageType)
//	{
//	    sendMessage(client, message, messageType, (short)0);
//	}

		/**
		 * Task thats invokes <code>Client#message</code> method.
		 */
		private class ClientMessageTask implements Runnable
		{
			private Client client;
			private String message;
			private MessageType messageType;
		        private short messageID;

			public ClientMessageTask(Client client, String message, MessageType messageType, short messageID)
			{
				this.client = client;
				this.message = message;
				this.messageType = messageType;
				this.messageID = messageID;
			}

			public void run()
			{
				//final int MAX_RETRIES = 3;

				//for (int retries = 0; retries < MAX_RETRIES; retries++)
				{
					try
					{
					        if (messageID > 0)
					        {
						        client.taggedmessage(messageType, messageID, message);
						} else {
						        client.message(messageType, message);
						}
						//break;
					}
					catch (Throwable re)
					{
						logger.log(Level.WARNING, "Exception caught while invoking 'Client.message' on "+client+".", re);
					}
				}
			}
		}

	/**
	 * Sends an message to the client.
	 * @param	client		client to receive the message, non-<code>null</code>
	 * @param	message		message to be sent, non-<code>null</code>
	 * @param	messageType	type of the message, non-<code>null</code>
	 * @param       messageID       identifier for this message
	 */
	private void sendMessage(Client client, String message, MessageType messageType, short messageID)
	{
		assert (client != null);
		assert (message != null);
		assert (messageType != null);

		boolean newClient = false;
		synchronized (clientMessageQueue) {
			LinkedList<ClientMessageTask> list = clientMessageQueue.get(client);
			if (list == null) {
				list = new LinkedList<ClientMessageTask>();
				clientMessageQueue.put(client, list);
				newClient = true;
			}
			list.addLast(new ClientMessageTask(client, message, messageType, messageID));
		}
		
		if (newClient)
		{
			class ClientQueuMessageTask implements Runnable
			{
				private Client client;

				public ClientQueuMessageTask(Client client)
				{
					this.client = client;
				}

				public void run()
				{
					boolean moreMessages = true;
					ClientMessageTask clientTask;
					while (moreMessages) {
						synchronized (clientMessageQueue) {
							LinkedList<ClientMessageTask> list = clientMessageQueue.get(client);
							if (list == null)
								break;
							clientTask = list.removeFirst();
							moreMessages = !list.isEmpty();
							if (!moreMessages) {
								clientMessageQueue.remove(client);
							}
						}
						// exception safe
						if (clientTask != null) clientTask.run();
					}
				}
			}

			// spawn new task which surely does not block
			threadPool.execute(new ClientQueuMessageTask(client));
		}
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

		try {
			// check if already shutdown
			if (id != this.getHandle() && shutdown.get())
			{
				// already shutdown
				AcsJNoPermissionEx npe = new AcsJNoPermissionEx();
				npe.setID(HandleHelper.toString(id));
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
					componentsLock.lock();
					try {
						if (components.isAllocated(handle))
						{
							ComponentInfo info = (ComponentInfo)components.get(handle);
							if (info != null && info.getHandle() == id)
								invalidHandle = false;
							grantedRights = AccessRights.REGISTER_COMPONENT;
						}
					} finally {
						componentsLock.unlock();
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
				npe.setID(HandleHelper.toString(id));
				
				HandleMonitorEntry hme = getHandleReleaseLog(id);
				if (hme != null) {
					final String timeISO = IsoDateFormat.formatDate(new Date(hme.timestamp));
					switch (hme.reason)
					{
						case REMOVED:
							npe.setReason("Invalid handle, handle was properly removed at " + timeISO + ".");
							break;
						case TIMEOUT:
							npe.setReason("Invalid handle, handle was removed due to timeout at " + timeISO + ".");
							break;
						case DISAPPEARED:
							npe.setReason("Invalid handle, handle was removed due to client/container/component disappearing at " + timeISO + ".");
							break;
					}
				}
				else
					npe.setReason("Invalid handle, handle was never known.");
				
				throw npe;
			}

			if ((grantedRights & requiredRights) != requiredRights)
			{
				// NO_PERMISSION
				AcsJNoPermissionEx npe = new AcsJNoPermissionEx();
				npe.setID(HandleHelper.toString(id));
				npe.setReason("Insufficient rights.");
				throw npe;
			}
		} catch (AcsJNoPermissionEx ex) {
			logger.log(AcsLogLevel.DELOUSE, "securityCheck fails with AcsJNoPermissionEx:", ex);
			throw ex;
		}

	}

	/**
	 * Get client info. for specified id of <code>Client</code> or <code>Administrator</code>.
	 *
	 * @param	id	handle of the client whose info. should be returned
	 * @param	returns	requested info, <code>null</code> if client with requested handle does not exits
	 */
	public ClientInfo getClientInfo(int id)
	{

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

		return info;
	}

	/**
	 * Get client info. (no sync version) for specified id of <code>Client</code> or <code>Administrator</code>.
	 *
	 * @param	id	handle of the client whose info. should be returned
	 * @param	returns	requested info, <code>null</code> if client with requested handle does not exits
	 */
	public ClientInfo noSyncGetClientInfo(int id)
	{

		// parse handle part
		int handle	= id & HANDLE_MASK;

		// info to be returned
		ClientInfo info = null;

		switch	(id & TYPE_MASK)
		{
			case CLIENT_MASK:
				{
					if (clients.isAllocated(handle))
						info = (ClientInfo)clients.get(handle);
				}
				break;

			case ADMINISTRATOR_MASK:
				{
					if (administrators.isAllocated(handle))
						info = (ClientInfo)administrators.get(handle);
				}
				break;
		}

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

		// parse handle part
		int handle	= id & HANDLE_MASK;

		// info to be returned
		ContainerInfo info = null;

		synchronized (containers)
		{
			if (containers.isAllocated(handle))
				info = (ContainerInfo)containers.get(handle);
		}

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

		// parse handle part
		int handle	= id & HANDLE_MASK;

		// info to be returned
		ComponentInfo info = null;

		componentsLock.lock();
		try {
			if (components.isAllocated(handle))
				info = (ComponentInfo)components.get(handle);
		} finally {
			componentsLock.unlock();
		}

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
            throws AcsJCannotGetComponentEx
	{
		return internalRequestComponent(requestor, curl, status, true);
	}

	/**
	 * Internal method for requesting components.
	 * @param	requestor	requestor of the component.
	 * @param	curl	curl of the component to be requested.
	 * @param	status	status of the component.
	 * @param	activate	<code>true</code> if component has to be activated
	 * @return	component		requested component.
	 */
	private Component internalRequestComponent(int requestor, URI curl, StatusHolder status, boolean activate)
            throws AcsJCannotGetComponentEx
	{

		// extract unique name
		String name = extractName(curl);

		try {
			checkCyclicDependency(requestor, name);
		} catch (AcsJCyclicDependencyDetectedEx e) {
			AcsJCannotGetComponentEx cgce = new AcsJCannotGetComponentEx(e);
			cgce.setCURL(name);
			throw cgce;
		}

		// try to acquire lock
		long lockTimeoutMillis = getLockTimeout();
		boolean lockAcquired = acquireSynchronizationObject(name, lockTimeoutMillis);
		if (lockAcquired)
		{
			boolean releaseRWLock = true;
			try
			{
				// try to acquire activation readers lock first
				// NOTE: the locks are NOT reentrant
				activationPendingRWLock.readLock().lock();

				// Let AcsJCannotGetComponentEx fly up
				ComponentInfo componentInfo = null;
				try
				{
					componentInfo = internalNoSyncRequestComponent(requestor, name, null, null, null, RELEASE_TIME_UNDEFINED, status, activate);
				}
				catch(AcsJComponentSpecIncompatibleWithActiveComponentEx ciwace)
				{
					status.setStatus(ComponentStatus.COMPONENT_NOT_ACTIVATED);
					AcsJCannotGetComponentEx cgce = new AcsJCannotGetComponentEx(ciwace);
					cgce.setCURL(name);
					throw cgce;
					
				}

				if (componentInfo != null) {
					return componentInfo.getComponent();
				}
				else {
					return null;
				}
			}
			finally
			{
				if (releaseRWLock) {
					activationPendingRWLock.readLock().unlock();
				}
				releaseSynchronizationObject(name);
			}
		}
		else
		{
			String msg = "Failed to obtain synchronization lock for component '" + name + "' within '" + lockTimeoutMillis + "' ms, possible deadlock." ;
			logger.fine(msg);
			NoResourcesException nre = new NoResourcesException(msg);
			throw nre;
		}
	}

	/**
	 * Check for cyclic dependency between components, if detected path is returned.
	 * @param requestor	handle of requestor component
	 * @param requested	handle of requested component
	 * @return if cycle is detected then path is returned, otherwise <code>null</code>
	 */
	private ArrayList<ComponentInfo> doCycleCheck(int requestor, int requested)
	{
		componentsLock.lock();
		try {
			ComponentInfo info = (ComponentInfo)components.get(requested & HANDLE_MASK);
			if (info == null)
				return null;

			if (requested == requestor)
			{
				// detected
				ArrayList<ComponentInfo> list = new ArrayList<ComponentInfo>();
				list.add(info);
				return list;
			}

			int[] subcomponents = info.getComponents().toArray();
			for (int i = 0; i < subcomponents.length; i++)
			{
				ArrayList<ComponentInfo> list = doCycleCheck(requestor, subcomponents[i]);
				if (list != null) {
					list.add(info);
					return list;
				}
			}
		} finally {
			componentsLock.unlock();
		}
		return null;
	}

	/**
	 * Check for cyclic dependency between components, if detected <code>NoResourcesException</code> exception is thrown.
	 * @param requestor
	 * @param requestedComponentName
	 */
	private void checkCyclicDependency(int requestor, String requestedComponentName)
	    throws AcsJCyclicDependencyDetectedEx
	{

		// check only if component is requesting component
		if ((requestor & TYPE_MASK) != COMPONENT_MASK)
			return;

		// check if requested component is already activated (and pending activations)
		ComponentInfo componentInfo = null;
		componentsLock.lock();
		try {
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
				pendingComponentInfo = pendingActivations.get(requestedComponentName);
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

			ArrayList<ComponentInfo> pathList = doCycleCheck(requestor, componentInfo.getHandle());
			// no dependency detected
			if (pathList == null)
				return;

			// stringify
			StringBuffer pathBuffer = new StringBuffer();
			for (int i = pathList.size()-1; i >= 0; i--)
				pathBuffer.append(pathList.get(i).getName()).append(" -> ");
			pathBuffer.append(componentInfo.getName());
			// TODO @todo no pathBuffer is used, printed-out
			
			// If we get to this point there is a cyclical dependency and we throw the exception
			
			// not an owner
			AcsJCyclicDependencyDetectedEx cde = new AcsJCyclicDependencyDetectedEx();
			cde.setCURL(requestedComponentName);
			cde.setRequestor(requestor);
			throw cde;
		} finally {
			componentsLock.unlock();
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
	    throws  AcsJCannotGetComponentEx, AcsJSyncLockFailedEx,
	            AcsJComponentSpecIncompatibleWithActiveComponentEx
	{
		AcsJCannotGetComponentEx bcex = null;
		
		if (name == null) {
			bcex = new AcsJCannotGetComponentEx();
			logger.log(Level.SEVERE, "Cannot activate component with NULL name.", bcex);
		    throw bcex;
		}
		if (status == null) {
			bcex = new AcsJCannotGetComponentEx();
			logger.log(Level.SEVERE, "Component " + name + " has NULL status.", bcex);
			throw bcex;
		}    

		try {
			checkCyclicDependency(requestor, name);
		} catch (AcsJCyclicDependencyDetectedEx e) {
			AcsJCannotGetComponentEx cgce = new AcsJCannotGetComponentEx(e);
			cgce.setCURL(name);
			throw cgce;
		}

		// try to acquire lock
		boolean lockAcquired = acquireSynchronizationObject(name, getLockTimeout());
		if (lockAcquired)
		{
			boolean releaseRWLock = true;
			try
			{
				// try to acquire activation readers lock first
				// NOTE: the locks are NOT reentrant
				activationPendingRWLock.readLock().lock();

				// AcsJComponentSpecIncompatibleWithActiveComponentEx flies up
				return internalNoSyncRequestComponent(requestor, name, type, code, containerName, keepAliveTime, status, activate);
			}
			finally
			{
				if (releaseRWLock)
					activationPendingRWLock.readLock().unlock();
				releaseSynchronizationObject(name);
			}
		}
		else
		{
			AcsJSyncLockFailedEx slfe = new AcsJSyncLockFailedEx();
			slfe.setCURL(name);
			slfe.setRequestor(requestor);
			throw slfe;
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
	    throws  AcsJCannotGetComponentEx,
                AcsJComponentSpecIncompatibleWithActiveComponentEx
	{

		AcsJCannotGetComponentEx bcex = null;
		
		if (name == null) {
		    bcex = new AcsJCannotGetComponentEx();
		    logger.log(Level.SEVERE,"Cannot activate component with NULL name.",bcex);
		    throw bcex;
		}
		if (status == null) {
		    bcex = new AcsJCannotGetComponentEx();
		    logger.log(Level.SEVERE,"Component " + name + " has NULL status.",bcex);
	    	throw bcex;
		}

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

		componentsLock.lock();
		try {
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
						AcsJComponentSpecIncompatibleWithActiveComponentEx ciwace =
							new AcsJComponentSpecIncompatibleWithActiveComponentEx();
						ciwace.setCURL(componentInfo.getName());
						ciwace.setComponentType(componentInfo.getType());
						ciwace.setComponentCode(componentInfo.getCode() != null ? componentInfo.getCode() : "<unknown>");
						ciwace.setContainerName(containerInfo != null ? containerInfo.getName() : "<none>");
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
						notifyComponentRequested(new int[] { requestor }, new int[] { componentInfo.getHandle() }, System.currentTimeMillis());

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
		} finally {
			componentsLock.unlock();
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
				bcex = new AcsJCannotGetComponentEx();
				logger.log(Level.SEVERE,"Failed to reactivate dynamic component '"+componentInfo+"'.",bcex);
				status.setStatus(ComponentStatus.COMPONENT_DOES_NO_EXIST);
				throw bcex;
			}
			else
			{
				// reread info
				type = componentInfo.getType();
				code = componentInfo.getCode();
				containerName = componentInfo.getDynamicContainerName();
				keepAliveTime = componentInfo.getKeepAliveTime();
			}
		}
		// is CDB lookup needed
		else if (!isOtherDomainComponent &&
				(type == null || code == null || containerName == null))
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
					bcex = new AcsJCannotGetComponentEx();
					status.setStatus(ComponentStatus.COMPONENT_DOES_NO_EXIST);
					logger.log(Level.SEVERE,"Component "+ name +" not found in CDB",bcex);
					throw bcex;
				}

			}

			if (code == null)
			{
				code = readStringCharacteristics(dao, name+"/Code");
				if (code == null)
				{
					bcex = new AcsJCannotGetComponentEx();
					logger.log(Level.SEVERE,"Misconfigured CDB, there is no code of component '"+name+"' defined.",bcex);
					status.setStatus(ComponentStatus.COMPONENT_DOES_NO_EXIST);
					throw bcex;
				}
			}

			if (type == null)
			{
				type = readStringCharacteristics(dao, name+"/Type");
				if (type == null)
				{
					bcex = new AcsJCannotGetComponentEx();
					logger.log(Level.SEVERE,"Misconfigured CDB, there is no type of component '"+name+"' defined.",bcex);
					status.setStatus(ComponentStatus.COMPONENT_DOES_NO_EXIST);
					throw bcex;
				}
			}

			if (containerName == null)
			{
				containerName = readStringCharacteristics(dao, name+"/Container");
				if (containerName == null)
				{
					bcex = new AcsJCannotGetComponentEx();
					logger.log(Level.SEVERE,"Misconfigured CDB, there is no container of component '"+name+"' defined.",bcex);
					status.setStatus(ComponentStatus.COMPONENT_DOES_NO_EXIST);
					throw bcex;
				}
			}

			if (keepAliveTime == RELEASE_TIME_UNDEFINED)
			{
				// defaults to 0 == RELEASE_IMMEDIATELY
				keepAliveTime = readLongCharacteristics(dao, name+"/KeepAliveTime", RELEASE_IMMEDIATELY, true);
			}

		}

		// we have keepAlive missing, try to get it
		if (keepAliveTime == RELEASE_TIME_UNDEFINED)
		{
			DAOProxy dao = getComponentsDAOProxy();
			if (dao != null)
			{
				// defaults to 0 == RELEASE_IMMEDIATELY
				keepAliveTime = readLongCharacteristics(dao, name+"/KeepAliveTime", RELEASE_IMMEDIATELY, true);
			}
			else
			{
				// this is a case where all data for dynamic component is specified and there is not entry in CDB
				keepAliveTime = RELEASE_IMMEDIATELY;
			}
		}

		// read impl. language.
		DAOProxy dao = getComponentsDAOProxy();
		String componentImplLang = null;
		if (dao != null)
		{
			// silent read
			componentImplLang = readStringCharacteristics(dao, name+"/ImplLang", true);
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
			    if (remoteManager == null) {
			    	bcex = new AcsJCannotGetComponentEx();
			    	logger.log(Level.WARNING, "Failed to obtain manager for domain '" + domainName + "'.", bcex);
			        throw bcex;
			    }
		    } catch (Throwable th) {
				bcex = new AcsJCannotGetComponentEx(th);
				logger.log(Level.WARNING, "Failed to obtain non-local manager required by component '"+name+"'.", bcex);
				status.setStatus(ComponentStatus.COMPONENT_NOT_ACTIVATED);
				throw bcex;
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
				bcex = new AcsJCannotGetComponentEx();
				logger.log(Level.SEVERE, "Container '"+containerName+"' required by component '"+name+"' is not logged in.",bcex);
				status.setStatus(ComponentStatus.COMPONENT_NOT_ACTIVATED);
				throw bcex;
			}
		}
		
		// check container vs component ImplLang
		ImplLang containerImplLang = containerInfo.getImplLang();
		if (containerImplLang != null && containerImplLang != ImplLang.not_specified) {
			if (componentImplLang != null && ImplLang.fromString(componentImplLang) != containerImplLang)
			{
				AcsJCannotGetComponentEx af = new AcsJCannotGetComponentEx();
				logger.log(Level.WARNING, "Component and container implementation language do not match (" + componentImplLang + " != " + containerImplLang.name() + ")", af);
				throw af;
			}
		}

		//
		// get handle
		//

		// obtain handle
		componentsLock.lock();
		try {
			// only preallocate (if necessary)
			if (!reactivate)
			{
				// !!! ACID 2
				Integer objHandle = (Integer)executeCommand(new ComponentCommandPreallocate());
				h = (objHandle == null) ? 0 : objHandle.intValue();
				//h = components.preallocate();
			}
			
			// failed to obtain handle
			if (h == 0)
			{
				AcsJCannotGetComponentEx af = new AcsJCannotGetComponentEx();
				logger.log(Level.WARNING, "Preallocation of new handle failed, too many registered components.", af);
				throw af;
			}

			// create temporary ComponentInfo - to allow hierarchical components
			if (!reactivate)
			{
				ComponentInfo data = new ComponentInfo(h | COMPONENT_MASK, name, type, code, null);
				data.setKeepAliveTime(keepAliveTime);
				// !!! ACID
				executeCommand(new ComponentCommandSet(h, data));

				// add to pending activation list
				synchronized (pendingActivations)
				{
					pendingActivations.put(name, data);
				}

				// add component to client component list to allow dependency checks
				if ((requestor & TYPE_MASK) == COMPONENT_MASK)
					addComponentOwner(data.getHandle(), requestor);
			}

		} finally {
			componentsLock.unlock();
		}

		//
		// invoke get_component
		//

		componentInfo = null;
		long executionId = 0;
		long activationTime = 0;
		
		boolean timeoutError = false;
		
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
			    activationTime = System.currentTimeMillis();
			    
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
				    //    throw new RemoteException("Failed to obtain component info for '"+name+"' from remote manager.");
			    }
			    //else
			    //    throw new RemoteException("Failed to obtain component '"+name+"' from remote manager, status: " + statusHolder.getStatus() + ".");
			}
			catch (Throwable ex)
			{
				bcex = new AcsJCannotGetComponentEx(ex);
				logger.log(Level.SEVERE, "Failed to obtain component '"+name+"' from remote manager.", bcex);
				timeoutError = (ex instanceof TimeoutRemoteException);
			}
		}
		else
		{
			//
			// invoke get_component on container
			//

			// log info
			logger.log(Level.INFO,"Activating component '"+name+"' (" + HandleHelper.toString(h | COMPONENT_MASK) + ") on container '" + containerInfo.getName() + "'.");

			boolean callSyncActivate = System.getProperties().containsKey(NAME_SYNC_ACTIVATE);
			if (containerInfo.getImplLang() == ImplLang.py)
				callSyncActivate = true;
			
			if (callSyncActivate)
				{
				// sync
				try
				{
					executionId = generateExecutionId();
					activationTime = System.currentTimeMillis();
					componentInfo = container.activate_component(h | COMPONENT_MASK, executionId, name, code, type);
				}
				catch (Throwable ex)
				{
					bcex = new AcsJCannotGetComponentEx(ex);
					logger.log(Level.SEVERE, "Failed to activate component '"+name+"' on container '"+containerName+"'.", bcex);
					timeoutError = (ex instanceof TimeoutRemoteException);
				}
			}
			else
			{
				// async
				try
				{
					executionId = generateExecutionId();
					activationTime = System.currentTimeMillis();
					
					ComponentInfoCompletionCallbackImpl callback = new ComponentInfoCompletionCallbackImpl(
							requestor, name, type,
							code, containerName, keepAliveTime, status,
							isOtherDomainComponent, isDynamicComponent, h, reactivate,
							container, containerInfo, executionId,
							activationTime);
					container.activate_component_async(h | COMPONENT_MASK, executionId, name, code, type, callback);
					
					return callback.waitUntilActivated(getLockTimeout());
				}
				catch (Throwable ex)
				{
					bcex = new AcsJCannotGetComponentEx(ex);
					logger.log(Level.SEVERE, "Failed to activate component '"+name+"' on container '"+containerName+"'.", bcex);
					throw bcex;
				}
			}
		}
		
			
		// call this immediately if bcex != null or sync call
		return internalNoSyncRequestComponentPhase2(requestor, name, type,
				code, containerName, keepAliveTime, status, bcex,
				isOtherDomainComponent, isDynamicComponent, h, reactivate,
				componentInfo, container, containerInfo, executionId,
				activationTime, timeoutError);
	
	}
	
	private class ComponentInfoCompletionCallbackImpl implements ComponentInfoCompletionCallback
	{
		int requestor;
		String name; String type; String code; String containerName;
		int keepAliveTime; StatusHolder status;
		boolean isOtherDomainComponent; boolean isDynamicComponent;
		int h; boolean reactivate;
		ComponentInfo componentInfo; Container container;
		ContainerInfo containerInfo;
		long executionId; long activationTime;
		boolean timeoutError;
		
		Throwable exception = null;
		boolean done = false;
		
		public ComponentInfoCompletionCallbackImpl(int requestor, String name,
				String type, String code, String containerName,
				int keepAliveTime, StatusHolder status,
				boolean isOtherDomainComponent, boolean isDynamicComponent,
				int h, boolean reactivate,
				Container container, ContainerInfo containerInfo,
				long executionId, long activationTime) {

			this.requestor = requestor;
			this.name = name;
			this.type = type;
			this.code = code;
			this.containerName = containerName;
			this.keepAliveTime = keepAliveTime;
			this.status = status;
			this.isOtherDomainComponent = isOtherDomainComponent;
			this.isDynamicComponent = isDynamicComponent;
			this.h = h;
			this.reactivate = reactivate;
			this.container = container;
			this.containerInfo = containerInfo;
			this.executionId = executionId;
			this.activationTime = activationTime;
		}

		public synchronized ComponentInfo waitUntilActivated(long timeToWait) throws Throwable
		{
			while (!done)
			{
				try {
				this.wait(timeToWait);
				if (!done)
					throw new TimeoutRemoteException("Activation did not finish in time.");
				} catch (InterruptedException ex) {
					exception = ex;
					break;
				}
			}
			
			if (exception != null)
				throw exception;
			return componentInfo;
		}
		
		@Override
		public synchronized void done(ComponentInfo result) {
			try
			{
				componentInfo = internalNoSyncRequestComponentPhase2(requestor, name, type,
						code, containerName, keepAliveTime, status, null,
						isOtherDomainComponent, isDynamicComponent, h, reactivate,
						result, container, containerInfo, executionId,
						activationTime, timeoutError);
			} catch (Throwable th) {
				exception = th;
			}
			finally {
				done = true;
				this.notifyAll();
			}
		}

		@Override
		public synchronized void failed(ComponentInfo result, Throwable exception) {
			try
			{
				boolean timeoutError = (exception instanceof TimeoutRemoteException);
				
				AcsJCannotGetComponentEx bcex;
				if (exception instanceof AcsJCannotGetComponentEx)
					bcex = (AcsJCannotGetComponentEx)exception;
				else
					bcex = new AcsJCannotGetComponentEx(exception);
				
				logger.log(Level.SEVERE, "Failed to activate component '"+name+"' on container '"+containerName+"'.", bcex);
				
				componentInfo = internalNoSyncRequestComponentPhase2(requestor, name, type,
						code, containerName, keepAliveTime, status, bcex,
						isOtherDomainComponent, isDynamicComponent, h, reactivate,
						result, container, containerInfo, executionId,
						activationTime, timeoutError);
			} catch (Throwable th) {
				this.exception = th;
			}
			finally {
				done = true;
				this.notifyAll();
			}
		}
		
		
	}

	private ComponentInfo internalNoSyncRequestComponentPhase2(
			int requestor,
			String name, String type, String code, String containerName,
			int keepAliveTime, StatusHolder status,
			AcsJCannotGetComponentEx bcex,
			boolean isOtherDomainComponent, boolean isDynamicComponent,
			int h, boolean reactivate,
			ComponentInfo componentInfo, Container container,
			ContainerInfo containerInfo,
			long executionId, long activationTime,
			boolean timeoutError) throws AcsJCannotGetComponentEx
	{
		// remove component from client component list, will be added later (first lots of checks has to be done)
		if ((requestor & TYPE_MASK) == COMPONENT_MASK) {
			removeComponentOwner(h | COMPONENT_MASK, requestor);
		}

		// remove from pending activation list
		synchronized (pendingActivations)
		{
			pendingActivations.remove(name);
		}

		// failed to activate
		if (componentInfo == null || componentInfo.getHandle() == 0 || componentInfo.getComponent() == null)
		{
			logger.log(Level.SEVERE,"Failed to activate component '"+name+"' (" + HandleHelper.toString(h | COMPONENT_MASK) + ").");

			componentsLock.lock();
			try {
				// !!! ACID 3
				if (!reactivate)
					executeCommand(new ComponentCommandDeallocate(h, h | COMPONENT_MASK,
											timeoutError ? WhyUnloadedReason.TIMEOUT : WhyUnloadedReason.REMOVED, true));
					//components.deallocate(h, true);
			} finally {
				componentsLock.unlock();
			}

			status.setStatus(ComponentStatus.COMPONENT_NOT_ACTIVATED);
			if (bcex == null)
			    return null;
			else
			    throw bcex;
		}

		// log info
		logger.log(Level.INFO,"Component '"+name+"' (" + HandleHelper.toString(h | COMPONENT_MASK) + ") activated successfully.");

		//
		// check type consistency
		//
		if (!isOtherDomainComponent && !componentInfo.getComponent().doesImplement(type))
		{
			// just output SEVERE message
			logger.log(Level.SEVERE,"Activated component '" + name + "' does not implement specified type '" + type + "'.");
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
		componentsLock.lock();
		try {

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
						executeCommand(new ComponentCommandDeallocate(h, componentInfo.getHandle(), WhyUnloadedReason.REMOVED, true));
						//components.deallocate(h, true);

					bcex = new AcsJCannotGetComponentEx();
					logger.log(Level.SEVERE, "Container returned another handle than given, failed to fix handle since returned handle is already allocated.", bcex);

					status.setStatus(ComponentStatus.COMPONENT_ACTIVATED);		// component is activated, but cannot be managed by the Manager
					throw bcex;
				}
				else
				{
					// handle is free, relocate handle

					ComponentInfo existingData = null;

					// !!! ACID 3
					// deallocate old
					if (!reactivate)
						executeCommand(new ComponentCommandDeallocate(h, componentInfo.getHandle(), WhyUnloadedReason.REPLACED, true));
						//components.deallocate(h, true);
					else
					{
						// !!! ACID 3
						existingData = (ComponentInfo)components.get(h);
						executeCommand(new ComponentCommandDeallocate(h, componentInfo.getHandle(), WhyUnloadedReason.REPLACED));
						//components.deallocate(h);
					}

					// !!! ACID 3
					// preallocate new
					Integer objHandle  = (Integer)executeCommand(new ComponentCommandAllocateHandle(componentHandle, true));
					//h = components.allocate(componentHandle, true);
					if (objHandle == null || (h = objHandle.intValue()) == 0)
					{
						// failed to allocate new
						bcex = new AcsJCannotGetComponentEx();
						logger.log(Level.SEVERE,"Container returned another handle than given, failed to fix handle due to handle relocation failure.", bcex);
						status.setStatus(ComponentStatus.COMPONENT_ACTIVATED);		// Component is activated, but cannot be managed by the Manager
						throw bcex;
					}
					// !!! ACID 3
					else if (existingData != null)
						executeCommand(new ComponentCommandSet(h, existingData));
						//components.set(h, existingData);

					logger.log(Level.SEVERE,"Container returned another handle than given, handle fixed.");
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

			data.setKeepAliveTime(keepAliveTime);	// remember keep alive time

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
		} finally {
			componentsLock.unlock();
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
			catch (Throwable ex)
			{
				bcex = new AcsJCannotGetComponentEx(ex);
				logger.log(Level.SEVERE, "Failed to construct component '"+name+"', exception caught when invoking 'construct()' method.", bcex);

			}

			// remove component from client component list, will be added later
			if ((requestor & TYPE_MASK) == COMPONENT_MASK)
				removeComponentOwner(componentInfo.getHandle(), requestor);

			if (!constructed)
			{
				// release Component
				componentsLock.lock();
				try {
					// !!! ACID 3
					if (!reactivate)
						executeCommand(new ComponentCommandDeallocate(h, componentInfo.getHandle(), WhyUnloadedReason.REMOVED));
						//components.deallocate(h);

					// deactivate
					try
					{
						container.deactivate_component(componentInfo.getHandle());
					}
					catch (Exception ex)
					{
						bcex = new AcsJCannotGetComponentEx(ex);
						logger.log(Level.SEVERE, "Failed to deactivate component '"+name+"' on container '"+containerName+"'.", bcex);
					}

					status.setStatus(ComponentStatus.COMPONENT_NOT_ACTIVATED);
					throw bcex;
				} finally {
					componentsLock.unlock();
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

		// COMMENTED OUT: this can lead to bad performance on slow FS
		/*
		// take snapshot of manager state
		if( prevayler != null )
		{
			try {
				synchronized (prevayler) {
					((SnapshotPrevayler)prevayler).takeSnapshot();
				}
			}
			catch (IOException e) {
				e.printStackTrace();
			}
		}
		*/

		//
		// bind to remote directory
		//
		bind(convertToHiearachical(name), "O", componentInfo.getComponent().getObject());

		//
		// notify administrators about the activation
		//
		if (!isOtherDomainComponent)
			notifyComponentActivated(componentInfo, activationTime, executionId);
		
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
		notifyComponentRequested(new int[] { requestor }, new int[] { componentInfo.getHandle() }, activationTime);

		if (reactivate)
			logger.log(Level.FINE,"Component '"+name+"' (" + HandleHelper.toString(componentInfo.getHandle()) + ") reactivated.");
		else
			logger.log(Level.FINE,"Component '"+name+"' (" + HandleHelper.toString(componentInfo.getHandle()) + ") activated.");

		// notify about the change (only this-domain container which activated the component)...
		if (containerInfo != null)
			topologySortManager.notifyTopologyChange(containerInfo.getHandle());

		status.setStatus(ComponentStatus.COMPONENT_ACTIVATED);
		return componentInfo;
	}

	class ReleaseComponentResult
	{
		public int owners;
		public Throwable exception;

		public ReleaseComponentResult(int owners, Throwable exception) {
			this.owners = owners;
			this.exception = exception;
		}
	}
	
	/**
	 * Internal method for releasing components.
	 *
	 * @param	owner	owner of the component, if manager's own handle then deactivation will be forced
	 * @param	curl	CURL of the component to be released.
	 * @param	force	force deactivate, if still has owners then component will be made unavailable.
	 * @return			Number of clients that are still using the component after the operation completed.
	 */
	private ReleaseComponentResult internalReleaseComponent(int owner, URI curl, boolean force)
	   throws AcsJNoPermissionEx, AcsJBadParameterEx 
	{

		// resolve handle from curl
		int h = 0;

		String name = extractName(curl);

		componentsLock.lock();
		try {
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
		} finally {
			componentsLock.unlock();
		}

		// if found, delegate operation, otherwise do nothing
		if (h != 0)
			return internalReleaseComponent(owner, h, force);
		else
			return new ReleaseComponentResult(0, null);
	}

	/**
	 * Internal method for deactivating components.
	 *
	 * @param	name	name of the component to be released.
	 */
	private void internalDeactivateComponent(String name)
	{

		// try to acquire lock
		boolean lockAcquired = acquireSynchronizationObject(name, lockTimeout);
		if (lockAcquired)
		{
			boolean releaseRWLock = false;
			try
			{
				// resolve componentInfo from curl
				ComponentInfo componentInfo = null;
				componentsLock.lock();
				try {
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
				} finally {
					componentsLock.unlock();
				}

				// component is already gone, nothing to do
				if (componentInfo == null)
					return;

				// try to acquire activation readers lock first
				// NOTE: the locks are NOT reentrant
				releaseRWLock = true;
				activationPendingRWLock.readLock().lock();

				try {
					internalNoSyncDeactivateComponent(componentInfo);
				} catch (Throwable th) {
					// no handling, already logged
				}
			}
			finally
			{
				if (releaseRWLock)
					activationPendingRWLock.readLock().unlock();
				releaseSynchronizationObject(name);
			}
		}
		else
		{
			NoResourcesException nre = new NoResourcesException("Failed to obtain synchronization lock for component '"+name+"', possible deadlock.");
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
	private ReleaseComponentResult internalReleaseComponent(int owner, int h, boolean force)
		   throws AcsJNoPermissionEx, AcsJBadParameterEx 
	{

		// extract name
		String name = null;
		componentsLock.lock();
		try {
			int handle = h & HANDLE_MASK;
			ComponentInfo componentInfo = null;
			if (components.isAllocated(handle))
				componentInfo = (ComponentInfo)components.get(handle);

			if (componentInfo == null)
			{
				// invalid Component handle
				AcsJBadParameterEx ex = new AcsJBadParameterEx();
				ex.setParameter("componentInfo");
				ex.setParameterValue("null");
				throw ex;			
			}
			if (componentInfo.getHandle() != h)
			{
				// invalid Component handle
				AcsJBadParameterEx ex = new AcsJBadParameterEx();
				ex.setParameter("h");
				throw ex;			
			}

			name = componentInfo.getName();

		} finally {
			componentsLock.unlock();
		}

		// try to acquire lock
		boolean lockAcquired = acquireSynchronizationObject(name, lockTimeout);
		if (lockAcquired)
		{
			boolean releaseRWLock = true;
			try
			{
				// try to acquire activation readers lock first
				// NOTE: the locks are NOT reentrant
				activationPendingRWLock.readLock().lock();

				return internalNoSyncReleaseComponent(owner, h, force);
			}
			finally
			{
				if (releaseRWLock)
					activationPendingRWLock.readLock().unlock();
				releaseSynchronizationObject(name);
			}
		}
		else
		{
			NoResourcesException nre = new NoResourcesException("Failed to obtain synchronization lock for component '"+name+"', possible deadlock.");
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
	private ReleaseComponentResult internalNoSyncReleaseComponent(int owner, int h, boolean force)
	     throws AcsJNoPermissionEx 
	{

		int handle = h & HANDLE_MASK;
		int owners = 0;

		ComponentInfo componentInfo = null;
		componentsLock.lock();
		try {
			if (components.isAllocated(handle))
				componentInfo = (ComponentInfo)components.get(handle);

			if (componentInfo == null || componentInfo.getHandle() != h)
			{
				// invalid component handle
				BadParametersException af = new BadParametersException("Invalid component handle.");
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
					npe.setID(HandleHelper.toString(owner));
					npe.setProtectedResource(componentInfo.getName());
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
		} finally {
			componentsLock.unlock();
		}

		/****************** component deactivation ******************/

		ReleaseComponentResult result = new ReleaseComponentResult(owners, null);

		// there is no owners of the component, deactivate it
		if (force)
		{
			try {
				internalNoSyncDeactivateComponent(componentInfo);
			} catch (Throwable th) {
				result.exception = th;
			}
		}
		else if (owners == 0)
		{
			int keepAliveTime = RELEASE_IMMEDIATELY;
			String name = componentInfo.getName();
			boolean isOtherDomainComponent = name.startsWith(CURL_URI_SCHEMA);
			if (!isOtherDomainComponent)
			{
				keepAliveTime = componentInfo.getKeepAliveTime();
				if (keepAliveTime == RELEASE_TIME_UNDEFINED)
				{
					// when info is passed from the container
					DAOProxy dao = getComponentsDAOProxy();
					if (dao != null)
						keepAliveTime = readLongCharacteristics(dao, name+"/KeepAliveTime", RELEASE_IMMEDIATELY, true);
					else
						keepAliveTime = RELEASE_IMMEDIATELY;
				}
			}

			if (keepAliveTime == 0) {
				try {
					internalNoSyncDeactivateComponent(componentInfo);
				} catch (Throwable th) {
					result.exception = th;
				}
			}
			else if (keepAliveTime > 0)
				delayedDeactivationTask.schedule(new DeactivateComponentTask(name), keepAliveTime * 1000);

			// negative means immortal, however this could not happen since immortal
			// components have manager as an owner
		}
/// TODO !!!!!!!!!!!!!! no more handle -> componentInfo data
		// notify administrators about the release request
		notifyComponentReleased(new int[] { owner }, new int[] { h }, System.currentTimeMillis());

		logger.log(Level.FINE,"Component '"+componentInfo.getName()+"' (" + HandleHelper.toString(componentInfo.getHandle()) + ") released.");

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

		return result;
	}


	/**
	 * Deactivate component, issue deactivate reeust to container (or other manager).
	 * @param componentInfo	info about component to be deactivated.
	 */
	private void internalNoSyncDeactivateComponent(ComponentInfo componentInfo) throws Throwable
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
				        throw new CoreException("Failed to obtain manager for domain '" + domainName + "'.");
			    } catch (Throwable th) {
					logger.log(Level.WARNING, "Failed to obtain non-local manager required by component '"+name+"'.", th);
					throw th;
			    }

				// @todo MF call release_component on other manager (logout?)
			    // release component
				try
				{
				    URI curlName = CURLHelper.createURI(name);
				    // @todo MF tmp (handle)
				    remoteManager.releaseComponent(INTERDOMAIN_MANAGER_HANDLE, curlName);
				}
				catch (Throwable th)
				{
					logger.log(Level.WARNING, "Failed to release component '"+componentInfo.getName()+"' on remote manager.'", th);
					throw th;
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
						logger.log(Level.WARNING,"Container '"+containerName+"' required by component '"+componentInfo.getName()+"' is not logged in.");
					}

				}

				if (container != null)
				{

					// log info
					logger.log(Level.INFO,"Deactivating component '"+componentInfo.getName()+"' (" + HandleHelper.toString(componentInfo.getHandle()) + ") on container '" + containerInfo.getName() + "'.");

					// destruct
					try
					{
						componentInfo.getComponent().destruct();
					}
					catch (Throwable ex)
					{
						RemoteException re = new RemoteException("Failed to destruct component '"+componentInfo.getName()+"', exception caught when invoking 'destruct()' method.", ex);
						logger.log(Level.SEVERE, re.getMessage(), re);
						throw ex;
					}

					long deactivationTime = 0;

					// deactivate component in anycase
					try
					{
						container.deactivate_component(componentInfo.getHandle());
					    deactivationTime = System.currentTimeMillis();
					}
					catch (AcsJException aex)
					{
						logger.log(Level.SEVERE, aex.getMessage(), aex);
						throw aex;
					}
					catch (Throwable ex)
					{
						RemoteException re = new RemoteException("Failed to deactivate component '"+componentInfo.getName()+"' (" + HandleHelper.toString(componentInfo.getHandle()) + ") on container '"+containerInfo.getName()+"'.", ex);
						logger.log(Level.SEVERE, re.getMessage(), re);
						throw ex;
					}

					// notify administrators about deactivation, but not if failed
					if (deactivationTime != 0)
						notifyComponentDeactivated(componentInfo.getHandle(), deactivationTime);

					// shutdown container if required (and necessary)
					conditionalShutdownContainer(containerInfo);

				}

			}

		} finally {
			if (owners == 0)
			{
				// deallocate Component
				componentsLock.lock();
				try {
					executeCommand(new ComponentCommandDeallocate(handle, componentInfo.getHandle(), WhyUnloadedReason.REMOVED));
					//components.deallocate(handle);
				} finally {
					componentsLock.unlock();
				}
			}
		}

		// log info
		logger.log(Level.INFO,"Component '"+componentInfo.getName()+"' (" + HandleHelper.toString(componentInfo.getHandle()) + ") deactivated.");
		
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
		// not to mess with same component name
		final String LOCK_NAME = "container-" + containerName;

		// try to acquire lock
		boolean lockAcquired = acquireSynchronizationObject(LOCK_NAME, lockTimeout);
		if (lockAcquired)
		{
			try
			{
				// double check pattern
				ContainerInfo info = getContainerInfo(containerName);
				if (info != null)
					return info;
				
				return internalNoSyncStartUpContainer(containerName);
			}
			finally
			{
				releaseSynchronizationObject(LOCK_NAME);
			}
		}
		else
		{
			NoResourcesException nre = new NoResourcesException("Failed to obtain synchronization lock for container '"+containerName+"', possible deadlock.");
			throw nre;
		}

	}
	
	/**
	 * Start-up container (if it has a deploy info).
	 * @param containerName	name of the container to start up.
	 * @return container info of container, <code>null</code> if failed to start.
	 */
	private ContainerInfo internalNoSyncStartUpContainer(String containerName)
	{
		DAOProxy dao = getContainersDAOProxy();
		if (dao == null)
			return null;

		//
		// read DeployInfo and initiate start-up
		//

		String startOnDemand = readStringCharacteristics(dao, containerName + "/DeployInfo/StartOnDemand", true);
		if (startOnDemand == null || !startOnDemand.equalsIgnoreCase("TRUE"))
			return null;
		
		String host = readStringCharacteristics(dao, containerName + "/DeployInfo/Host", true);
		if (host == null)
			return null;

		String flags = readStringCharacteristics(dao, containerName + "/DeployInfo/Flags", true);
		if (flags == null)
			flags = "";

		String impLang = readStringCharacteristics(dao, containerName + "/ImplLang", true);
		if (impLang == null)
			impLang = "";


		// add itself as manager reference
		flags += " -m " + transport.getManagerReference();

		short instance = (short)ACSPorts.getBasePort();

		try
		{
			Daemon daemon = transport.getDaemon(host);
			if (daemon != null)
				daemon.startContainer(impLang, containerName, instance, flags);
			else
				throw new RuntimeException("Failed to get daemon.");

		} catch (Throwable th)
		{
			RemoteException re = new RemoteException("Failed to connect to ACS daemon on host '"+host+"' to start container '"+containerName+"'.", th);
			logger.log(Level.SEVERE, re.getMessage(), re);
			return null;
		}


		//
		// wait for login
		//

		// HSO: raised timeout from 15 sec to 2 min because of increased usage of autostart containers,
		//      where container start times get extremely long when started in parallel on one machine.
		//      TODO: Refactor manager interface to use callbacks for component getter methods, 
		//            to not block the manager ORB threads with long-lasting container starts.
		final int CONTAINER_STARTUP_TIMEOUT = 120000;

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
				if (info != null) {
					return info;
				}
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

		// resolve handle from curl
		int h = 0;

		String name = extractName(curl);

		componentsLock.lock();
		try {
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
		} finally {
			componentsLock.unlock();
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

		// extract name
		String name = null;
		componentsLock.lock();
		try {
			int handle = h & HANDLE_MASK;
			ComponentInfo componentInfo = null;
			if (components.isAllocated(handle))
				componentInfo = (ComponentInfo)components.get(handle);

			if (componentInfo == null || componentInfo.getHandle() != h)
			{
				// invalid Component handle
				BadParametersException af = new BadParametersException("Invalid component handle.");
				throw af;
			}

			name = componentInfo.getName();

		} finally {
			componentsLock.unlock();
		}

		// try to acquire lock
		boolean lockAcquired = acquireSynchronizationObject(name, lockTimeout);
		if (lockAcquired)
		{
			boolean releaseRWLock = true;
			try
			{
				// try to acquire activation readers lock first
				// NOTE: the locks are NOT reentrant
				activationPendingRWLock.readLock().lock();

				return internalNoSyncRestartComponent(owner, h);
			}
			finally
			{
				if (releaseRWLock)
					activationPendingRWLock.readLock().unlock();
				releaseSynchronizationObject(name);
			}
		}
		else
		{
			NoResourcesException nre = new NoResourcesException("Failed to obtain synchronization lock for component '"+name+"', possible deadlock.");
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

		int handle = h & HANDLE_MASK;

		ComponentInfo componentInfo = null;
		componentsLock.lock();
		try {
			if (components.isAllocated(handle))
				componentInfo = (ComponentInfo)components.get(handle);

			if (componentInfo == null || componentInfo.getHandle() != h)
			{
				// invalid component handle
				BadParametersException af = new BadParametersException("Invalid component handle.");
				throw af;
			}

			// remove ownership of the component
			if (!componentInfo.getClients().contains(owner))
			{
				// not an owner
				AcsJNoPermissionEx npe = new AcsJNoPermissionEx();
				npe.setReason("Restarting component that client does not own.");
				npe.setID(HandleHelper.toString(owner));
				npe.setProtectedResource(componentInfo.getName());
				throw npe;
			}

		} finally {
			componentsLock.unlock();
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
				logger.log(Level.WARNING,"Container '"+containerName+"' required by component '"+componentInfo.getName()+"' is not logged in.");
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
					RemoteException re = new RemoteException("Failed to restart component '"+componentInfo.getName()+"', 'null' returned.");
					throw re;
				}

				// @todo what about notifying clients, marking component as available, updating reference...

			}
			catch (Throwable ex)
			{
				RemoteException re = new RemoteException("Failed to restart component '"+componentInfo.getName()+"' on container '"+containerInfo.getName()+"'.", ex);
				logger.log(Level.SEVERE, re.getMessage(), re);
			}
		}

		logger.log(Level.FINE,"Component '"+componentInfo.getName()+"' restarted.");

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

		if (type == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException("Non-null 'type' expected.");
			throw af;
		}

		/****************************************************************/

		// check handle and NONE permissions
		securityCheck(id, AccessRights.NONE);

		logger.log(Level.INFO,"Getting default component for type '" + type + "'.");

		ComponentInfo componentInfo = internalRequestDefaultComponent(id, type);

		/****************************************************************/

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

		String defaultComponentName = null;
		ComponentInfo defaultComponentInfo = null;

		// first check default components table
		synchronized (defaultComponents)
		{
			defaultComponentInfo = defaultComponents.get(type);
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
							logger.log(Level.WARNING,"Misconfigured CDB, there is no type of component '"+ids[i]+"' defined.");
							continue;
						}

						// do not search dynamic components (they cannot be marked as default in CDB anyway)
						if (!name.equals(ComponentSpec.COMPSPEC_ANY))
						{

							// read type
							String componentType = readStringCharacteristics(componentsDAO, ids[i]+"/Type");
							if (type == null)
							{
								logger.log(Level.WARNING,"Misconfigured CDB, there is no type of component '"+name+"' defined.");
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
				catch (Throwable ex)
				{
					CoreException ce = new CoreException("Failed to obtain component data from the CDB.", ex);
					reportException(ce);
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
					CoreException huse = new CoreException("Failed to return default component: '" + defaultComponentName + "', container with '" +
																		HandleHelper.toString(defaultComponentInfo.getContainer()) + "' not logged in.");
					reportException(huse);
					return null;
				}

				ComponentInfo componentInfo = internalRequestComponent(requestor,
																	   defaultComponentInfo.getName(), defaultComponentInfo.getType(),
																	   defaultComponentInfo.getCode(),	containerInfo.getName(), RELEASE_IMMEDIATELY, status, true);
				if (componentInfo == null || status.getStatus() != ComponentStatus.COMPONENT_ACTIVATED)
				{
					CoreException huse = new CoreException("Failed to obtain default component: '" + defaultComponentName + "'.");
					reportException(huse);
					// no error handling...
					return null;
				}

				return componentInfo;

			}
			catch (Throwable t)
			{
				CoreException huse = new CoreException("Failed to return default component: '" + defaultComponentName + "'.", t);
				reportException(huse);
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
				CoreException huse = new CoreException("Failed to create CURL from default component name: '" + defaultComponentName + "'.", use);
				reportException(huse);
				return null;
			}

			try
			{
				// request for component
				StatusHolder status = new StatusHolder();

				Component component = internalRequestComponent(requestor, curl, status, true);
				if (component == null || status.getStatus() != ComponentStatus.COMPONENT_ACTIVATED)
				{
					CoreException huse = new CoreException("Failed to obtain default component: '" + defaultComponentName + "'.");
					reportException(huse);
					return null;
				}

				// return component info
				ComponentInfo[] componentInfo = getComponentInfo(requestor, new int[0], defaultComponentName, type, true);
				if (componentInfo == null || componentInfo.length != 1)
				{
					CoreException huse = new CoreException("Failed to obtain activated default component ComponentInfo: '" + defaultComponentName + "'.");
					reportException(huse);
					return null;
				}
				else
					return componentInfo[0];

			}
			catch (Throwable t)
			{
				CoreException huse = new CoreException("Failed to return default component: '" + defaultComponentName + "'.", t);
				reportException(huse);
				return null;
			}
		}

		// not found
		NoDefaultComponentException ndce = new NoDefaultComponentException("No default component for type '" + type + "' found.");
		throw ndce;
	}


	/**
	 * @see com.cosylab.acs.maci.Manager#getDynamicComponent(int, com.cosylab.acs.maci.ComponentSpec, boolean)
	 */
	// TODO MF not supported
	public ComponentInfo getDynamicComponent(int id, ComponentSpec componentSpec, boolean markAsDefault)
		throws AcsJCannotGetComponentEx, AcsJNoPermissionEx, AcsJIncompleteComponentSpecEx,
			   AcsJInvalidComponentSpecEx, AcsJComponentSpecIncompatibleWithActiveComponentEx
	{
		try {
			// check if null
			if (componentSpec == null)
			{
				AcsJNullPointerEx ex = new AcsJNullPointerEx();
				ex.setVariable("componentSpec");
				throw ex;
			}
			// check componentSpec components are null
			if (componentSpec.getName() == null)
				{
					AcsJNullPointerEx ex = new AcsJNullPointerEx();
					ex.setVariable("componentSpec.Name");
					throw ex;
				}
			if (componentSpec.getType() == null)
				{
					AcsJNullPointerEx ex = new AcsJNullPointerEx();
					ex.setVariable("componentSpec.Type");
					throw ex;
				}
			if (componentSpec.getCode() == null)
				{
					AcsJNullPointerEx ex = new AcsJNullPointerEx();
					ex.setVariable("componentSpec.Code");
					throw ex;
				}
			if (componentSpec.getContainer() == null)
				{
					AcsJNullPointerEx ex = new AcsJNullPointerEx();
					ex.setVariable("componentSpec.Container");
					throw ex;
				}
			// check for empty componentSpec.name
			if (componentSpec.getName().length() == 0)
			{
				AcsJBadParameterEx ex = new AcsJBadParameterEx();
				ex.setParameter("componentSpec.Name");
				ex.setParameterValue("EMPTY");
				ex.setReason("Non empty Component Name expected");
				throw ex;			
			}

		} catch (AcsJNullPointerEx e) {
			AcsJInvalidComponentSpecEx ex = new AcsJInvalidComponentSpecEx(e);
			throw ex;
		}
		catch (AcsJBadParameterEx e) {
			AcsJInvalidComponentSpecEx ex = new AcsJInvalidComponentSpecEx(e);
			throw ex;
		}
		
		// check handle and NONE permissions
		// Throws AcsJNoPermissionEx that is let flying up
		securityCheck(id, AccessRights.NONE);

		/****************************************************************/

		// Same exceptions fly up
		ComponentInfo componentInfo = null;
		try {
			componentInfo = internalRequestDynamicComponent(id, componentSpec);
		} catch (AcsJSyncLockFailedEx e) {
			AcsJCannotGetComponentEx ex = new AcsJCannotGetComponentEx();
			ex.setCURL(componentSpec.getName());
			ex.setReason("Failed to get Synchronisation lock");
			throw ex;
		}
		
		// update default components table
		if (componentInfo != null && markAsDefault)
		{
			synchronized (defaultComponents)
			{
				// !!! ACID 3
				executeCommand(new DefaultComponentCommandPut(componentInfo.getType(), componentInfo));
				//defaultComponents.put(componentInfo.getType(), componentInfo.getName());
			}

			logger.log(Level.INFO,"'" + componentInfo.getName() +"' has been marked as a default component of type '" + componentInfo.getType() +"'.");
		}

		/****************************************************************/

		if(componentInfo == null)
		{
		    /**
             * @todo Is it OK to get here? Always? 
             *       This is a place where we have to check carefully.
             */
			AcsJCannotGetComponentEx ex = new AcsJCannotGetComponentEx();
			ex.setCURL(componentSpec.getName());
			throw ex;
		}
		
		return componentInfo;
	}

	/**
	 * @see com.cosylab.acs.maci.Manager#getDynamicComponents(int, com.cosylab.acs.maci.ComponentSpec[])
	 * @todo this method still returns null in case of errors and only throws AcsJNoPermissions or
	 *       a couple of runtime exceptions.
	 *       Needs to be refactored or, probably better, deprecated.
	 */
	public ComponentInfo[] getDynamicComponents(int id,	ComponentSpec[] components)
		throws AcsJNoPermissionEx
	{

		// check if null
		if (components == null)
		{
			// BAD_PARAM
			BadParametersException af = new BadParametersException("Non-null 'components' expected.");
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

				CoreException ce = new CoreException("Failed to get dynamic component '"+components[i]+"'.", ex);
				reportException(ce);
			}
		}

		logger.log(Level.INFO,obtained + " of " + components.length +" dynamic components obtained.");

		/****************************************************************/

		return componentInfos;

	}

	/**
	 * @see com.cosylab.acs.maci.Manager#getCollocatedComponent(int, com.cosylab.acs.maci.ComponentSpec, boolean, URI)
	 */
	/// @todo MF not supported
	public ComponentInfo getCollocatedComponent(int id, ComponentSpec componentSpec,
			boolean markAsDefault, URI targetComponentURI)
		throws AcsJCannotGetComponentEx, AcsJNoPermissionEx, AcsJIncompleteComponentSpecEx,
			   AcsJInvalidComponentSpecEx, AcsJComponentSpecIncompatibleWithActiveComponentEx
	{
		
		try {
			// check if null
			if (componentSpec == null)
			{
				AcsJNullPointerEx ex = new AcsJNullPointerEx();
				ex.setVariable("componentSpec");
				throw ex;
			}
			// check componentSpec components are null
			if (componentSpec.getName() == null)
			{
				AcsJNullPointerEx ex = new AcsJNullPointerEx();
				ex.setVariable("componentSpec.Name");
				throw ex;
			}
			if (componentSpec.getType() == null)
			{
				AcsJNullPointerEx ex = new AcsJNullPointerEx();
				ex.setVariable("componentSpec.Type");
				throw ex;
			}
			if (componentSpec.getCode() == null)
			{
				AcsJNullPointerEx ex = new AcsJNullPointerEx();
				ex.setVariable("componentSpec.Code");
				throw ex;
			}
			if (componentSpec.getContainer() == null)
			{
				AcsJNullPointerEx ex = new AcsJNullPointerEx();
				ex.setVariable("componentSpec.Container");
				throw ex;
			}
			// check for empty componentSpec.name
			if (componentSpec.getName().length() == 0)
			{
				AcsJBadParameterEx ex = new AcsJBadParameterEx();
				ex.setParameter("componentSpec.Name");
				ex.setParameterValue("EMPTY");
				ex.setReason("Non empty Component Name expected");
				throw ex;			
			}
			// check if null
			if (targetComponentURI == null)
			{
				AcsJNullPointerEx ex = new AcsJNullPointerEx();
				ex.setVariable("targetComponentURI");
				throw ex;
			}
			if (!componentSpec.getContainer().equals(ComponentSpec.COMPSPEC_ANY))
			{
				AcsJBadParameterEx ex = new AcsJBadParameterEx();
				ex.setParameter("componentSpec.Container");
				ex.setParameterValue(componentSpec.getContainer());
				ex.setReason("COMPSPEC_ANY expected");
				throw ex;			
			}
			
		} catch (AcsJNullPointerEx e) {
			AcsJInvalidComponentSpecEx ex = new AcsJInvalidComponentSpecEx(e);
			throw ex;
		}
		catch (AcsJBadParameterEx e) {
			AcsJInvalidComponentSpecEx ex = new AcsJInvalidComponentSpecEx(e);
			throw ex;
		}
		
		
		// check handle and NONE permissions
		// Throws AcsJNoPermissionEx that is let flying up
		securityCheck(id, AccessRights.NONE);
		
		/****************************************************************/
		
		/// @todo temporary quick implementation (does not look in the CDB if component is not activated)
		String name = extractName(targetComponentURI);
		
		int h = 0;
		ComponentInfo targetComponentInfo = null;
		componentsLock.lock();
		try {
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
		} finally {
			componentsLock.unlock();
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
			AcsJIncompleteComponentSpecEx ex = new AcsJIncompleteComponentSpecEx();
			ex.setCURL(name);
			ex.setContainerName(componentSpec.getContainer());
			throw ex;
		}
		
		// request for component
		// same exceptions are let flying up.
		ComponentInfo componentInfo = null;
		try {
			componentInfo = internalRequestDynamicComponent(id, componentSpec);
		} catch (AcsJSyncLockFailedEx e) {
			AcsJCannotGetComponentEx ex = new AcsJCannotGetComponentEx();
			ex.setCURL(name);
			ex.setReason("Failed to get Synchronisation lock");
			throw ex;
		}
		
		// update default components table
		if (componentInfo != null && markAsDefault)
		{
			synchronized (defaultComponents)
			{
				// !!! ACID 3
				executeCommand(new DefaultComponentCommandPut(componentInfo.getType(), componentInfo));
				//defaultComponents.put(componentInfo.getType(), componentInfo.getName());
			}
			
			logger.log(Level.INFO,"'" + componentInfo.getName() +"' has been marked as a default component of type '" + componentInfo.getType() +"'.");
		}
		
		if(componentInfo == null)
		{
			AcsJCannotGetComponentEx ex = new AcsJCannotGetComponentEx();
			ex.setCURL(name);
			throw ex;
		}
		
		/****************************************************************/
		
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
	private String[] searchDynamicComponent(String[] fieldNames, String[] requiredValues,
											boolean[] equalityRequired, int[] equalityPoints,
											IntHolder keepAliveTimeHolder)
	{
		assert(fieldNames != null);
		assert(equalityRequired != null);
		assert(equalityPoints != null);
		assert(fieldNames.length == equalityRequired.length);
		assert(equalityRequired.length == equalityPoints.length);

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
			CoreException af = new CoreException("Failed to retrieve data from CDB.", ex);
			reportException(af);
			return null;
		}

		int len = fieldNames.length;
		int maxPoints = Integer.MIN_VALUE;
		String[] bestMatch = null;
		String[] currentMatch = new String[len];
		int bestMatchKeepAliveTime = RELEASE_TIME_UNDEFINED;

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
				bestMatchKeepAliveTime = readLongCharacteristics(componentsDAO, fieldIDs[fi]+"/KeepAliveTime", RELEASE_TIME_UNDEFINED, true);
			}
		}

		keepAliveTimeHolder.value = bestMatchKeepAliveTime;
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
		throws AcsJCannotGetComponentEx, 
		       AcsJSyncLockFailedEx,
		       AcsJNoPermissionEx, 
		       AcsJIncompleteComponentSpecEx, 
		       AcsJInvalidComponentSpecEx,
			   AcsJComponentSpecIncompatibleWithActiveComponentEx
	{

		boolean unspecifiedName = componentSpec.getName().endsWith(ComponentSpec.COMPSPEC_ANY);
		boolean unspecifiedType = componentSpec.getType().equals(ComponentSpec.COMPSPEC_ANY);

		// check completeness of componentSpec
		//   *  |   *  | throw IncompleteComponentSpecException
		if (unspecifiedName && unspecifiedType)
		{
			AcsJInvalidComponentSpecEx ex = new AcsJInvalidComponentSpecEx();
			ex.setReason("'name' and 'type' cannot be both '" + ComponentSpec.COMPSPEC_ANY +"'.");
			throw ex;
		}
		// all fields are fully specified, no search needed
		else if (!unspecifiedName && !unspecifiedType &&
			!componentSpec.getCode().equals(ComponentSpec.COMPSPEC_ANY) &&
			!componentSpec.getContainer().equals(ComponentSpec.COMPSPEC_ANY))
		{
			StatusHolder statusHolder = new StatusHolder();

			// We let exceptions occurring here fly up
			return internalRequestComponent(requestor, componentSpec.getName(),
							 			    componentSpec.getType(), componentSpec.getCode(),
											componentSpec.getContainer(), RELEASE_TIME_UNDEFINED, statusHolder, true);
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
		IntHolder keepAliveTimeHolder = new IntHolder(RELEASE_TIME_UNDEFINED);
		String[] result = prohibitSearch ? null : searchDynamicComponent(fieldNames, requiredValues, equalityRequired, equalityPoints, keepAliveTimeHolder);

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
				AcsJInvalidComponentSpecEx ex = new AcsJInvalidComponentSpecEx();
				ex.setReason("Requested ComponentSpec does not match any entry in the CDB.");
				throw ex;
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

				AcsJIncompleteComponentSpecEx ex = new AcsJIncompleteComponentSpecEx();
				ex.setReason("'" + fieldNames[i] + "' equals '" + ComponentSpec.COMPSPEC_ANY +"'.");
				throw ex;
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
		
		// Same exceptions are let flying up
		return internalRequestComponent(requestor, result[0], result[1],
									    result[2], result[3], keepAliveTimeHolder.value, statusHolder, true);
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
			        	// noop
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
				CoreException ce = new CoreException("Failed to bind name '"+name+"' to the remote directory.", ne);
				logger.log(Level.FINE, ce.getMessage(), ce);
			}
		}

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
				CoreException ce = new CoreException("Failed to rebind name '"+name+"' to the remote directory.", ne);
				logger.log(Level.FINE, ce.getMessage(), ce);
			}
		}

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
				CoreException ce = new CoreException("Failed to lookup name '"+name+"' in the remote directory.", ne);
				logger.log(Level.FINE, ce.getMessage(), ce);
			}
		}

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
				CoreException ce = new CoreException("Failed to unbind name '"+name+"' from the remote directory.", ne);
				logger.log(Level.FINE, ce.getMessage(), ce);
			}
		}

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
			CoreException ce = new CoreException("Failed to unbind (potential) empty context for '"+name+"'.", th);
			logger.log(Level.FINE, ce.getMessage(), ce);
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
		int reqHandle = id & HANDLE_MASK;

		boolean invalidHandle = true;
		StringBuffer name = new StringBuffer(30);

		switch	(id & TYPE_MASK)
		{
			case CONTAINER_MASK:
				//name.append("Container ");
				synchronized (containers)
				{
					if (containers.isAllocated(reqHandle))
					{
						ContainerInfo info = (ContainerInfo)containers.get(reqHandle);
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
					if (clients.isAllocated(reqHandle))
					{
						ClientInfo info = (ClientInfo)clients.get(reqHandle);
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
					if (administrators.isAllocated(reqHandle))
					{
						ClientInfo info = (ClientInfo)administrators.get(reqHandle);
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
				componentsLock.lock();
				try {
					if (components.isAllocated(reqHandle))
					{
						ComponentInfo info = (ComponentInfo)components.get(reqHandle);
						// do additional preallocation check
						if (info != null && info.getHandle() == id)
						{
							invalidHandle = false;
							name.append(info.getName());
						}
					}
				} finally {
					componentsLock.unlock();
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
	private void checkCURL(URI curl) throws AcsJBadParameterEx
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
	private void checkCURL(URI curl, boolean allowNonLocalDomains) throws AcsJBadParameterEx
	{

		// check if null
		if (curl == null)
		{
			// BAD_PARAM
			AcsJBadParameterEx af = new AcsJBadParameterEx();
			af.setParameter("curl");
			af.setParameterValue("null");
			throw af;
		}

		if (curl.getPath() == null || curl.getPath().length() == 0 ||
			((curl.getScheme() != null && curl.getScheme().startsWith(CURL_URI_SCHEMA))))
		{
			// BAD_PARAM
			AcsJBadParameterEx af = new AcsJBadParameterEx();
			af.setParameter("curl");
			af.setParameterValue(curl.toString());
			throw af;
		}

		// check if CURL belongs to this domain
		if (!allowNonLocalDomains && !isLocalDomainCURL(curl))
		{
			// BAD_PARAM
		    String domain = curl.getAuthority();
			AcsJBadParameterEx af = new AcsJBadParameterEx();
			af.setParameter("curl");
			af.setParameterValue("CURL does not belong to this domain ('"+domain+"' not one of '"+domains+"').");
			throw af;
		}

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
		if(curl == null)
			return "";
		
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
			catch (Throwable ex)
			{
				CoreException ce = new CoreException("Failed to retrieve list of service components.", ex);
				logger.log(Level.FINE, ce.getMessage(), ce);
			}

		}

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
			lock = activationSynchronization.get(name);

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
			ReferenceCountingLock lock = activationSynchronization.get(name);

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

		if (managerDAO == null)
			managerDAO = createDAO("MACI/Managers/Manager");

		return managerDAO;

	}

	/**
	 * Read manager configuration.
	 */
	private void readManagerConfiguration()
	{
		enableHandleMonitoring = System.getProperties().containsKey(NAME_HANDLE_MONITORING);

		DAOProxy managerDAO = getManagerDAOProxy();
		if (managerDAO == null)
			return;
			
		clientPingInterval = (int)(readDoubleCharacteristics(managerDAO, "ClientPingInterval", clientPingInterval/(double)1000, true)*1000);
		clientPingInterval = Math.max(1000, clientPingInterval);

		administratorPingInterval = (int)(readDoubleCharacteristics(managerDAO, "AdministratorPingInterval", administratorPingInterval/(double)1000, true)*1000);
		administratorPingInterval = Math.max(1000, administratorPingInterval);
		
		containerPingInterval = (int)(readDoubleCharacteristics(managerDAO, "ContainerPingInterval", containerPingInterval/(double)1000, true)*1000);
		containerPingInterval = Math.max(1000, containerPingInterval);
		
		try {
			String strTimeOut = System.getProperty("jacorb.connection.client.pending_reply_timeout");
			if (strTimeOut != null) {
				long t = Long.valueOf(strTimeOut);
				if (t > 0) {
					// add 1 minute
					lockTimeout = t + 60000;
					// check for overflow
					if (lockTimeout < 0)
						lockTimeout = Long.MAX_VALUE;
				}
			}
		} catch (Throwable th) {
			// noop (left default)
		}

		poolThreads = readLongCharacteristics(managerDAO, "ServerThreads", poolThreads, true);
		poolThreads = Math.max(3, poolThreads);

	}
	
	/**
	 * Destroys Manager DAO (CDB access).
	 */
	private synchronized void destroyManagerDAOProxy()
	{

		if (managerDAO != null)
			destroyDAO(managerDAO);

		managerDAO = null;

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
                String attributes = dc.get_field_data(prefix + "_characteristics");

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
            CoreException ce = new CoreException("Failed to obtain list of all components.", th);
			logger.log(Level.WARNING, ce.getMessage(), ce);
        }

        String[] retVal = new String[componentList.size()];
        componentList.toArray(retVal);

	logger.log(Level.INFO,"Found " + retVal.length + " component entries in the configuration database.");

        return retVal;
    }

	/**
	 * Destroys components DAO (CDB access).
	 */
	private synchronized void destroyComponetsDAOProxy()
	{

		if (componentsDAO != null)
			destroyDAO(componentsDAO);

		componentsDAO = null;

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

		if (containersDAO == null)
			containersDAO = createDAO("MACI/Containers");

		return containersDAO;

	}

	/**
	 * Destroys containers DAO (CDB access).
	 */
	private synchronized void destroyContainersDAOProxy()
	{

		if (containersDAO != null)
			destroyDAO(containersDAO);

		containersDAO = null;

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
				logger.log(Level.FINE,"Failed to create DAO for '"+entity+"'.");
				// TODO @todo logging?!
				//CoreException ce = new CoreException("Failed to create DAO for '"+entity+"'.", th);
				//logger.log(Level.FINE, ce.getMessage(), ce);
			}
		}

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

		String retVal = null;

		try
		{
			retVal = dao.get_string(path);
		}
		catch (Throwable th)
		{
			CoreException ce = new CoreException("Failed to read '"+path+"' field on DAO dao '"+dao+"'.", th);
			if (!silent)
				reportException(ce);
		}

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

		int retVal = defaultValue;

		try
		{
			retVal = dao.get_long(path);
		}
		catch (Throwable th)
		{
			CoreException ce = new CoreException("Failed to read '"+path+"' field on DAO dao '"+dao+"'.", th);
			if (!silent)
				reportException(ce);
		}

		return retVal;

	}

	/**
	 * Reads DAO (CDB access) of double type.
	 *
	 * @param	path		path to be read non-<code>null</code>.
	 * @param	dao			DAO on which to perform read request.
	 * @param	silent		do not complain, if characteristics not found.
	 * @return	double		value read, <code>0.0</code> on failure.
	 */
	private double readDoubleCharacteristics(DAOProxy dao, String path, double defaultValue, boolean silent)
	{
		assert (path != null);

		double retVal = defaultValue;

		try
		{
			retVal = dao.get_double(path);
		}
		catch (Throwable th)
		{
			CoreException ce = new CoreException("Failed to read '"+path+"' field on DAO dao '"+dao+"'.", th);
			if (!silent)
				reportException(ce);
		}

		return retVal;

	}

	/**
	 * Destroys DAO (CDB access).
	 *
	 * @param	dao	DAO to be destroyed.
	 */
	private void destroyDAO(DAOProxy dao)
	{

		if (dao != null)
		{
			try
			{
				dao.destroy();
			}
			catch (Throwable th)
			{
				CoreException ce = new CoreException("Failed to destroy DAO dao '"+dao+"'.", th);
				logger.log(Level.FINE, ce.getMessage(), th);
			}

		}

	}

 	/*****************************************************************************/
	/************************* [ Federation methods ] ****************************/
	/*****************************************************************************/

	/**
	 * Initialize manager federation.
	 */
	public void initializeFederation(Hashtable federationDirectoryProperties) throws CoreException
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
				logger.log(Level.INFO,"Using domain list: " + givenDomainList + ".");

		}
		catch (Throwable t)
		{
			logger.log(Level.WARNING, "Failed to parse domain list '" + NAME_DOMAIN_LIST + "' variable, " + t.getMessage(), t);
		}

		// recovery data consistency check
		if (domains.size() != 0 && !domains.equals(givenDomainList))
		{
		    CoreException ie = new CoreException("Given domain list is not consistent with recovery data: " + givenDomainList + " != " + domains + ".");
		    throw ie;
		}
		else if (domains.size() == 0 && givenDomainList.size() != 0)
		    domains = givenDomainList;


		// no domain to control, no federation
		if (domains.size() == 0)
		{
			logger.log(Level.CONFIG,"No domain list given, manager federation disabled.");
			return;
		}

		//
		// local NS reference check
		//

		if (remoteDirectoryComponentReference == null)
		{
			logger.log(Level.WARNING,"No valid local domain naming service reference found, manager federation disabled.");
			return;
		}

	    //
		// read federation naming service
	    //

		String domainNS = System.getProperty(NAME_DOMAIN_DIRECTORY);
		if (domainNS == null)
		{
			logger.log(Level.WARNING,"No federation directory reference given, manager federation disabled.");
			return;
		}
		// set NS address
		federationDirectoryProperties.put(Context.PROVIDER_URL, domainNS);

		logger.log(Level.INFO,"Connecting to the federation directory with reference '"+domainNS+"'...");

		try
		{
			federationDirectory = new InitialContext(federationDirectoryProperties);
		}
		catch (Throwable th)
		{
			logger.log(Level.INFO, "Failed to connect to the federation directory with reference '"+domainNS+"'...", th);
			return;
		}

		logger.log(Level.INFO,"Connected to the federation directory with reference '"+domainNS+"'.");

		// register domains
		Iterator iter = domains.iterator();
		while (iter.hasNext()) {
		    bind(federationDirectory, dottedToHierarchical(iter.next().toString()),
		            null, remoteDirectoryComponentReference);
		}

		federationEnabled = true;

		logger.log(Level.INFO,"Manager federation enabled.");

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
            return managerCache.get(domainName);

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

	// ALARM SYSTEM codes
	protected final static String FAULT_FAMILY = "Manager";
	protected final static String FAULT_MEMBER = "Prevayler";
	protected final static int FAULT_CODE = 2;

	/**
	 * Convenience method for send_alarm with given state.
	 *
	 * @param faultMember
	 * @param state
	 */
	public void reportPrevaylerState(final boolean raise, Throwable alarmEx) {
		
		// log message
		if (raise) {
			logger.log(Level.WARNING, "Manager persistence subsystem failed to store pesistent data.", alarmEx);
		}
		else {
			logger.log(Level.FINER, "Manager persistence subsystem is functional.");
		}
		
		// if no alarm system initialized ignore
		if (alarmSource == null) {
			return;
		}

		try {
			// alarm source internally does async processing
			alarmSource.setAlarm(FAULT_FAMILY, FAULT_MEMBER, FAULT_CODE, raise);
		} catch (Throwable th) {
			logger.log(Level.WARNING, "Failed to send alarm.", th);
		}
	}
	
	/**
	 * @param command
	 * @return
	 */
	
	private Serializable executeCommand(Command command) throws NoResourcesException
	{
		if (prevayler != null)
		{
			try {
				final Serializable retVal;
				synchronized (prevayler) {
					retVal = prevayler.executeCommand(command);
				}
				reportPrevaylerState(false, null);
				return retVal;
			} catch (IOException ioex) {
				// filesystem error, prevailey failed
				// log, raise alarm and bypass prevayler (do not return here)
				reportPrevaylerState(true, ioex);
			} catch (Throwable th) {
				// most likely command execution error
				throw new NoResourcesException("Failed to execute command.", th);
			}
			
		}
		
		// bypass prevayler
		try {
			return command.execute(this);
		} catch (Throwable th) {
			throw new NoResourcesException("Failed to execute command.", th);
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
	 * Log handle release.
	 * @param handle
	 * @param reason
	 */
	public void logHandleRelease(int handle, WhyUnloadedReason reason) {
		if (!enableHandleMonitoring)
			return;
		
		final long now = System.currentTimeMillis();
		synchronized (releasedHandles) {
			// this simply overrides the old entry
			releasedHandles.put(handle, new HandleMonitorEntry(now, reason));
		}
	}

	/**
	 * Get handle release log.
	 * @param handle
	 * @return log or <code>null</code> if it does not exist.
	 */
	public HandleMonitorEntry getHandleReleaseLog(int handle) {
		synchronized (releasedHandles) {
			return releasedHandles.get(handle);
		}
	}
	
	/**
	 * Get handle released map.
	 * @return
	 */
	public Map<Integer, HandleMonitorEntry> getReleasedHandles()
	{
		return releasedHandles;
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
	public Map<String, ComponentInfo> getDefaultComponents()
	{
		return defaultComponents;
	}

	/*****************************************************************************/
	/*****************************************************************************/
	/*****************************************************************************/

	/**
	 * Returns the remoteDirectory.
	 * @return Context
	 */
	public Context getRemoteDirectory()
	{
		return remoteDirectory;
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

	/*****************************************************************************/
	/*****************************************************************************/
	/*****************************************************************************/

	/**
	 * @return the activeAlarms
	 */
	public HashSet getActiveAlarms() {
		return activeAlarms;
	}

	/**
	 * Flag if alarm is active.
	 */
	public boolean hasActiveAlarm(String faultMember) {
		synchronized (activeAlarms) {
			return activeAlarms.contains(faultMember);
		}
	}
	
	/**
	 * Remember that alarms has been raised.
	 */
	public void alarmRaised(String faultMember) {
		synchronized (activeAlarms) {
			executeCommand(new AlarmRaised(faultMember));
		}
	}

	/**
	 * Remember that alarms has been cleared.
	 */
	public void alarmCleared(String faultMember) {
		synchronized (activeAlarms) {
			executeCommand(new AlarmCleared(faultMember));
		}
	}
}

