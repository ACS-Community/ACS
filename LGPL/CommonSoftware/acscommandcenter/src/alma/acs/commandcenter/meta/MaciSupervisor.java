/*
 * Created on Oct 13, 2003 by mschilli
 */
package alma.acs.commandcenter.meta;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;
import java.util.logging.Logger;

import org.omg.CORBA.OBJECT_NOT_EXIST;
import org.omg.CORBA.ORB;
import org.omg.CORBA.SystemException;
import org.omg.CORBA.TRANSIENT;

import si.ijs.maci.Administrator;
import si.ijs.maci.AdministratorPOA;
import si.ijs.maci.AuthenticationData;
import si.ijs.maci.ClientInfo;
import si.ijs.maci.ClientType;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ContainerInfo;
import si.ijs.maci.ImplLangType;
import si.ijs.maci.Manager;
import si.ijs.maci.ManagerHelper;
import alma.acs.util.AcsLocations;
import alma.acs.util.UTCUtility;
import alma.maciErrType.NoPermissionEx;

import com.xtmod.util.collections.ListDiff;

/**
 * @author mschilli
 */
public class MaciSupervisor implements IMaciSupervisor {


	protected String name = null;
	protected String managerLoc = null;
	protected ORB orb = null;
	protected Logger log = null;
	

	/**
	 * Creates a MaciSupervisor running on the given ORB, it will connect to the specified
	 * manager.
	 * 
	 * @param clientName name-prefix like "AcsCommandCenter" or "OMC"
	 * @param managerLoc the manager corbaloc
	 * @param orb the orb to use
	 */
	protected MaciSupervisor(String clientName, String managerLoc, ORB orb, Logger log) {
		this.name = clientName + ".MaciSupervisor";
		this.managerLoc = managerLoc;
		this.orb = orb;
		this.log = log;


		acImpl = new AdministratorImplementation();

		// set up the invariable tree structure
		maciInfo = new MaciInfo();
	}

	private final String connect = "MaciSupervisor/connect: ";
	private final String disconnect = "MaciSupervisor/disconnect: ";
	private final String trigger = "MaciSupervisor/triggered: ";
	private final String read = "MaciSupervisor/read: ";
	private final String write = "MaciSupervisor/write: ";


	// ==================================================================
	// Recovering from Errors
	// ==================================================================

	public void setConnectsAutomatically (boolean b) {
		this.connectsAutomatically = b;
	}
	
	protected volatile boolean connectsAutomatically = true;
	
	protected volatile ManagerConnectionExceptionHandler mcehandler;

	protected class ManagerConnectionExceptionHandler {

		public ManagerConnectionExceptionHandler () {
			connectorThread = new ConnectorThread();
			connectorThread.setName("MaciSupervisor.Connector");
			connectorThread.setDaemon(true);
		}

		public void start () {
			connectorThread.start(); // thread will go to suspend instantly
		}

		public void stop () {
			connectorThread.interrupt(); // thread will terminate
		}

		public void handleExceptionTalkingToManager (UnknownErrorException exc) {
			System.err.println("PROBLEM: couldn't access Acs Manager for unexpected reason: " + exc);
			System.err.println("Please report this stacktrace to the Acs Team, together");
			System.err.println("with a description of what you've been doing:");
			System.err.println("---------------------------");
			exc.printStackTrace(System.err);
			System.err.println("---------------------------");
		}
		
		public void handleExceptionTalkingToManager (Exception exc) {
			if (!connectsAutomatically)
				return;

			MaciSupervisor.this.dismissManager();
			synchronized (connector) {
				connector.notify();
			}
		}



		// the lock should only be held for short periods
		protected final java.lang.Object connector = new java.lang.Object();

		protected final ConnectorThread connectorThread;
			
		// a notify on the wait object will awake the thread
		// an interrupt will terminate the thread
		protected class ConnectorThread extends Thread {
			
			@Override
			public void run () {
				ALIVE: while (true) {

					synchronized (connector) {
						try {
							connector.wait();
							log.fine(connect+"auto-connector triggered");
						} catch (InterruptedException exc) {
							break ALIVE; // quit
						}
					}

					RECONNECT: while (true) {
						try {
							MaciSupervisor.this.start();
							break RECONNECT; // managed to reconnect
							
						} catch (Exception exc) {
							log.fine(connect+"auto-connector attempt failed with "+exc);
							try {
								sleep(5000); // take a break
							} catch (InterruptedException exc2) {
								break ALIVE; // quit
							}
						}
					}
					log.fine(connect+"auto-connector suspended");
				}
			}
		};


	}



	// ==================================================================
	// API
	// ==================================================================


	/**
	 * This start implementation is smart, it can be called repeatedly.
	 * 
	 * @throws NoPermissionEx 
	 * @throws CorbaTransientException 
	 * @throws CorbaNotExistException 
	 * @throws UnknownErrorException 
	 */
	public synchronized void start() throws NoPermissionEx, CannotRetrieveManagerException, CorbaTransientException, CorbaNotExistException, UnknownErrorException {
		if (mcehandler == null) {
			mcehandler = new ManagerConnectionExceptionHandler();
			mcehandler.start();
		}

		enableRefreshTask(true);

		if (isConnected())
			return;

		try {
			this.connectToManager();
			return;

			// go into error handling. the default error handler will trigger the reconnect thread
		} catch (CannotRetrieveManagerException exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw exc;
			
		} catch (NoPermissionEx exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw exc;
			
		} catch (TRANSIENT exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw new CorbaTransientException(exc);

		} catch (OBJECT_NOT_EXIST exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw new CorbaNotExistException(exc);
			
		} catch (RuntimeException exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw new UnknownErrorException(exc);
		}
	}


	/**
	 * Tear down this instance.
	 */
	public synchronized void stop () {
		if (mcehandler!=null) {
			mcehandler.stop();
			mcehandler = null;
		}

		enableRefreshTask(false);

		disconnectFromManager();
	}


	/**
	 * msc (2004-11-09): This was requested, suggested, and agreed upon as a workaround for
	 * the situation where a manager has gone down (and the application using this
	 * macisupervisor knows that the manager is down, e.g. because it made it go down)
	 * while this macisupervisor does not yet know that the manager is down. It will only
	 * realize that the next time it tries to access it. Doing so will provoke some
	 * no_permission error messages in the manager log. To enable the application to avoid
	 * these error messages this API method was added.
	 */
	public synchronized void dismissManager () {
		administratorClientInfo = null;
	}


	/**
	 * Whether this is connected
	 */
	public boolean isConnected () {
		return administratorClientInfo != null;
	}

	/**
	 * 
	 * @throws NotConnectedToManagerException
	 */
	public int myMaciHandle () throws NotConnectedToManagerException {
		if (administratorClientInfo == null)
			throw new NotConnectedToManagerException("not logged in to manager");

		return administratorClientInfo.h;
	}

	/**
	 * 
	 * @throws NotConnectedToManagerException
	 */
	protected Manager myManagerReference () throws NotConnectedToManagerException {
		if (managerRef == null)
			throw new NotConnectedToManagerException("no reference to the manager");

		return managerRef;
	}


	/**
	 * Some people are interested in the weirdest things..
	 */
	public String getManagerLocation () {
		return this.managerLoc;
	}



	// ==================================================================
	// Internal
	// ==================================================================



	// ============= Connecting / Disconnecting ===============


	/**
	 * 
	 */
	protected void connectToManager () throws CannotRetrieveManagerException, NoPermissionEx, SystemException {
		log.fine(connect+"connecting to manager");

		try {

			org.omg.CORBA.Object object = orb.string_to_object(managerLoc);
			managerRef = ManagerHelper.narrow(object);

		} catch (Exception exc) {
			log.fine(connect+"failed to connect to manager: "+exc);
			throw new CannotRetrieveManagerException("could not retrieve manager reference", exc);
		}

		if (managerRef == null) {
			log.fine(connect+"failed to connect to manager: got null manager reference");
			throw new CannotRetrieveManagerException("orb delivered null-reference for manager-location " + managerLoc);
		}

		Administrator admin = acImpl.asCorbaObject(orb); // <-- may throw "port occupied"
		administratorClientInfo = managerRef.login(admin);

		log.info(connect+"connected to manager (" + getManagerLocation() + ") as "+administratorClientInfo.h +" (= 0x"+Integer.toHexString(administratorClientInfo.h) +")");
	}

	/**
	 * 
	 */
	protected void disconnectFromManager () {

		try {
			int hhhh = myMaciHandle();
			myManagerReference().logout(hhhh);

		} catch (NotConnectedToManagerException exc) {
			log.fine(disconnect+"couldn't log out from manager: " + exc);

		} catch (NoPermissionEx exc) {
			log.fine(disconnect+"couldn't log out from manager: " + exc);
		}

		dismissManager();
	}



	// ================== Retrieving Info from the Manager =================



	/**
	 * The containerHandles argument cannot be specified here. Reason: There's apparently a
	 * bug in Manager.get_activator_info() in ACS2.x (nothing known about ACS3.x): The
	 * containerHandles argument is not evaluated properly, instead empty (therefore
	 * useless) ContainerInfos are returned.
	 * 
	 * @param name_wildcard not verified to work as expected, recommended to use '*'.
	 * @return
	 * @throws NoPermissionEx
	 * @throws NotConnectedToManagerException
	 * @throws SystemException
	 */
	protected ContainerInfo[] retrieveContainerInfo (String name_wildcard) throws NotConnectedToManagerException, NoPermissionEx, SystemException {

		int hhhhh = myMaciHandle();
		int[] container_handles = new int[]{}; // bug workaround, see above
		return myManagerReference().get_container_info(hhhhh, container_handles, name_wildcard);
	}

	/**
	 * @throws NoPermissionEx 
	 * @throws NotConnectedToManagerException
	 * @throws SystemException
	 */
	protected ClientInfo[] retrieveClientInfo (String name_wildcard) throws NotConnectedToManagerException, NoPermissionEx, SystemException {

		int hhhhh = myMaciHandle();
		int[] client_handles = new int[]{};
		return myManagerReference().get_client_info(hhhhh, client_handles, name_wildcard);
	}

	/**
	 * @throws NoPermissionEx 
	 * @throws NotConnectedToManagerException
	 * @throws SystemException
	 */
	protected ComponentInfo[] retrieveComponentInfo (String name_wildcard) throws NotConnectedToManagerException, NoPermissionEx, SystemException {

		int hhhhh = myMaciHandle();
		int[] component_handles = new int[]{};
		String type_wildcard = "*";
		boolean active_only = false;
		return myManagerReference().get_component_info(hhhhh, component_handles, name_wildcard, type_wildcard, active_only);
	}


	final protected MaciInfo maciInfo;

	/**
	 * Returns a TreeModel compiled from information from the Acs manager. The returned
	 * model will never become invalid during the lifetime of this MaciSupervisor, and it
	 * will be automatically updated.
	 * <p>
	 * A call to this method will automatically trigger a refresh, possibly 
	 * throwing an exception or blocking the current thread for a long time. Calling
	 * {@link #getMaciInformation()} and {@link #refreshSoon()} will shield you from
	 * these effects.
	 *
	 * @return a stable maciInfo instance, never <code>null</code>
	 * @throws NotConnectedToManagerException 
	 * @throws NoPermissionEx 
	 * @throws UnknownErrorException 
	 * @throws CorbaNotExistException 
	 * @throws CorbaTransientException
	 */
	public MaciInfo getMaciInfo () throws NoPermissionEx, NotConnectedToManagerException, CorbaTransientException, CorbaNotExistException, UnknownErrorException {

		refreshNow();

		return maciInfo;
	}

	
	/**
	 * Returns a TreeModel compiled from information from the Acs manager. The returned
	 * model will never become invalid during the lifetime of this MaciSupervisor, and it
	 * will be automatically updated.
	 * <p>
	 * The returned model may be out-of-date. Call {@link #refreshSoon()} to have it updated.
	 *
	 * @return a stable maciInfo instance, never <code>null</code>
	 */
	public MaciInfo getMaciInformation () {
		return maciInfo;
	}

	/**
	 * Request the MaciInfo be refreshed in the background.
	 */
	public void refreshSoon() {
		infoShouldBeRefreshed = true;
	}


	/**
	 * Refresh every once in a while, even if there wasn't any
	 * indication from the manager that something has changed.
	 */
	public synchronized void setRefreshesPeriodically (boolean b) {
		this.refreshCountdown = 0; // provoke refresh at next run (if refreshing enabled)
		this.refreshWithoutACause = b;
	}

	protected volatile boolean refreshWithoutACause;
	protected volatile int refreshCountdown;

	protected volatile int refreshCountFrom = 5;
	protected volatile int refreshLag = 2;
	
	private synchronized void enableRefreshTask (boolean b) {
		if (b==true && refreshTask==null) {
			refreshTask = new RefreshIfNeeded();
			timer.schedule (refreshTask, 2000, refreshLag*1000);
		}
		if (b==false && refreshTask!=null) {
			refreshTask.cancel();
			refreshTask = null;
		}
	}

	/**
	 * Configures the polling rate and responsiveness for updates from
	 * the manager. Updates from the manager are not applied one-by-one
	 * but are instead collected for a certain length of time (ie. the
	 * <b>lag</b>) and then applied altogether. The manager does not
	 * send notifications for all potential changes that happen in the
	 * system. It is thus recommendable to poll the manager for new
	 * information in regular intervals (ie. the <b>period</b>).
	 *
	 * @param lag - the <i>worst-case response time</i> before updates
	 * from the manager will be adopted. Note that the <i>average response
	 * time</i> is only 1/2 of this. By accepting a higher lag you can
	 * reduce the load on the manager, and vice-versa.
	 *
	 * @param period - the interval by which the manager is polled for
	 * new updates. By accepting a longer period you can reduce the load
	 * on the manager, and vice-versa.
	 */
	public synchronized void setRefreshDelay (int lag, int period) {

		if (lag != refreshLag) {
			refreshLag = lag;
			
			// need to restart the task
			enableRefreshTask(false);
			enableRefreshTask(true);
		}

		int countfrom = period/lag;
		if (countfrom != refreshCountFrom) {
			refreshCountFrom = countfrom;
			refreshCountdown = countfrom;
		}
	}

	/*
	 * We check 'refresh needed' regularly
	 */
	protected RefreshIfNeeded refreshTask;
	protected Timer timer = new Timer("MaciSupervisor.Refresher", true);
	

	protected class RefreshIfNeeded extends TimerTask {

		@Override
		public void run () {

			/*
			 * Every n'th run we refresh, even if no incoming event has set the
			 * "shouldBeRefreshed" flag.
			 *
			 * But we shouldn't refresh blindly, otherwise we'd produce periodic errors when
			 * the manager is not available. We keep the flag set, so if the manager becomes
			 * available, we will refresh.
			 */
			if (refreshWithoutACause) {
				if (refreshCountdown > 0)
					refreshCountdown--;

				if (refreshCountdown <= 0)
					infoShouldBeRefreshed = true;

				if (!isConnected())
					return;
			}


			/* We reset it here. If the refresh succeeds this is the obvious behavior,
			 * but also in case of error this prevents us from indefinitely trying to
			 * poll info from a not-responding manager. */
			if (infoShouldBeRefreshed) {
				infoShouldBeRefreshed = false;
				refreshCountdown = refreshCountFrom;
				try {
					refreshNow();
				} catch (Throwable exc) {
					/* Timer dies on uncaught exceptions,
					 * but we're not interested in errors */}
			}
		}
	};


	/**
	 * Tries to refresh the component-section, container-section, and client-section
	 * of the info tree model. The treemodel will be updated when things go well
	 * and also when things fail.
	 * 
 	 * A call to this method will instantly perform a refresh, possibly 
	 * throwing an exception or blocking the current thread for a long time. Calling
	 * {@link #getMaciInformation()} and {@link #refreshSoon()} will shield you from
	 * these effects.
	 * 
	 * @throws NoPermissionEx error during refresh
	 * @throws NotConnectedToManagerException error during refresh
	 * @throws CorbaTransientException 
	 * @throws CorbaNotExistException 
	 * @throws UnknownErrorException 
	 */
	public synchronized void refreshNow() throws NoPermissionEx, NotConnectedToManagerException, SystemException, CorbaTransientException, CorbaNotExistException, UnknownErrorException {
		log.fine(read+"retrieving acs deployment info from acs manager");

		List<ComponentInfo> newComponents = Collections.EMPTY_LIST;
		List<ContainerInfo> newContainers = Collections.EMPTY_LIST;
		List<ClientInfo>    newClientApps = Collections.EMPTY_LIST;
		Map<Object,String>  newAuxiliary  = Collections.EMPTY_MAP;
		boolean nothingChanged = false;

		try {
	
			try {
				// retrieve data from manager
				// -------------------------------
				newComponents = Arrays.asList( this.retrieveComponentInfo("*"));
				newContainers = Arrays.asList( this.retrieveContainerInfo("*"));
				newClientApps = Arrays.asList( this.retrieveClientInfo("*"));
				
			/* If the retrieval bails out it is (as far as i've seen)
			 * always because the manager is not reachable at all.
			 * thus, there's no need to try and retrieve e.g. the 
			 * clients if the components have already failed. thus,
			 * one exception handler for all retrievals is enough */
	
			} catch (NotConnectedToManagerException exc) {
				log.fine(read+"problem: " + exc);
				mcehandler.handleExceptionTalkingToManager(exc);
				throw exc;
	
			} catch (NoPermissionEx exc) {
				log.fine(read+"problem: " + exc);
				mcehandler.handleExceptionTalkingToManager(exc);
				throw exc;
	
			} catch (org.omg.CORBA.TRANSIENT exc) {
				log.fine(read+"problem: " + exc);
				mcehandler.handleExceptionTalkingToManager(exc);
				throw new CorbaTransientException(exc);
	
			} catch (org.omg.CORBA.OBJECT_NOT_EXIST exc) {
				log.fine(read+"problem: " + exc);
				mcehandler.handleExceptionTalkingToManager(exc);
				throw new CorbaNotExistException(exc);
	
			} catch (RuntimeException exc) {
				log.fine(read+"problem: " + exc);
				mcehandler.handleExceptionTalkingToManager(exc);
				throw new UnknownErrorException(exc);
			}

			diffComponents.diff (maciInfo.components, newComponents);
			diffContainers.diff (maciInfo.containers, newContainers);
			diffClientApps.diff (maciInfo.clientApps, newClientApps);

			if (diffComponents.areEqual() 
				&& diffContainers.areEqual() 
				&& diffClientApps.areEqual()) {
				log.finer(write+"no change found");
				nothingChanged = true;

			} else {

				// we provide some additional info over what is available in the manager's
				// info structs. we must compute it upfront since you need an ORB to do it.
				newAuxiliary = new HashMap<Object,String>();
				
				for (ContainerInfo info : newContainers) {
					int infokey = System.identityHashCode(info);
					newAuxiliary.put(infokey+".location", extractLocation(info.reference));
				}
				for (ClientInfo info : newClientApps) {
					int infokey = System.identityHashCode(info);
					newAuxiliary.put(infokey+".location", extractLocation(info.reference));
				}
				
			}

		} finally {
			if (nothingChanged == false) {
				log.fine(write+"writing changes to maci-info structure");
				maciInfo.setContents (newComponents, newContainers, newClientApps, newAuxiliary);
		   }
		}

	}

	
	

	/**
	 * assigned in connectToManager()
	 */
	protected Manager managerRef = null;


	/**
	 * Overridden to provide pretty description.
	 */
	@Override
	public String toString () {
		String s1 = managerLoc;
		if (s1 == null) {
			s1 = "<no manager location>";
		} else if (s1.length() > 30) { // limit to 30 chars
			s1 = s1.substring(0, 22) + "..." + s1.substring(s1.length() - 5, s1.length());
		}
		return getClass().getName() + "[mgr=" + s1 + "]";
	}


	// ==================================================================
	// Maci Info Cache
	// ==================================================================


	/**
	 * The 'refresh needed' flag used by the lazy-refresh timer task.
	 */
	protected volatile boolean infoShouldBeRefreshed;


	private final ListDiff<ComponentInfo, String, ComponentInfo> diffComponents =
		new ListDiff<ComponentInfo, String, ComponentInfo>() {
		@Override protected String identifyExisting (ComponentInfo x) {return x.name;}
		@Override protected String identifyIncoming (ComponentInfo x) {return x.name;}
		@Override protected boolean isUpdate (ComponentInfo e, ComponentInfo i) {
			return !Arrays.deepEquals(
				new Object[]{e.h, e.container, e.clients},
				new Object[]{i.h, i.container, i.clients});
		}
	};

	private final ListDiff<ContainerInfo, String, ContainerInfo> diffContainers =
		new ListDiff<ContainerInfo, String, ContainerInfo>() {
		@Override protected String identifyExisting (ContainerInfo x) {return x.name;}
		@Override protected String identifyIncoming (ContainerInfo x) {return x.name;}
		@Override protected boolean isUpdate (ContainerInfo e, ContainerInfo i) {
			return !Arrays.deepEquals(
				new Object[]{e.h, e.components},
				new Object[]{i.h, i.components});
		}
	};

	private final ListDiff<ClientInfo, Integer, ClientInfo> diffClientApps =
		new ListDiff<ClientInfo, Integer, ClientInfo>() {
		@Override protected Integer identifyExisting (ClientInfo x) {return x.h;}
		@Override protected Integer identifyIncoming (ClientInfo x) {return x.h;}
		@Override protected boolean isUpdate (ClientInfo e, ClientInfo i) {
			return !Arrays.deepEquals(
				new Object[]{e.name, e.components},
				new Object[]{i.name, i.components});
		}
	};



	/**
	 * Helper for createNode()
	 * 
	 * @return <code>{host, port}</code>, or <code>null</code> on failure
	 */
	protected String extractLocation (org.omg.CORBA.Object reference) {
		String location;
		try {
			location = orb.object_to_string(reference);
			String[] hostport = AcsLocations.convert(location);
			location = hostport[0] + ":" + hostport[1];
		} catch (Exception e) {
			location = null;
		}
		return location;
	}



	// ==================================================================
	// Administrator Client Implementation
	// ==================================================================

	
	/**
	 * assigned in connectToManager(). unassigned in disconnectFromManager(). This means,
	 * this field indicates the "connected" status.
	 */
	protected volatile ClientInfo administratorClientInfo = null;

	/**
	 * Instantiated in constructor. The retrieved descriptor will be stored in field
	 * administratorClientInfo.
	 */
	protected volatile AdministratorImplementation acImpl;


	/**
	 * Implementation of Administrator Interface
	 */
	protected class AdministratorImplementation extends AdministratorPOA {

		private Administrator asCorbaObject;

		// instantiate client, make sure we call _this() only once
		// to allow the manager to re-identify us on a re-login.
		protected Administrator asCorbaObject (ORB orb) {
			if (asCorbaObject == null)
				asCorbaObject = _this(orb); // <-- may throw "port occupied"
			return asCorbaObject;
		}

		// ===== Incoming Notifications =====


		public void client_logged_in (ClientInfo info, long timestamp, long execution_id) {
			log.finer(trigger+"client_logged_in() received");
			infoShouldBeRefreshed = true;
		}

		public void client_logged_out (int h, long timestamp) {
			log.finer(trigger+"client_logged_out() received");
			infoShouldBeRefreshed = true;
		}

		public void container_logged_in (ContainerInfo info, long timestamp, long execution_id) {
			log.finer(trigger+"container_logged_in() received");
			infoShouldBeRefreshed = true;
		}

		/**
		 * For some ways a container can terminate, the
		 * components_released() method will be called
		 * shortly after this, but not for all. 
		 */
		public void container_logged_out (int h, long timestamp) {
			log.finer(trigger+"container_logged_out() received");
			infoShouldBeRefreshed = true;
		}

		/** 
		 * Will be called for autostart-components, too.
		 */
		public void components_requested (int[] clients, int[] components, long timestamp) {
			log.finer(trigger+"components_requested() received");
			infoShouldBeRefreshed = true;
		}

		/**
		 * Will always be called when a client (that was holding
		 * components) has terminated in any way.
		 */
		public void components_released (int[] clients, int[] components, long timestamp) {
			log.finer(trigger+"components_released() received");
			infoShouldBeRefreshed = true;
		}

		/**
		 * maci.idl: this is a notification about components this client is holding (= has
		 * requested and not yet released)
		 */
		public void components_available (ComponentInfo[] arg0) {
			//log.finer("fyi: components_available() received");
		}

		/**
		 * maci.idl: this is a notification about components this client is holding (= has
		 * requested and not yet released)
		 */
		public void components_unavailable (String[] arg0) {
			//log.finer("fyi: components_unavailable() received");
		}

		public void component_activated(ComponentInfo info, long timestamp, long execution_id) {
			//log.finer("fyi: component_activated() received");
		}

		public void component_deactivated(int h, long timestamp) {
			//log.finer("fyi: component_deactivated() received");
		}
		

		// ===== Requests from the Manager =====


		public String name () {
			//log.finer("fyi: name() received");
			return name;
		}

    	private final long startTimeUTClong = UTCUtility.utcJavaToOmg(System.currentTimeMillis());		
      private long executionId = -1; 

		public AuthenticationData authenticate (long execution_id, String question) {
			log.finer("fyi: authenticate() received");
			
	        // keep old executionId if it exists
	        if (executionId < 0) {
	        	executionId = execution_id;
	        }
	        
			AuthenticationData ret = new AuthenticationData(
					 "S", 
					 ClientType.ADMINISTRATOR_TYPE,
					 ImplLangType.JAVA,
					 false, 
					 startTimeUTClong,
					 executionId);
			return ret;
		}

		public void message (short arg0, String arg1) {
			//log.finer("fyi: message() received");
			log.info("Incoming Maci Network Message: '" + arg1 + "'");
		}

		public void taggedmessage(short type, short messageID, String message) {
			//log.finer("fyi: taggedmessage() received");
			log.info("Incoming Maci Network Message: '" + message + "' (messageId="+messageID+")");
		}

		public boolean ping () {
			//log.finer("fyi: ping() received");
			return true;
		}

		public void disconnect () {
			log.finer(trigger+"disconnect() received");
			disconnectFromManager();
		}
	}


}
