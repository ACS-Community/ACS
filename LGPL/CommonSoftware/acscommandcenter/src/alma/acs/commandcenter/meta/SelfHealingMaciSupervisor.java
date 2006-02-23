/*
 * Created on Jul 20, 2005 by mschilli
 */
package alma.acs.commandcenter.meta;

import javax.swing.tree.DefaultTreeModel;

import org.omg.CORBA.ORB;
import org.omg.CORBA.Object;

import alma.acs.commandcenter.meta.IMaciSupervisor.NotConnectedToManagerException;

import si.ijs.maci.ClientInfo;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ContainerInfo;


/**
 * Wraps all methods ("guarded methods") of an underlying {@link IMaciSupervisor} with
 * some reconnect-logic.
 * 
 * @author mschilli
 */
public class SelfHealingMaciSupervisor implements IMaciSupervisor {

	protected IMaciSupervisor inner;

	/**
	 * Creates an instance that will automatically reconnect if one of its methods
	 * failes with one of the typical connection-related runtime exceptions. 
	 * 
	 * @param inner the MaciSupervisor that does the underlying work
	 */
	public SelfHealingMaciSupervisor(IMaciSupervisor inner) {
		this.inner = inner;
		connector.start();
	}

	/** 
	 * Invoked from {@link SelfHealingMaciSupervisor#stop()}. 
	 */
	protected void myStop() {
		connector.interrupt();
	}

	// ////////////////////////////////////////////////////////////
	// ------------ Recovering from Errors -------------------- //
	// ////////////////////////////////////////////////////////////

	// the lock should only be held for short periods
	protected java.lang.Object wait = new java.lang.Object();

	// a notify on the wait object will awake the thread
	// an interrupt will terminate the thread
	protected Thread connector = new Thread() {

		public void run () {
			ALIVE: while (true) {

				synchronized (wait) {
					try {
						wait.wait();
					} catch (InterruptedException exc) {
						break ALIVE; // quit
					}
				}

				RECONNECT: while (true) {

					try {
						inner.start();
						break RECONNECT; // managed to reconnect

					} catch (Exception e) { // failed to reconnect
						try {
							sleep(5000); // take a break
						} catch (InterruptedException exc) {
							break ALIVE; // quit
						}
					}
				}

			}
		}
	};

	
	protected void handleException (RuntimeException exc) {

		if (exc instanceof org.omg.CORBA.NO_PERMISSION) {
			seemsManagerHasChangedOrHasCutConnection();

		} else if (exc instanceof IllegalStateException) {
			seemsWeHaveDisconnected();

		} else if (exc instanceof NotConnectedToManagerException) {
			seemsWeHaveDisconnected();

		} else if (exc instanceof org.omg.CORBA.OBJECT_NOT_EXIST) {
			seemsManagerIsDown();

		} else if (exc instanceof org.omg.CORBA.TRANSIENT) {
			seemsManagerIsDown();

		} else if (exc instanceof ComponentUnknownInLocalCacheException) {
			// given that the cache is up-to-date, the component is not in CDB 

		} else if (exc instanceof ComponentNotActivatedInLocalCacheException) {
			// given that the cache is up-to-date, the component is not activated

		} else if (exc instanceof ContainerUnknownInLocalCacheException) {
			// given that the cache is up-to-date, the container doesn't exist

		} else {
			System.err.println("PROBLEM: couldn't access Acs Manager for unknown reason.");
			System.err.println("Please report the following lines to the Acs Team: " + exc);
			System.err.println("---------------------------");
			exc.printStackTrace(System.err);
			System.err.println("---------------------------");
		}

	}


	protected void seemsManagerIsDown () {
		dismissManager();

		synchronized (wait) {
			wait.notify();
		}
	}

	protected void seemsManagerHasChangedOrHasCutConnection () {
		dismissManager();

		synchronized (wait) {
			wait.notify();
		}
	}

	protected void seemsWeHaveDisconnected () {
		dismissManager();

		synchronized (wait) {
			wait.notify();
		}
	}



	// ////////////////////////////////////////////////////////// //
	// ------------------- Guarded Methods --------------------- //
	// ////////////////////////////////////////////////////////// //


	public void start () {
		try {

			inner.start();

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}

	//
	public void stop () {
		try {

			inner.stop();

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
		
		myStop();
	}

	//
	public void shutdownManager () {
		try {

			inner.shutdownManager();

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}

	//
	public void logoutContainer (ContainerInfo info) {
		try {

			inner.logoutContainer(info);

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}

	//
	public void logoutClient (ClientInfo info) {
		try {

			inner.logoutClient(info);

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}

	//
	public void shutdownContainers () {
		try {

			inner.shutdownContainers();

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}

	//
	public void shutdownContainer (String name) {
		try {

			inner.shutdownContainer(name);

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}

	//
	public void disconnectContainer (String name) {
		try {

			inner.disconnectContainer(name);

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}

	//
	public void pingContainer (String name) {
		try {

			inner.pingContainer(name);

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}

	//
	public ContainerInfo[] retrieveContainerInfo (String name_wildcard) {
		try {

			return inner.retrieveContainerInfo(name_wildcard);

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}

	//
	public ClientInfo[] retrieveClientInfo () {
		try {

			return inner.retrieveClientInfo();

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}

	//
	public ComponentInfo[] retrieveComponentInfo (int[] cobHandles, String name_wildcard, String type_wildcard, boolean active_only) {
		try {

			return inner.retrieveComponentInfo(cobHandles, name_wildcard, type_wildcard, active_only);

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}

	public Object getComponent (String curl) {
		try {

			return inner.getComponent(curl);

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}
	
	public void forceReleaseComponent(String curl) {
		try {

			inner.forceReleaseComponent(curl);

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}

	
	public void releaseComponents (String[] curls) {
		try {

			inner.releaseComponents(curls);

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}
	
	//
	public DefaultTreeModel getMaciInfo () {
		try {

			return inner.getMaciInfo();

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}


	public void shutdownContainer (ContainerInfo info) {
		try {

			inner.shutdownContainer(info);

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}

	public void disconnectContainer (ContainerInfo info) {
		try {

			inner.disconnectContainer(info);

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}

	public void pingContainer (ContainerInfo info) {

		try {

			inner.pingContainer(info);

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}

	public void sendMessageToContainer (ContainerInfo info, short msgType, String msg) {

		try {

			inner.sendMessageToContainer(info, msgType, msg);

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}

	public void disconnectClient (ClientInfo info) {

		try {

			inner.disconnectClient(info);

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}

	public void pingClient (ClientInfo info) {

		try {

			inner.pingClient(info);

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}

	public void sendMessageToClient (ClientInfo info, short msgType, String msg) {

		try {

			inner.sendMessageToClient(info, msgType, msg);

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}


	public ContainerInfo getContainerInfoForComponent (String componentName) {

		try {

			return inner.getContainerInfoForComponent(componentName);

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}

	
	
	// ////////////////////////////////////////////////////////// //
	// ----------------- NON-Guarded Methods ------------------- //
	// ////////////////////////////////////////////////////////// //

	public boolean isConnected() {
		return inner.isConnected();
	}

	public void dismissManager () {
		inner.dismissManager();
	}

	public String getManagerLocation () {
		return inner.getManagerLocation();
	}

	

}
