/*
 * Created on Aug 29, 2003 by mschilli
 */
package alma.acs.commandcenter.meta;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.PortableServer.POA;

import alma.acs.component.client.ComponentClient;
import alma.acs.container.ContainerServices;
import alma.acs.container.corba.AcsCorba;
import alma.acs.util.AcsLocations;


/**
 * Helps to bring an application into the Acs environment.
 * <p>
 * Usage: When interested in component retrieval:
 * <ol>
 * <li>fs = new Firestarter();
 * <li>fs.prepareContainerServices(null, null);
 * <li>fs.giveContainerServices()
 * </ol>
 * 
 * When interested in Maci Administration:
 * <ol>
 * <li>fs = new Firestarter();
 * <li>fs.prepareSupervisorFactory(null)
 * <li>fs.giveSupervisorFactory()
 * </ol>
 * 
 * Eventually, in both cases, you should always call:
 * <ul>
 * <li>fs.shutdown();
 * </ul>
 * 
 * </p>
 * 
 * 
 * @author mschilli
 */
public class Firestarter {

	protected String clientName = null;

	protected AcsCorba acsCorba = null;
	protected POA rootPoa = null;
	protected ComponentClient componentClient = null;
	protected MaciSupervisorFactory maciSupervisorFactory = null;

	protected Logger firestarterLog = null;
	
	protected Logger acscorbaLog = null;
	protected Integer orbPort = null;
	protected Integer orbPortSearchRetry = null;


	// ==========================================================
	// Firestarter
	// ==========================================================


	/**
	 * Creates a Firestarter for a client with the specified name (like "AcsCommandCenter"
	 * or "OMC").
	 * 
	 * @param clientName will be used as namespace prefix for the default-loggers
	 * 
	 * @see 
	 * @since ACS 5.0
	 */
	public Firestarter (String clientName) {
		this(clientName, null);
	}


	/**
	 * Creates a Firestarter for a client with the specified name (like "AcsCommandCenter"
	 * or "OMC") and the specified Logger.
	 * 
	 * @param clientName will be used as namespace prefix for the default-loggers
	 * @param firestarterLog will be used to log messages from this class
	 * 
	 * @see 
	 * @since ACS 5.0
	 */
	public Firestarter (String clientName, Logger firestarterLog) {

		if (clientName == null) {
			throw new NullPointerException("clientName should not be null");
		}
	
		if (firestarterLog == null) {
			firestarterLog = Logger.getLogger(clientName + ".Firestarter");
		}

		this.clientName = clientName;
		this.firestarterLog = firestarterLog;
	}
	


	/**
	 * Should be invoked to clean up the Firestarter.
	 */
	public void shutdown () {

		try {
			if (componentClient != null) {
				componentClient.tearDown();
				componentClient = null;
			}
		} catch (Throwable t) {
			firestarterLog.log(Level.INFO, "failed to stop componentclient", t);
		}

		try {
			if (maciSupervisorFactory != null) {
				maciSupervisorFactory.stop();
			   maciSupervisorFactory = null;
			}
		} catch (Throwable t) {
			firestarterLog.log(Level.INFO, "failed to stop macisupervisor(s)", t);
		}

		try {
			if (acsCorba != null) {
				new Thread() {

					public void run () {
						acsCorba.shutdownORB(false);
					}
				}.start();
				acsCorba.blockOnORB();
				acsCorba = null;
				rootPoa = null;
			}
		} catch (Throwable t) {
			firestarterLog.log(Level.INFO, "failed to stop orb", t);
		}

	}


	// ==========================================================
	// ContainerServices
	// ==========================================================

	
	/**
	 * This class is <i>fail-fast</i> in that it tries to set up the remote connectivity
	 * instantly on a call to this method. If this first call goes well, subsequent calls
	 * will succeed (aside from other general errors).
	 * <p>
	 * After the first *successful* execution, it will ignore subsequent invokations. This
	 * means, this method can be called as often as wanted.</p>
	 * <p>
	 * If you don't specify the <code>managerLoc</code>, this will delegate to
	 * {@link alma.acs.util.AcsLocations} to find the manager. </p>
	 * 
	 * @param compclientLog use <code>null</code> for default
	 * @param managerLoc use <code>null</code> for default
	 * @throws Exception if construction of {@link Logger} or {@link ComponentClient} fails
	 */
	public void prepareContainerServices (Logger compclientLog, String managerLoc) throws Exception {

		if (componentClient != null) {
			return; // already prepared
		}
		
		if (compclientLog == null) {
			compclientLog = Logger.getLogger(clientName + ".ComponentClient");
		} 

		if (managerLoc == null) {
			managerLoc = AcsLocations.figureOutManagerLocation();
		} 

		/* the following call may fail with exception. if it succeeds, 
		 * we can continue working with acsCorba and rootPoa
		 */
		prepareAcsCorba(); 
		
		this.componentClient = createComponentClient(compclientLog, managerLoc);
	}



	/**
	 * Hook for subclasses
	 * 
	 * @throws Exception if creation of component client fails
	 */
	protected ComponentClient createComponentClient (Logger compclientLog, String managerLoc) throws Exception {
		ComponentClient ret = new MyComponentClient(compclientLog, managerLoc, clientName+".ComponentClient");
		return ret;
	}


	/**
	 * ComponentClient extension that does not create its own AcsCorba instance, but
	 * instead works with the AcsCorba instance defined in Firestarter.
	 */
	protected class MyComponentClient extends ComponentClient {

		public MyComponentClient(Logger logger, String managerLoc, String clientName) throws Exception {
			super(logger, managerLoc, clientName);
		}

		protected POA initAcsCorba () throws Exception {
			return rootPoa;
		}

	}

	/**
	 * Returns the {@link ContainerServices}. Note: {@link #prepareContainerServices()}
	 * must have been executed successfully for this method to work
	 * 
	 * @throws IllegalStateException if {@link #prepareContainerServices()} was not yet
	 *            executed successfully
	 * @return the {@link ContainerServices}
	 */
	public ContainerServices giveContainerServices () {
		if (componentClient == null) {
			throw new IllegalStateException("container services not prepared");
		}
		return componentClient.getContainerServices();
	}
	
	
	// ==========================================================
	// MaciSupervisors
	// ==========================================================


	/**
	 * This class is <i>fail-fast</i> in that it tries to set up the remote connectivity
	 * instantly on a call to this method. If this first call goes well, subsequent calls
	 * will succeed (aside from other general errors).
	 * 
	 * @param factoryLog use <code>null</code> for default
	 * @throws Exception if construction of {@link MaciSupervisor} fails
	 */
	public void prepareSupervisorFactory (Logger factoryLog) throws Exception {

		if (maciSupervisorFactory != null) {
			return; // already prepared
		}

		if (factoryLog == null) {
			factoryLog = Logger.getLogger(clientName + ".Factory");
		} 
		
		/* the following call may fail with exception. if it succeeds, 
		 * we can continue working with acsCorba and acsCorba.getORB()
		 */
		prepareAcsCorba();

		maciSupervisorFactory = new MaciSupervisorFactory(factoryLog, acsCorba.getORB());
	}

	/**
	 * Returns a MaciSupervisorFactory. Note: {@link #prepareSupervisorFactory()} must have been executed
	 * successfully for this method to work
	 * 
	 * @throws IllegalStateException if {@link #prepareSupervisorFactory()} was not yet executed
	 *            successfully
	 * @return a MaciSupervisor
	 */
	public MaciSupervisorFactory giveSupervisorFactory () {
		if (maciSupervisorFactory == null) {
			throw new IllegalStateException("supervisor factory not prepared");
		}
		return maciSupervisorFactory;
	}

	

	// ==========================================================
	// AcsCorba
	// ==========================================================
	
	
	/**
	 * Set AcsCorba's logger and orb-port strategy.
	 * <p>
	 * For a description of the parameters <code>orbPort</code> and
	 * <code>orbPortSearchRetry</code>, see
	 * {@link AcsCorba#setPortOptions(Integer, Integer)}.
	 * </p>
	 * 
	 * @param acscorbaLog use <code>null</code> for default
	 * @param orbPort use <code>null</code> for default
	 * @param orbPortSearchRetry use <code>null</code> for default
	 */
	public void setAcsCorbaOptions (Logger acscorbaLog, Integer orbPort, Integer orbPortSearchRetry) {
		this.acscorbaLog = acscorbaLog;
		this.orbPort = orbPort;
		this.orbPortSearchRetry = orbPortSearchRetry;
	}


	/**
	 * This class is <i>fail-fast</i> in that it tries to create an {@link AcsCorba}
	 * instantly on a call to this method. If this first call goes well, subsequent calls
	 * will succeed (aside from other general errors).
	 * 
	 * @throws Exception if construction of {@link AcsCorba} fails
	 */
	protected void prepareAcsCorba () throws Exception {

		if (this.rootPoa != null) {
			return; // already prepared
		}

		if (acscorbaLog == null) {
			acscorbaLog = Logger.getLogger(clientName + ".AcsCorba");
		}
		
		acsCorba = new AcsCorba(acscorbaLog);
		acsCorba.setPortOptions(orbPort, orbPortSearchRetry);

		boolean isAdmin = true;
		rootPoa = acsCorba.initCorbaForClient(isAdmin);
	}


}
