/*
 * Created on Aug 29, 2003 by mschilli
 */
package alma.acs.commandcenter.meta;

import java.util.logging.Level;
import java.util.logging.Logger;

import alma.acs.component.client.AdvancedComponentClient;
import alma.acs.container.corba.AcsCorba;
import alma.acs.util.AcsLocations;


/**
 * Helper for advanced Acs clients.
 * <p>
 * Create a subclass and, potentially, set some of the fields to desired values:
 * <ul>
 * <li> Used by acscorba:
 * <li> --- acscorbaLog
 * <li> --- orbPort
 * <li> --- orbPortSearchRetry
 * <li> Used by componentclient:
 * <li> --- compclientLog
 * <li> --- managerLoc
 * <li> Used by this class itself:
 * <li> --- firestarterLog
 * </ul>
 * or leave them <code>null</code> to use defaults.
 * 
 * <p>
 * Then use these methods to have some work done for you:
 * <ul>
 * <li>giveOrb()
 * <li>giveAdvancedComponentClient()
 * </ul>
 * 
 * <p>
 * Finally, stop this firestarter through a call to
 * <ul>
 * <li>shutdown()
 * </ul>
 * 
 * @author mschilli
 */
public class Firestarter {


	// ==========================================================
	// API to use
	// ==========================================================


	protected String clientName = null;


	/**
	 * Creates a Firestarter for a client with the specified name (like "AcsCommandCenter"
	 * or "OMC") and the specified Logger.
	 * 
	 * @param clientName will be used as namespace prefix for the default-loggers
	 * 
	 * @see
	 * @since ACS 5.0
	 */
	protected Firestarter(String clientName) {
		if (clientName == null)
			throw new NullPointerException("clientName should not be null");

		this.clientName = clientName;
	}


	// used when creating the acscorba
	protected Logger acscorbaLog = null;
	protected Integer orbPort = null;
	protected Integer orbPortSearchRetry = null;

	// used when creating the componentclient
	protected Logger compclientLog = null;
	protected String managerLoc = null;

	// used when shutting down this firestarter
	protected Logger firestarterLog = null;


	/**
	 * Should only be used by people who know what they're doing!
	 * 
	 * @return an advanced component client
	 * @throws Exception any connection-related exception
	 */
	protected AdvancedComponentClient giveAdvancedComponentClient () throws Exception {
		prepareAdvancedComponentClient(); // may fail

		return advancedComponentClient;
	}



	/**
	 * Should only be used by people who know what they're doing!
	 * 
	 * @return the orb
	 * @throws Exception any connection-related exception
	 */
	protected org.omg.CORBA.ORB giveOrb () throws OrbInitException {
		prepareAcsCorba(); // may fail

		return acsCorba.getORB();
	}


	/**
	 * Should be invoked to clean up the Firestarter.
	 */
	public void shutdown () {

		try {
			if (advancedComponentClient != null) {
				advancedComponentClient.tearDown();
				advancedComponentClient = null;
			}
		} catch (Throwable t) {
			if (firestarterLog == null)
				firestarterLog = Logger.getLogger(clientName + ".Firestarter");
			firestarterLog.log(Level.INFO, "failed to stop advancedcomponentclient", t);
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
			}
		} catch (Throwable t) {
			if (firestarterLog == null)
				firestarterLog = Logger.getLogger(clientName + ".Firestarter");
			firestarterLog.log(Level.INFO, "failed to stop orb", t);
		}

	}


	// ==========================================================
	// AdvancedComponentClient
	// ==========================================================


	protected AdvancedComponentClient advancedComponentClient = null;

	/**
	 * Helper, sets up an advanced component client if necessary.
	 * 
	 * @throws Exception
	 */
	protected void prepareAdvancedComponentClient () throws Exception {

		if (advancedComponentClient != null)
			return; // already prepared


		if (compclientLog == null)
			compclientLog = Logger.getLogger(clientName + ".ComponentClient");

		if (managerLoc == null)
			managerLoc = AcsLocations.figureOutManagerLocation();


		prepareAcsCorba(); // may fail

		AdvancedComponentClient x = new AdvancedComponentClient(compclientLog, managerLoc, clientName + ".ComponentClient",
				acsCorba);
		advancedComponentClient = x;

	}



	// ==========================================================
	// AcsCorba
	// ==========================================================


	protected AcsCorba acsCorba = null;

	/**
	 * Helper, sets up an acs corba instance if necessary.
	 * 
	 * @throws OrbInitException if initialization of {@link AcsCorba} fails
	 */
	protected void prepareAcsCorba () throws OrbInitException {

		if (acsCorba != null && acsCorba.isInitialized())
			return; // already prepared


		if (acscorbaLog == null)
			acscorbaLog = Logger.getLogger(clientName + ".AcsCorba");


		acsCorba = new AcsCorba(acscorbaLog);
		acsCorba.setPortOptions(orbPort, orbPortSearchRetry);
		boolean isAdmin = true;
		
		try {
			acsCorba.initCorbaForClient(isAdmin);
		} catch (Exception exc) {
			throw new OrbInitException("acsCorba.initCorbaForClient(true) failed", exc);
		}
	}

	
	public static class OrbInitException extends Exception {

		OrbInitException(String message, Throwable cause) {
			super(message, cause);
		}
		
	}
	

}
