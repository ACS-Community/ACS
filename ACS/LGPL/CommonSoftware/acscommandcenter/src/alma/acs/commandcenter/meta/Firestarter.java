/*
 * Created on Aug 29, 2003 by mschilli
 */
package alma.acs.commandcenter.meta;

import java.util.logging.Level;

import alma.acs.component.client.AdvancedComponentClient;
import alma.acs.container.corba.AcsCorba;
import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;
import alma.acs.util.AcsLocations;


/**
 */
public class Firestarter {


	// Lifecycle
	// ==========================================================

	protected String clientName;
	protected AcsLogger logger = null;

	protected Firestarter(String clientName) {
		this(clientName, null, null);
	}

	/**
	 * Creates a Firestarter for a client.
	 * 
	 * @param clientName - e.g. "AcsCommandCenter" or "OMC"
	 * @param logger - logger which will be passed to delegates
	 * @param managerLoc - the corbaloc of the manager
	 */
	public Firestarter(String clientName, AcsLogger logger, String managerLoc) {

		if (clientName == null)
			throw new NullPointerException("clientName should not be null");
		this.clientName = clientName;

		if (logger == null)
			logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(clientName + ".Firestarter", false);
		this.logger = logger;

		if (managerLoc == null)
			managerLoc = AcsLocations.figureOutManagerLocation();
		this.managerLoc = managerLoc;
	}


	public void shutdown () {

		try {
			if (advancedComponentClient != null) {
				advancedComponentClient.tearDown();
				advancedComponentClient = null;
			}
		} catch (Throwable t) {
			logger.log(Level.INFO, "failed to stop AdvancedComponentClient", t);
		}


		try {
			if (acsCorba != null) {
				new Thread() {
					@Override
					public void run () {
						boolean wait_for_completion = true;
						boolean isOrbThread = false;
						acsCorba.shutdownORB(wait_for_completion, isOrbThread);
						acsCorba = null;
					}
				}.start();
			}
		} catch (Throwable t) {
			logger.log(Level.INFO, "failed to stop AcsCorba", t);
		}

	}



	// AdvancedComponentClient
	// ==========================================================

	protected AdvancedComponentClient advancedComponentClient = null;
	protected String managerLoc = null;

	public AdvancedComponentClient giveAdvancedComponentClient () throws Exception {

		if (advancedComponentClient == null) {
			giveAcsCorba(); // may fail
			advancedComponentClient = new AdvancedComponentClient(logger, managerLoc, clientName + ".ComponentClient", acsCorba);
		}

		return advancedComponentClient;
	}


	// ORB
	// ==========================================================

	
	public org.omg.CORBA.ORB giveOrb () throws OrbInitException {
		giveAcsCorba(); // may fail
		return acsCorba.getORB();
	}

	// AcsCorba
	// ==========================================================


	protected AcsCorba acsCorba = null;
	protected Integer orbPort = null;
	protected Integer orbPortSearchRetry = null;


	public AcsCorba giveAcsCorba () throws OrbInitException {

		if (acsCorba == null) {
			acsCorba = new AcsCorba(logger);
			acsCorba.setPortOptions(orbPort, orbPortSearchRetry);
		}

		if (!acsCorba.isInitialized()) {
			try {
				boolean isAdmin = true;
				acsCorba.initCorbaForClient(isAdmin);

			} catch (Exception exc) {
				throw new OrbInitException("acsCorba.initCorbaForClient(true) failed", exc);
			}
		}

		return acsCorba;
	}

	public static class OrbInitException extends Exception {
		OrbInitException(String message, Throwable cause) {
			super(message, cause);
		}
	}


}
