package alma.jconttest.util;

import java.util.logging.Level;

import org.omg.CORBA.ORB;

import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogger;

public class JconttestUtil
{
	private static final String PROPERTYNAME_CLIENTORBTIMEOUT = "jacorb.connection.client.pending_reply_timeout";
	
	private final ContainerServices containerServices;
	private final AcsLogger logger;
	
	public JconttestUtil(ContainerServices containerServices) {
		this.containerServices = containerServices;
		this.logger = containerServices.getLogger();
	}
	
	/**
	 * We get the timeout property value from the ORB configuration.
	 * System-level timeout defaults are not standardized in Corba, thus we need jacorb-specific access.
	 * @throws IllegalArgumentException if the ORB timeout is configured as a negative value
	 * @throws AcsJCouldntPerformActionEx if the ORB-level timeout could not be read, e.g. because the ORB is not jacorb.
	 */
	public int getSystemLevelOrbTimeoutMillis() throws AcsJCouldntPerformActionEx {
		int orbLevelTimeout = -1;
		ORB orb = containerServices.getAdvancedContainerServices().getORB();
		if(orb instanceof org.jacorb.orb.ORB) {
			try {
				orbLevelTimeout = ((org.jacorb.orb.ORB)orb).getConfiguration().getAttributeAsInteger(PROPERTYNAME_CLIENTORBTIMEOUT);
				if (orbLevelTimeout < 0) {
					throw new IllegalArgumentException("system-level roundtrip timeout must be non-negative!");
				}
			} catch (org.apache.avalon.framework.configuration.ConfigurationException e){
				logger.log(Level.SEVERE, "Failed to read the system-level ORB timeout setting.", e);
				throw new AcsJCouldntPerformActionEx();
			}
		}
		else {
			logger.log(Level.SEVERE, "Wrong ORB " + orb.getClass().getName());
			throw new AcsJCouldntPerformActionEx();
		}
		return orbLevelTimeout;
	}


	/**
	 * Reads the Container.Timeout field from the CDB. 
	 * Note that the default (currently defined in Container.xsd) will be returned if the container config 
	 * does not override that Timeout.
	 */
	public double getContainerLevelOrbTimeout(String containerName) throws AcsJContainerServicesEx {
		double timeoutSeconds;
		try {
			timeoutSeconds = containerServices.getCDB().get_DAO_Servant("MACI/Containers/" + containerName).get_double("Timeout");
		} catch (AcsJContainerServicesEx ex) {
			throw ex;
		} catch (Exception ex2) {
			throw new AcsJContainerServicesEx(ex2);
		}
		return timeoutSeconds;
	}
}
