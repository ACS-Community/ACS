package alma.contLogTest.TestLogLevelsCompImpl;

import java.util.logging.Logger;

import si.ijs.maci.AdministratorOperations;
import si.ijs.maci.AuthenticationData;
import si.ijs.maci.ClientInfo;
import si.ijs.maci.ClientType;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ContainerInfo;
import si.ijs.maci.ImplLangType;

import alma.acs.util.UTCUtility;

/**
 * Dummy manager client for custom manager logins as administrator.
 */
public class ManagerAdminClient implements AdministratorOperations
{
	private String name;
	private Logger logger;

	public ManagerAdminClient(String name, Logger logger) {
		this.name = name;
		this.logger = logger;
	}
	
	@Override
	public void client_logged_in(ClientInfo info, long timestamp, long execution_id) {
		// TODO Auto-generated method stub

	}

	@Override
	public void client_logged_out(int h, long timestamp) {
		// TODO Auto-generated method stub

	}

	@Override
	public void component_activated(ComponentInfo info, long timestamp, long execution_id) {
		// TODO Auto-generated method stub

	}

	@Override
	public void component_deactivated(int h, long timestamp) {
		// TODO Auto-generated method stub

	}

	@Override
	public void components_released(int[] clients, int[] components, long timestamp) {
		// TODO Auto-generated method stub

	}

	@Override
	public void components_requested(int[] clients, int[] components, long timestamp) {
		// TODO Auto-generated method stub

	}

	@Override
	public void container_logged_in(ContainerInfo info, long timestamp, long execution_id) {
		// TODO Auto-generated method stub

	}

	@Override
	public void container_logged_out(int h, long timestamp) {
		// TODO Auto-generated method stub

	}

	@Override
	public AuthenticationData authenticate(long execution_id, String question) {
		logger.info("authenticate called: id=" + execution_id + "; question=" + question);

		return new AuthenticationData(
				 "S",
				 ClientType.ADMINISTRATOR_TYPE,
				 ImplLangType.JAVA,
				 false, 
				 UTCUtility.utcJavaToOmg(System.currentTimeMillis()),
				 execution_id);
	}

	@Override
	public void components_available(ComponentInfo[] components) {
		// TODO Auto-generated method stub

	}

	@Override
	public void components_unavailable(String[] component_names) {
		// TODO Auto-generated method stub

	}

	@Override
	public void disconnect() {
		// TODO Auto-generated method stub

	}

	@Override
	public void taggedmessage(short type, short messageID, String message) {
		// @todo Auto-generated method stub
		
	}
	
	public void message(short type, String message) {
		logger.info(message);
	}

	public String name() {
		return name;
	}

	public boolean ping() {
		return true;
	}


}
