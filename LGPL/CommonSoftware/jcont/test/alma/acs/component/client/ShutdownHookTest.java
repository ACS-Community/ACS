package alma.acs.component.client;

import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;

public class ShutdownHookTest {

	public static void main(String []args) throws Exception {

		AcsLogger m_logger;
		ComponentClient m_client;

		m_logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("ShutdownHookTestClient", false);
		m_client = new ComponentClient(m_logger, System.getProperty("ACS.manager"), "ShutdownHookTestClient");

		// Sleep a little bit... we'll kill this baby from the outside
		Thread.sleep(100*1000);

		m_client.tearDown();

	}
}
