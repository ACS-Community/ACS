package alma.acs.component.client;

import java.util.logging.Handler;
import java.util.logging.Level;

import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;
import alma.acs.logging.StdOutConsoleHandler;

public class ShutdownHookTest {

	public static void main(String []args) throws Exception {

		AcsLogger logger;
		ComponentClient m_client;

		logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("ShutdownHookTestClient", false);

		if( Integer.parseInt(args[0]) == 0 )
			m_client = new ComponentClient(logger, System.getProperty("ACS.manager"), "ShutdownHookTestClient") {
				public void tearDown() throws Exception {
					for(Handler h : m_logger.getHandlers())
						if(h instanceof StdOutConsoleHandler)
							h.setLevel(Level.FINE); // to get the "duplicate call" log
					super.tearDown();
				}
			};
		else
			m_client = new ComponentClient(logger, System.getProperty("ACS.manager"), "ShutdownHookTestClient") {
				public void tearDown() throws Exception {
					for(Handler h : m_logger.getHandlers())
						if(h instanceof StdOutConsoleHandler)
							h.setLevel(Level.FINE); // to get the "duplicate call" log
					super.tearDown();
					Thread.sleep(10*1000);
				}
			};

		// Sleep a little bit... depending on the amount of time, we might want to kill
		// this baby from the outside, who knows
		Thread.sleep(Integer.parseInt(args[1])*1000);

		// We may want to call tearDown() as many times as we want
		int iterations = Integer.parseInt(args[2]);
		for(int i=0; i != iterations; i++)
			m_client.tearDown();

	}

}