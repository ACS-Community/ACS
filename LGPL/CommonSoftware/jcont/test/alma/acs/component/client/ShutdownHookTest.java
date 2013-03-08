package alma.acs.component.client;

import java.util.logging.Handler;
import java.util.logging.Level;

import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;
import alma.acs.logging.StdOutConsoleHandler;
import alma.acs.util.AcsLocations;

public class ShutdownHookTest {

	/**
	 * Creates the ComponentClient "ShutdownHookTestClient" and tests its cleanup
	 * using calls to tearDown coming both from the shutdown hook and from explicit invocations.
	 * <p>
	 * In the <code>tearDown</code> method, it enables FINE logs for stdout 
	 * before calling {@link ComponentClient#tearDown()},
	 * in order to not miss the "duplicate call" log that the base class creates 
	 * if <code>tearDown</code> is called more than once.
	 * <p>
	 * Meaning of the 3 int args:
	 * <ol>
	 *   <li> <code>0</code>: tearDown calls super.tearDown and exits. <br>
	 *        <code>!=0</code>: tearDown sleeps 10 seconds after calling super.tearDown
	 *   <li> Sleep time in seconds after creating the ComponentClient, before returning from this main method call.
	 *   <li> Number of additional calls to the ComponentClient's tearDown method.
	 * </ol>
	 * @param args
	 * @throws Exception
	 */
	public static void main(String[] args) throws Exception {

		AcsLogger logger;
		ComponentClient m_client;

		logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("ShutdownHookTestClient", false);

		if (Integer.parseInt(args[0]) == 0)
			m_client = new ComponentClient(logger, AcsLocations.figureOutManagerLocation(), "ShutdownHookTestClient") {
				public void tearDown() throws Exception {
					for (Handler h : m_logger.getHandlers()) {
						if (h instanceof StdOutConsoleHandler) {
							h.setLevel(Level.FINE); // to get the "duplicate call" log
						}
					}
					super.tearDown();
				}
			};
		else
			m_client = new ComponentClient(logger, AcsLocations.figureOutManagerLocation(), "ShutdownHookTestClient") {
				public void tearDown() throws Exception {
					for (Handler h : m_logger.getHandlers())
						if (h instanceof StdOutConsoleHandler) {
							h.setLevel(Level.FINE); // to get the "duplicate call" log
						}
					super.tearDown();
					Thread.sleep(10 * 1000);
				}
			};

		// Sleep a little bit... depending on the amount of time, we might want to kill
		// this baby from the outside, who knows
		Thread.sleep(Integer.parseInt(args[1]) * 1000);

		// We may want to call tearDown() as many times as we want
		int iterations = Integer.parseInt(args[2]);
		for (int i = 0; i != iterations; i++) {
			m_client.tearDown();
		}

	}

}