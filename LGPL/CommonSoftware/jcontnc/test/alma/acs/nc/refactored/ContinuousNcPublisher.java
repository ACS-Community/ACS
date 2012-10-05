package alma.acs.nc.refactored;

import alma.ADMINTEST1.OnOffStates;
import alma.ADMINTEST1.statusBlockEvent1;
import alma.acs.component.client.ComponentClient;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.Helper;
import alma.acs.util.CmdLineArgs;
import alma.acs.util.CmdLineRegisteredOption;

/**
 * Client application that continuously publishes events on a given channel. 
 * Can be used for manual tests with NCs, eventGUI etc.
 * <p>
 * Out of laziness we reuse 'statusBlockEvent1' as the published event type,
 * which is already defined locally in IDL. 
 * 
 * @author hsommer
 */
public class ContinuousNcPublisher extends ComponentClient
{
	public ContinuousNcPublisher(String managerLoc) throws Exception {
		super(null, managerLoc, ContinuousNcPublisher.class.getSimpleName());
	}
	
	
	private void run(String channelName, int eventsPerBurst, int delayBurstsMillis, boolean logAfterBurst) throws AcsJException {
		m_logger.info("Will continuously publish 'statusBlockEvent1' dummy events on NC '" + channelName +
				"', with bursts of " + eventsPerBurst + " events and delays of " + delayBurstsMillis + " ms between bursts.");
		
		NCPublisher<statusBlockEvent1> publisher = null;
//		publisher.enableEventQueue(queueSize, handler);
		
		statusBlockEvent1 event = new statusBlockEvent1(OnOffStates.ON, "testingEvent", 0, 0, 0, false, 0.0f);
		
		try {
			publisher = new NCPublisher<statusBlockEvent1>(
					channelName, 
					getContainerServices(), 
					Helper.getNamingServiceInitial(getContainerServices()));
			
			while (!Thread.currentThread().isInterrupted()) {
				for (int i = 0; i < eventsPerBurst; i++) {
//					m_logger.info("About to publish event");
					publisher.publishEvent(event);
					event.counter2++;
				}
				if (logAfterBurst) {
					m_logger.info("Done with event burst " + event.counter1 + ".");
				}
				event.counter1++;
				event.counter2 = 0;
				try {
					Thread.sleep(delayBurstsMillis);
				} catch (InterruptedException ex) {
					// while loop checks...
				}
			}
		}
		finally {
			if (publisher != null) {
				publisher.disconnect();
			}
			m_logger.info("Done.");
		}
	}
	
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		
		String managerLoc = System.getProperty("ACS.manager");
		if (managerLoc == null) {
			System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
			System.exit(-1);
		}

		try {
			CmdLineArgs cmdArgs = new CmdLineArgs();
			CmdLineRegisteredOption optChannelName = new CmdLineRegisteredOption("-channelName", 1);
			cmdArgs.registerOption(optChannelName);
			CmdLineRegisteredOption optEventsPerBurst = new CmdLineRegisteredOption("-eventsPerBurst", 1);
			cmdArgs.registerOption(optEventsPerBurst);
			CmdLineRegisteredOption optDelayBurstsMillis = new CmdLineRegisteredOption("-delayBurstsMillis", 1);
			cmdArgs.registerOption(optDelayBurstsMillis);
			CmdLineRegisteredOption optLogAfterBurst = new CmdLineRegisteredOption("-logAfterBurst", 1);
			cmdArgs.registerOption(optLogAfterBurst);
			
			cmdArgs.parseArgs(args);
			
			String channelName = null;
			if (cmdArgs.isSpecified(optChannelName)) {
				channelName = cmdArgs.getValues(optChannelName)[0].trim();
			} 
			else {
				throw new IllegalArgumentException("Option " + optChannelName.getName() + " must be specified.");
			}
			
			int eventsPerBurst = 1;
			if (cmdArgs.isSpecified(optEventsPerBurst)) {
				eventsPerBurst = Integer.parseInt(cmdArgs.getValues(optEventsPerBurst)[0]);
			} 

			int delayBurstsMillis = 1000;
			if (cmdArgs.isSpecified(optDelayBurstsMillis)) {
				delayBurstsMillis = Integer.parseInt(cmdArgs.getValues(optDelayBurstsMillis)[0]);
			}
			
			boolean logAfterBurst = false;
			if (cmdArgs.isSpecified(optLogAfterBurst)) {
				logAfterBurst = Boolean.parseBoolean(cmdArgs.getValues(optLogAfterBurst)[0]);
			}
			
			new ContinuousNcPublisher(managerLoc).run(channelName, eventsPerBurst, delayBurstsMillis, logAfterBurst);
			
		} catch (Throwable thr) {
			thr.printStackTrace();
		}
	}

}
