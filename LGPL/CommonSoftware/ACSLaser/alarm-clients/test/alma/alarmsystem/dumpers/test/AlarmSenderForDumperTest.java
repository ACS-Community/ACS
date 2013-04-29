package alma.alarmsystem.dumpers.test;

import java.util.logging.Logger;

import alma.acs.alarmsystem.source.AlarmSource;
import alma.acs.component.client.ComponentClient;
import alma.acs.logging.ClientLogManager;
import alma.acs.logging.adapters.Log4jFactory;

/**
 * The test is composed of 2 pieces of software that run at the same time.
 * This one publishes three alarms. 
 * The second one, {@link TestSourceDumper}, runs the source dumps that prints in the stdout
 * the alarms received by the source dumper.
 * The output is in stdout so tat checks for correctness.
 * 
 * 
 * @author acaproni
 *
 */
public class AlarmSenderForDumperTest extends ComponentClient {
		
	// The alarm source
	private final AlarmSource m_alarmSource;
	
	/**
	 * Constructor
	 */
	public AlarmSenderForDumperTest(Logger logger, String managerLoc) throws Exception {
		super(logger,managerLoc,"AlarmSenderForDumperTest");
		m_alarmSource = getContainerServices().getAlarmSource();
	}
	
	public void sendAlarms() {
		try {
			m_alarmSource.raiseAlarm("testFamily1", "TestMember1", 12);
			try {
				Thread.sleep(500);
			} catch (InterruptedException ie) {}
			m_alarmSource.clearAlarm("testFamily2", "TestMember2", 2);
			try {
				Thread.sleep(500);
			} catch (InterruptedException ie) {}
			m_alarmSource.raiseAlarm("testFamily3", "TestMember3", 40);
		} catch (Throwable t) {
			System.out.println("Exception caught sending an alarm: "+t.getMessage());
			t.printStackTrace();
		}
		try {
			Thread.sleep(1000);
		} catch (InterruptedException ie) {}
	}
	
	public void close() throws Exception {
		System.out.println("AlarmSenderForDumperTest.close()");
		tearDown();
		System.out.println("AlarmSenderForDumperTest.close() terminated");
	}
	
	public static void main(String[] args) {
		// Connect the component client
		Log4jFactory.enableAcsLogging();
		Logger logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("AlarmSenderForDumperTest",true);
        String managerLoc = System.getProperty("ACS.manager");
        if (managerLoc == null) {
        	System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
        	System.exit(-1);
        }
        AlarmSenderForDumperTest sender = null;
        try  {
        	sender = new AlarmSenderForDumperTest(logger,managerLoc);
        } catch (Throwable t) {
			System.out.println("Exception instantiating the alarmSender: "+t.getMessage());
			t.printStackTrace();
			System.exit(-1);
		}
        try {
			Thread.sleep(5000);
		} catch (InterruptedException ie) {}
        System.out.println("AlarmSenderForDumperTest sending alarms.");
        sender.sendAlarms();
        try {
			Thread.sleep(5000);
		} catch (InterruptedException ie) {}
        System.out.println("AlarmSenderForDumperTest closing.");
        try  {
        	sender.close();
		} catch (Throwable t) {
        	System.err.println("Exception caught closing the component client "+t.getMessage());
        	t.printStackTrace(System.err);
        }
        System.out.println("AlarmSenderForDumperTest done.");
	}
}
