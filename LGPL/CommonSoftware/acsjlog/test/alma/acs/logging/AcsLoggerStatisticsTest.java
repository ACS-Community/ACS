package alma.acs.logging;

import java.util.logging.Level;
import junit.framework.TestCase;


public class AcsLoggerStatisticsTest extends TestCase {

	private AcsLogger acsLogger;
	
	protected void setUp() throws Exception {
		System.out.println("\n------------ " + getName() + " --------------");
		acsLogger = LocalOnlyAcsLogger.getInstance(AcsLoggerStatisticsTest.class.getName(), Level.ALL);
	}
	
	// ICT-3604: check that statistics are enabled by default when LOCATION is SET 
	public void testLoggerStatisticsParameters() {

		if(System.getenv("LOCATION") != null) {
			assertEquals(acsLogger.stats.getDisableStatistics().booleanValue(), false);
			assertEquals(acsLogger.stats.getStatisticsCalculationPeriod(), 10 * 60);
			assertEquals(acsLogger.stats.getStatisticsGranularity(), 3);
		} else {
			assertEquals(acsLogger.stats.getDisableStatistics().booleanValue(), true);
		}
	}
	
	public void testLoggerStatistics() {
		// reconfigure statistics to show statistics
		acsLogger.stats.configureStatistics("statisticsTest", false, 4, 1);
		
		// Logs not counted
		acsLogger.log(AcsLogLevel.TRACE, "Message TRACE");
		acsLogger.log(AcsLogLevel.DELOUSE, "Message DELOUSE");
		acsLogger.log(AcsLogLevel.DEBUG, "Message DEBUG");
		acsLogger.log(AcsLogLevel.INFO, "Message INFO");
		acsLogger.log(AcsLogLevel.NOTICE, "Message NOTICE");
		acsLogger.log(AcsLogLevel.WARNING, "Message WARNING");
		acsLogger.log(AcsLogLevel.ERROR, "Message ERROR");
		acsLogger.log(AcsLogLevel.CRITICAL, "Message CRITICAL");
		acsLogger.log(AcsLogLevel.ALERT, "Message ALERT");
		acsLogger.log(AcsLogLevel.EMERGENCY, "Message EMERGENCY");
			
		// Logs counted (10 messages)
		acsLogger.log(AcsLogLevel.TRACE, "Message TRACE");
		acsLogger.log(AcsLogLevel.DELOUSE, "Message DELOUSE");
		acsLogger.log(AcsLogLevel.DEBUG, "Message DEBUG");
		acsLogger.log(AcsLogLevel.INFO, "Message INFO");
		acsLogger.log(AcsLogLevel.NOTICE, "Message NOTICE");
		acsLogger.log(AcsLogLevel.WARNING, "Message WARNING");
		acsLogger.log(AcsLogLevel.ERROR, "Message ERROR");
		acsLogger.log(AcsLogLevel.CRITICAL, "Message CRITICAL");
		acsLogger.log(AcsLogLevel.ALERT, "Message ALERT");
		acsLogger.log(AcsLogLevel.EMERGENCY, "Message EMERGENCY");
		
		// This will force the statistics to be printed
        try {
        	Thread.sleep(4000);                 //1000 milliseconds is one second.
        } catch (Exception e) {
              System.out.println(e);
        }
        acsLogger.log(AcsLogLevel.TRACE, "Message TRACE 2");
        
        // This will force the statistics to be printed (again)
        try {
        	Thread.sleep(4000);                 //1000 milliseconds is one second.
        } catch (Exception e) {
              System.out.println(e);
        }
        
        acsLogger.log(AcsLogLevel.TRACE, "Message TRACE 3");
        acsLogger.log(AcsLogLevel.TRACE, "Message TRACE 4");
        acsLogger.log(AcsLogLevel.TRACE, "Message TRACE 5");
        acsLogger.log(AcsLogLevel.TRACE, "Message TRACE 6");
        
        // Close logger module, that will force statistics to be printed
        acsLogger.closeLogger();
	}
}
