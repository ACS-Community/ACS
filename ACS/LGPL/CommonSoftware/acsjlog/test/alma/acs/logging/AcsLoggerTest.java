package alma.acs.logging;

import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

import junit.framework.TestCase;

import alma.acs.logging.domainspecific.AntennaContextLogger;
import alma.acs.logging.domainspecific.ArrayContextLogger;
import alma.acs.logging.domainspecific.DeveloperLogger;
import alma.acs.logging.domainspecific.OperatorLogger;
import alma.acs.logging.domainspecific.ScienceLogger;
import alma.acs.logging.formatters.ConsoleLogFormatter;
import alma.acs.testsupport.TestLogger;
import alma.log_audience.OPERATOR;

public class AcsLoggerTest extends TestCase {

	private AcsLogger acsLogger;
	
	protected void setUp() throws Exception {
		System.out.println("\n------------ " + getName() + " --------------");
		acsLogger = LocalOnlyAcsLogger.getInstance(AcsLoggerTest.class.getName(), Level.ALL);
	}
	
	public void testSimpleLog() {
		acsLogger.finer("a really simple log");
		acsLogger.info("and quite informative");
	}
	
	
	public void testAudienceLog() {
		acsLogger.logToAudience(Level.WARNING, "Log with audience", OPERATOR.value);
		acsLogger.logToAudience(Level.WARNING,
				"Log exception with audience", new Exception(
				"My dummy exception"), OPERATOR.value);	
	}
	
	public void testAntennaContextLog() {
		AntennaContextLogger antennaLogger = new AntennaContextLogger(acsLogger);
		antennaLogger.log(Level.WARNING, "Log with audience, array and antenna", OPERATOR.value, "Array01", "Antenna01");
		antennaLogger.log(Level.WARNING, "Log with array and antenna", "Array01", "Antenna01");
		antennaLogger.log(Level.WARNING,
				"Log exception with audience, array and antenna",
				new Exception("My dummy exception"), OPERATOR.value,
				"Array01", "Antenna01");
		antennaLogger.log(Level.WARNING, "Log exception with array and antenna",
				new Exception("My dummy exception"), "Array01", "Antenna01");
	}
	public void testArrayContextLog() {
		ArrayContextLogger arrayLogger = new ArrayContextLogger(acsLogger);
		arrayLogger.log(Level.WARNING, "Log with audience and array", OPERATOR.value, "Array01");
		arrayLogger.log(Level.WARNING, "Log with array", "Array01");
		arrayLogger.log(Level.WARNING,
				"Log exception with audience and array",
				new Exception("My dummy exception"), OPERATOR.value,
				"Array01");
		arrayLogger.log(Level.WARNING, "Log exception with array",
				new Exception("My dummy exception"), "Array01");
	}
	
	public void testWrapJdkLogger() {
		assertSame("Expected reuse of AcsLogger", acsLogger, AcsLogger.fromJdkLogger(acsLogger, null));
		
		Logger jdkLogger = TestLogger.getLogger("myJdkLogger");
		Handler[] handlers = jdkLogger.getHandlers();
		assertEquals(1, handlers.length);
		handlers[0].setFormatter(new ConsoleLogFormatter()); // to get an ISO timestamp on stdout, for TAT/sed filtering.

		AcsLogger wrapper = AcsLogger.fromJdkLogger(jdkLogger, null);
		assertEquals("myJdkLoggerwrapper", wrapper.getName());
		
		wrapper.info("A message logged by the AcsLogger that wraps the jdkLogger");
	}
	
	public void testAbuseLevelOff() throws Exception {
		try {
			acsLogger.log(Level.OFF, "Bad log with Level.OFF");
			fail("Logging with level OFF should throw an exception, see COMP-1928");
		} catch (IllegalArgumentException ex) {
			// good
		}
		try {
			acsLogger.log(AcsLogLevel.OFF, "Bad log with AcsLogLevel.OFF");
			fail("Logging with level OFF should throw an exception, see COMP-1928");
		} catch (IllegalArgumentException ex) {
			// good
		}
	}
	
	/*Added by request of COMP-3130*/
	public void testAudienceLoggers(){
		DeveloperLogger.info("Testing DeveloperLogger class", acsLogger);
		(new DeveloperLogger(acsLogger)).fine("Testing DeveloperLogger object");
		
		(new OperatorLogger(acsLogger)).warning("Testing OperatorLogger object");
		OperatorLogger.info("Testing OperatorLogger class", acsLogger);
		
		ScienceLogger.info("Testing ScienceLogger class", acsLogger);
	}
}
