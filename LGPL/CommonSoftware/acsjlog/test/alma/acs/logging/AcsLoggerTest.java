package alma.acs.logging;

import java.util.logging.Level;

import alma.acs.logging.domainspecific.AntennaContextLogger;
import alma.acs.logging.domainspecific.ArrayContextLogger;
import alma.log_audience.OPERATOR;

import junit.framework.TestCase;

public class AcsLoggerTest extends TestCase {

	private AcsLogger acsLogger;
	
	protected void setUp() throws Exception {
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
}
