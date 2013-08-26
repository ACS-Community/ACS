package alma.acs.logging.adapters;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertThat;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import alma.acs.logging.ClientLogManager;
import alma.acs.logging.config.LogConfig;
import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.acs.logging.testsupport.JUnit4StandaloneTestBase;


/**
 * Tests the interception of log4j logs by the ACS logging framework.
 */
public class Log4jAdapterTest extends JUnit4StandaloneTestBase
{
	@Before
	public void setUp() throws Exception {
		super.setUp();
	}

	@After
	public void tearDown() throws Exception {
		super.tearDown();
	}

	/**
	 */
	@Test
	public void testLog4jLogger() throws Exception {

		Log4jFactory.enableAcsLogging();
		
		// Get a logger using the current class name, which is standard with log4j
		// The call stack does not come from a recognized framework such as CERN laser libs, so that 
		// the logger name will be "unknown", with the method name appended.
		org.apache.log4j.Logger log4jLogger = org.apache.log4j.Logger.getLogger(getClass());
		log4jLogger.info("log4jLogger speaking INFO ...");
		
		// Some other logger, should again be "unknown@<TestMethodName>".
		org.apache.log4j.Logger log4jLogger2 = org.apache.log4j.Logger.getLogger("someOtherLog4jLogger");
		assertThat(log4jLogger2, sameInstance(log4jLogger));
		log4jLogger2.debug("stupid DEBUG msg");
		
		// DEBUG should be enabled (and the above log should show up in stdout)
		assertThat(log4jLogger.isDebugEnabled(), is(true));
		assertThat(log4jLogger.isTraceEnabled(), is(false));
		
		// Now we change log levels 
		LogConfig sharedLogConfig = ClientLogManager.getAcsLogManager().getLogConfig();
		sharedLogConfig.setMinLogLevelLocal(AcsLogLevelDefinition.INFO, "unknown@testLog4jLogger");
		assertThat(log4jLogger.isDebugEnabled(), is(true));
		sharedLogConfig.setMinLogLevel(AcsLogLevelDefinition.INFO, "unknown@testLog4jLogger");
		// now that both local and remote logs are set to INFO, isDebugEnabled should be false
		assertThat(log4jLogger.isDebugEnabled(), is(false));
		log4jLogger.info("This info message should appear");
		log4jLogger.debug("This debug message should not appear");
		
		log4jLogger.warn("A warning with exception!", new RuntimeException());
		
	}
}
