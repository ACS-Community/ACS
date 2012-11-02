/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.component.client;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.lessThanOrEqualTo;
import static org.junit.Assert.assertThat;
import junit.framework.JUnit4TestAdapter;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;
import alma.acs.logging.config.LogConfig;
import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.acs.util.AcsLocations;

/**
 * Tests the jacorb etc framework loggers in the context of a ComponentClient.
 * <p>
 * This test was developed while investigating http://jira.alma.cl/browse/COMP-8283.
 * In retrospect, we could have isolated that problem even further,
 * in a test that does not need the ComponentClient base class. 
 * However, it seems useful to keep this test also as it is.
 * <p>
 * The jacorb logging problem was found in ACSLaser/demo :: DemoTest,
 * where jacorb logs suddenly appeared after NCSubscriber got used (instead of Consumer), 
 * whose implementation requests the "scxml" framework logger,
 * which in turn calls LogConfig#notifySubscribers() and thus
 * turned on the jacorb logs as we normally would expect them
 * when 'jacorb.log.default.verbosity' gets ignored. See the jira ticket for a better description.
 * <p>
 * As a side effect, this test also shows that with JUnit4 we can use ComponentClient
 * and no longer need a separate ComponentClientTestCase.
 * 
 * @author hsommer
 */
public class FrameworkLoggerTest extends ComponentClient
{

	@Rule 
	public TestName testName = new TestName();
	
	private ClientLogManager clientLogManager;
	
	/**
	 * For compatibility with JUnit3 based TATJUnitRunner
	 */
	public static junit.framework.Test suite() {
		return new JUnit4TestAdapter(FrameworkLoggerTest.class);
	}

	public FrameworkLoggerTest() throws Exception {
		super(null, AcsLocations.figureOutManagerLocation(), FrameworkLoggerTest.class.getSimpleName());
	}

	@Before
	public void setUp() throws Exception {
		String testMethodName = testName.getMethodName();
		m_logger.info("----------------- " + testMethodName + " ----------------- ");
		clientLogManager = ClientLogManager.getAcsLogManager();
	}

	@After
	public void tearDown() throws Exception {
		Thread.sleep(500);
		super.tearDown();
	}

	@Test
	public void testJacorbLogger() throws Exception {
		
		LogConfig logConfig = clientLogManager.getLogConfig();
		final String expectedJacorbLoggerName = "jacorb@" + FrameworkLoggerTest.class.getSimpleName();
		
		assertThat("jacorb logger should be known already from base class corba calls.",
				logConfig.isKnownLogger(expectedJacorbLoggerName), 
				is(true));

		// JacORB config file "orb.properties" currently contains "jacorb.log.default.verbosity=2" (mapped to INFO), 
		// but it is not visible as a system property and thus cannot be verified here.
		// This test must be started with "-Djacorb.log.default.verbosity=3" (DEBUG) so that we can assert
		// the right jacorb log level.
		assertThat(System.getProperty("jacorb.log.default.verbosity"), equalTo("3"));
		assertThat("Stdout log level must be DEBUG or lower, to not override the jacorb log level.",
				logConfig.getDefaultMinLogLevelLocal(), 
				lessThanOrEqualTo(AcsLogLevelDefinition.DEBUG));
		
		assertThat("jacorb logger configuration should be DEBUG for local logging.", 
				AcsLogLevelDefinition.fromXsdLogLevel(logConfig.getNamedLoggerConfig(expectedJacorbLoggerName).getMinLogLevelLocal()), 
				equalTo(AcsLogLevelDefinition.DEBUG));
		
		// Check if the jacorb logger itself is configured according to the above logConfig results
		
		AcsLogger jacorbLogger = ClientLogManager.getAcsLogManager().getLoggerForCorba("jacorb", true);
		jacorbLogger.fine("A jacorb test log, should be shown.");
		assertThat(jacorbLogger.isLoggable(AcsLogLevel.TRACE), is(false));
		assertThat(jacorbLogger.isLoggable(AcsLogLevel.DELOUSE), is(false)); 
		assertThat(jacorbLogger.isLoggable(AcsLogLevel.DEBUG), is(true));
		
		// this will call LogConfig#notifySubscribers(), which I suspect to bring out the jacorb logs.
		logConfig.setMinLogLevelLocal(AcsLogLevelDefinition.DELOUSE, "SomeOtherFramework");

		jacorbLogger.finer("jacorb test log #2, shown.");
	}
	
	
	/**
	 * Other framework loggers did not have the same problem
	 * as the jacorb logger. Thus this test always passed.
	 */
	@Test
	public void testDummyFrameworkLogger() throws Exception {
		
		LogConfig logConfig = clientLogManager.getLogConfig();
		final String expectedFrameworkLoggerName = "myFW@" + FrameworkLoggerTest.class.getSimpleName();
		
		assertThat("Dummy framework logger should NOT be known beforehand.",
				logConfig.isKnownLogger(expectedFrameworkLoggerName), 
				is(false));

		assertThat("framework logger configuration should be DELOUSE for local logging.", 
				AcsLogLevelDefinition.fromXsdLogLevel(logConfig.getNamedLoggerConfig(expectedFrameworkLoggerName).getMinLogLevelLocal()), 
				equalTo(AcsLogLevelDefinition.DELOUSE));
		
		// Check if the framework logger is configured in line with the above logConfig results
		
		AcsLogger myFWLogger = ClientLogManager.getAcsLogManager().getLoggerForCorba("myFW", true);
		myFWLogger.finer("A framework test log, should be shown.");
		assertThat(myFWLogger.isLoggable(AcsLogLevel.TRACE), is(false));
		assertThat(myFWLogger.isLoggable(AcsLogLevel.DELOUSE), is(true));
	}

}