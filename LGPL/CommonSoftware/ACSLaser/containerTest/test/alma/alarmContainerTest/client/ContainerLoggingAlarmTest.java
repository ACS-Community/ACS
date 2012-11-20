package alma.alarmContainerTest.client;

import static org.junit.Assert.*;

import java.util.logging.Logger;

import junit.framework.JUnit4TestAdapter;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;


import alma.acs.component.client.ComponentClient;
import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.acs.util.AcsLocations;
import alma.acs.util.StopWatch;
import alma.alarmContainerTest.AlarmTestComponent;
import alma.alarmContainerTest.AlarmTestComponentHelper;



import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.lessThan;
import static org.hamcrest.Matchers.lessThanOrEqualTo;
import static org.junit.Assert.assertThat;

/**
 * Creates logs via a component. 
 * Currently all test verification is done in the bash script that runs this test. 
 * 
 * @author hsommer
 */
public class ContainerLoggingAlarmTest extends ComponentClient
{

	public ContainerLoggingAlarmTest() throws Exception {
		super(null, AcsLocations.figureOutManagerLocation(), ContainerLoggingAlarmTest.class.getSimpleName());
	}

	/**
	 * TODO: Check if this rule and the getMethodName() call in setUp() can be moved up to ComponentClient,
	 *      if that adds a runtime dependency on junit, and how bad that would be.
	 *      Probably we should add a class ComponentClientTestCaseJUnit4 that extends ComponentClient
	 *      and only adds this testname business.
	 */
	@Rule 
	public TestName testName = new TestName();
	
	/**
	 * For compatibility with JUnit3 based TATJUnitRunner
	 */
	public static junit.framework.Test suite() {
		return new JUnit4TestAdapter(ContainerLoggingAlarmTest.class);
	}

	
	@Before
	public void setUp() throws Exception {
		// nothing beyond ComponentClient ctor at the moment

		String testMethodName = testName.getMethodName();
		m_logger.info("----------------- Running test " + testMethodName + " ----------------- ");
	}

	@After
	public void tearDown() throws Exception {
		super.tearDown();
	}

	/**
	 */
	@Test
	public void testLogOverflowNoThrottle() throws Exception {
		String compName = "TestcompJavaUnthrottled";
		int numLogs = 10000;
		// The level must be INFO or higher so that it gets dropped only when the queue is 100% full.
		// DEBUG and below levels get dropped already at 70% full.
		AcsLogLevelDefinition level = AcsLogLevelDefinition.INFO;
		AlarmTestComponent comp = AlarmTestComponentHelper.narrow(getContainerServices().getComponent(compName));
		StopWatch sw = new StopWatch(m_logger);
		
		comp.logBurst((short)level.value, numLogs);
		sw.logLapTime("send " + numLogs + " " + level.name() + " logs from component " + compName, AcsLogLevel.INFO);
		
		// now send Debug logs at max speed, to check how they get dropped in the log queue
		numLogs = 1000;
		level = AcsLogLevelDefinition.DEBUG;
		comp.logBurst((short)level.value, numLogs);
		sw.logLapTime("send " + numLogs + " " + level.name() + " logs from component " + compName, AcsLogLevel.INFO);
		// sleep a bit so that the log queue can drain. This gives a chance to see the "no longer dropping" kind of message
		Thread.sleep(2000);
		comp.logBurst((short)level.value, numLogs);
		sw.logLapTime("send " + numLogs + " " + level.name() + " logs from component " + compName, AcsLogLevel.INFO);

		getContainerServices().releaseComponent(compName);
	}

	@Test
	public void testLogThrottleAlarm() throws Exception {
		int numLogs = 1000;
		AcsLogLevelDefinition level = AcsLogLevelDefinition.DEBUG;
		String compName = "TestcompJavaThrottled";
		AlarmTestComponent comp = AlarmTestComponentHelper.narrow(getContainerServices().getComponent(compName));
		StopWatch sw = new StopWatch(m_logger);
		
		comp.logBurst((short)level.value, numLogs);
		sw.logLapTime("send " + numLogs + " " + level.name() + " logs from component " + compName, AcsLogLevel.INFO);
		
		getContainerServices().releaseComponent(compName);
	}

}
