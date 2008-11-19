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
package alma.contLogTest.TestLogLevelsCompImpl;


import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;

import junit.framework.Assert;
import junit.framework.TestCase;

import si.ijs.maci.LoggingConfigurable;

import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.logging.engine.LogReceiver;
import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.contLogTest.TestLogLevelsComp;

/**
 * Requires Java component "TESTLOG1" of type <code>alma.contLogTest.TestLogLevelsComp</code> to be running.
 * 
 * @author eallaert 30 October 2007
 */
public class TestLogLevelsCompTest extends ComponentClientTestCase
{
	public static final String PROPERTYNAME_COMPONENTNAMES = "TEST_COMP_NAMES";
        
    private Set<String> componentNames = new LinkedHashSet<String>();
    private List<TestLogLevelsComp> components;
    private ContainerTestUtil containerTestUtil; 
    
    /**
	 * @throws java.lang.Exception
	 */
	public TestLogLevelsCompTest() throws Exception
	{
		super(TestLogLevelsCompTest.class.getName());
	}

	/**
	 * @see TestCase#setUp()
	 */
	protected void setUp() throws Exception {
		super.setUp();
		
		// set names of test components, if not done already in the main method
		String compNames = System.getProperty(PROPERTYNAME_COMPONENTNAMES);
		if (compNames != null) {
			StringTokenizer tok = new StringTokenizer(compNames);
			while (tok.hasMoreTokens()) {
				componentNames.add(tok.nextToken());
			}
		}
		
		components = new ArrayList<TestLogLevelsComp>();		
		for (String compName : componentNames) {			
			components.add(alma.contLogTest.TestLogLevelsCompHelper.narrow(getContainerServices().getComponent(compName)));
		}
		
		containerTestUtil = new ContainerTestUtil(getContainerServices(), m_acsManagerProxy);
		containerTestUtil.loginToManager();
	}

	/**
	 * @see TestCase#tearDown()
	 */
	protected void tearDown() throws Exception {
		for (String compName : componentNames) {
			//System.out.println("tearDown - about to release "+ compName);
			getContainerServices().releaseComponent(compName);
		}
		//System.out.println("tearDown - about to log-out from Manager");
		containerTestUtil.logoutFromManager();
		//System.out.println("tearDown - about to tearDown() superClass");
		super.tearDown();
		//System.out.println("tearDown - done with tearDown() of superClass");
	}
	
	
	/**
	 * @throws Exception
	 */
	public void testGetLevels() throws Exception
	{
		int[] levels;
		
		for (TestLogLevelsComp testlogComp : components) {
			try {
				levels = testlogComp.getLevels();
			} catch (CouldntPerformActionEx ex) {
				throw AcsJCouldntPerformActionEx.fromCouldntPerformActionEx(ex);
			}

			String componentName = testlogComp.name();
			// Prepare the log levels via the container's LoggingConfigurable interface
			String containerName = containerTestUtil.resolveContainerName(componentName);
			LoggingConfigurable containerLogConfig = containerTestUtil.getContainerLoggingIF(containerName);
			
			// @TODO get logger name from the container via a new IDL method, because the logger name 
			//       may not be the same as the container (e.g. for C++ containers).
			//String loggerName = containerName;
			String loggerName = componentName;

			// Get the log level that our test component is subject to 
			si.ijs.maci.LoggingConfigurablePackage.LogLevels componentLogLevels = containerLogConfig.get_logLevels(loggerName);
			if (componentLogLevels.useDefault) {
				m_logger.info("levels from component's getLevels method (hardcoded remote, local, effective): "
						+ levels[0] + ", " + levels[1] + ", " + levels[2] + ", " + levels[3] + ", " + levels[4]);
				Assert.assertEquals(levels[3], componentLogLevels.minLogLevel);
				Assert.assertEquals(levels[4], componentLogLevels.minLogLevelLocal);
			} 
			else {
				m_logger.info("levels from component's getLevels method and LoggingConfigurable: " 
						+ levels[0] + ", " + levels[1] + ", " + levels[2] + ", " + levels[3] + ", " + levels[4]);
			}

			// levels[0-4]: hardcoded remote, hardcoded local, AcsLogger, AcsLogHandler, StdoutConsoleHandler
			Assert.assertTrue(levels[3] != -1);
			Assert.assertTrue(levels[4] != -1);
			// The AcsLogger setting should be the minimum of the one for AcsLogHandler and StdoutConsoleHandler
			int minLevel = levels[3];
			if (levels[3] > levels[4] || levels[3] == -1)
				minLevel = levels[4];
			Assert.assertEquals(levels[2], minLevel);
		}
		// Sleep is to work-around a race condition, whereby the tearDown() can get called "too early"
		// (logging not properly initialised when it is already  stopped after running this short method).
		Thread.sleep(3000);
	}
	
	
	
	/**
	 * This test uses the LoggingConfigurable interface implemented by all containers to set log levels
	 * These log levels are then expected to be independent of the initial CDB settings, according to precedence rules.
	 * The test verifies that the levels are applied correctly by commanding the test components to log messages
	 * at various log levels. The test listens on the Log service to compare the messages actually sent
	 * with what is expected for the current log level settings.
	 */
	public void testLoggingConfigurableCentralLogLevels() throws Exception {

		LogSeriesExpectant expectant = new LogSeriesExpectant(getLogReceiver());

		// All log levels, including OFF
		int[] centralLogLevels;
		// Excluding OFF
		int[] sendLogLevels;
		// Only with OFF
		int[] offLogLevels;
		centralLogLevels = new int[AcsLogLevelDefinition.values().length];
		sendLogLevels = new int[AcsLogLevelDefinition.values().length - 1];
		offLogLevels = new int[1];
		int i = 0;
		int maxLogLevel = -1;
		// We will rely here on the fact that AcsLogLevelDefinition has the
		// log levels ordered, with OFF being the last one.
		for (AcsLogLevelDefinition lld : AcsLogLevelDefinition.values()) {
			if (lld.name.equalsIgnoreCase("OFF")) {
				offLogLevels[0] = lld.value;
			}
			else {
				maxLogLevel = Math.max(maxLogLevel, lld.value);
				sendLogLevels[i] = lld.value;
			}
			centralLogLevels[i++] = lld.value;
		}
		int waitTimeSeconds = 5;
		
		for (TestLogLevelsComp testComp : components) {
			String componentName = testComp.name();
			// Prepare the log levels via the container's LoggingConfigurable interface
			String containerName = containerTestUtil.resolveContainerName(componentName);
			LoggingConfigurable containerLogConfig = containerTestUtil.getContainerLoggingIF(containerName);

			// @TODO get logger name from the component via a new IDL method, because the logger name 
			//       may not be the same as the component name (e.g. for C++ components).
			String loggerName = componentName;

			// Set the log level that our test component is subject to (either default level or individual level depending on CDB config)
			si.ijs.maci.LoggingConfigurablePackage.LogLevels componentLogLevels = containerLogConfig.get_logLevels(loggerName);
			for (int level : centralLogLevels) {
				// The min log level we set for the component and test its log output against.
				short minLogLevelCentral = (short)level;
				componentLogLevels.minLogLevel = minLogLevelCentral;
				if (componentLogLevels.useDefault) {
					// @TODO: add a test as well to change the specific level, and then verify
					// if useDefault becomes false.
					containerLogConfig.set_default_logLevels(componentLogLevels);
					m_logger.info("Set default log level for container " + containerName + " to " + minLogLevelCentral + " to be used by component " + componentName);
				}
				else {
					// @TODO: add a test to change the default level, and see if it impacts specific logger
					containerLogConfig.set_logLevels(loggerName, componentLogLevels);
					m_logger.info("Set individual log level for component " + componentName + " (running in container " + containerName + ") to " + minLogLevelCentral);
				}

				expectant.clearLogs();
				m_logger.info("Will call 'logDummyMessages' on component " + componentName);
				testComp.logDummyMessages(sendLogLevels);
				//testComp.logDummyMessages(centralLogLevels);
				m_logger.info("Will wait " + waitTimeSeconds + " seconds to receive (some of) these log messages.");
				LogSeriesExpectant.LogList logRecordsReceived = expectant.awaitLogRecords(loggerName, waitTimeSeconds);

				System.out.println("Got " + logRecordsReceived.size() + " records from logger " + loggerName);
				for (LogReceiver.ReceivedLogRecord logRecord : logRecordsReceived) {
					System.out.println("(level=" + logRecord.getLevel().acsCoreLevel.value + ") " + logRecord.getMessage());
				}

				if (minLogLevelCentral > maxLogLevel) {
					// minLogLevelCentral must be set to OFF - no logs should get through
					assertEquals(0, logRecordsReceived.size());
				}
				else {
					// The next assertion will fail here if there were no logs received,
					// as getMinLogLevel will return then the value 2147483647 (-1).
					assertEquals(minLogLevelCentral, logRecordsReceived.getMinLogLevel());
					assertEquals(maxLogLevel, logRecordsReceived.getMaxLogLevel());
				}
				
				// logging a msg at level OFF should lead to an exception, independent from central level 
				try {
					testComp.logDummyMessages(offLogLevels);
					// C++ containers don't seem to be able to throw exception in this case 
					m_logger.info("Sending a log with level OFF did not throw exception.");
					// Consume the log that says it is the last log message, unless the current level is OFF
					if (minLogLevelCentral <= maxLogLevel) {
						logRecordsReceived = expectant.awaitLogRecords(loggerName, waitTimeSeconds);
						assertEquals(1, logRecordsReceived.size());
					}
				} 
				catch (org.omg.CORBA.UNKNOWN ex) {
					if (ex.getLocalizedMessage().startsWith("Server-side Exception: java.lang.IllegalArgumentException")) {
						// Obviously the case for Java
						System.out.println("Got illegal argument exception for attempting to send log with level OFF, as expected.");
					}
					else if (ex.getLocalizedMessage().startsWith("Server-side Exception: null")) {
						// This is what Python does
						System.out.println("Got exception for attempting to send log with level OFF, as expected.");
					}
					else {
						// The exception we got is not the one expected
						throw ex;
					}
				}
				
				
				//System.out.println("Finished testing level " + level);
			}
			//System.out.println("Finished testing component " + componentName);
		}
		//System.out.println("Finished testing all components - done with testLoggingConfigurableCentralLogLevels()");

	}

	
	
	public static void main(String[] args)
	{
		junit.textui.TestRunner.run(TestLogLevelsCompTest.class);
		//System.out.println("Finished running TestLogLevelsCompTest main");
		//System.exit(0);
	}

}


