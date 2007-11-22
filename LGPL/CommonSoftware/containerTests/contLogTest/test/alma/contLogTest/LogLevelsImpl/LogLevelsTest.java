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
package alma.contLogTest.LogLevelsImpl;


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
import alma.contLogTest.LogLevels;

/**
 * Requires Java component "TESTLOG1" of type <code>alma.contLogTest.LogLevels</code> to be running.
 * 
 * @author eallaert 30 October 2007
 */
public class LogLevelsTest extends ComponentClientTestCase
{
	public static final String PROPERTYNAME_COMPONENTNAMES = "TEST_COMP_NAMES";
        
    /**
     * @TODO make not static once main method args are replaced with properties in the tests
     */
    private static Set<String> componentNames = new LinkedHashSet<String>();
    private List<LogLevels> components;
    private ContainerTestUtil containerTestUtil; 
    
    /**
	 * @throws java.lang.Exception
	 */
	public LogLevelsTest() throws Exception
	{
		super(LogLevelsTest.class.getName());
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
		
		components = new ArrayList<LogLevels>();		
		for (String compName : componentNames) {			
			components.add(alma.contLogTest.LogLevelsHelper.narrow(getContainerServices().getComponent(compName)));
		}
		
		containerTestUtil = new ContainerTestUtil(getContainerServices(), m_acsManagerProxy);
		containerTestUtil.loginToManager();
	}

	/**
	 * @see TestCase#tearDown()
	 */
	protected void tearDown() throws Exception {
		for (String compName : componentNames) {
			getContainerServices().releaseComponent(compName);
		}
		containerTestUtil.logoutFromManager();
		super.tearDown();
	}
	
	
	
	/**
	 * @throws Exception
	 */
	public void testGetLevels() throws Exception
	{
		int levels[];
		
		for (LogLevels testlogComp : components) {
			
			try {
				levels = testlogComp.getLevels();
			} catch (CouldntPerformActionEx ex) {
				throw AcsJCouldntPerformActionEx.fromCouldntPerformActionEx(ex);
			}
			m_logger.info("levels from component's getLevels method (hardcoded remote, local, effective): "
					+ levels[0] + ", " + levels[1] + ", " + levels[2] + ", " + levels[3] + ", " + levels[4]);
			// levels[0-4]: hardcoded remote, hardcoded local, AcsLogger, AcsLogHandler, StdoutConsoleHandler
			Assert.assertTrue(levels[3] != -1);
			Assert.assertTrue(levels[4] != -1);
			// The AcsLogger setting should be the minimum of the one for AcsLogHandler and StdoutConsoleHandler
			int minLevel = levels[3];
			if (levels[3] > levels[4] || levels[3] == -1)
				minLevel = levels[4];
			Assert.assertEquals(levels[2], minLevel);
		}
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

		// The min log level we set for the component and test its log output against.
		// @TODO: loop over many different levels to improve the test.
		short minLogLevelCentral = 8;
		
		int[] dummyLogLevels = {2, 3, 4, 5, 6, 8, 9, 10, 11};
		int waitTimeSeconds = 20;
		
		for (LogLevels testComp : components) {
			String componentName = testComp.name();
			// Prepare the log levels via the container's LoggingConfigurable interface
			String containerName = containerTestUtil.resolveContainerName(componentName);
			LoggingConfigurable containerLogConfig = containerTestUtil.getContainerLoggingIF(containerName);
			
			// @TODO get logger name from the component via a new IDL method, because the logger name 
			//       may not be the same as the component name (e.g. for C++ components).
			String loggerName = componentName;

			// Set the log level that our test component is subject to (either default level or individual level depending on CDB config)
			si.ijs.maci.LoggingConfigurablePackage.LogLevels componentLogLevels = containerLogConfig.get_logLevels(loggerName);
			componentLogLevels.minLogLevel = minLogLevelCentral;
			if (componentLogLevels.useDefault) {
				containerLogConfig.set_default_logLevels(componentLogLevels);
				m_logger.info("Set default log level for container " + containerName + " to " + minLogLevelCentral + " to be used by component " + componentName);
			}
			else {
				containerLogConfig.set_logLevels(loggerName, componentLogLevels);
				m_logger.info("Set individual log level for component " + componentName + " (running in container " + containerName + ") to " + minLogLevelCentral);
			}

			expectant.clearLogs();
			m_logger.info("Will call 'logDummyMessages' on component " + componentName);
			testComp.logDummyMessages(dummyLogLevels);
			m_logger.info("Will wait " + waitTimeSeconds + " seconds to receive (some of) these log messages.");
			LogSeriesExpectant.LogList logRecordsReceived = expectant.awaitLogRecords(loggerName, waitTimeSeconds);
			
			System.out.println("Got " + logRecordsReceived.size() + " records from logger " + loggerName);
			for (LogReceiver.ReceivedLogRecord logRecord : logRecordsReceived) {
				System.out.println("(level=" + logRecord.getLevel() + ") " + logRecord.getMessage());
			}
			
			assertEquals(minLogLevelCentral, logRecordsReceived.getMinLogLevel());
			assertEquals(11, logRecordsReceived.getMaxLogLevel()); // @todo Compute the max level from dummyLogLevels
		}
	}

	
	
	/**
	 * @TODO We usually don't require a main method for a JUnit test to run successfully.
	 * Therefore instead of getting component names from the arg list, 
	 * they should be given in the PROPERTYNAME_COMPONENTNAMES Java property that gets evaluated in the setUp method.
	 */
	public static void main(String[] args)
	{
	    for (int i = 0; i < args.length; i++)
		{
	    	componentNames.add(args[i]);
		}
		junit.textui.TestRunner.run(LogLevelsTest.class);
	}

}


