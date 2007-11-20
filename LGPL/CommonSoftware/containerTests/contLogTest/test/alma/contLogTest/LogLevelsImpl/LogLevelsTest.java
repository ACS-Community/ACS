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
import java.util.List;
import java.util.StringTokenizer;

import com.cosylab.logging.engine.log.ILogEntry;

import junit.framework.Assert;
import junit.framework.TestCase;

import si.ijs.maci.Administrator;
import si.ijs.maci.AdministratorPOATie;
import si.ijs.maci.Container;
import si.ijs.maci.ContainerInfo;
import si.ijs.maci.LoggingConfigurableHelper;
import si.ijs.maci.LoggingConfigurableOperations;

import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.JavaContainerError.wrappers.AcsJContainerEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.container.AcsManagerProxy;
import alma.contLogTest.LogLevels;
import alma.maciErrType.NoPermissionEx;

/**
 * Requires Java component "TESTLOG1" of type <code>alma.contLogTest.LogLevels</code> to be running.
 * 
 * @author eallaert 30 October 2007
 */
public class LogLevelsTest extends ComponentClientTestCase
{
	public static final String PROPERTYNAME_COMPONENTNAMES = "TEST_COMP_NAMES";
    private LogLevels m_testlogComp;
        
    /**
     * @TODO make not static once main method args are replaced with properties in the tests
     */
    private static List<String> componentNames = new ArrayList<String>();

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
		String compNames = System.getProperty(PROPERTYNAME_COMPONENTNAMES);
		if (compNames != null) {
			StringTokenizer tok = new StringTokenizer(compNames);
			while (tok.hasMoreTokens()) {
				componentNames.add(tok.nextToken());
			}
		}
	}

	/**
	 * @see TestCase#tearDown()
	 */
	protected void tearDown() throws Exception {
		super.tearDown();
	}
	
	
	
	/**
	 * @TODO use getLogLevelsComponents method
	 * @throws Exception
	 */
	public void testGetLevels() throws Exception
	{
		int levels[];
		
		for (int i = 0; i < componentNames.size(); i++)
		{
		        m_testlogComp = alma.contLogTest.LogLevelsHelper.narrow(getContainerServices().getComponent(componentNames.get(i)));

			try {
				levels = m_testlogComp.getLevels();
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
	
	
	public void testRemoteLogReception() throws Exception {
		LogSeriesExpectant expectant = new LogSeriesExpectant(getLogReceiver());

		int[] logLevels = {2, 3, 4, 5, 6, 8, 9, 10, 11};
		int waitTimeSeconds = 30;
		
		for (LogLevels testComp : getLogLevelsComponents()) {
			// @TODO get logger name from the component via a new IDL method
			String loggerName = "TESTLOG1";
			
			expectant.clearLogs();
			m_logger.info("Will call 'logDummyMessages' on component " + testComp.name());
			testComp.logDummyMessages(logLevels);
			m_logger.info("Will wait " + waitTimeSeconds + " seconds to receive (some of) these log messages.");
			List<ILogEntry> logRecordsReceived = expectant.awaitLogRecords(loggerName, waitTimeSeconds);
			
			// @TODO vary the log level settings and add hard asserts about which messages may come through
			
			System.out.println("Got " + logRecordsReceived.size() + " records from logger " + loggerName);
			for (ILogEntry logEntry : logRecordsReceived) {
				System.out.println(logEntry.getField(ILogEntry.Field.LOGMESSAGE));
			}
		}
	}


//	public void testGetLoggingConfigurableInterface() throws Exception {
//	LoggingConfigurableOperations containerLogConfig = getContainerLoggingIF("heikoContainer");
//	String[] loggerNames = containerLogConfig.get_logger_names();
//	assertNotNull(loggerNames);
//}
//
	
	/**
	 * Gets a reference to the LoggingConfigurableOperations interface of a container with a given name. 
	 * <p>
	 * Note that only in test code like here we are allowed to talk directly with the manager.
	 * For operational code, the ContainerServices methods must be used and extended if necessary.
	 *  
	 * @param containerName
	 */
	protected LoggingConfigurableOperations getContainerLoggingIF(String containerName) throws AcsJContainerEx, NoPermissionEx {
    	AdministratorPOATie adminpoa = new AdministratorPOATie(new ManagerAdminClient(getName(), m_logger)); 
		Administrator adminCorbaObj = adminpoa._this(getContainerServices().getAdvancedContainerServices().getORB());
		
		// we need an new manager connection because the JUnit test does not have admin right.
		AcsManagerProxy adminProxy = m_acsManagerProxy.createInstance(); 
		Container containerRef;
		try {
			adminProxy.loginToManager(adminCorbaObj, false);
			int adminManagerHandle = adminProxy.getManagerHandle();
			assertTrue(adminManagerHandle > 0);
			
			// ask the manager for the container reference
			ContainerInfo[] containerInfos = adminProxy.getManager().get_container_info(adminManagerHandle, new int[0], containerName);
			assertEquals(1, containerInfos.length);
			containerRef = containerInfos[0].reference;
		} finally {
			// now that we got our reference, we can logout the manager client again.
			// Of course this is too dirty for operational code, because manager cannot keep track of 
			// admin clients holding container references...
			adminProxy.logoutFromManager();
		}
		
		// cast up to LoggingConfigurable interface
		return LoggingConfigurableHelper.narrow(containerRef);
	}
	
	
	/**
	 * @TODO Eliminate <code>component</code> field, see comment at main method.
	 */
	protected List<LogLevels> getLogLevelsComponents() throws AcsJContainerServicesEx 
	{
		List<LogLevels> compRefs = new ArrayList<LogLevels>();
		
		for (String compName : componentNames) {			
			compRefs.add(alma.contLogTest.LogLevelsHelper.narrow(getContainerServices().getComponent(compName)));
		}
		return compRefs;
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


