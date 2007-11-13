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


import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import junit.framework.Assert;

import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.component.client.ComponentClient;
import alma.acs.component.client.ComponentClientTestCase;
import alma.contLogTest.LogLevels;
import alma.maci.loggingconfig.LoggingConfig;

/**
 * Requires Java component "TESTLOG1" of type <code>alma.contLogTest.LogLevels</code> to be running.
 * 
 * @author eallaert 30 October 2007
 */
public class LogLevelsTest extends ComponentClientTestCase
{
	private ComponentClient hlc = null;

    private LogLevels m_testlogComp;
    private static String component[];

    /**
	 * @param name
	 * @throws java.lang.Exception
	 */
	public LogLevelsTest() throws Exception
	{
		super(LogLevelsTest.class.getName());
	}

	/**
	 * @see TestCase#setUp()
	 */
	protected void setUp() throws Exception
	{
		super.setUp();
		if (false) {
			String managerLoc = System.getProperty("ACS.manager");
			if (managerLoc == null) {
				System.out
						.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
				System.exit(-1);
			}
			/*
			String clientName = "LogLevelsClient";
			try {
				hlc = new ComponentClient(null, managerLoc, clientName);
				m_logger.info("got LogLevel client");
			} catch (Exception e) {
				try {
					Logger logger = hlc.getContainerServices().getLogger();
					logger.log(Level.SEVERE, "Client application failure", e);
				} catch (Exception e2) {
					e.printStackTrace(System.err);
				}
			}
			*/
		}
	}

	/**
	 * @see TestCase#tearDown()
	 */
	protected void tearDown() throws Exception
	{
	    if (hlc != null)
		{
    	try 
    		{
    		hlc.tearDown();
    		}
    	catch (Exception e3) 
    		{
    		// bad luck
    		e3.printStackTrace();
    		}
		}
		super.tearDown();
	}
	
	public void testGetLevels() throws Exception
	{
		int levels[];
		for (int i = 0; i < component.length; i++)
		{
		        m_testlogComp = alma.contLogTest.LogLevelsHelper.narrow(getContainerServices().getComponent(component[i]));

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

	public static void main(String[] args)
	{
	    component = new String[args.length];
	    for (int i = 0; i < args.length; i++)
		{
                component[i] = new String(args[i]);
		}
		junit.textui.TestRunner.run(LogLevelsTest.class);
	}

}


