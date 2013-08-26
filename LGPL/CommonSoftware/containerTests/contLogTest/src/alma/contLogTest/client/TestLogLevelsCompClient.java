/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory,
 * 2002 Copyright by ESO (in the framework of the ALMA collaboration), All
 * rights reserved
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without ++++even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package alma.contLogTest.client;

import java.util.logging.Level;
import java.util.logging.Logger;

import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.component.client.ComponentClient;
import alma.cdbErrType.CDBRecordDoesNotExistEx;
import alma.cdbErrType.CDBXMLErrorEx;
import alma.contLogTest.TestLogLevelsComp;

/**
 * Client application that accesses the TestLogLevelsComp component. It demonstrates
 * how the class {@link ComponentClient} can be used as a base class.
 * 
 * @author eallaert 30 October 2007
 */
public class TestLogLevelsCompClient extends ComponentClient
{
    private TestLogLevelsComp m_testlogComp;

    /**
     * @param logger
     * @param managerLoc
     * @param clientName
     * @throws Exception
     */
    public TestLogLevelsCompClient(Logger logger, String managerLoc, String clientName)
	throws Exception {
	super(logger, managerLoc, clientName);
    }

    /**
     * Calls getLevels() on the testlog component.
     * @throws AcsJContainerServicesEx 
     * @throws CDBRecordDoesNotExistEx 
     * @throws CDBXMLErrorEx 
     * @throws CouldntPerformActionEx 
     */
    public void doSomeStuff() throws AcsJContainerServicesEx, CDBXMLErrorEx, CDBRecordDoesNotExistEx, CouldntPerformActionEx {
    	m_logger.info("calling doSomeStuff ...");
    	try 
    	{
    		Thread.sleep(10000);
    	}
    	catch (InterruptedException e) {}
    	m_testlogComp = alma.contLogTest.TestLogLevelsCompHelper.narrow(getContainerServices().getComponent("TESTLOG1"));

    	int levels[] = m_testlogComp.getLevels();
    	m_logger.info("got levels from component's getLevels method: "
    			+ levels[0] + ", " + levels[1] + ", " + levels[2]);
    	try 
    	{
    		Thread.sleep(10000);
    	}
    	catch (InterruptedException e) {}
    }

    /**
     * Checks whether the Java property 'ACS.manager' is set and calls the
     * other methods from this class.
     */
    public static void main(String[] args) {
    	String managerLoc = System.getProperty("ACS.manager");
    	if (managerLoc == null) {
    		System.out
    		.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
    		System.exit(-1);
    	}
    	String clientName = "TestLogLevelsCompClient";
    	TestLogLevelsCompClient hlc = null;
    	try 
    	{
    		hlc = new TestLogLevelsCompClient(null, managerLoc, clientName);
    		hlc.doSomeStuff();
    	}
    	catch (Exception e)
    	{
    		try 
    		{
    			Logger logger = hlc.getContainerServices().getLogger();
    			logger.log(Level.SEVERE, "Client application failure", e);
    		}
    		catch (Exception e2) 
    		{
    			e.printStackTrace(System.err);
    		}
    	}
    	finally
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
    	}
    }
}
