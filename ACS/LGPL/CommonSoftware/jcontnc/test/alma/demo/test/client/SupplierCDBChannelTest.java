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
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package alma.demo.test.client;

import java.util.logging.Logger;

import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.component.client.ComponentClient;
import alma.demo.SupplierComp;

/**
 * Client application that accesses a supplier and sends events.
 */
public class SupplierCDBChannelTest extends ComponentClient
{
    private SupplierComp m_supplier = null;
    
    /**
     * @param logger
     * @param managerLoc
     * @param clientName
     * @throws Exception
     */
    public SupplierCDBChannelTest(Logger logger, String managerLoc, String clientName)
	throws Exception 
	{
	    super(logger, managerLoc, clientName);
	    m_supplier = alma.demo.SupplierCompHelper.narrow(getContainerServices().getComponent("SUPPLIER_CDB_CHANNEL_COMP1"));
	}

	public void doSomeStuff() throws AcsJCouldntPerformActionEx {
		try {
			m_supplier.sendEvents((short)10);
		} catch (CouldntPerformActionEx e) {
			throw AcsJCouldntPerformActionEx.fromCouldntPerformActionEx(e);
		}
	}

	public void cleanupNC() throws Exception 
	{
	    getContainerServices().releaseComponent("SUPPLIER_CDB_CHANNEL_COMP1");
	}
    
    /**
     * Checks whether the Java property 'ACS.manager' is set and calls the
     * other methods from this class.
     */
    public static void main(String[] args) 
	{
	    String managerLoc = System.getProperty("ACS.manager");
	    if (managerLoc == null) 
		{
		System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
		System.exit(-1);
		}
	    String clientName = "SupplierCDBChannelTest";
	    SupplierCDBChannelTest hlc = null;
	    try 
		{
		hlc = new SupplierCDBChannelTest(null, managerLoc, clientName);
		hlc.doSomeStuff();
		hlc.cleanupNC();
		}
	    catch (Exception e) 
		{
		e.printStackTrace(System.err);
		}
	    finally 
		{
		if (hlc != null) 
		    {
		    try 
			{
			hlc.tearDown();
			}
		    catch (Exception e1) 
			{
			// bad luck
			}
		    }
		}
	}
}

