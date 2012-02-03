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

import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.component.client.ComponentClient;
import alma.demo.ConsumerComp;
import alma.demo.SupplierComp;

/**
 */
public class EventILComponentTest extends ComponentClient
{
    private SupplierComp m_supplier;
    private ConsumerComp m_consumer;

	/**
	 * @param logger
	 * @param managerLoc
	 * @param clientName
	 * @throws Exception
	 */
	public EventILComponentTest(String managerLoc) throws Exception {
		super(null, managerLoc, "EventILComponentTest");
		m_consumer = alma.demo.ConsumerCompHelper.narrow(getContainerServices()
								 .getComponent("IL_CONSUMERCOMP1"));
		m_supplier = alma.demo.SupplierCompHelper.narrow(getContainerServices()
								 .getComponent("IL_SUPPLIERCOMP1"));
		//give the consumer a few seconds to discover the channel exists and subscribe
		Thread.sleep(5000);
	}

	/**
	 */
	public void doSomeStuff() throws AcsJCouldntPerformActionEx {
		try {
			m_supplier.sendEvents((short)10);
		} catch (CouldntPerformActionEx e) {
			throw AcsJCouldntPerformActionEx.fromCouldntPerformActionEx(e);
		}
	}

    public void cleanupNC() throws Exception {
	getContainerServices().releaseComponent("SUPPLIERCOMP1");
	//give consumer fifty seconds to process the events
	Thread.sleep(50000);
	getContainerServices().releaseComponent("CONSUMERCOMP1");
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
		EventILComponentTest hlc = null;
		try {
			hlc = new EventILComponentTest(managerLoc);
			hlc.doSomeStuff();
			hlc.cleanupNC();
		}
		catch (Exception e) {
			e.printStackTrace(System.err);
		}
		finally {
			if (hlc != null) {
				try {
					hlc.tearDown();
				}
				catch (Exception e1) {
					// bad luck
				}
			}
		}
	}
}

