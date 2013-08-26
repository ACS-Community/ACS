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
import alma.FRIDGE.TemperatureStatus;
import alma.FRIDGE.FridgeControlPackage.NestedFridgeEvent;
import alma.acs.component.client.ComponentClient;
import alma.demo.ConsumerComp;
import alma.demo.SupplierComp;

/**
 */
public class EventComponentTest extends ComponentClient
{
	private final SupplierComp m_supplierComp;
	private final ConsumerComp m_consumerComp;

	/**
	 * @param managerLoc
	 */
	public EventComponentTest(String managerLoc) throws Exception {
		super(null, managerLoc, "EventComponentTest");
		m_consumerComp = alma.demo.ConsumerCompHelper.narrow(getContainerServices().getComponent("CONSUMERCOMP1"));
		m_supplierComp = alma.demo.SupplierCompHelper.narrow(getContainerServices().getComponent("SUPPLIERCOMP1"));
		//give the consumer a few seconds to discover the channel exists and subscribe
		Thread.sleep(5000);
	}

	/**
	 * Gets the supplier component to send out events 
	 */
	public void doSomeStuff() throws AcsJCouldntPerformActionEx {
		try {
			// get the supplier component to create 10 default events.
			// The consumer will unsubscribe after having received 5 of these events.
			m_supplierComp.sendEvents((short)10);

			// Now give the supplier component a bunch of event structs to publish on the fridge channel
			// Note that the event type is different from the above.
            int nNestedEvents = 5;
            NestedFridgeEvent[] nestedEvents = new NestedFridgeEvent[nNestedEvents];
            for (int i = 0; i < nNestedEvents; i++) {
            	nestedEvents[i] = new NestedFridgeEvent();
            	nestedEvents[i].status = TemperatureStatus.ATREF;
            }
            m_supplierComp.sendEventsSpecial(nestedEvents);
			
		} catch (CouldntPerformActionEx e) {
			throw AcsJCouldntPerformActionEx.fromCouldntPerformActionEx(e);
		}
	}

	public void cleanupNC() throws Exception {
		getContainerServices().releaseComponent("SUPPLIERCOMP1");
		//give consumer five seconds to process the events
		Thread.sleep(5000);
		getContainerServices().releaseComponent("CONSUMERCOMP1");
	}

	/**
	 * Checks whether the Java property 'ACS.manager' is set and calls the
	 * other methods from this class.
	 */
	public static void main(String[] args) {
		String managerLoc = System.getProperty("ACS.manager");
		if (managerLoc == null) {
			System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
			System.exit(-1);
		}
		EventComponentTest instance = null;
		try {
			instance = new EventComponentTest(managerLoc);
			instance.doSomeStuff();
			instance.cleanupNC();
		}
		catch (Exception e) {
			e.printStackTrace(System.err);
		}
		finally {
			if (instance != null) {
				try {
					instance.tearDown();
				}
				catch (Exception e1) {
					// bad luck
				}
			}
		}
	}
}

