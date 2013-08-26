/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) Associated Universities Inc., 2002
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
 *
 * ncTestCompImpl.java
 *
 * Created on April 11, 2003, 2:21 PM
 */
package alma.demo.test.EventILSupplierImpl;

import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.FRIDGE.FridgeControlPackage.NestedFridgeEvent;
import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.nc.AcsEventPublisher;
import alma.acsnc.EventDescription;
import alma.demo.SupplierCompOperations;

/**
 * Class designed for testing event suppliers.
 * 
 * @author dfugate
 */
public class EventSupplierImpl extends ComponentImplBase implements SupplierCompOperations
{
	private AcsEventPublisher<EventDescription> m_supplier = null;

	/**
	 * Sends some events to an event channel.
	 * 
	 * @param param
	 *            number of events to send
	 */
	@Override
	public void sendEvents(short param) {
		System.out.println("Now sending simplesupplier events...");
		try {
			// first send out some number of events.
			EventDescription t_block = new EventDescription("no name", 32L, 64L);
			for (short i = 0; i < param; i++) {
				m_supplier.publishEvent(t_block);
				Thread.sleep(1);
			}
		} catch (Exception e) {
			System.err.println(e);
		}
	}

	@Override
	public void sendEventsSpecial(NestedFridgeEvent[] eventData) throws CouldntPerformActionEx {
		throw new org.omg.CORBA.NO_IMPLEMENT("Method not implemented yet");
	}


	/** Disconnects the supplier. */
	public void cleanUp() {
		m_logger.info("cleanUp() called...");

		try {
			m_supplier.disconnect();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Sets up the SimpleSupplier.
	 * 
	 * @param containerServices
	 *            Services to components.
	 * @throws ComponentLifecycleException
	 *             Not thrown.
	 */
	public void initialize(ContainerServices containerServices) throws ComponentLifecycleException {
		super.initialize(containerServices);
		m_logger.info("initialize() called...");

		try {
			// Instantiate our supplier
			m_supplier = m_containerServices.createNotificationChannelPublisher("blarIL", EventDescription.class);
		} catch (Exception e) {
			e.printStackTrace(System.err);
			throw new ComponentLifecycleException(e);
		}
	}

}
