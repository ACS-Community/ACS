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
package alma.demo.test.EventSupplierImpl;

import java.util.logging.Level;

import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.portable.IDLEntity;

import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.FRIDGE.FridgeControlPackage.NestedFridgeEvent;
import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.nc.AcsEventPublisher;
import alma.acs.nc.refactored.NCPublisher;
import alma.acsnc.EventDescription;
import alma.demo.SupplierCompOperations;
import alma.maciErrType.wrappers.AcsJComponentCleanUpEx;

/** Class designed for testing event suppliers.
 * @TODO: consolidate with similar class in jcontexmpl
 * @author dfugate
 */
public class EventSupplierImpl extends ComponentImplBase implements SupplierCompOperations 
{
	/**
	 * To publish "NestedFridgeEvent" or "EventDescription" events
	 */
	private NCPublisher<IDLEntity> m_supplier = null;

	

	/** Sets up the NCPublisher.
	 * @param containerServices Services to components.
	 * @throws ComponentLifecycleException Not thrown.
	 */
	public void initialize(ContainerServices containerServices) throws ComponentLifecycleException {
		super.initialize(containerServices);

		try {
			//Instantiate our supplier
			AcsEventPublisher<IDLEntity> pubIF = containerServices.createNotificationChannelPublisher("blar", IDLEntity.class);
			// For special testing, we cast to the expected publisher impl class (normally not needed)
			m_supplier = (NCPublisher<IDLEntity>) pubIF;
			m_logger.info("NCPublisher for 'blar' channel created.");
		} catch (Exception e) {
			throw new ComponentLifecycleException(e);
		}
	}

	
	/** Sends some events to an event channel.
	 * @param param number of events to send
	 */
	public void sendEvents(short param) {
		m_logger.info("Now sending events via NCPublisher...");
		try {
			//first send out some number of events.
			EventDescription t_block = new EventDescription("no name", 32L, 64L);
			for (short i = 0; i < param; i++) {
				m_supplier.publishEvent(t_block);
				Thread.sleep(1);
			}

			//fake a subscription change notification (should be disabled on the proxy consumer in the real system)
			try {
				m_supplier.subscription_change(new org.omg.CosNotification.EventType[] {},
						new org.omg.CosNotification.EventType[] {});
				m_logger.warning("Call to 'subscription_change' did not produce the expected NO_IMPLEMENT exception.");
			} catch (NO_IMPLEMENT ex) {
				// expected
			}

		} catch (Exception e) {
			System.err.println(e);
		}
	}

	public void sendEventsSpecial(NestedFridgeEvent[] eventData) throws CouldntPerformActionEx {
		try {
			m_logger.info("Now sending " + eventData.length + " NestedFridgeEvent events...");
			for (NestedFridgeEvent event : eventData) {
				m_supplier.publishEvent(event);
			}
		} catch (Throwable thr) {
			m_logger.log(Level.WARNING, "failed to send NestedFridgeEvent. Will not try again.");
			throw (new AcsJCouldntPerformActionEx(thr)).toCouldntPerformActionEx();
		}
	}

	/** Disconnects the supplier. */
	public void cleanUp() throws AcsJComponentCleanUpEx {
		m_logger.info("cleanUp() called...");

		try {
			// Fake a consumer disconnecting...
			// This should produce the FINE log "A Consumer has disconnected from the 'blar' channel"
			m_supplier.disconnect_structured_push_supplier();
			
			m_supplier.disconnect();
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		super.cleanUp();
	}
}
