/*
 * ALMA - Atacama Large Millimiter Array
 * (c) European Southern Observatory, 2009 
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */

package alma.acs.nc.refactored;

import java.util.logging.Logger;

import alma.ADMINTEST1.OnOffStates;
import alma.ADMINTEST1.statusBlockEvent1;
import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.container.ContainerServices;
import alma.acs.nc.Helper;

/**
 * This test class aims to test the NCPublisher class.
 * 
 * @author jslopez
 * 
 */
public class NCPublisherTest extends ComponentClientTestCase {
	private NCPublisher publisher = null;
	private ContainerServices services = null;
	private Logger logger = null;
	
	public NCPublisherTest(String name) throws Exception {
		super(name);
	}

	protected void tearDown() throws Exception {
		// Quick way instead of proper sync with subscriber object
		Thread.sleep(1000 * 3);
		super.tearDown();
	}

	/**
	 * This test creates one Publisher to a channel and send events.
	 */
	public void testPublisherSendingEvents() throws Exception {
		// Creating a new Publisher
		int numEvents = 1000;
		publisher = new NCPublisher("testingChannel", services, Helper.getNamingServiceInitial(getContainerServices()));
		assertTrue(publisher != null);

		// Sending numEvents;
		while (publisher.count < numEvents) {
			// Constructing the event
			OnOffStates onOff = OnOffStates.ON;
			statusBlockEvent1 event = null;
			event = new statusBlockEvent1(onOff, "testingEvent", 0,
					(int) publisher.count, numEvents, false, 100);
			// Publishing the event
			publisher.publishEvent(event);
			// simulates the period
			Thread.sleep((int) event.period);
		}
	}

	protected void setUp() throws Exception {
		super.setUp();
		services = getContainerServices();
		logger = services.getLogger();
		logger.info("Creating subscriber");
	}	
}