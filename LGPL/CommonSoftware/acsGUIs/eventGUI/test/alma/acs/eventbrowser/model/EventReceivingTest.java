/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.acs.eventbrowser.model;

import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.eventbrowser.Application;
import alma.acs.exceptions.AcsJException;
import alma.acs.logging.AcsLogger;
import junit.framework.TestCase;

public class EventReceivingTest extends TestCase {

	private static final int EVENTS_TO_SEND = 10000;
	private EventModel em;
	private AdminConsumer consumer = null;
	private EventSupplierImpl supplier;
	private ContainerServices cs;
	private AcsLogger logger;
	private final int QUEUE_SIZE;

	public EventReceivingTest(String name) {
		super(name);
		QUEUE_SIZE=Application.equeue.remainingCapacity();

	}

	@Override
	protected void setUp() throws Exception {
		super.setUp();
		try {
			em = EventModel.getInstance();
			cs = em.getContainerServices();
			logger = cs.getLogger();
			
		} catch (Exception e) {
			e.printStackTrace();
			fail();
		}
		try {
			consumer = em.getAdminConsumer("blar");
			consumer.consumerReady();
		} catch (AcsJException e) {
			e.printStackTrace();
			fail();
		}
		try {
			supplier = new EventSupplierImpl(logger, cs, "EventReceivingTest");
		} catch (Exception e) {
			e.printStackTrace();
			fail();
		}
		try {
			supplier.initialize(cs);
		} catch (ComponentLifecycleException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			fail();
		}
	}

	@Override
	protected void tearDown() throws Exception {
		if (consumer != null) {
			em.closeSelectedConsumer("blar", false);
			consumer = null;
		}
		if (supplier != null) {
			supplier.cleanUp();
			supplier = null;
		}
		Application.equeue.clear();
		em.tearDown();
		super.tearDown();
	}
	
	public void testReceiveEvents() throws InterruptedException {
		long startTime = System.currentTimeMillis();
		supplier.sendEvents((short) EVENTS_TO_SEND);
		long endTime = System.currentTimeMillis();
		long diff = endTime - startTime;
		logger.info("Time to send "+EVENTS_TO_SEND+" events was "+diff+" ms.");
		Thread.sleep(EVENTS_TO_SEND);
		logger.info("Consumer received "+AdminConsumer.getTotalEventCount());
		assertEquals(QUEUE_SIZE-EVENTS_TO_SEND, Application.equeue.remainingCapacity());

	}

}
