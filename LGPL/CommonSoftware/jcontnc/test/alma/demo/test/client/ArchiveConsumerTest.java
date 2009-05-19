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

import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.nc.ArchiveConsumer;

/**
 * 
 */
public class ArchiveConsumerTest extends ComponentClientTestCase
{
	private ArchiveConsumer m_consumer;
	private int count = 0;

	/**
	 * @param managerLoc
	 * @throws Exception
	 */
	public ArchiveConsumerTest() throws Exception {
		super(ArchiveConsumerTest.class.getSimpleName());
	}

	protected void setUp() throws Exception {
		super.setUp();
		m_consumer = new ArchiveConsumer(getContainerServices(), this);
		m_consumer.consumerReady();
	}
	
	protected void tearDown() throws Exception {
		m_consumer.disconnect();
		super.tearDown();
	}
	
	public void testReceiveFor15Seconds() throws Exception {
		Thread.sleep(15000);
		assertTrue("Should have received at least 11 events, but was " + count, count >= 11);
		System.out.println("Test passed!");
	}
	
	public void receive(Long a, String b, String c, Object d) {
		m_logger.info("receive method: b=" + b + ", c=" + c + ", d(type)=" + d.getClass());
		count++;
	}

}
