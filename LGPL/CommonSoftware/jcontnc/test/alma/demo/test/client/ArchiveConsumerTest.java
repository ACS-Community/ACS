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

import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.junit.Assert.assertThat;

import java.util.Date;

import org.omg.CosNaming.NamingContext;
import org.omg.CosNaming.NamingContextHelper;

import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.nc.ArchiveConsumer;
import alma.acs.nc.ArchiveConsumer.ArchiveReceiver;
import alma.acs.util.IsoDateFormat;
import alma.acs.util.UTCUtility;

/**
 * Currently this test is started via "acsStartJava junit.textui.TestRunner alma.demo.test.client.ArchiveConsumerTest",
 * and then 3 seconds later the "archiveeventsSupplier" is run, which sends off events and terminates.
 * Then TAT waits 30 seconds for this test to receive these events.
 */
public class ArchiveConsumerTest extends ComponentClientTestCase implements ArchiveReceiver
{
	private ArchiveConsumer m_consumer;
	private volatile int count = 0;

	/**
	 * @throws Exception
	 */
	public ArchiveConsumerTest() throws Exception {
		super(ArchiveConsumerTest.class.getSimpleName());
	}

	@Override
	protected void setUp() throws Exception {
		super.setUp();
		
		NamingContext ncRef = NamingContextHelper.narrow(
				getContainerServices().getAdvancedContainerServices().getORB().resolve_initial_references("NameService") );

		m_consumer = new ArchiveConsumer(this, getContainerServices(), ncRef);
		m_consumer.startReceivingEvents();
	}
	
	@Override
	protected void tearDown() throws Exception {
		m_consumer.disconnect();
		super.tearDown();
	}
	
	/**
	 * The types of events, as they get fired by the archiveeventsSupplier application,
	 * will be checked by TAT based on the output from {@link #receive(Long, String, String, Object)}.
	 */
	public void testReceiveFor15Seconds() throws Exception {
		
		// receive and count events for a while
		Thread.sleep(15000);
		
		assertThat("ArchivingChannel events received", count, greaterThanOrEqualTo(11) );
		System.out.println("Test passed!");
	}
	
	
	@Override
	public void receive(long timeStamp, String device, String property, Object value) {
		
		count++;

		String isoTimeStamp = IsoDateFormat.formatDate(new Date(UTCUtility.utcOmgToJava(timeStamp)));
		m_logger.info("ArchivingChannel data received: timeStamp=" + isoTimeStamp + ", device=" + device + 
				", property=" + property + ", value(type)=" + value.getClass().getSimpleName());
	}

}
