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

import static org.hamcrest.Matchers.equalTo;
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
 * Currently this test is started via "acsStartJava NoDotJUnitRunner alma.demo.test.client.ArchiveConsumerTest",
 * and then 4 seconds later the "archiveeventsSupplier" is run, which sends off events and terminates.
 * Then TAT waits a total of 30 seconds, in which this test should receive the archive events.
 */
public class ArchiveConsumerTest extends ComponentClientTestCase implements ArchiveReceiver
{
	private ArchiveConsumer m_consumer;
	private volatile int count;

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
	}
	
	@Override
	protected void tearDown() throws Exception {
		m_consumer.disconnect();
		super.tearDown();
	}
	
	/**
	 * The types of the 11 events, as they get fired by the jcontnc/test/archiveeventsSupplier.cpp test supplier,
	 * will be checked by TAT based on the output from {@link #receive(Long, String, String, Object)}.
	 * Here we only validate the number of events received.
	 */
	public void testReceiveFor15Seconds() throws Exception {
		
		count = 0;
		m_consumer.startReceivingEvents();
		m_logger.info(getClass().getSimpleName() + " is receiving archive events for the next 15 seconds.");

		// receive and count events 
		Thread.sleep(15000);
		
		assertThat("ArchivingChannel events received", count, equalTo(11) );
		m_logger.info("Test passed, got the expected 11 archive events.");
	}
	
	
	@Override // ArchiveReceiver
	public void receive(long timeStamp, String device, String property, Object value) {
		
		count++;

		String isoTimeStamp = IsoDateFormat.formatDate(new Date(UTCUtility.utcOmgToJava(timeStamp)));
		m_logger.info("ArchivingChannel data received: timeStamp=" + isoTimeStamp + ", device=" + device + 
				", property=" + property + 
				", value(type)=" + ( value != null ? value.getClass().getSimpleName() : "null" ) );
	}

}
