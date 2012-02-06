/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2006
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */

package alma.acs.nc;

import org.omg.CosNaming.Binding;
import org.omg.CosNaming.BindingIteratorHolder;
import org.omg.CosNaming.BindingListHolder;
import org.omg.CosNotifyChannelAdmin.EventChannel;

import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.exceptions.AcsJException;
import alma.acscommon.NC_KIND;
import alma.acscommon.NOTIFICATION_FACTORY_NAME;

/**
 * This test should investigate why the ComponentClientTestCase environment 
 * fails to create and destroy a notification channel repeatedly.
 * <p>
 * This problem first showed up in ACSLaser/laser-source when modular tests were converted
 * from (TestCase using ComponentClient) to ComponentClientTestCase.
 * There the Consumer class could not be reinstantiated after a previous test 
 * had disconnected the Consumer. 
 */
public class ConsumerWithComponentClientTestCaseTest extends ComponentClientTestCase {

	private static final String channelName = "dummyTestChannel";

	private Helper ncHelper;
	private EventChannel channel;
		

	public ConsumerWithComponentClientTestCaseTest() throws Exception {
		super("ConsumerWithComponentClientTestCaseTest");
	}	
	
	public void setUp() throws Exception {
		super.setUp();        
		channelUp();
	}

	private void channelUp() throws AcsJException, InterruptedException {
		// this is the problematic code from the Consumer ctor 
		ncHelper = new Helper(getContainerServices(), Helper.getNamingServiceInitial(getContainerServices()));
		channel = ncHelper.getNotificationChannel(channelName, NC_KIND.value, NOTIFICATION_FACTORY_NAME.value);
		m_logger.info("NC consumer installed on channel " + channelName);
		Thread.sleep(500);
	}

	
	public void tearDown() throws Exception {
		try {			
			m_logger.info("tearDown called.");
			channelDown();
			// The following hack is necessary for JUnit tests!
			//Helper.m_nContext = null;
		} finally { 
			super.tearDown();
		}
		System.out.println("tearDown done.");
	}

	private void channelDown() throws AcsJException {
		assertTrue(namingBindingsContain(channelName));
		ncHelper.destroyNotificationChannel(channelName, NC_KIND.value, channel);
		assertFalse(namingBindingsContain(channelName));
		channel = null;
	}

	////////////////////////////////////////////////////////////////
	
	
	public void testNoOp1() throws Exception {
		m_logger.info("No-Op 1 called");
		assertNotNull(channel);
		
		// within a JUnit method it's fine to bring the channel down and up again.
		channelDown();
		channelUp();
		
		m_logger.info("No-Op 1 done");
	}
	
	
	/**
	 * This method will never be called because the second setUp() call will hang in channelUp
	 */
	public void testNoOp2() throws Exception {
		m_logger.info("No-Op 2 called");
	}

	
	private boolean namingBindingsContain(String firstBindingName) throws AcsJException {
		BindingListHolder blh = new BindingListHolder();
		ncHelper.getNamingService().list(100, blh, new BindingIteratorHolder());
		for (Binding binding : blh.value) {
			if (binding.binding_name[0].id.equals(firstBindingName)) {
				return true;
			}
		}
		return false;
	}	
	
}
