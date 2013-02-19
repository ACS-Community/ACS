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

import java.util.ArrayList;

import junit.framework.TestCase;

public class EventModelTest extends TestCase {
	
	EventModel em;

	public EventModelTest(String name) throws Exception {
		super(name);
	}

	@Override
	protected void setUp() throws Exception {
		em = EventModel.getInstance();
	}

	@Override
	protected void tearDown() throws Exception {
	}

//	public void testGetServices() {
//		String[] serviceNames = em.getServices();
//		for (int i = 0; i < serviceNames.length; i++) {
//			System.out.println(serviceNames[i]);
//		} 
//	}
	
	public void testGetChannelStatistics() throws Exception {
		ArrayList<ChannelData> clist = null;
		clist = em.getChannelStatistics();
		for (ChannelData channelData : clist) {
			System.out.println(channelData);
		}
	}
	
// Currently commented out because of the required "namingService" reference.
// What is the purpose of this test? Should it run without ACS services around, 
// or should it rather extend ComponentClientTestCase so that it has access to the naming service?
//
//	public void testSubscribeToAnEventChannel() throws Exception {
//		String testChannel = "JoesTestChannel";
//		int startConsumers = 0;
//		ChannelData cData = getChannelData(testChannel);
//		if (cData != null) {
//			startConsumers = cData.getNumberConsumers();
//		}
//		AdminConsumer ac = new AdminConsumer(testChannel, em.getContainerServices(), namingService);
//		cData = getChannelData(testChannel);
//		assertEquals(startConsumers+1,cData.getNumberConsumers());
//		ac.disconnect();
//		cData = getChannelData(testChannel);
//		assertEquals(startConsumers,cData.getNumberConsumers());
//	}
	
	private ChannelData getChannelData(String channel) {
		ArrayList<ChannelData> cstat = em.getChannelStatistics();
		for (ChannelData channelData : cstat) {
			if (channelData.getName().equals(channel)) {
				return channelData;
			}
		}
		return null;
	}

}
