package alma.acs.eventbrowser.model;

import java.util.ArrayList;
import java.util.Iterator;

import junit.framework.TestCase;

public class EventModelTest extends TestCase {
	
	EventModel em;

	public EventModelTest(String name) {
		super(name);
		try {
			em = EventModel.getInstance();
		} catch (Exception e) {
			e.printStackTrace();
			fail();
		}
	}

	protected void setUp() throws Exception {
	}

	protected void tearDown() throws Exception {
	}

	public void testGetServices() {
		String[] serviceNames = em.getServices();
		for (int i = 0; i < serviceNames.length; i++) {
			System.out.println(serviceNames[i]);
		} 
	}
	
	public void testGetChannelStatistics() {
		ArrayList<ChannelData> clist = null;
		try {
			EventModel em = EventModel.getInstance();
			clist = em.getChannelStatistics();
			for (Iterator<ChannelData> iterator = clist.iterator(); iterator.hasNext();) {
				ChannelData object = iterator.next();
				System.out.println(object);
				
			}
		} catch (Exception e) {
			e.printStackTrace();
			fail();
		}
	}
	
	public void testSubscribeToAnEventChannel() throws Exception {
		String testChannel = "JoesTestChannel";
		int startConsumers = 0;
		ChannelData cData = getChannelData(testChannel);
		if (cData != null) startConsumers = cData.getNumberConsumers();
		AdminConsumer ac = new AdminConsumer(testChannel,em.getContainerServices());
		cData = getChannelData(testChannel);
		assertEquals(startConsumers+1,cData.getNumberConsumers());
		ac.disconnect();
		cData = getChannelData(testChannel);
		assertEquals(startConsumers,cData.getNumberConsumers());
	}
	
	private ChannelData getChannelData(String channel) {
		ArrayList<ChannelData> cstat = em.getChannelStatistics();
		for (Iterator iterator = cstat.iterator(); iterator.hasNext();) {
			ChannelData channelData = (ChannelData) iterator.next();
			if (channelData.getName().equals(channel)) {
				return channelData;
			}
		}
		return null;
	}

}
