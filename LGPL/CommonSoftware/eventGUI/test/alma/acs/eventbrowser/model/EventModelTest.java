package alma.acs.eventbrowser.model;

import junit.framework.TestCase;

public class EventModelTest extends TestCase {
	
	EventModel em;

	public EventModelTest(String name) {
		super(name);
		try {
			em = new EventModel();
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

}
