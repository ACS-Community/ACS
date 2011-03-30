package cl.utfsm.samplingSystemUITestCase;

import junit.framework.TestCase;
import cl.utfsm.samplingSystemUI.core.ComponentsManager;
import alma.acs.component.client.ComponentClient;

public class ComponentsManagerTest extends TestCase {
	
	ComponentClient client;
	ComponentsManager cManager=null;
	String managerLoc = System.getProperty("ACS.manager");

	protected void setUp() throws Exception {
		client = new ComponentClient(null, managerLoc, "ComponetsManagerTest");
		cManager = new ComponentsManager(client.getContainerServices());
	}

	protected void tearDown() throws Exception {
		client.tearDown();
	}

	public void testSetUp() throws Exception {
		assertNotNull(cManager);
	}

	public void testComponent() throws Exception {
		assertTrue(cManager.componentExists("LAMP1"));
		assertTrue(!cManager.componentExists("ODUCK1"));
	}

	public void testProperty() throws Exception {
		assertTrue(cManager.propertyExists("LAMP1","brightness"));
		assertTrue(!cManager.propertyExists("LAMP1","oduck"));
	}
	
}
