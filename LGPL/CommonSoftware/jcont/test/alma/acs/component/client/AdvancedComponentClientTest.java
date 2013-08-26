package alma.acs.component.client;

import java.util.logging.Logger;

import junit.framework.TestCase;

import alma.acs.container.ContainerServices;
import alma.acs.logging.ClientLogManager;
import alma.jconttest.ContainerServicesTester;
import alma.jconttest.ContainerServicesTesterHelper;

public class AdvancedComponentClientTest extends TestCase {

	private AdvancedComponentClient client;
	private Logger logger;
	
	protected void setUp() throws Exception {
		super.setUp();
		String clientName = getClass().getName();
		logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(clientName, true);
		String managerLoc = System.getProperty("ACS.manager").trim();
		
		client = new AdvancedComponentClient(logger, managerLoc, clientName);		
	}
	
	public void testCreateContainerServices() throws Exception {
    	String localClientName = "sillyLocalClient";
    	ContainerServices newCS = client.createContainerServices(localClientName, logger);
    	assertEquals(newCS.getName(), localClientName);
    	try {
    		newCS.getComponentStateManager();
    		fail("NPE expected in getComponentStateManager call!");
    	} catch (NullPointerException ex) {
    		; // that's good
    	}
    	
    	// get references to the same component through two different CS instances
    	final String compInstance = "CONT_SERVICES_TESTER";
    	ContainerServicesTester compRef1 = ContainerServicesTesterHelper.narrow(client.getContainerServices().getComponent(compInstance));
    	assertNotNull(compRef1);
    	ContainerServicesTester compRef2 = ContainerServicesTesterHelper.narrow(newCS.getComponent(compInstance));    		
    	assertNotNull(compRef2);
    	assertNotSame("Should have gotten a new CORBA proxy for the same component.", compRef1, compRef2);
    	assertEquals(compRef1.name(), compRef2.name());		
	}
}
