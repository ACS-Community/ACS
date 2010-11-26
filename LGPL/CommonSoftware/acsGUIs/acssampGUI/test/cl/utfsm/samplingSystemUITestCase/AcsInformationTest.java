package cl.utfsm.samplingSystemUITestCase;

import junit.framework.TestCase;
import cl.utfsm.samplingSystemUI.core.AcsInformation;

public class AcsInformationTest extends TestCase {


	protected void setUp() throws Exception {
		super.setUp();
		AcsInformation.getInstance("AcsInformationTest");
	}

	public void testSingleton() throws Exception {
		AcsInformation info1=null;
		AcsInformation info2=null;
		info1 = AcsInformation.getInstance();
		info2 = AcsInformation.getInstance();
		assertNotNull(info1);
		assertNotNull(info2);
		assertEquals(info1,info2);
	}

	public void testMultipleClients() throws Exception {
		AcsInformation info1=null;
		AcsInformation info2=null;
		info1 = AcsInformation.getInstance("AcsInformationTest1");
		info2 = AcsInformation.getInstance("AcsInformationTest2");
		assertEquals(info1,info2);
	}

	public void testComponent() throws Exception {

		AcsInformation info1=null;
		info1 = AcsInformation.getInstance("AcsInformationTest1");
		assertTrue(info1.componentExists("LAMP1"));
		assertTrue(!info1.componentExists("ODUCK1"));
	}

	public void testProperty() throws Exception {

		AcsInformation info1=null;
		info1 = AcsInformation.getInstance("AcsInformationTest1");
		assertTrue(info1.propertyExists("LAMP1","brightness"));
		assertTrue(!info1.propertyExists("LAMP1","oduck"));
	}

	public void tearDown() throws Exception {
		AcsInformation.getInstance("AcsInformationTest").shutDown();
	}
}
