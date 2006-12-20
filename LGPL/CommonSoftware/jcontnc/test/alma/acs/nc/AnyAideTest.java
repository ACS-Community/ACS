package alma.acs.nc;


import org.omg.CORBA.Any;

import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.container.AdvancedContainerServices;

public class AnyAideTest extends ComponentClientTestCase {

	private AnyAide anyAide;
	private AdvancedContainerServices advancedCS;
	
	public AnyAideTest() throws Exception {
		super("AnyAideTest");
	}
	
	protected void setUp() throws Exception {
		super.setUp();
		anyAide = new AnyAide(getContainerServices());
		advancedCS = getContainerServices().getAdvancedContainerServices();
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	public void testCorbaStructToJavaClass() {
		
		String nestedStructId = "IDL:alma/acssamp/NotNestedStruct:1.0";
		String qualClassName = anyAide.corbaStructToJavaClass(nestedStructId, false);
		assertEquals("alma.acssamp.NotNestedStruct", qualClassName);

		nestedStructId = "IDL:alma/acssamp/SampObj/SampDataBlock:1.0";
		qualClassName = anyAide.corbaStructToJavaClass(nestedStructId, true);
		assertEquals("alma.acssamp.SampObjPackage.SampDataBlock", qualClassName);

	}
}
