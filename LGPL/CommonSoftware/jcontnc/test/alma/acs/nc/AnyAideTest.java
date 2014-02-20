package alma.acs.nc;


import org.omg.CORBA.Any;

import alma.ADMINTEST1.NotNestedEvent;
import alma.ADMINTEST1.NotNestedEventHelper;
import alma.ADMINTEST1.InterfaceForNestedEventDefinitionPackage.NestedEvent;
import alma.ADMINTEST1.InterfaceForNestedEventDefinitionPackage.NestedEventHelper;
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
		
//		String nestedStructId = "IDL:alma/acssamp/NotNestedStruct:1.0";
		NotNestedEvent notNestedStruct = new NotNestedEvent();
		Any any1 = advancedCS.getAny();
		NotNestedEventHelper.insert(any1, notNestedStruct);
		String qualClassName = anyAide.corbaStructToJavaClass(any1.type(), false);
		assertEquals("alma.ADMINTEST1.NotNestedEvent", qualClassName);
		
//		nestedStructId = "IDL:alma/acssamp/SampObj/SampDataBlock:1.0";
		NestedEvent nestedStruct = new NestedEvent();
		Any any2 = advancedCS.getAny();
		NestedEventHelper.insert(any2, nestedStruct);
		qualClassName = anyAide.corbaStructToJavaClass(any2.type(), true);
		assertEquals("alma.ADMINTEST1.InterfaceForNestedEventDefinitionPackage.NestedEvent", qualClassName);
	}
}
