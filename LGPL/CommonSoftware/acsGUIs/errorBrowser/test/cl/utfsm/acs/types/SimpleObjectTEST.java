package cl.utfsm.acs.types;

import junit.framework.TestCase;

import cl.utfsm.acs.types.AcsComplexType;
import cl.utfsm.acs.types.AcsSimpleType;
import cl.utfsm.acs.types.AcsAttribute;
import java.util.ArrayList;

public class SimpleObjectTEST extends TestCase {

	private AcsSimpleType sType;

	protected void setUp() throws Exception {
		sType=new AcsSimpleType("namespace","name","documentation","restriction");
	}
	public void testType(){
		SimpleObject sObj=new SimpleObject(sType);
		assertEquals("Wrong value, ",sType.namespace,sObj.getTypeNamespace());
		assertEquals("Wrong value, ",sType.documentation,sObj.getTypeDocumentation());
		assertEquals("Wrong value, ",sType.name,sObj.getTypeName());
	}
	public void testValue(){
		SimpleObject sObj=new SimpleObject(sType);
		sObj.setValue("String Value");
		assertEquals("Wrong value, ",sObj.getValue(),"String Value");
		assertEquals("Wrong value, ",sObj.toString(),"String Value");
	}
	public void testRestriction(){
		// not used yet
	}
}
