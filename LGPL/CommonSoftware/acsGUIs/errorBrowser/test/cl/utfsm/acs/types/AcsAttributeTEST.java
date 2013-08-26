package cl.utfsm.acs.types;

import junit.framework.TestCase;

import cl.utfsm.acs.types.AcsAttribute;
import cl.utfsm.acs.types.AcsSimpleType;

// Example of a Test (is very very naive)
public class AcsAttributeTEST extends TestCase {
	private AcsSimpleType attrType;
	protected void setUp() throws Exception {
		attrType=new AcsSimpleType("namespace","name","documentation","restriction");
	}
	public void testContent(){
		String attrName="attrName";
		String attrUse="attrUse";
		AcsAttribute attr=new AcsAttribute(attrName,attrType,attrUse);
		assertEquals("Wrong Value, ",attr.name,attrName);
		assertEquals("Wrong Value, ",attr.use,attrUse);
		assertEquals(attr.type,attrType);
	}
						
}
