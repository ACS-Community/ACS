package cl.utfsm.acs.types;

import junit.framework.TestCase;

import cl.utfsm.acs.types.AcsSimpleType;

// Example of a Test (is very very naive)
public class AcsSimpleTypeTEST extends TestCase {
	protected void setUp() throws Exception {
		
	}
	public void testContent(){
		String namespace     = "namespace";
		String name	     = "name";
		String documentation = "documentation";
		String restriction   = "restriction";
		AcsSimpleType obj=new AcsSimpleType(namespace,name,documentation,restriction);
		assertEquals("Wrong value, ",namespace,obj.namespace);
		assertEquals("Wrong value, ",name,obj.name);
		assertEquals("Wrong value, ",documentation,obj.documentation);
		assertEquals("Wrong value, ",restriction,obj.restriction);
	}
						
}
