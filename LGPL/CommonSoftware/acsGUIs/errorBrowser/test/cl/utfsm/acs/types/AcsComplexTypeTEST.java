package cl.utfsm.acs.types;

import junit.framework.TestCase;

import cl.utfsm.acs.types.AcsComplexType;
import cl.utfsm.acs.types.AcsSimpleType;
import cl.utfsm.acs.types.AcsAttribute;
import java.util.ArrayList;

public class AcsComplexTypeTEST extends TestCase {

	private AcsSimpleType sType1,sType2;
	private AcsAttribute a1,a2,a3,a4;
	protected void setUp() throws Exception {
		sType1=new AcsSimpleType("namespace","name1","documentation","restriction");
		sType2=new AcsSimpleType("namespace","name2","documentation","restriction");
		a1=new AcsAttribute("attr1",sType1,"required");
		a2=new AcsAttribute("attr2",sType1,"optional");
		a3=new AcsAttribute("attr3",sType2,"required");
		a4=new AcsAttribute("attr4",sType2,"optional");
	}
	public void testNames(){
		AcsComplexType complex=new AcsComplexType("namespace","name","documentation");
		complex.addAttr(a1);
		complex.addAttr(a3);
		complex.addAttr(a2);
		complex.addAttr(a4);
		ArrayList list=complex.getAttrNames();
		assertEquals("Wrong Value",list.toString(),"[attr1, attr3, attr2, attr4]");
	}
	public void testEmpty(){
		AcsComplexType complex=new AcsComplexType("namespace","name","documentation");
		ArrayList list=complex.getAttrNames();
		String st=complex.getAttrUse("attr1");
		AcsSimpleType simple=complex.getAttrType("attr1");
                assertEquals("Non Empty value, ",list.toString(),"[]");
		assertNull(st);
		assertNull(simple);
	}
	public void testUseAndType(){
		AcsComplexType complex=new AcsComplexType("namespace","name","documentation");
		complex.addAttr(a1);
		complex.addAttr(a3);
		complex.addAttr(a2);
		complex.addAttr(a4);
		String st=complex.getAttrUse("attr1");
		AcsSimpleType simple=complex.getAttrType("attr2");
		assertEquals("Wrong Value",st,"required");
		assertEquals(sType1,simple);
		st=complex.getAttrUse("wrong");
		simple=complex.getAttrType("wrong");
		assertNull(simple);
	}
}
