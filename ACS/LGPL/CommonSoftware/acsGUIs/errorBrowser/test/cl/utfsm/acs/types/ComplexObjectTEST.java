package cl.utfsm.acs.types;

import junit.framework.TestCase;

import cl.utfsm.acs.types.AcsComplexType;
import cl.utfsm.acs.types.AcsSimpleType;
import cl.utfsm.acs.types.AcsAttribute;
import java.util.ArrayList;
import java.util.TreeMap;


public class ComplexObjectTEST extends TestCase {

	private AcsComplexType cType;
	String attrNames[]={"attr1","attr2","attr3","attr4"};
	String attrUsage[]={"required","optional","required","optional"};

	protected void setUp() throws Exception {
		AcsSimpleType sType1,sType2;
		sType1=new AcsSimpleType("namespace","double","documentation","restriction");
		sType2=new AcsSimpleType("namespace","string","documentation","restriction");
		AcsAttribute a1,a2,a3,a4;
		a1=new AcsAttribute(attrNames[0],sType1,attrUsage[0]);
		a2=new AcsAttribute(attrNames[1],sType1,attrUsage[1]);
		a3=new AcsAttribute(attrNames[2],sType2,attrUsage[2]);
		a4=new AcsAttribute(attrNames[3],sType2,attrUsage[3]);
		cType=new AcsComplexType("namespace","complexName","documentation");
		cType.addAttr(a1);
		cType.addAttr(a2);
		cType.addAttr(a3);
		cType.addAttr(a4);
	}
	public void testAttributes(){
		TreeMap<String,SimpleObject> attrs;
		ComplexObject obj=new ComplexObject(cType);
		attrs=obj.getAttributes();
		int i=0;
		for (String n : attrs.keySet()){
			SimpleObject simple=attrs.get(n);
			assertEquals("Wrong Value, ",n,attrNames[i]);
			if (i<2)
				assertEquals("Wrong Value, ",simple.getTypeName(),"double");
			else 
				assertEquals("Wrong Value, ",simple.getTypeName(),"string");
			assertEquals("Wrong Value, ",simple.getValue(),"");
			i++;
		}
	}

	public void testAttrValues(){
		ComplexObject obj=new ComplexObject(cType);
		obj.setAttributeValue("attr1","t1");
		obj.setAttributeValue("attr4","t2");
		assertEquals("Wrong Value, ",obj.getAttributeValue("attr1"),"t1");
		assertEquals("Wrong Value, ",obj.getAttributeValue("attr4"),"t2");
		boolean flag=false;
		try{
			obj.setAttributeValue("wrong attribute","t3");
		} catch (java.lang.IllegalArgumentException e){
		 	flag=true;
		}
		assertTrue(flag);
	}
}
