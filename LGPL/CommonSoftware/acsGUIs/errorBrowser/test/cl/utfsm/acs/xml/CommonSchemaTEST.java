package cl.utfsm.acs.xml;

import junit.framework.TestCase;

import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;

import cl.utfsm.acs.xml.*;
import cl.utfsm.acs.types.*;

public class CommonSchemaTEST extends TestCase {
	String xmlroot;
	protected void setUp() throws Exception {
		xmlroot = System.getProperty("test.xmldirs");
	}
	public void testSchema(){
		String myAttr="name nameType required,type MemberTypes required,description nonEmptyString optional,";
		CommonSchema test=new CommonSchema();
                AcsComplexType t1=(AcsComplexType)test.getType("Member_");
                AcsSimpleType t2=(AcsSimpleType)test.getType("nameType");
		assertEquals("Wrong value, ",t2.namespace+"."+t2.name,"common.nameType");
		assertEquals(t2.documentation.length(),53);
		String theText="";
                for (AcsAttribute at : ((AcsComplexType)t1).attrs){
                        theText+=at.name + " "+ at.type.name + " " + at.use+",";
                }
		assertEquals("Wrong value, ",myAttr,theText);
	}
						
}
