package cl.utfsm.acs.xml;

import junit.framework.TestCase;

import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;

import cl.utfsm.acs.xml.*;

// Example of a Test (is very very naive)
public class XmlSeekerTEST extends TestCase {
	String xmlroot;
	protected void setUp() throws Exception {
		xmlroot = System.getProperty("test.xmldirs");
	}
	public void testSeeker(){
		
		XmlSeeker test=new XmlSeeker();
		xmlroot+=File.separator;
		test.addDir(xmlroot + "dir1");
		test.addDir(xmlroot + "dir2");
		assertEquals(test.getXmls("Alma/ACSError ACSError.xsd").size(),3);
		test.removeDir(xmlroot + "dir2");
		assertEquals(test.getXmls("Alma/ACSError ACSError.xsd").size(),2);
		/*
		String output="["+xmlroot+"dir1/c.xml, "+xmlroot+"dir1/s.xml, "+xmlroot+"dir2/o.xml]";
		assertEquals("Wrong Value, ",output,test.getXmls("Alma/ACSError ACSError.xsd").toString());
		test.removeDir(xmlroot + "dir2");
		output="["+xmlroot+"dir1/s.xml, "+xmlroot+"dir1/c.xml]";
		assertEquals("Wrong Value, ",output,test.getXmls("Alma/ACSError ACSError.xsd").toString());
		*/
		test.clearDirs();
		assertEquals(test.getXmls("Alma/ACSError ACSError.xsd").size(),0);
		//output="[]";
		//assertEquals("Wrong Value, ",output,test.getXmls("Alma/ACSError ACSError.xsd").toString());
		
	}
						
}
