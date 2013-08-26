package cl.utfsm.acs.ebe;

import java.io.File;
import java.io.FileReader;

import junit.framework.TestCase;
import cl.utfsm.acs.types.AcsComplexType;


public class EbeDocumentTEST extends TestCase {
	ErrorSchema schema;
	String xmldirs;
	protected void setUp() throws Exception {
		xmldirs=System.getProperty("test.xmldirs");
		schema=new ErrorSchema();
                Member.setClassType((AcsComplexType)schema.getType("Member_"));
                Error.setClassType(schema.getErrorSchema());
                Completion.setClassType(schema.getCompletionSchema());
                EbeDocument.setClassType(schema.getTypeSchema());
	}
	public void testLoad(){
		EbeDocument doc=new EbeDocument();
		doc.setPath(xmldirs + "/dir1/c.xml");
		doc.load();
		String tmp;
		
		assertEquals("Wrong value, ",doc.getAttributeValue("name"),"veryComplex");
		assertEquals("Wrong value, ",doc.getAttributeValue("type"),"1");
		assertEquals("Wrong value, ",doc.getAttributeValue("xmlns"),"Alma/ACSError");
		Completion c1=(Completion)doc.getNode("Completion1");
		assertEquals("Wrong value, ",c1.getAttributeValue("shortDescription"),"The Completion 1");
		Error e1=(Error)doc.getNode("Error1");
		assertEquals("Wrong value, ",e1.getAttributeValue("shortDescription"),"The Error 1");
		Error e2=(Error)doc.getNode("Error2");
		Member m1 = e2.getMember("member1");
		assertEquals("Wrong value, ",m1.getAttributeValue("description"),"The member number 1");
		Member m2 = e2.getMember("member2");
		assertEquals("Wrong value, ",m2.getAttributeValue("type"),"string");
	}
	public void testSave(){
		EbeDocument doc=new EbeDocument();
		doc.setPath("save.xml");
		doc.setDocumentInfo("saveDocument");
		doc.setAttributeValue("type","54");
		Completion c=new Completion();
		c.setAttributeValue("name","myCompletion");
		c.setAttributeValue("shortDescription","My own Completion");
		c.setValue("myCompletion");
		doc.putNode(c);
		Error e=new Error();
		e.setAttributeValue("name","myError");
		e.setAttributeValue("shortDescription","My own Error");
		e.setValue("myError");
		Member m=new Member();
		m.setAttributeValue("name","myMember");
		m.setAttributeValue("description","My own Member");
		e.setValue("myMember");
		e.putMember(m);
		doc.putNode(e);
		FileReader ref;
		FileReader created;	
		char ref_buff[]=new char[10000];
		char created_buff[]=new char[10000];
		try{
			doc.save();
			ref=new FileReader(new File(xmldirs + "/save.xml"));
			created=new FileReader(new File("save.xml"));
			ref.read(ref_buff);
			created.read(created_buff);
		}
		catch (Exception ex){
			System.out.println(ex);
		}
		String ref_string=new String(ref_buff);
		String created_string=new String(created_buff);
		assertTrue(ref_string.compareTo(created_string)==0);
		try{
			(new File("save.xml")).delete();
		}
		catch (Exception ex){
			System.out.println(ex);
		}
		
	}
}
