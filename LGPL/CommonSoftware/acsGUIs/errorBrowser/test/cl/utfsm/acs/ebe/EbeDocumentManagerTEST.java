package cl.utfsm.acs.ebe;

import junit.framework.TestCase;


public class EbeDocumentManagerTEST extends TestCase {
	EbeDocumentManager manager;
	String xmldirs;

	protected void setUp() throws Exception {
		ErrorBrowserEditor.logInfo=false;
                xmldirs=System.getProperty("test.xmldirs");
		manager=new EbeDocumentManager();
	}
	public void testDefaults(){
		manager.addDefaults();
		assertTrue(manager.getDocuments().size()>1);
	}
	public void testDirectories(){
		manager.addDirectory(xmldirs);
		manager.addDirectory(xmldirs+"/dir1");
		manager.addDirectory(xmldirs+"/dir2");
		assertEquals(manager.getDocuments().size(),4);
	}
	public void testNew(){
		manager.newDocument("newDoc.xml","newDoc");
		assertEquals(manager.getDocuments().size(),1);
		try{
		manager.getDocuments().get("newDoc").save();
		}catch (Exception e){}
		manager.deleteDocument("newDoc");
		assertEquals(manager.getDocuments().size(),0);
	}
	public void testRemove(){
		manager.addDirectory(xmldirs);
		manager.addDirectory(xmldirs+"/dir1");
		manager.addDirectory(xmldirs+"/dir2");
		manager.removeDocument("veryComplex");
		assertEquals(manager.getDocuments().size(),3);
		manager.removeAll();
		assertEquals(manager.getDocuments().size(),0);
	}
}
