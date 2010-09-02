package alma.acsexmpl.clients;

import java.io.CharArrayReader;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.xml.sax.InputSource;

import alma.acs.component.client.ComponentClientTestCase;
import alma.demo.XmlComponent;
import alma.demo.XmlComponentHelper;
import alma.demo.XmlOffshoot;
import alma.xmlentity.XmlEntityStruct;


public class XmlComponentClientTest extends ComponentClientTestCase {

	private static final String xmlCompName = "XMLCOMP1";
	private XmlComponent xmlComponent;
	
	public XmlComponentClientTest() throws Exception {
		super("ErrorComponentTest");
	}
	
	protected void setUp() throws Exception {
		super.setUp();
		xmlComponent = XmlComponentHelper.narrow(getContainerServices().getComponent(xmlCompName));
		assertEquals(xmlCompName, xmlComponent.name());
	}

	protected void tearDown() throws Exception {
		getContainerServices().releaseComponent(xmlCompName);
		super.tearDown();
	}

	public void testOffshoot() throws Exception {

		DocumentBuilder db = DocumentBuilderFactory.newInstance().newDocumentBuilder();
		XmlOffshoot shoot = xmlComponent.getOffshoot();
		assertNotNull(shoot.getObsProposal());
		assertNotNull(shoot.getSchedBlock());

		// these values are hardcoded in the offshoot implementation
		XmlEntityStruct struct = shoot.getObsProposal();
		Document d = db.parse(new InputSource(new CharArrayReader(struct.xmlString.toCharArray())));
		assertEquals("rtobar", d.getElementsByTagName("PI").item(0).getTextContent());
		assertEquals("2010.0045.34S", d.getElementsByTagName("code").item(0).getTextContent());
		assertEquals("just for fun", d.getElementsByTagName("ScientificJustification").item(0).getTextContent());

		struct = shoot.getSchedBlock();
		d = db.parse(new InputSource(new CharArrayReader(struct.xmlString.toCharArray())));
		assertEquals("holography", d.getElementsByTagName("ns2:name").item(0).getTextContent());
		assertEquals("DONE", d.getElementsByTagName("ns1:status").item(0).getTextContent());
		assertEquals("true", d.getElementsByTagName("StandardMode").item(0).getTextContent());

		// just to check the setters
		shoot.setObsProposal(new XmlEntityStruct());
		shoot.setSchedBlock(new XmlEntityStruct());

		// deactivate the offshoot on the server-side
		xmlComponent.deactivateOffshoot();
		try {
			shoot.getObsProposal();
			fail("offshoot should be deactivated, I shouldn't be able to use it");
		} catch(org.omg.CORBA.UNKNOWN e) {}
	}
}
