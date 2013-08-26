/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory,
 * 2002 Copyright by ESO (in the framework of the ALMA collaboration), All
 * rights reserved
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package alma.demo.client;

import java.util.logging.Level;

import org.omg.CORBA.DATA_CONVERSION;

import alma.JContExmplErrTypeTest.XmlComponentErrorEx;
import alma.JContExmplErrTypeTest.wrappers.AcsJXmlComponentErrorEx;
import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.exceptions.AcsJException;
import alma.demo.SchedBlockHolder;
import alma.demo.XmlComponent;
import alma.demo.XmlComponentJ;
import alma.demo.XmlComponentOperations;
import alma.entities.commonentity.EntityT;
import alma.xmlentity.XmlEntityStruct;
import alma.xmljbind.test.obsproposal.ObsProposal;
import alma.xmljbind.test.schedblock.SchedBlock;

/**
 * JUnit test client for the XmlComponent. Demonstrates the use of
 * ComponentClientTestCase (which is the reason why we have it under /src), but
 * also serves as a real unit test for the ACS framework.
 * 
 * @author hsommer Jan 15, 2003 10:22:15 AM
 */
public class XmlComponentClient extends ComponentClientTestCase
{
	// standard CORBA interface
	private XmlComponent m_xmlComp;

	// transparent-XML interface
	private XmlComponentJ m_xmlCompJ;


	public XmlComponentClient() throws Exception {
		super("XmlComponentClient");
	}


	protected void setUp() throws Exception {
		super.setUp();

		org.omg.CORBA.Object compObj = getContainerServices().getComponent("XMLCOMP1");
		assertNotNull(compObj);

		m_xmlComp = alma.demo.XmlComponentHelper.narrow(compObj);
		assertNotNull(m_xmlComp);

		m_xmlCompJ = getContainerServices().getTransparentXmlWrapper(
				XmlComponentJ.class, m_xmlComp, XmlComponentOperations.class);
		assertNotNull(m_xmlCompJ);
	}

	/**
	 * Makes sure the method sayHello() on the xmlcomponent returns a reply.
	 */
	public void testSayHelloUsingHelloDemoComponent() {
		String reply = m_xmlCompJ.sayHello();
		assertNotNull(reply);
		System.out.println("received reply " + reply);
		assertEquals("reply must be 'hello'", "hello", reply);
	}

	/**
	 * Makes sure the obs proposal exists and checks its entity id.
	 */
	public void testCreateObsProposal() {
		ObsProposal obsProp = m_xmlCompJ.createObsProposal();
		assertNotNull(obsProp);

		EntityT ent = obsProp.getObsProposalEntity();
		assertNotNull(ent);

		String id = ent.getEntityId();
		assertNotNull(id);

		System.out.println("received ObsProposal with id " + id);
	}

	/**
	 * Takes a hard-coded XML of a SchedBlock and sends it to <code>addNewSchedBlocks</code>.
	 *
	 */
	public void testAddNewSchedBlocks() {
		XmlEntityStruct[] xesArray = new XmlEntityStruct[1];
		
		// first use valid xml
		xesArray[0] = createSchedBlockXml(false);		
		m_xmlComp.addNewSchedBlocks(xesArray);

		// now make Castor unmarshalling fail
		xesArray[0] = createSchedBlockXml(true);		
		try {
			m_xmlComp.addNewSchedBlocks(xesArray);
			fail("illegal XML should have caused an exception in the server-side unmarshalling.");
		}
		catch (DATA_CONVERSION ex) {			
			m_logger.log(Level.INFO, "illegal XML yielded exception as expected: " + ex.toString() );
		}
}

		
	/**
	 * creates schedblock xml. not fancy, always the same ID, but good enough for the test.
	 */
	private XmlEntityStruct createSchedBlockXml(boolean breakUnmarshaller) {
		String sbXML =  
		"<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>" +
		"<ns1:SchedBlock xmlns:ns1=\"AlmaTest/SchedBlock\">" +
			"<ns1:SchedBlockEntity " +
				"entityIdEncrypted=\"-- id encryption not yet implemented --\" " +
				"entityId=\"uid://X0000000000000064/X10000001\" entityTypeName=\"SchedBlock\"/> " +
			"<ns1:SchedBlockControl repeatCount=\"1\" entityPartId=\"X00000008\" />";
		if (breakUnmarshaller) {
			sbXML += "<ns1:ElementTooMany whyIsThis=\"to martially break the unmarshaller\" />";
		}
		sbXML += "</ns1:SchedBlock>";
		
		XmlEntityStruct entStruct = new XmlEntityStruct();
		entStruct.xmlString = sbXML;
		entStruct.entityId = "uid://X0000000000000064/X10000001";
		entStruct.entityTypeName = "SchedBlock"; 
		entStruct.schemaVersion = "";
		entStruct.timeStamp = "";
		return entStruct;
	}


	public void testXmlInOutMethod() {
		ObsProposal obsProp = m_xmlCompJ.createObsProposal();
		assertNotNull(obsProp);
		SchedBlockHolder sbh = new SchedBlockHolder();

		m_xmlCompJ.xmlInOutMethod(obsProp, sbh);

		SchedBlock schedBlock = sbh.value;
		assertNotNull(schedBlock);

		EntityT ent = schedBlock.getSchedBlockEntity();
		assertNotNull(ent);
		String id = ent.getEntityId();
		assertNotNull(id);

		System.out.println("received out-param SchedBlock with id " + id);
	}

	/**
	 * Throws an exception.
	 * 
	 * @throws Exception
	 */
	public void testException() throws Exception {
		boolean gotException = false;

		try {
			m_xmlCompJ.exceptionMethod();
		}
		catch (XmlComponentErrorEx e) {
			gotException = true;
            m_logger.info("received " + XmlComponentErrorEx.class.getName() + " as intended. Log messages of ErrorTrace follow:");
            AcsJException acsJEx = AcsJXmlComponentErrorEx.fromXmlComponentErrorEx(e);
            acsJEx.log(m_logger);
		}

		assertTrue("must receive " + XmlComponentErrorEx.class.getName(), gotException);
	}

	// Just to release the component at the end
	public void testReleaseComponent() throws Exception  {
		getContainerServices().releaseComponent("XMLCOMP1");
	}
}

