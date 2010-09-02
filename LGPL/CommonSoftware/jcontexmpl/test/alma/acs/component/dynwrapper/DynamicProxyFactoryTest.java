/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.component.dynwrapper;

import java.lang.reflect.UndeclaredThrowableException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Random;
import java.util.logging.Logger;

import junit.framework.TestCase;

import org.exolab.castor.xml.MarshalException;

import alma.ACS.ComponentStates;
import alma.JContExmplErrTypeTest.XmlComponentErrorEx;
import alma.acs.component.ComponentImplBase;
import alma.acs.logging.ClientLogManager;
import alma.demo.ObsProjectTree;
import alma.demo.ObsProjectTreeJ;
import alma.demo.SchedBlockHolder;
import alma.demo.XmlComponentJ;
import alma.demo.XmlComponentOperations;
import alma.demo.XmlOffshootJ;
import alma.xmlentity.XmlEntityStruct;
import alma.xmlentity.XmlEntityStructHolder;
import alma.xmljbind.test.obsproposal.ObsProposal;
import alma.xmljbind.test.obsproposal.ObsProposalEntityT;
import alma.xmljbind.test.schedblock.SchedBlock;
import alma.xmljbind.test.schedblock.SchedBlockControlT;
import alma.xmljbind.test.schedblock.SchedBlockEntityT;

/**
 * A quick test for the <code>alma.acs.component.dynwrapper</code> package 
 * that uses a non-component implementation of the {@link alma.demo.XmlComponentJ} interface, 
 * thus shortcutting container overhead.  
 * 
 * @author hsommer Dec 4, 2002 4:49:21 PM
 * $Id$
 */
public class DynamicProxyFactoryTest extends TestCase
{
	private Class<XmlComponentOperations> corbaIF = alma.demo.XmlComponentOperations.class;
	private XmlTestComponent compImpl;
	private Class<XmlComponentJ> compIF = XmlComponentJ.class;
	private Logger m_logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("DynamicProxyFactoryTest", false);
	
	/**
	 * Constructor for DynamicProxyFactoryTest.
	 * @param name
	 */
	public DynamicProxyFactoryTest(String name)
	{
		super(name);
	}

	/**
	 * @see TestCase#setUp()
	 */
	protected void setUp() throws Exception
	{
		compImpl = new DynamicProxyFactoryTest.XmlTestComponent();
	}


	public void testCreateServerProxy() throws DynWrapperException
	{
		assertNotNull(corbaIF);
		assertNotNull(compImpl);
		assertNotNull(compIF);

		XmlComponentOperations serverProxy = createServerProxy();
			
		assertNotNull(serverProxy);
	}

	private XmlComponentOperations createServerProxy() throws DynWrapperException
	{
		return (XmlComponentOperations) 
			DynamicProxyFactory.getDynamicProxyFactory(m_logger).
				createServerProxy(corbaIF, compImpl, compIF);
	}
	
	
	public void testCallCreateObsProposal() throws DynWrapperException
	{
		XmlComponentOperations serverProxy = createServerProxy();
		assertNotNull(serverProxy);
		
		XmlEntityStruct entStruct = serverProxy.createObsProposal();
		assertNotNull(entStruct);
		assertNotNull(entStruct.xmlString);
		
		System.out.println("received ObsProposal as XML: " + entStruct.xmlString);
	}

	public void testCallXmlInOutMethod() throws DynWrapperException
	{
		XmlComponentOperations serverProxy = createServerProxy();
		assertNotNull(serverProxy);
		
		XmlEntityStruct entStructObsProp = serverProxy.createObsProposal();
		XmlEntityStructHolder xesh = new XmlEntityStructHolder(); 
		
		// send invalid xml
		entStructObsProp.xmlString = 
			"<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>" +
			"<ns1:ObsProposal xmlns:ns1=\"AlmaTest/ObsProposal\">" +
			"	<ns1:ObsProposalEntity entityIdEncrypted=\"ljasd;ljfa;lsfd\"" +
			"		entityId=\"uid://X0000000000000000/X00000001\" entityTypeName=\"ObsProposal\"/>" +
			"	<ns1:ContactPerson>Otis P. Driftwood.</ns1:ContactPerson>" +
			"</ns1:ObsProposal>";
		serverProxy.xmlInOutMethod(entStructObsProp, xesh);
		assertNotNull(xesh.value);
		String xml = xesh.value.xmlString;
		assertNotNull(xml);
		
		System.out.println("received out-param SchedBlock as XML: " + xml);
	}

	public void testCallGetAllSchedBlocks() throws DynWrapperException 
	{
		XmlComponentOperations serverProxy = createServerProxy();
		assertNotNull(serverProxy);
		
		XmlEntityStruct[] entStructSchedBlocks = serverProxy.getAllSchedBlocks();
		
		assertNotNull(entStructSchedBlocks);
		System.out.println("received SchedBlocks as XML: ");
		for (int i = 0; i < entStructSchedBlocks.length; i++)
		{
			XmlEntityStruct structSB = entStructSchedBlocks[i];
			assertNotNull(structSB);
			assertNotNull(structSB.xmlString);
			System.out.println(structSB.xmlString);
		}		
	}

	public void testCallGetEntireTreeInAStruct() throws DynWrapperException
	{
		XmlComponentOperations serverProxy = createServerProxy();
		assertNotNull(serverProxy);
		
		ObsProjectTree struct = serverProxy.getEntireTreeInAStruct();
		
		assertNotNull("returned ObsProjectTree object not null", struct);
		
		assertNotNull("ObsProposal not null", struct.prop);
		System.out.println("received ObsProposal as XML: " + struct.prop.xmlString);

		assertNotNull("SchedBlockArray not null", struct.schedBlocks);
		assertTrue("SchedBlockArray not empty", struct.schedBlocks.length > 0);
		
//		System.out.println();
	}


	/**
	 * Sends an empty (one blank) XML representation of an ObsProposal to the method
	 * <code>xmlInOutMethod</code>. Expects to not get an exception.
	 * @throws DynWrapperException
	 */
	public void testEmptyXml() throws DynWrapperException
	{
		XmlComponentOperations serverProxy = createServerProxy();
		assertNotNull(serverProxy);
		
		// just to have a valid struct
		XmlEntityStruct entStructObsProp = serverProxy.createObsProposal();
		XmlEntityStructHolder xesh = new XmlEntityStructHolder(); 
		
		// send empty xml
		entStructObsProp.xmlString = " "; 
		serverProxy.xmlInOutMethod(entStructObsProp, xesh);
		// SchedBlock struct must be null
		assertNull(xesh.value);
	}


	public void testInvalidXml() throws DynWrapperException
	{
		XmlComponentOperations serverProxy = createServerProxy();
		assertNotNull(serverProxy);
		
		XmlEntityStruct entStructObsProp = serverProxy.createObsProposal();
		XmlEntityStructHolder xesh = new XmlEntityStructHolder(); 
		
		// send invalid xml
		entStructObsProp.xmlString = 
			"<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>" +
//			"<ns1:ObsProposal xmlns:ns1=\"AlmaTest/ObsProposal\">" +
			"	<ns1:ObsProposalEntity entityIdEncrypted=\"ljasd;ljfa;lsfd\"" +
			"		entityId=\"uid://X0000000000000000/X00000001\" entityTypeName=\"ObsProposal\"/>" +
			"	<ns1:PerformanceGoals>peak performance enduring a 24-7-365 schedule.</ns1:PerformanceGoals>" +
			"</ns1:ObsProposal>";
		
		try
		{
			serverProxy.xmlInOutMethod(entStructObsProp, xesh);
			fail("invalid XML should have caused an exception");
		}
		catch (UndeclaredThrowableException e)
		{
			Throwable cause = e.getCause();
			assertTrue(cause instanceof DynWrapperException);
			
			Throwable cause2 = cause.getCause();
			assertTrue(cause2 instanceof MarshalException);
			assertTrue(((MarshalException)cause2).getCause() instanceof org.xml.sax.SAXException);
		}		
	}




	/**
	 * Dumb little test implementation of the (inner) component interface
	 * @author hsommer Dec 18, 2002 2:34:16 PM
	 */
	private static class XmlTestComponent extends ComponentImplBase implements XmlComponentJ
	{
		private ArrayList<SchedBlock> m_schedBlockList = new ArrayList<SchedBlock>();
	
		XmlTestComponent()
		{
			populateListWithStupidIncompleteSBs();
		}
			
		/**
		 * This test "component" will never run inside a container, so the 
		 * component state is not needed -- will return COMPSTATE_OPERATIONAL for fun...
		 * 
		 * @see alma.ACS.ACSComponentOperations#componentState()
		 */
		public ComponentStates componentState()
		{
			return ComponentStates.COMPSTATE_OPERATIONAL;
		}

		/**
		 * Again, just a formality to make the compiler shut up
		 * @see alma.ACS.ACSComponentOperations#name()
		 */
		public String name()
		{
			return "not a component, just for testing";
		}

		/**
		 * @see alma.demo.XmlComponentJ#addNewSchedBlocks(SchedBlock[])
		 */
		public void addNewSchedBlocks(SchedBlock[] newSchedBlocks)
		{
			System.out.println("received call to addNewSchedBlocks in component implementation.");
			m_schedBlockList.addAll(Arrays.asList(newSchedBlocks));
		}
	
		/**
		 * @see alma.demo.XmlComponentJ#createObsProposal()
		 */
		public ObsProposal createObsProposal()
		{
			System.out.println("received call to createObsProposal in component implementation.");
			ObsProposal obsProp = new ObsProposal();
			
			ObsProposalEntityT entity = new ObsProposalEntityT();
			entity.setEntityId("uid://X0000000000000000/X00000001");
			entity.setEntityIdEncrypted("ljasd;ljfa;lsfd");
			obsProp.setObsProposalEntity(entity);
			
			obsProp.setScientificJustification("peak performance enduring a 24-7-365 schedule.");
			
			return obsProp;
		}
	
		/**
		 * @see alma.demo.XmlComponentJ#dumbMethod(java.lang.String)
		 */
		public int dumbMethod(String somevalue)
		{
			System.out.println("received call to 'dumbMethod' in component implementation. " +
				"parameter somevalue = " + somevalue);
			return 1234567;
		}
	
		/**
		 * @see alma.demo.XmlComponentJ#getAllSchedBlocks()
		 */
		public SchedBlock[] getAllSchedBlocks()
		{
			System.out.println("received call to 'getAllSchedBlocks' in component implementation.");
			return m_schedBlockList.toArray(new SchedBlock[0]);
		}
	
		/**
		 * @see alma.demo.XmlComponentJ#getBestSchedBlock()
		 */
		public SchedBlock getBestSchedBlock()
		{
			System.out.println("received call to 'getBestSchedBlock' in component implementation.");
			Random random = new Random(System.currentTimeMillis());
			int sbIndex = random.nextInt(m_schedBlockList.size());
			
			SchedBlock sb = m_schedBlockList.get(sbIndex);
			return sb;
		}
	
		/**
		 * @see alma.demo.XmlComponentJ#sayHello()
		 */
		public String sayHello()
		{
			System.out.println("received call to 'sayHello' in component implementation.");
			return "hello";
		}
	
		/**
		 * If <code>obsPropIn</code> is null, <code>schedBlockOut</code> will contain a null SchedBlock;
		 * otherwise a real SchedBlock as returned from <code>getBestSchedBlock()</code>.
		 * 
		 * @see alma.demo.XmlComponentJ#xmlInOutMethod(alma.xmljbind.test.obsproposal.ObsProposal, alma.demo.SchedBlockHolder)
		 */
		public void xmlInOutMethod(ObsProposal obsPropIn, SchedBlockHolder schedBlockOut)
		{
			System.out.println("received call to 'xmlInOutMethod' in component implementation.");

			SchedBlock sb = null;
			
			if (obsPropIn != null)
			{
				sb = getBestSchedBlock();
				assertNotNull(sb);
			}
			schedBlockOut.value = sb;
		}
	

		/**
		 * @see alma.demo.XmlComponentJ#getEntireTreeInAStruct()
		 */
		public ObsProjectTreeJ getEntireTreeInAStruct()
		{
			ObsProjectTreeJ struct = new ObsProjectTreeJ();

			struct.prop = createObsProposal();
			struct.schedBlocks = getAllSchedBlocks();
					
			return struct;
		}


		private void populateListWithStupidIncompleteSBs()
		{
			for (int sbCount = 0; sbCount < 10; sbCount++)
			{
				SchedBlock sb = new SchedBlock();
				
				SchedBlockEntityT entity = new SchedBlockEntityT();
				entity.setEntityId("uid://X0000000000000000/X0000000" + Integer.toHexString(sbCount));
				entity.setEntityIdEncrypted("not yet encrypted");
				sb.setSchedBlockEntity(entity);
				
				SchedBlockControlT sbCtrl = new SchedBlockControlT(); 
				sb.setSchedBlockControl(sbCtrl);
				sbCtrl.setRepeatCount(sbCount%3);
							
				m_schedBlockList.add(sb);			
			}
		}

		public void exceptionMethod() throws XmlComponentErrorEx
		{
			throw new XmlComponentErrorEx();
		}

		@Override
		public XmlOffshootJ getOffshoot() {
			return null;
		}

		@Override
		public void activateOffshoot() {
		}

		@Override
		public void deactivateOffshoot() {
		}

	}

}

