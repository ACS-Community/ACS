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
package alma.demo.XmlComponentImpl; 

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Random;
import java.util.logging.Level;

import alma.JContExmplErrTypeTest.XmlComponentErrorEx;
import alma.JContExmplErrTypeTest.wrappers.AcsJXmlComponentErrorEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.demo.ObsProjectTreeJ;
import alma.demo.SchedBlockHolder;
import alma.demo.XmlComponentJ;
import alma.demo.XmlOffshootJ;
import alma.demo.XmlOffShootImpl.XmlOffShootImpl;
import alma.xmljbind.test.obsproposal.ObsProposal;
import alma.xmljbind.test.obsproposal.ObsProposalEntityT;
import alma.xmljbind.test.schedblock.SchedBlock;
import alma.xmljbind.test.schedblock.SchedBlockControlT;
import alma.xmljbind.test.schedblock.SchedBlockEntityT;


/**
 * A test component that implements the <code>XmlComponentImpl</code> interface from <code>demo.idl</code>.
 * 
 * The implemented functional interface, <code>XmlComponentJ</code>, uses Castor binding classes, 
 * where the IDL (and <code>XmlComponentOperations</code>) use serialized XML strings inside the 
 * <code>XmlEntityStruct</code>.
 * <p>
 * In addition to methods for testing the transparent-xml feature, there is the method <code>sayHello</code>,
 * which is meant to internally contact another component (<code>HelloDemo</code>). 
 * 
 * @author hsommer Nov 27, 2002 8:22:17 PM
 */
public class XmlComponentImpl extends ComponentImplBase implements XmlComponentJ
{

	private ArrayList<SchedBlock> m_schedBlockList;	
	private XmlOffshootJ m_offshoot;

	/////////////////////////////////////////////////////////////
	// Implementation of ComponentLifecycle 
	// (only the parts we don't take from the default impl in ComponentImplBase)
	/////////////////////////////////////////////////////////////

	/**
	 * @see alma.acs.component.ComponentLifecycle#initialize(ContainerServices)
	 */
	public void initialize(ContainerServices contServices) throws ComponentLifecycleException
	{
		super.initialize(contServices);
		
		m_schedBlockList = new ArrayList<SchedBlock>();
		
		try
		{
			// create a few (incomplete) SchedBlocks
			for (int sbCount = 0; sbCount < 10; sbCount++)
			{
				SchedBlock sb = new SchedBlock();
				
				SchedBlockEntityT entity = new SchedBlockEntityT();
				m_containerServices.assignUniqueEntityId(entity);
				sb.setSchedBlockEntity(entity);
				
				SchedBlockControlT sbCtrl = new SchedBlockControlT(); 
				sb.setSchedBlockControl(sbCtrl);
				sbCtrl.setRepeatCount(sbCount%3);
//				sbCtrl.setEntityPartId("X00000008"); // set this on an ObsUnitSet instead...
							
				m_schedBlockList.add(sb);			
			}
		}
		catch (AcsJContainerServicesEx e)
		{
			m_logger.log(Level.WARNING, "failed to initialize list of SchedBlocks. ", e);
			m_schedBlockList.clear();
//			throw new ComponentLifecycleException(e); // not required
		}

	}


	/////////////////////////////////////////////////////////////
	// Implementation of XmlComponentJ
	/////////////////////////////////////////////////////////////

	/**
	 * from IDL: <code>long dumbMethod(in string somevalue);</code>
	 *  
	 * @see alma.demo.XmlComponentJ#dumbMethod(java.lang.String)
	 */
	public int dumbMethod(String somevalue)
	{
		return 1234567;
	}

	/**
	 * from IDL: <code>ObsProposal createObsProposal();</code>
	 * 
	 * @see alma.demo.XmlComponentJ#createObsProposal()
	 */
	public ObsProposal createObsProposal()
	{
		ObsProposal obsProp = new ObsProposal();
		
		try
		{
			ObsProposalEntityT entity = new ObsProposalEntityT();
			m_containerServices.assignUniqueEntityId(entity);
			obsProp.setObsProposalEntity(entity);
			
			obsProp.setScientificJustification("peak performance enduring a 24-7-365 schedule.");
		}
		catch (AcsJContainerServicesEx e)
		{
			m_logger.log(Level.SEVERE, "failed to create ObsProposal. ", e);
		}
		
		return obsProp;
	}

	/**
	 * @see alma.demo.XmlComponentJ#getBestSchedBlock()
	 */
	public SchedBlock getBestSchedBlock()
	{
		SchedBlock sb = null;
		if (m_schedBlockList.size() > 0) {
			Random random = new Random(System.currentTimeMillis());
			int sbIndex = random.nextInt(m_schedBlockList.size());
			
			sb = m_schedBlockList.get(sbIndex);
		}
		return sb;
	}

	/**
	 * from IDL: <code>SchedBlockSeq getAllSchedBlocks();</code>
	 * 
	 * @see alma.demo.XmlComponentJ#getAllSchedBlocks()
	 */
	public SchedBlock[] getAllSchedBlocks()
	{
		return m_schedBlockList.toArray(new SchedBlock[0]);
	}


	/**
	 * @see alma.demo.XmlComponentJ#addNewSchedBlocks(alma.xmljbind.test.schedblock.SchedBlock[])
	 */
	public void addNewSchedBlocks(SchedBlock[] newSchedBlocks)
	{
		m_schedBlockList.addAll(Arrays.asList(newSchedBlocks));
		m_logger.info("successfully added " + newSchedBlocks.length + " SchedBlock instance(s)");
	}


	/**
	 * from IDL: <code>void xmlInOutMethod(in ObsProposal opsPropIn, out SchedBlock schedBlockOut);</code>
	 * 
	 * @see alma.demo.XmlComponentJ#xmlInOutMethod(alma.xmljbind.test.obsproposal.ObsProposal, 
	 * alma.demo.SchedBlockHolder)
	 */
	public void xmlInOutMethod(ObsProposal opsPropIn, SchedBlockHolder schedBlockOut)
	{
		schedBlockOut.value = getBestSchedBlock();
	}


	/**
	 * Returns the <code>ObsProposal</code> from {@link #createObsProposal} 
	 * and the <code>SchedBlock</code>s from {@link #getAllSchedBlocks} together in a struct.
	 * 
	 * Illustrates and tests the usage of xml entity classes inside IDL structs,
	 * or rather their usage as Java class members from an implementation point of view.
	 * 
	 * @see alma.demo.XmlComponentJ#getEntireTreeInAStruct()
	 */
	public ObsProjectTreeJ getEntireTreeInAStruct()
	{
		ObsProjectTreeJ struct = new ObsProjectTreeJ();
		
		struct.prop = createObsProposal();
		struct.schedBlocks = getAllSchedBlocks();
		
		return struct;
	}


	/**
	 * @see alma.demo.XmlComponentJ#sayHello()
	 */
	public String sayHello()
	{
		String ret = "";
		
		try
		{
			alma.demo.HelloDemo helloComp = 
				alma.demo.HelloDemoHelper.narrow(m_containerServices.getComponent("HELLODEMO1"));
			ret = helloComp.sayHello();
			m_containerServices.releaseComponent("HELLODEMO1");
		}
		catch (Exception e)
		{
			m_logger.severe("failed to obtain the hello string from the remote component!");
		}
		
		
		return ret;
	}


	/**
	 * At the CORBA interface level, we must use the CORBA-exceptions.
	 * @throws XmlComponentErrorEx with an {@link alma.ACSErr.ErrorTrace} inside;
	 * 			the <code>ErrorTrace</code> will contain a NullPointerException.
	 */
	public void exceptionMethod() throws XmlComponentErrorEx
	{
		try {
			// an internal method that works with native Java exceptions
			internalExceptionMethod();
		} catch (AcsJXmlComponentErrorEx e) {
            m_logger.info("deliberately created and caught the following exception (trace):");
            e.log(m_logger);
			// convert to CORBA-compatible exception
			throw e.toXmlComponentErrorEx();
		}
	}

	/**
	 * Inside the Java implementation, we can throw around 
	 * native Java exceptions generated from the error specifications. 
	 * @throws AcsJXmlComponentErrorEx because that one is easier to work with than the corresponding
	 * 			{@link XmlComponentErrorEx}, and this method is not part of the (CORBA) component interface.
	 */
	public void internalExceptionMethod() throws AcsJXmlComponentErrorEx {
		try {
			// do something that can throw an exception
			throw new NullPointerException("dirty NPE for testing...");
		} catch (NullPointerException npe) {
			// this shows how native Java exceptions can be daisy-chained
			AcsJXmlComponentErrorEx ex = new AcsJXmlComponentErrorEx(npe);
			ex.setProperty("myProperty","use me for container tests");
			throw ex;
		}
	}

	public XmlOffshootJ getOffshoot() {
		if( m_offshoot == null ) {
			m_offshoot = new XmlOffShootImpl(m_containerServices);
			try {
				m_containerServices.activateOffShoot(m_offshoot, XmlOffshootJ.class);
			} catch (AcsJContainerServicesEx e) {
				e.printStackTrace();
				return null;
			}
		}
		return m_offshoot;
	}

	public void deactivateOffshoot() {
		if( m_offshoot != null )
			try {
				m_containerServices.deactivateOffShoot(m_offshoot);
				m_offshoot = null;
			} catch (AcsJContainerServicesEx e) {
				e.printStackTrace();
			}
	}

	public void activateOffshoot() {
		if( m_offshoot == null )
			try {
				m_containerServices.activateOffShoot(m_offshoot, XmlOffshootJ.class);
			} catch (AcsJContainerServicesEx e) {
				e.printStackTrace();
			}
	}
}
