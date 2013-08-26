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
package alma.acs.component.client;


import java.util.logging.Logger;

import com.cosylab.CDB.DAL;
import com.cosylab.CDB.DALChangeListener;
import com.cosylab.CDB.DALChangeListenerHelper;
import com.cosylab.CDB.DALChangeListenerPOA;

import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.CBvoid;
import alma.ACS.CBvoidPOA;
import alma.ACSErr.Completion;
import alma.ACS.OffShoot;
import alma.ACS.RWdouble;
import alma.acsexmplLamp.Lamp;
import alma.acsexmplLamp.LampHelper;

/**
 * Requires C++ component "LAMP1" of type <code>alma.acsexmplLamp.Lamp</code> to be running.
 * 
 * @author hsommer May 30, 2003 4:01:48 PM
 */
public class ComponentClientTestCaseTest extends ComponentClientTestCase
{
	// C++ component that takes a callback object
	private static final String m_lampCurl = "LAMP1";


	/**
	 * @param name
	 * @throws java.lang.Exception
	 */
	public ComponentClientTestCaseTest() throws Exception
	{
		super(ComponentClientTestCaseTest.class.getName());
	}

	/**
	 * @see TestCase#setUp()
	 */
	protected void setUp() throws Exception
	{
		super.setUp();
	}

	/**
	 * @see TestCase#tearDown()
	 */
	protected void tearDown() throws Exception
	{
		super.tearDown();
	}
	
	public void testGetDAL() throws Exception
	{
		DAL dal = getContainerServices().getCDB();
		assertNotNull(dal);
		
		String managerDAOString = dal.get_DAO("MACI/Managers/Manager");
		assertNotNull(managerDAOString);
		
		m_logger.info("received manager DAO string from the CDB:\n" + managerDAOString);
	}
	
	public void testActivateOffShoot() throws Exception
	{
		// activate callback object
		MyTestCBvoid cb = new MyTestCBvoid(m_logger);
		m_logger.info("MyTestCBvoid instantiated...");
		CBvoid cbvoid = alma.ACS.CBvoidHelper.narrow(
			getContainerServices().activateOffShoot(cb) );
		m_logger.info("MyTestCBvoid activated.");


		// activate dal listener object
		m_logger.info("activating dal listener offshoot...");
		DALChangeListenerPOA dalListenerSrv = new MyTestDALChangeListener();
		OffShoot offshoot2 = getContainerServices().activateOffShoot(dalListenerSrv);
		DALChangeListener dalListenerRef = DALChangeListenerHelper.narrow(offshoot2);
		m_logger.info("done activating dal listener offshoot.");	
		
		
		// check if dal listener works...
		m_logger.info("calling dal listener...");
		dalListenerRef.object_changed("happyCurl");
		m_logger.info("done calling dal listener.");

		
		// check if callback works...
		RWdouble brightness = null;
		Lamp lamp =
			LampHelper.narrow(
				getContainerServices().getComponent(m_lampCurl));
		
		assertNotNull("reference to Lamp component not null", lamp);
		
		brightness = lamp.brightness();
		assertNotNull("brightness property of lamp not null", brightness);

		CBDescIn desc = new CBDescIn();
		m_logger.info("m_desc instantiated...");

		m_logger.info("before setting callback...");
		brightness.set_async(7.345, cbvoid, desc);
		m_logger.finer("callback set...");
		
//todo: do something and get feedback from the callback object		
//		m_logger.finer("changing value (sync)...");
//		brightness.set_sync(8.456);
	}
	
	

	public static void main(String[] args)
	{
		junit.textui.TestRunner.run(ComponentClientTestCaseTest.class);
	}


	private class MyTestDALChangeListener extends DALChangeListenerPOA
	{	
		public void object_changed(String curl) 
		{
			m_logger.fine("called object_changed with " + curl);
		}
	}
	

}



class MyTestCBvoid extends CBvoidPOA 
{
	private Logger m_logger;

	public MyTestCBvoid(Logger logger)
	{
		m_logger = logger;		
	}

	/**
	 * @see alma.ACS.CBvoidOperations#working(alma.ACSErr.Completion, alma.ACS.CBDescOut)
	 */
	public void working(Completion completion, CBDescOut desc)
	{
		m_logger.fine("called working()...");
	}
	/**
	 * @see alma.ACS.CBvoidOperations#done(alma.ACSErr.Completion, alma.ACS.CBDescOut)
	 */
	public void done(Completion completion, CBDescOut desc)
	{
		m_logger.fine("called done()...");
	}
	/**
	 * @see alma.ACS.CallbackOperations#negotiate(long, alma.ACS.CBDescOut)
	 */
	public boolean negotiate(long myLong, CBDescOut desc)
	{
		m_logger.fine("called negotiate()...");
		return true;
	}
}


