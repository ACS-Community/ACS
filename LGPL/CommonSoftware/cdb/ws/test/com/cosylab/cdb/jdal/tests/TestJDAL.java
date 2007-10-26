/***************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration)
*    and Cosylab 2002, All rights reserved
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
* Created on Apr 20, 2003
*
* To change this generated comment go to 
* Window>Preferences>Java>Code Generation>Code Template
*/
package com.cosylab.cdb.jdal.tests;

import java.util.Hashtable;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NameClassPair;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;

import org.omg.CORBA.ORB;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;

import com.cosylab.CDB.DALChangeListener;
import com.cosylab.CDB.DALChangeListenerPOA;
import com.cosylab.CDB.JDAL;
import com.cosylab.CDB.JDALHelper;

import junit.framework.TestCase;
import junit.framework.TestSuite;
import alma.acs.util.ACSPorts;

/**
 * @author dragan
 */

public class TestJDAL extends TestCase {

	private String strIOR = "corbaloc::" + ACSPorts.getIP() + ":" + ACSPorts.getCDBPort() + "/CDB";
	private ORB orb;
	private JDAL dal;
	private boolean objectChanged;

	/**
	 * Change listener used in this test.
	 */
	private class ChangeListener extends DALChangeListenerPOA {
		public void object_changed(String curl) {
			objectChanged = true;
		}
	};
	
	/**
	 * This test constructor 
	 */
	public TestJDAL(String arg0) {
		super(arg0);
	}


	/* (non-Javadoc)
	 * @see junit.framework.TestCase#setUp()
	 */
	protected void setUp() throws Exception {
		String args[] = {
		};
		orb = ORB.init(args, null);
		dal = JDALHelper.narrow(orb.string_to_object(strIOR));
	}

	/* (non-Javadoc)
	 * @see junit.framework.TestCase#tearDown()
	 */
	protected void tearDown() throws Exception {
	}

	/**
	 * Tests listing function 
	 */
	public void testListNodes() {
		String list = dal.list_nodes("MACI");
		assertTrue(list.indexOf("Containers") != -1);
		assertTrue(list.indexOf("Managers") != -1);
		list = dal.list_nodes("Hardly to have");
		assertTrue(list.length() == 0);
	}

	/**
	 * Test ChangeListener
	 * Creates a changeListener object and registers for a curl
	 * then invokes clear_cache for that function in order to get 
	 * a call from JDAL server
	 */
	public void testChangeListener() {
		try {
			// we will have change listener so we have to activate POA
			POA rootpoa = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
			rootpoa.the_POAManager().activate();

			// create our listener
			ChangeListener changeListener = new ChangeListener();
			DALChangeListener cl = changeListener._this(orb);
			// register it
			int listenerID = dal.add_change_listener(cl);
			

			// tell add that we will listen for 'test'
			dal.listen_for_changes("test", listenerID);

			// our change listener will set objectChanged
			objectChanged = false;
			dal.clear_cache("test");
			// give DAL chance (1s) to call us
			for (int i = 0; i < 10; i++) {
				Thread.sleep(100);
				if (objectChanged)
					break;
			}
			// by now the DAL should call us
			assertTrue(objectChanged);
			// remove it from DAL listeners
			dal.remove_change_listener(listenerID);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	/**
	 * Listing different contexts - simple and XML contexts
	 * We shouldn't see any difference in listing just 'directory' context
	 * or listing inside a XML data
	 */
	public void testListContext() throws Exception {
		Hashtable env = new Hashtable();
		env.put(Context.INITIAL_CONTEXT_FACTORY, "com.cosylab.cdb.jdal.JNDIContextFactory");
		env.put(Context.PROVIDER_URL, strIOR);
		Context context = null;
		context = new InitialContext(env);
		assertNotNull(context);
		// try simple listing
		String list = "";
		NamingEnumeration ne = context.list("MACI");
		while (ne.hasMore()) {
			NameClassPair pair = (NameClassPair) ne.nextElement();
			list = list + pair.getName() + " ";
		}
		// same as we did listing
		assertTrue(list.indexOf("Containers") != -1);
		assertTrue(list.indexOf("Managers") != -1);

		// try with a XML
		ne = ((Context) (((Context)context.lookup("MACI")).lookup("Managers")) ).list("Manager");
		list = "";
		while (ne.hasMore()) {
			NameClassPair pair = (NameClassPair) ne.nextElement();
			list = list + pair.getName() + " ";
			//System.out.println(pair.getName() + " " + pair.getClassName());
		}
		// this should be in Manager data
		assertTrue(list.indexOf("ServiceComponents") != -1);
	}

	/**
	 * Test lookup functions. We can just obtain an InitialContext
	 * and then use it to get any information in the CDB
	 */
	public void testLookupContext() throws Exception {
		Hashtable env = new Hashtable();
		env.put(Context.INITIAL_CONTEXT_FACTORY, "com.cosylab.cdb.jdal.JNDIContextFactory");
		env.put(Context.PROVIDER_URL, strIOR);
		Context context = null;
		context = new InitialContext(env);
		assertNotNull(context);
		// try simple lookup
		String list = "";
		Object ob = context.lookup("MACI");
		assertTrue(ob instanceof Context);
		// try with a value inside XML
		ob = ((Context) (((Context) (((Context)context.lookup("MACI")).lookup("Managers")) ).lookup("Manager")) ).lookup("ContainerPingInterval");
		assertTrue(ob instanceof String);
		double contPingInt = Double.parseDouble((String) ob);
		assertEquals(2.0, contPingInt);
	}

	/**
	 * Creates a suite for this test
	 * @return TestSuite
	 */
	public static TestSuite suite() {
		TestSuite retVal = new TestSuite("All jDAL Tests");
		retVal.addTest(new TestSuite(TestJDAL.class));
		return retVal;
	}
	
	/**
	 * If we are called from java directly
	 */
	public static void main(String args[]) {
		junit.textui.TestRunner.run(suite());
		System.exit(0);
	}
}
