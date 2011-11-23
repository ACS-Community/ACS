/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package cl.utfsm.samplingSystemUITestCase;

import junit.framework.TestCase;
import cl.utfsm.samplingSystemUI.core.ComponentsManager;
import alma.acs.component.client.ComponentClient;

public class ComponentsManagerTest extends TestCase {
	
	ComponentClient client;
	ComponentsManager cManager=null;
	String managerLoc = System.getProperty("ACS.manager");

	protected void setUp() throws Exception {
		client = new ComponentClient(null, managerLoc, "ComponetsManagerTest");
		cManager = new ComponentsManager(client.getContainerServices());
	}

	protected void tearDown() throws Exception {
		client.tearDown();
	}

	public void testSetUp() throws Exception {
		assertNotNull(cManager);
	}

	public void testComponent() throws Exception {
		assertTrue(cManager.componentExists("LAMP1"));
		assertTrue(!cManager.componentExists("ODUCK1"));
	}

	public void testProperty() throws Exception {
		assertTrue(cManager.propertyExists("LAMP1","brightness"));
		assertTrue(!cManager.propertyExists("LAMP1","oduck"));
	}
	
}
