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
import cl.utfsm.samplingSystemUI.core.AcsInformation;

public class AcsInformationTest extends TestCase {


	protected void setUp() throws Exception {
		super.setUp();
		AcsInformation.getInstance("AcsInformationTest");
	}

	public void testSingleton() throws Exception {
		AcsInformation info1=null;
		AcsInformation info2=null;
		info1 = AcsInformation.getInstance();
		info2 = AcsInformation.getInstance();
		assertNotNull(info1);
		assertNotNull(info2);
		assertEquals(info1,info2);
	}

	public void testMultipleClients() throws Exception {
		AcsInformation info1=null;
		AcsInformation info2=null;
		info1 = AcsInformation.getInstance("AcsInformationTest1");
		info2 = AcsInformation.getInstance("AcsInformationTest2");
		assertEquals(info1,info2);
	}

	public void testComponent() throws Exception {

		AcsInformation info1=null;
		info1 = AcsInformation.getInstance("AcsInformationTest1");
		assertTrue(info1.componentExists("LAMP1"));
		assertTrue(!info1.componentExists("ODUCK1"));
	}

	public void testProperty() throws Exception {

		AcsInformation info1=null;
		info1 = AcsInformation.getInstance("AcsInformationTest1");
		assertTrue(info1.propertyExists("LAMP1","brightness"));
		assertTrue(!info1.propertyExists("LAMP1","oduck"));
	}

	public void tearDown() throws Exception {
		AcsInformation.getInstance("AcsInformationTest").shutDown();
	}
}
