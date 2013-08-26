/*
 *    ALMA - Atacama Large Millimeter Array
 *    (c) Universidad Tecnica Federico Santa Maria, 2009
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
package cl.utfsm.acs.acg.core;

import junit.framework.TestCase;

public class AcsInformationTest extends TestCase {

	private final String TEST_CLIENT = "Test Client";

	public void testCreation() throws Exception {

		AcsInformation a1 = null;
		a1 = new AcsInformation(TEST_CLIENT);
		assertNotNull(a1);

	}

	public void testContainerServices() throws Exception {
		
		AcsInformation a1 = new AcsInformation(TEST_CLIENT);
		assertNotNull(a1);
		assertNotNull(a1.getContainerServices());
	
	}

	public void testDisconnect() throws Exception {
		AcsInformation a = new AcsInformation(TEST_CLIENT);

		assertNotNull(a);
		boolean exception = false;
		try {
			a.disconnect();
		} catch (Exception e) {
			exception = true;
		}
		assertFalse(exception);

	}

	public void testResetConnection() throws Exception {
		
		AcsInformation a = new AcsInformation(TEST_CLIENT);
		assertNotNull(a);

		a.disconnect();

		a = new AcsInformation(TEST_CLIENT);
		assertNotNull(a);
		assertNotNull(a.getContainerServices());
		assertNotNull(a.getDAL());
		assertNotNull(a.getLogger());
	}

}
