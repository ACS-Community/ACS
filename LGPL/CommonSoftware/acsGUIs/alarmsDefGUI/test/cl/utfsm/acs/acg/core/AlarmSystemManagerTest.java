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

/**
 * @author rtobar
 *
 */
public class AlarmSystemManagerTest extends TestCase {

	public void testGetInstance() throws Exception {
		UserAuthenticator auth = new UserAuthenticator();
		UserAuthenticator.Role role = auth.authenticate("admin", "admin");

		boolean exception = false;
		try {
			AlarmSystemManager.getInstance();
		} catch(IllegalStateException e) {
			exception = true;
		}
		assertTrue(exception);

		exception = false;
		try {
			AlarmSystemManager.getInstance(null);
		} catch(IllegalArgumentException e) {
			exception = true;
		}

		assertTrue(exception);
		AlarmSystemManager asm1;
		AlarmSystemManager asm2;
		AlarmSystemManager asm3;
		
		asm1 = AlarmSystemManager.getInstance(role);
		asm2 = AlarmSystemManager.getInstance(role);
		asm3 = AlarmSystemManager.getInstance();
		
		assertEquals(asm1, asm2);
		assertEquals(asm1, asm3);
	}

	public void testConnectoToManagerAndDAL() throws Exception {

		AlarmSystemManager asm = AlarmSystemManager.getInstance();
		
		boolean exception = false;
		try {
			asm.connectToDAL();
		} catch (Exception e) {
			exception = true;
		}
		assertTrue(exception);

		exception = false;
		try {
			asm.connectToManager();
		} catch(Exception e) {
			exception = true;
		}
		assertFalse(exception);

		exception = false;
		try {
			asm.connectToDAL();
		} catch (Exception e) {
			exception = true;
		}
		assertFalse(exception);

	}

	public void testLoadCDB() throws Exception {
		AlarmSystemManager asm = AlarmSystemManager.getInstance();

		asm.disconnectFromManager();
		boolean exception = false;
		try {
			asm.loadFromCDB();
		} catch (IllegalStateException e) {
			exception = true;
		}
		assertTrue(exception);

		AlarmSystemManager.destroy();
		UserAuthenticator auth = new UserAuthenticator();
		UserAuthenticator.Role role = auth.authenticate("admin", "admin");
		asm = AlarmSystemManager.getInstance(role);
		exception = false;
		try {
			asm.connectToManager();
			asm.connectToDAL();
		} catch (Exception e1) {
			exception = true;
		}
		assertFalse(exception);

		exception = false;
		try {
			asm.loadFromCDB();
		} catch (IllegalStateException e) {
			e.printStackTrace();
			exception = true;
		}
		assertFalse(exception);
	}

	public void testReconnect() throws Exception {

		UserAuthenticator auth = new UserAuthenticator();
		UserAuthenticator.Role role = auth.authenticate("admin", "admin");

		AlarmSystemManager.getInstance(role);
		AlarmSystemManager.destroy();
		
		AlarmSystemManager.getInstance(role).connectToManager();
		assertTrue(true);

	}
}
