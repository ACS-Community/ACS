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

import cl.utfsm.acs.acg.core.UserAuthenticator.Role;
import junit.framework.TestCase;

public class UserAuthenticatorTest extends TestCase {

	public void testAuthenticate() {
		UserAuthenticator userAuth = new UserAuthenticator();

		boolean exception = false;
		try {
			userAuth.authenticate(null, null);
		} catch (IllegalArgumentException e) {
			exception = true;
		} catch (UserAuthenticatorException e) {
		}
		assertTrue(exception);

		exception = false;
		try {
			userAuth.authenticate("", "");
		} catch (IllegalArgumentException e) {
			exception = true;
		} catch (UserAuthenticatorException e) {
		}
		assertTrue(exception);

		exception = false;
		try {
			userAuth.authenticate("foo","bar");
		} catch (UserAuthenticatorException e) {
			exception = true;
		}
		assertTrue(exception);

		exception = false;
		try {
			userAuth.authenticate("admin","operator");
		} catch (UserAuthenticatorException e) {
			exception = true;
		}
		assertTrue(exception);

		exception = false;
		try {
			userAuth.authenticate("operator","admin");
		} catch (UserAuthenticatorException e) {
			exception = true;
		}
		assertTrue(exception);

		exception = false;
		Role role = null;
		try {
			role = userAuth.authenticate("admin","admin");
		} catch (UserAuthenticatorException e) {
			exception = true;
		}
		assertFalse(exception);
		assertEquals(Role.Administrator, role);

		exception = false;
		try {
			role = userAuth.authenticate("operator","operator");
		} catch (UserAuthenticatorException e) {
			exception = true;
		}
		assertFalse(exception);
		assertEquals(Role.Operator, role);
	}

}
