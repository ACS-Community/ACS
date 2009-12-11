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

/**
 * This class is used to handle the authentication of the users that
 * make use of the ACG. There are two kind of users:
 * <li>
 * 	<ul>Administrator: The admin role allows the user to perform any action on the ACG.
 *      This includes: Sources <acronym title="Creation, Removal, Modification">CRM</acronym>,
 *      Alarm CRM, Categories CRM and Reduction Rules CRM.</ul>
 * 	<ul>Operator: The operator role should only make available to the user Alarms CRM
 *      in the ACG.</ul>
 * </li>
 * 
 * @author rtobar
 *
 */
public class UserAuthenticator {

	/**
	 * Type of users that are allowed to use ACG. They include:
	 * <ul>
	 *   <li>Administrator: He/she is allowed to perform all the operations
	 *       that can be carried away over the Alarm System configuration.</li>
	 *   <li>Operator: He/she is only allowed to perform modifications in the
	 *       Alarm configuration (add/remove/change fault families/members/codes),
	 *       and in the Reduction Rules configuration (add/remove/change reduction/node rules).</li>
	 * </ul>
	 * @author rtobar
	 */
	public enum Role {
		Administrator,
		Operator
	};

	/**
	 * Authenticates a user with a password against the authentication system (whatever it may be)
	 * @param user The username
	 * @param password The password associated with the <code>user</code>
	 * @return The role of the user in the authentication system
	 * @throws UserAuthenticatorException If the username of password are invalid
	 * @see UserAuthenticator.Role
	 */
	public Role authenticate(String user, String password) throws UserAuthenticatorException {
		
		if( user == null || password == null || user.isEmpty() || password.isEmpty() )
			throw new IllegalArgumentException("User and/or password fields are null");

		if( user.compareTo("admin") == 0 && 
			password.compareTo("admin") == 0 )
			return Role.Administrator;
		else if ( user.compareTo("operator") == 0 &&
			      password.compareTo("operator") == 0 )
			return Role.Operator;
		else
			throw new UserAuthenticatorException("Invalid username/password");
	}
}
