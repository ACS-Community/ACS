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
package acs.benchmark.util;

import java.util.logging.Logger;

import si.ijs.maci.AdministratorOperations;
import si.ijs.maci.AuthenticationData;
import si.ijs.maci.ClientInfo;
import si.ijs.maci.ClientType;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ContainerInfo;
import si.ijs.maci.ImplLangType;

import alma.acs.util.UTCUtility;

/**
 * Dummy manager client for custom manager logins as administrator.
 * <p>
 * TODO: Unify with similar class in ACS/LGPL/CommonSoftware/containerTests/contLogTest 
 *       (which builds before this, but does not get installed).
 */
public class ManagerAdminClient implements AdministratorOperations
{
	private final String name;
	private final Logger logger;

	public ManagerAdminClient(String name, Logger logger) {
		this.name = name;
		this.logger = logger;
	}
	
	@Override
	public void client_logged_in(ClientInfo info, long timestamp, long execution_id) {
		// TODO Auto-generated method stub

	}

	@Override
	public void client_logged_out(int h, long timestamp) {
		// TODO Auto-generated method stub

	}

	@Override
	public void component_activated(ComponentInfo info, long timestamp, long execution_id) {
		// TODO Auto-generated method stub

	}

	@Override
	public void component_deactivated(int h, long timestamp) {
		// TODO Auto-generated method stub

	}

	@Override
	public void components_released(int[] clients, int[] components, long timestamp) {
		// TODO Auto-generated method stub

	}

	@Override
	public void components_requested(int[] clients, int[] components, long timestamp) {
		// TODO Auto-generated method stub

	}

	@Override
	public void container_logged_in(ContainerInfo info, long timestamp, long execution_id) {
		// TODO Auto-generated method stub

	}

	@Override
	public void container_logged_out(int h, long timestamp) {
		// TODO Auto-generated method stub

	}

	@Override
	public AuthenticationData authenticate(long execution_id, String question) {
		logger.info("authenticate called: id=" + execution_id + "; question=" + question);

		return new AuthenticationData(
				 "S",
				 ClientType.ADMINISTRATOR_TYPE,
				 ImplLangType.JAVA,
				 false, 
				 UTCUtility.utcJavaToOmg(System.currentTimeMillis()),
				 execution_id);
	}

	@Override
	public void components_available(ComponentInfo[] components) {
		// TODO Auto-generated method stub

	}

	@Override
	public void components_unavailable(String[] component_names) {
		// TODO Auto-generated method stub

	}

	@Override
	public void disconnect() {
		// TODO Auto-generated method stub

	}

	@Override
	public void taggedmessage(short type, short messageID, String message) {
		// @todo Auto-generated method stub
		
	}
	
	public void message(short type, String message) {
		logger.info(message);
	}

	public String name() {
		return name;
	}

	public boolean ping() {
		return true;
	}


}
