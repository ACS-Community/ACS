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

import java.util.logging.Logger;

import com.cosylab.CDB.DAL;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.component.client.ComponentClient;
import alma.acs.container.ContainerServices;

/**
 * Class used to handle the connection of the application with the ACS Manager.
 * When a new class is created, it ensures that the application is connected to
 * the Manager. Either way, it throws an exception.
 * 
 * @author rtobar
 */
public class AcsInformation {

	private String managerLoc = System.getProperty("ACS.manager");
	private ComponentClient _client;
	
	public AcsInformation(String name) throws Exception  {
		_client = new ComponentClient(null,managerLoc,name);
	}

	/**
	 * Retrieves a reference to the DAL use by the ACS instance that we connect to.
	 * @return A reference for the DAL in use by the system
	 * @throws AcsJContainerServicesEx If an error ocurs while trying to get the reference to the DAL
	 */
	public DAL getDAL() throws AcsJContainerServicesEx {
		if( _client == null ) {
			
		}
		return _client.getContainerServices().getCDB();
	}

	/**
	 * Gets a reference to the ContainerServices that our ACS client has.
	 * @return A reference to the current ContainerServices
	 */
	public ContainerServices getContainerServices() {
		return _client.getContainerServices();
	}

	/**
	 * Gets a reference to the Logger that our ACS client has.
	 * @return A reference to the Logger
	 */
	public Logger getLogger() {
		return _client.getContainerServices().getLogger();
	}

	/**
	 * Disconnects the application from the ACS Manager
	 */
	public void disconnect() {
		if( _client == null ) {
			
		}
		try {
			_client.tearDown();
		} catch (Exception e) {
			e.printStackTrace();
		}
		_client = null;
	}

}
