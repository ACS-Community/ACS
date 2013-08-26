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
/**
 *      @author Julio Araya (jaray[at]alumnos.inf.utfsm.cl) &
 *      Nicolas Troncoso (ntroncos[at]alumnos.inf.utfsm.cl)
 **/

package cl.utfsm.samplingSystemUI.core;
import alma.JavaContainerError.wrappers.AcsJContainerEx;
import alma.acs.component.client.ComponentClient;
import alma.acs.container.ContainerServices;
import java.util.logging.Logger;

import org.omg.CORBA.ORB;
import org.omg.CORBA.Object;
import org.omg.CORBA.Repository;
import org.omg.CORBA.RepositoryHelper;


/**
 * Singleton class that provides system wide information. This class creates
 * the logger, and is reposible for the proper shutdown.
 *
 */ 
public class AcsInformation {
	private static AcsInformation _instance;
	String managerLoc = System.getProperty("ACS.manager");
	String IRloc = System.getProperty("ACS.repository");
	ComponentClient client = null;
	ComponentsManager cManager=null;
	//ThreadCommunicator tCommunicator;
	Logger m_logger=null;
	Repository IR = null;

	


	/**
	* Returns an instance of AcsInformation
	*
	* @param clientName The name for the ClientComponent to create
	*/
	public static synchronized AcsInformation getInstance(String clientName) throws AcsInformationException, AcsJContainerEx { 
		if (_instance==null) { 
			_instance = new AcsInformation(clientName);
		} 
		return _instance; 
	}

	/**
	* Returns an instance of AcsInformation
	*/
	public static synchronized AcsInformation getInstance() { 
		if (_instance==null) { 
			/* it should never enter into this if. It is here meerly as safe guard.*/
			throw new IllegalStateException("No instance available, and connot create one with out a client name");
		} 
		return _instance; 
	}

	/**
	* Creates an instance for the ComponentClient
	* @param clientName
	*/
	private AcsInformation(String clientName) throws AcsInformationException, AcsJContainerEx {
		
		try {
			client = new ComponentClient(null, managerLoc, clientName);
		} catch (Exception e) {
			if(e instanceof AcsJContainerEx) {
				throw (AcsJContainerEx)e;
			}
			throw new AcsInformationException(clientName+" can not connect to "+managerLoc+
				"\nCheck if the ACS is up and running, or that you have conectivity to the manager.",e);
		}
		
		try{
			m_logger = client.getContainerServices().getLogger();
			m_logger.info("Sampling System UI startup");
			cManager = new ComponentsManager(client.getContainerServices());
			
			ORB orb = client.getContainerServices().getAdvancedContainerServices().getORB();
			Object tmp = orb.string_to_object(IRloc);
			IR = RepositoryHelper.narrow(tmp);

		//	tCommunicator = ThreadCommunicator.getInstance();
		}
		catch(Exception e){
			throw new AcsInformationException(clientName+" can not connect to "+managerLoc+
				"\nCheck if the ACS is up and running, or that you have conectivity to the manager.",e);
		}
	}

	private AcsInformation(){
	}

	public Repository getIrReference(){
		return IR;
	}
	public ComponentClient getClient(){
		return client;
	}
	public ContainerServices getContainerServices(){
		return client.getContainerServices();
	}
	public void shutDown() throws java.lang.Exception{
		m_logger.info("Sampling System UI tear down requested");
		client.tearDown();
		_instance = null;
	}

	public boolean componentExists(String componentName){
		return cManager.componentExists(componentName);
	}

	public boolean propertyExists(String componentName, String propertyName){
		return cManager.propertyExists(componentName,propertyName);
	}

	public ComponentsManager getCManager() {
		return cManager;
	}
}
