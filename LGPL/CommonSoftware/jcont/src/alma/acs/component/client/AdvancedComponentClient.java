/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2005
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

package alma.acs.component.client;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ThreadFactory;
import java.util.logging.Logger;

import org.omg.CORBA.ORB;

import si.ijs.maci.Client;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.container.AcsManagerProxy;
import alma.acs.container.CleaningDaemonThreadFactory;
import alma.acs.container.ContainerServices;
import alma.acs.container.ContainerServicesImpl;
import alma.acs.container.corba.AcsCorba;

/**
 * Special version of <code>ComponentClient</code>, which gives more power to specialized applications such as the OMC Gui ("Exec").
 * @author hsommer
 */
public class AdvancedComponentClient extends ComponentClient {

	private Map<ContainerServicesImpl, AcsManagerProxy> additionalContainerServices = new HashMap<ContainerServicesImpl, AcsManagerProxy>();
	
	public AdvancedComponentClient(Logger logger, String managerLoc, String clientName) throws Exception {
		super(logger, managerLoc, clientName);
	}

	/**
	 * This ctor's implementation comes already from the base class for practical reasons, but there it is not public.
	 * Only here it becomes available to client applications.
	 */
	public AdvancedComponentClient(Logger logger, String managerLoc, String clientName, AcsCorba externalAcsCorba) throws Exception {
		super(logger, managerLoc, clientName, externalAcsCorba);
	}	
	
	
	/**
	 * Factory method for additional container service instances.
	 * This method should only be used by specialized clients such as the OMC GUI
	 * which needs independent ContainerServices instances for the plug-ins it runs.
	 * <p>
	 * Make sure to call {@link #destroyContainerServices(ContainerServices)} when done with the new CS.
	 *  
	 * @param clientName  name for {@link ContainerServices#getName()}
	 * @param csLogger  logger to be used internally by the new ContainerServices instance 
	 *                  (which is different from the Logger returned in {@link ContainerServices#getLogger()}). 
	 */
    public ContainerServices createContainerServices(String clientName, Logger csLogger) throws AcsJContainerServicesEx {

    	try {
	// if not passed as parameter:   	Logger csLogger = containerServicesImpl.m_logger;
	        ThreadFactory threadFactory = new CleaningDaemonThreadFactory(clientName, csLogger);
	        
	        // separately log in to the manager to get a new client handle.
	        // TODO: if this does not work, then we need a way to get a new handle from manager without logging in separately. 
	        //       Note that when activating components, the container receives the new handle directly from the manager.
	    	AcsManagerProxy acsManagerProxy = m_acsManagerProxy.createInstance();
			ManagerClient clImpl = new ManagerClient(clientName, csLogger);
			Client managerClient = clImpl._this(acsCorba.getORB());
	        acsManagerProxy.loginToManager(managerClient, false);
	        int clientHandle = acsManagerProxy.getManagerHandle();
	        
	        ContainerServicesImpl cs = new ContainerServicesImpl(acsManagerProxy, acsCorba.getRootPOA(), acsCorba, csLogger, clientHandle, clientName, null, threadFactory);
	    	additionalContainerServices.put(cs, acsManagerProxy);
	    	return cs;
    	} catch (AcsJContainerServicesEx ex) {
    		throw ex;
    	} catch (Throwable thr) {
    		throw new AcsJContainerServicesEx(thr);
    	}
    }
	
    
    /**
     * "un-factory" method which inverts {@link #createContainerServices(String, Logger)}.
     * @param cs ContainerServices instance created by {@link #createContainerServices(String, Logger)}.
     */
    public void destroyContainerServices(ContainerServices cs) throws AcsJContainerServicesEx {
    	if (!additionalContainerServices.containsKey(cs)) {
    		throw new AcsJContainerServicesEx("The given ContainerServices object was not created by this AdvancedComponentClient!");
    	}
    	try {
			ContainerServicesImpl csImpl = (ContainerServicesImpl) cs;
			AcsManagerProxy acsManagerProxy = additionalContainerServices.get(cs);
			acsManagerProxy.shutdownNotify();
			csImpl.releaseAllComponents();
			((CleaningDaemonThreadFactory) csImpl.getThreadFactory()).cleanUp();							
			acsManagerProxy.logoutFromManager();
			additionalContainerServices.remove(cs);
		} catch (Throwable thr) {
			throw new AcsJContainerServicesEx("Failed to destroy additional container services instance", thr);
		}
    }
    
    
	/**
	 * Use only when direct access to the ORB is absolutely necessary. 
	 * We try to not expose the ORB to applications. 
	 *  
	 * @return ORB
	 */
	public ORB getORB() {
		return acsCorba.getORB();
	}
	
	public AcsCorba getAcsCorba() {
		return acsCorba;
	}

}