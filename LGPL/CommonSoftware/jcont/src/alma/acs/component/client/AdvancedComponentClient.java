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

import si.ijs.maci.Client;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.container.AcsManagerProxy;
import alma.acs.container.CleaningDaemonThreadFactory;
import alma.acs.container.ContainerServices;
import alma.acs.container.ContainerServicesImpl;
import alma.acs.container.corba.AcsCorba;
import alma.acs.logging.AcsLogger;

/**
 * Special version of <code>ComponentClient</code>, which gives more power to specialized applications such as the OMC Gui ("Exec").
 * Using some of these methods can be dangerous to the stability of the entire system.
 * <p>
 * If you find that your application needs to use <code>AdvancedComponentClient</code> instead of <code>ComponentClient</code>,
 * better check with the ACS team if there really is no other way.
 * 
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
	 * Factory method for additional container service instances. This method should only be used by specialized clients
	 * such as the OMC GUI which needs independent ContainerServices instances for the plug-ins it runs.
	 * <p>
	 * Make sure to call {@link #destroyContainerServices(ContainerServices)} when done with the new CS.
	 * 
	 * @param clientName
	 *            name for {@link ContainerServices#getName()}
	 * @param csLogger
	 *            logger to be used internally by the new ContainerServices instance (which is different from the Logger
	 *            returned in {@link ContainerServices#getLogger()}). 
	 *            Since ACS 8.0 it is recommended to supply an {@link AcsLogger} instead of a plain JDK Logger because a 
	 *            plain Logger will have to be wrapped inside this method.
	 */
	public ContainerServices createContainerServices(String clientName, Logger csLogger) throws AcsJContainerServicesEx {

		if (clientName == null) {
			throw new IllegalArgumentException("clientName must not be null");
		}
		if (csLogger == null) {
			throw new IllegalArgumentException("csLogger must not be null");
		}
		try {
			// wrap csLogger if necessary
			AcsLogger acsLogger = AcsLogger.fromJdkLogger(csLogger, null);
			
			ThreadFactory threadFactory = new CleaningDaemonThreadFactory(clientName, csLogger);

			// separately log in to the manager to get a new client handle.
			// TODO: if this does not work, then we need a way to get a new handle from manager without logging in separately.
			// Note that when activating components, the container receives the new handle directly from the manager.
			AcsManagerProxy acsManagerProxy = m_acsManagerProxy.createInstance();
			ManagerClient clImpl = new ManagerClient(clientName, acsLogger);
			Client managerClient = clImpl._this(acsCorba.getORB());
			acsManagerProxy.loginToManager(managerClient, true);
			int clientHandle = acsManagerProxy.getManagerHandle();

			ContainerServicesImpl cs = new ContainerServicesImpl(acsManagerProxy, acsCorba.getRootPOA(), acsCorba,
					acsLogger, clientHandle, clientName, null, threadFactory);
			additionalContainerServices.put(cs, acsManagerProxy);
			return cs;
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
    		AcsJContainerServicesEx ex = new AcsJContainerServicesEx();
    		ex.setContextInfo("The given ContainerServices object was not created by this AdvancedComponentClient!");
    		throw ex;
    	}
    	try {
			ContainerServicesImpl csImpl = (ContainerServicesImpl) cs;
			AcsManagerProxy acsManagerProxy = additionalContainerServices.get(cs);
			acsManagerProxy.shutdownNotify();
			csImpl.releaseAllComponents();
			((CleaningDaemonThreadFactory) csImpl.getThreadFactory()).cleanUp();
			acsManagerProxy.logoutFromManager();
			additionalContainerServices.remove(cs);
			csImpl.cleanUp();
		} catch (Throwable thr) {
    		AcsJContainerServicesEx ex = new AcsJContainerServicesEx();
    		ex.setContextInfo("Failed to destroy additional container services instance");
    		throw ex;
		}
    }
        
	/**
	 * Gives direct access to the CORBA ORB, POAs etc, encapsulated by the AcsCorba object.
	 * Caution: interfering with the ComponentClient's usage of CORBA can have nasty side effects! 
	 */
	public AcsCorba getAcsCorba() {
		return acsCorba;
	}

	/**
	 * Gives direct access to the ACS manager, encapsulated by the AcsManagerProxy object.
	 * Caution: interfering with ACS's usage of the manager can have nasty side effects in the entire system! 
	 */
	public AcsManagerProxy getAcsManagerProxy() {
		return m_acsManagerProxy;
	}

}