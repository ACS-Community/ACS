/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
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
package alma.acs.container;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.omg.CORBA.ORB;

import si.ijs.maci.Administrator;
import si.ijs.maci.AdministratorOperations;
import si.ijs.maci.AdministratorPOATie;
import si.ijs.maci.SynchronousAdministratorOperations;
import si.ijs.maci.SynchronousAdministratorPOATie;

import alma.JavaContainerError.wrappers.AcsJContainerEx;


/**
 * This class implements the more exotic methods from the container services interface,
 * which have been refactored out of <code>ContainerServices</code> into a separate interface.
 * 
 * @see alma.acs.container.ContainerServicesImpl  
 * @author hsommer 
 */
public class AdvancedContainerServicesImpl implements AdvancedContainerServices
{
    // the container services which this class logically belongs to 
	protected ContainerServicesImpl containerServicesImpl;

    // logger used by this class
	protected Logger logger;
    
	protected final Map<AdministratorOperations, AcsManagerProxy> adminClientsToManagerProxy =
		new HashMap<AdministratorOperations, AcsManagerProxy>();
	
	AdvancedContainerServicesImpl(ContainerServicesImpl containerServicesImpl, Logger logger) {
        this.containerServicesImpl = containerServicesImpl;
        this.logger = logger;
    }

    
	/* (non-Javadoc)
	 * @see alma.acs.container.ContainerServices#corbaObjectToString(org.omg.CORBA.Object)
	 */
	public String corbaObjectToString(org.omg.CORBA.Object objRef)
	{
		ORB orb = getORB(); 
		String str = orb.object_to_string(objRef);
		logger.finer("converted corba object reference of type " + objRef.getClass().getName() +
						" to the string " + str);
		return str;
	}

	/* (non-Javadoc)
	 * @see alma.acs.container.ContainerServices#corbaObjectFromString(java.lang.String)
	 */
	public org.omg.CORBA.Object corbaObjectFromString(String strObjRef)
	{
		ORB orb = getORB();
		org.omg.CORBA.Object objRef = orb.string_to_object(strObjRef);
		logger.finer("converted corba object reference string " + strObjRef + 
						" back to a corba object reference.");
		return objRef;
	}


	/**
	 * @throws IllegalStateException if the ORB reference is null or has not been initialized before.
	 */
	public ORB getORB() {
		return containerServicesImpl.getAcsCorba().getORB();
	}
    
	
    /**
     * Returns a reference to a new CORBA Any. In Java the only way to do 
     * this is through the ORB itself (i.e., the create_any method).
     * @return org.omg.CORBA.Any 
     * @throws NullPointerException if the Any object could not be created.
     */
    public org.omg.CORBA.Any getAny() {
    	ORB orb = getORB();
    	org.omg.CORBA.Any any = orb.create_any();
    	if (any == null) {
    		// should never happen, but we check just in case, 
    		// since there is a difficult to verify NPE when Any is created by MC for sending alarms.
    		String msg = "Failed to create org.omg.CORBA.Any";
    		logger.warning(msg);
    		throw new NullPointerException(msg);
    	}
    	return any;
	}

	/**
	 * Allows to connect a manager admin object to the manager, to receive notifications etc.
	 * <p>
	 * This method accepts and distinguishes <code>AdministratorOperations</code> objects and the subtyped
	 * {@link SynchronousAdministratorOperations} objects.
	 * <p>
	 * TODO: (1) container could implement a single proxy/interceptor admin client so that we only have at most one such
	 *          login on the manager, even if many components register their admin clients. 
	 *       (2) Discuss if the <code>retryConnectOnFailure</code> flag makes sense, or if such a specialized component that uses this method
	 *          should implement its own retry strategy and should rather be notified quickly if there are problems.
	 * 
	 * @param adminOp
	 *            admin object for manager callbacks.
	 * @param retryConnectOnFailure
	 *            retry if the manager is not available or the connection failed.
	 * @throws AcsJContainerEx
	 * @throws IllegalArgumentException
	 *             if adminOp == <code>null</code>.
	 */
	public void connectManagerAdmin(AdministratorOperations adminOp, boolean retryConnectOnFailure)
			throws AcsJContainerEx {

		if (adminOp == null) {
			throw new IllegalArgumentException("adminOp == null not allowed.");
		}
		synchronized (adminClientsToManagerProxy) {
			if (adminClientsToManagerProxy.containsKey(adminOp)) {
				logger.info("Attempt to connect manager admin of type " + adminOp.getClass().getName()
						+ " again will be ignored.");
				return;
			}

			// need to create our own manager proxy
			AcsManagerProxy acsManagerProxy = this.containerServicesImpl.m_acsManagerProxy.createInstance();

			// for CORBA activation we cannot use polymorphism, but have to choose the correct poa skeleton class.
			// The use of "instanceof" is certainly unsatisfying, but saves us from exposing a specialized second method
			// in the
			// AdvancedContainerServices interface.
			Administrator admin = null;
			if (adminOp instanceof SynchronousAdministratorOperations) {
				SynchronousAdministratorPOATie adminpoa = new SynchronousAdministratorPOATie(
						(SynchronousAdministratorOperations) adminOp);
				admin = adminpoa._this(getORB());
			} 
			else {
				AdministratorPOATie adminpoa = new AdministratorPOATie(adminOp);
				admin = adminpoa._this(getORB());
			}
			acsManagerProxy.loginToManager(admin, retryConnectOnFailure);

			adminClientsToManagerProxy.put(adminOp, acsManagerProxy);
		}
	}

	public void disconnectManagerAdmin(AdministratorOperations adminOp) {
		synchronized (adminClientsToManagerProxy) {
			AcsManagerProxy prox = adminClientsToManagerProxy.get(adminOp);
			if (prox != null) {
				prox.logoutFromManager();
			}
		}
	}
}
