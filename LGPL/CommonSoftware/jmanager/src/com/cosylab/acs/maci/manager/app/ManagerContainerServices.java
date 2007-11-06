/**
 * 
 */
package com.cosylab.acs.maci.manager.app;

import java.awt.event.ComponentListener;
import java.lang.reflect.Method;
import java.util.concurrent.ThreadFactory;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Object;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.Servant;

import com.cosylab.CDB.DAL;

import si.ijs.maci.AdministratorOperations;
import si.ijs.maci.ComponentSpec;

import alma.ACS.OffShoot;
import alma.ACS.OffShootHelper;
import alma.ACS.OffShootOperations;
import alma.ACSErrTypeCommon.wrappers.AcsJBadParameterEx;
import alma.JavaContainerError.wrappers.AcsJContainerEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.container.AdvancedContainerServices;
import alma.acs.container.ContainerServicesBase;
import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;

/**
 * @author msekoranja
 */
public class ManagerContainerServices implements ContainerServicesBase,
		AdvancedContainerServices {

	private final ORB orb;
	private final DAL dal;
	private final Logger logger;
    private AcsLogger componentLogger;
	private final POA clientPOA;
	private final POA offshootPoa;
	
	/**
	 * @param orb
	 * @param dal
	 * @param logger
	 */
	public ManagerContainerServices(ORB orb, POA clientPOA, DAL dal, Logger logger)
	{
		this.orb = orb;
		this.clientPOA = clientPOA;
		this.offshootPoa = clientPOA; // @TODO: perhaps use child POA 
		this.dal = dal;
		this.logger = logger;
		
	}
	
	/**
	 * @see alma.acs.container.ContainerServices#activateOffShoot(org.omg.PortableServer.Servant)
	 */
	public OffShoot activateOffShoot(Servant servant)
		throws AcsJContainerServicesEx
	{
		checkOffShootServant(servant);
				
		OffShoot shoot = null;
		try  {
			if (servant == null) {
				String msg = "activateOffShoot called with missing parameter.";
				AcsJContainerEx ex = new AcsJContainerEx();
				ex.setContextInfo(msg);
				throw ex;
			}
			
			org.omg.CORBA.Object actObj = null;
			offshootPoa.activate_object(servant);
			actObj = offshootPoa.servant_to_reference(servant);
			actObj._hash(Integer.MAX_VALUE); // just to provoke an exc. if something is wrong with our new object
			logger.finer("offshoot of type '" + servant.getClass().getName() + "' activated as a CORBA object.");
			
			shoot = OffShootHelper.narrow(actObj);
		}
		catch (Throwable thr) {
			String msg = "failed to activate offshoot object of type '" + servant.getClass().getName() +
							"' for client '" + getName() + "'. ";
			// flatten the exception chain by one level if possible
			if (thr instanceof AcsJContainerServicesEx && thr.getCause() != null) {
				msg += "(" + thr.getMessage() + ")"; 
				thr = thr.getCause();
			}
			logger.log(Level.FINE, msg, thr);
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx(thr);
			throw ex;
		}	
		return shoot;
	}


	
	public void deactivateOffShoot(Servant servant)
		throws AcsJContainerServicesEx
	{
		checkOffShootServant(servant);
		if (servant == null) {
			String msg = "deactivateOffShoot called with missing parameter.";
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx();
			ex.setContextInfo(msg);
			throw ex;
		}
		
		byte[] id = null;
		try {
			id = offshootPoa.servant_to_id(servant);
			offshootPoa.deactivate_object(id);
		}
		catch (Throwable thr) {
			String msg = "failed to deactivate offshoot of type '" + servant.getClass().getName() +
							"' (ID=" + String.valueOf(id) + ")";
			logger.log(Level.WARNING, msg, thr);
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx(thr);
			ex.setContextInfo(msg);
			throw ex;
		}
	}

	/**
	 * @param cbServant
	 * @throws ContainerException
	 */
	private void checkOffShootServant(Servant servant) throws AcsJContainerServicesEx {
		if (servant == null) {
			AcsJBadParameterEx cause = new AcsJBadParameterEx();
			cause.setParameter("servant");
			cause.setParameterValue("null");
			throw new AcsJContainerServicesEx(cause);
		}		
		
		if (!(servant instanceof OffShootOperations)) {
			String msg = "invalid offshoot servant provided. Must implement " + OffShootOperations.class.getName();
			logger.fine(msg);
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx();
			ex.setContextInfo(msg);
			throw ex;
		}
	}

	public AdvancedContainerServices getAdvancedContainerServices() {
		return this;
	}

	public DAL getCDB() {
		return dal;
	}

	public synchronized AcsLogger getLogger() {
        if (componentLogger == null) {
    		componentLogger = ClientLogManager.getAcsLogManager().getLoggerForComponent(getName());
        }
        return componentLogger;
	}

	public String getName() {
		return "ManagerContainerServices";
	}

	public ThreadFactory getThreadFactory() {
		throw new NO_IMPLEMENT();
	}

	public String corbaObjectToString(org.omg.CORBA.Object objRef)
	{
		String str = orb.object_to_string(objRef);
		logger.finer("converted corba object reference of type " + objRef.getClass().getName() +
						" to the string " + str);
		return str;
	}

	public org.omg.CORBA.Object corbaObjectFromString(String strObjRef)
	{
		org.omg.CORBA.Object objRef = orb.string_to_object(strObjRef);
		logger.finer("converted corba object reference string " + strObjRef + 
						" back to a corba object reference.");
		return objRef;
	}

    /**
     * Returns a reference to a new CORBA Any. In Java the only way to do 
     * this is through the ORB itself (i.e., the create_any method).
     * @return org.omg.CORBA.Any 
     * @throws NullPointerException if the Any object could not be created.
     */
    public org.omg.CORBA.Any getAny() {
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

	public ORB getORB() {
		return orb;
	}

	public void connectManagerAdmin(AdministratorOperations adminOp, boolean retryConnectOnFailure)
			throws AcsJContainerEx {
		throw new NO_IMPLEMENT();
	}

	public void disconnectManagerAdmin(AdministratorOperations adminOp) {
		throw new NO_IMPLEMENT();
	}

}
