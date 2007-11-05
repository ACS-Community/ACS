/**
 * 
 */
package com.cosylab.acs.maci.manager.app;

import java.lang.reflect.Method;
import java.util.concurrent.ThreadFactory;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Object;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.Servant;

import si.ijs.maci.AdministratorOperations;
import si.ijs.maci.ComponentSpec;
import alma.ACS.OffShoot;
import alma.ACS.OffShootHelper;
import alma.ACS.OffShootOperations;
import alma.ACSErrTypeCommon.wrappers.AcsJBadParameterEx;
import alma.acs.component.ComponentDescriptor;
import alma.acs.component.ComponentQueryDescriptor;
import alma.acs.component.ComponentStateManager;
import alma.acs.container.AdvancedContainerServices;
import alma.acs.container.ContainerSealant;
import alma.acs.container.ContainerServices;
import alma.acs.container.corba.AcsCorba;
import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;

import com.cosylab.CDB.DAL;

/**
 * @author msekoranja
 */
public class ManagerContainerServices implements ContainerServices,
		AdvancedContainerServices {

	private ORB orb;
	private AcsCorba acsCorba;
	private DAL dal;
	private Logger logger;
    private AcsLogger componentLogger;
	private POA clientPOA;
	
	/**
	 * @param orb
	 * @param dal
	 * @param logger
	 */
	public ManagerContainerServices(ORB orb, POA clientPOA, DAL dal, Logger logger)
	{
		this.orb = orb;
		this.clientPOA = clientPOA;
		this.dal = dal;
		this.logger = logger;

		// AcsCorba is not completely initialized, but should work for offshots...
		acsCorba = new AcsCorba(logger);
	}
	
	/* (non-Javadoc)
	 * @see alma.acs.container.ContainerServices#activateOffShoot(org.omg.PortableServer.Servant)
	 */
	/**
	 * @see alma.acs.container.ContainerServices#activateOffShoot(org.omg.PortableServer.Servant)
	 */
	public OffShoot activateOffShoot(Servant servant)
		throws AcsJContainerServicesEx
	{
		checkOffShootServant(servant);
		String servantName = servant.getClass().getName();
		// check if the servant is the Tie variant, which allows proxy-based call interception by the container
		boolean isTie = false;
		if (servantName.endsWith("POATie")) {
			try {
				// the _delegate getter method is mandated by the IDL-to-Java mapping spec
				Method implGetter = servant.getClass().getMethod("_delegate", (Class[]) null);
				isTie = true;
				Class operationsIF = implGetter.getReturnType();
				java.lang.Object offshootImpl = implGetter.invoke(servant, (java.lang.Object[]) null);
				// now we insert the interceptor between the tie skeleton and the impl.
				// Offshoots have no name, so we construct one from the component name and the offshoot interface name
				// 
				String qualOffshootName = getName() + "/" + operationsIF.getName().substring(0, operationsIF.getName().length() - "Operations".length());
				java.lang.Object interceptingOffshootImpl = ContainerSealant.createContainerSealant(
						operationsIF, offshootImpl, qualOffshootName, true, logger, 
// TODO @todo Heiko how to handle "methodsExcludedFromInvocationLogging" 
						Thread.currentThread().getContextClassLoader(), /*methodsExcludedFromInvocationLogging*/ new Method[0]);
				Method implSetter = servant.getClass().getMethod("_delegate", new Class[]{operationsIF});
				implSetter.invoke(servant, new java.lang.Object[]{interceptingOffshootImpl});
				logger.fine("created sealant for offshoot " + qualOffshootName);
			} catch (NoSuchMethodException e) {
				// so this was not a Tie skeleton, even though its name ends misleadingly with "POATie"
			} catch (Exception e) {
				logger.log(Level.WARNING, "Failed to create interceptor for offshoot " + servantName, e);
			} 
		}		

		if (!isTie) {
// TODO: perhaps require tie offshoots with ACS 5.0, and enable this warning log			
//			logger.warning("Offshoot servant '" + servantName + "' from component '" + getName() + 
//					"' does not follow the tie approach. Calls can thus not be intercepted by the container.");
		}
				
		OffShoot shoot = null;
		try  {
			org.omg.CORBA.Object obj = acsCorba.activateOffShoot(servant, clientPOA);
			shoot = OffShootHelper.narrow(obj);
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
//		logger.fine("successfully activated offshoot of type " + cbServant.getClass().getName());
		return shoot;
	}

	/* (non-Javadoc)
	 * @see alma.acs.container.ContainerServices#deactivateOffShoot(org.omg.PortableServer.Servant)
	 */
	public void deactivateOffShoot(Servant cbServant)
		throws AcsJContainerServicesEx
	{
		checkOffShootServant(cbServant);
		try {
			acsCorba.deactivateOffShoot(cbServant, clientPOA);
		} catch (AcsJContainerEx e) {
			throw new AcsJContainerServicesEx(e);
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

	/* (non-Javadoc)
	 * @see alma.acs.container.ContainerServices#findComponents(java.lang.String, java.lang.String)
	 */
	public String[] findComponents(String curlWildcard, String typeWildcard) {
		throw new NO_IMPLEMENT();
	}

	/* (non-Javadoc)
	 * @see alma.acs.container.ContainerServices#getAdvancedContainerServices()
	 */
	public AdvancedContainerServices getAdvancedContainerServices() {
		return this;
	}

	/* (non-Javadoc)
	 * @see alma.acs.container.ContainerServices#getCDB()
	 */
	public DAL getCDB() {
		return dal;
	}

	/* (non-Javadoc)
	 * @see alma.acs.container.ContainerServices#getCollocatedComponent(java.lang.String, java.lang.String)
	 */
	public Object getCollocatedComponent(String compUrl, String targetCompUrl) {
		throw new NO_IMPLEMENT();
	}

	/* (non-Javadoc)
	 * @see alma.acs.container.ContainerServices#getCollocatedComponent(alma.acs.component.ComponentQueryDescriptor, boolean, java.lang.String)
	 */
	public Object getCollocatedComponent(ComponentQueryDescriptor compSpec,
			boolean markAsDefaul, String targetCompUrl) {
		throw new NO_IMPLEMENT();
	}

	/* (non-Javadoc)
	 * @see alma.acs.container.ContainerServices#getComponent(java.lang.String)
	 */
	public Object getComponent(String componentUrl) {
		throw new NO_IMPLEMENT();
	}

	/* (non-Javadoc)
	 * @see alma.acs.container.ContainerServices#getComponentDescriptor(java.lang.String)
	 */
	public ComponentDescriptor getComponentDescriptor(String componentUrl) {
		throw new NO_IMPLEMENT();
	}

	/* (non-Javadoc)
	 * @see alma.acs.container.ContainerServices#getComponentNonSticky(java.lang.String)
	 */
	public Object getComponentNonSticky(String curl) {
		throw new NO_IMPLEMENT();
	}

	/* (non-Javadoc)
	 * @see alma.acs.container.ContainerServices#getComponentStateManager()
	 */
	public ComponentStateManager getComponentStateManager() {
		throw new NO_IMPLEMENT();
	}

	/* (non-Javadoc)
	 * @see alma.acs.container.ContainerServices#getDefaultComponent(java.lang.String)
	 */
	public Object getDefaultComponent(String componentIDLType) {
		throw new NO_IMPLEMENT();
	}

	/* (non-Javadoc)
	 * @see alma.acs.container.ContainerServices#getDynamicComponent(alma.acs.component.ComponentQueryDescriptor, boolean)
	 */
	public Object getDynamicComponent(ComponentQueryDescriptor compSpec,
			boolean markAsDefault) {
		throw new NO_IMPLEMENT();
	}

	/* (non-Javadoc)
	 * @see alma.acs.container.ContainerServices#getDynamicComponent(si.ijs.maci.ComponentSpec, boolean)
	 */
	public Object getDynamicComponent(ComponentSpec compSpec,
			boolean markAsDefault) {
		throw new NO_IMPLEMENT();
	}

	/* (non-Javadoc)
	 * @see alma.acs.container.ContainerServices#getLogger()
	 */
	public synchronized AcsLogger getLogger() {
        if (componentLogger == null) {
    		componentLogger = ClientLogManager.getAcsLogManager().getLoggerForComponent(getName());
        }
        return componentLogger;
	}

	/* (non-Javadoc)
	 * @see alma.acs.container.ContainerServices#getName()
	 */
	public String getName() {
		return "ManagerContainerServices";
	}

	/* (non-Javadoc)
	 * @see alma.acs.container.ContainerServices#getThreadFactory()
	 */
	public ThreadFactory getThreadFactory() {
		throw new NO_IMPLEMENT();
	}

	/* (non-Javadoc)
	 * @see alma.acs.container.ContainerServices#getTransparentXmlComponent(java.lang.Class, org.omg.CORBA.Object, java.lang.Class)
	 */
	public <T> T getTransparentXmlComponent(Class<T> transparentXmlIF,
			Object componentReference, Class flatXmlIF) {
		throw new NO_IMPLEMENT();
	}

	/* (non-Javadoc)
	 * @see alma.acs.container.ContainerServices#registerComponentListener(alma.acs.container.ContainerServices.ComponentListener)
	 */
	public void registerComponentListener(ComponentListener listener) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see alma.acs.container.ContainerServices#releaseComponent(java.lang.String)
	 */
	public void releaseComponent(String componentUrl) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see alma.acs.container.AdvancedContainerServices#connectManagerAdmin(si.ijs.maci.AdministratorOperations, boolean)
	 */
	public void connectManagerAdmin(AdministratorOperations adminOp,
			boolean retryConnectOnFailure) {
		throw new NO_IMPLEMENT();
	}

	/* (non-Javadoc)
	 * @see alma.acs.container.AdvancedContainerServices#corbaObjectToString(org.omg.CORBA.Object)
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
	 * @see alma.acs.container.AdvancedContainerServices#corbaObjectFromString(java.lang.String)
	 */
	public org.omg.CORBA.Object corbaObjectFromString(String strObjRef)
	{
		ORB orb = getORB();
		org.omg.CORBA.Object objRef = orb.string_to_object(strObjRef);
		logger.finer("converted corba object reference string " + strObjRef + 
						" back to a corba object reference.");
		return objRef;
	}

	/* (non-Javadoc)
	 * @see alma.acs.container.AdvancedContainerServices#disconnectManagerAdmin(si.ijs.maci.AdministratorOperations)
	 */
	public void disconnectManagerAdmin(AdministratorOperations adminOp) {
		throw new NO_IMPLEMENT();
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

	/* (non-Javadoc)
	 * @see alma.acs.container.AdvancedContainerServices#getORB()
	 */
	public ORB getORB() {
		return orb;
	}

}
