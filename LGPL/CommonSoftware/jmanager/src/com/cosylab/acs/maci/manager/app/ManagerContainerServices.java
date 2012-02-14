/**
 * 
 */
package com.cosylab.acs.maci.manager.app;

import java.lang.reflect.Constructor;
import java.util.concurrent.ThreadFactory;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Policy;
import org.omg.CosNaming.NamingContext;
import org.omg.CosNaming.NamingContextHelper;
import org.omg.PortableServer.IdAssignmentPolicyValue;
import org.omg.PortableServer.LifespanPolicyValue;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.RequestProcessingPolicyValue;
import org.omg.PortableServer.Servant;
import org.omg.PortableServer.ServantRetentionPolicyValue;
import org.omg.PortableServer.POAPackage.AdapterAlreadyExists;
import org.omg.PortableServer.POAPackage.AdapterNonExistent;
import org.omg.PortableServer.POAPackage.InvalidPolicy;

import com.cosylab.CDB.DAL;

import si.ijs.maci.AdministratorOperations;

import alma.ACS.OffShoot;
import alma.ACS.OffShootHelper;
import alma.ACS.OffShootOperations;
import alma.ACSErrTypeCommon.wrappers.AcsJBadParameterEx;
import alma.ACSErrTypeCommon.wrappers.AcsJNotImplementedEx;
import alma.ACSErrTypeCommon.wrappers.AcsJUnexpectedExceptionEx;
import alma.JavaContainerError.wrappers.AcsJContainerEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.concurrent.DaemonThreadFactory;
import alma.acs.container.AdvancedContainerServices;
import alma.acs.container.ContainerServicesBase;
import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;
import alma.acs.nc.AcsEventPublisher;
import alma.acs.nc.AcsEventSubscriber;

/**
 * @TODO: Share some code with jcont and laser alarms who also implement ContainerServicesBase, 
 * e.g. through a new ContainerServicesBaseImpl class in module acsContainerServices.
 * 
 * @author msekoranja
 */
public class ManagerContainerServices implements ContainerServicesBase,
		AdvancedContainerServices {

	private final ORB orb;
	private final DAL dal;
	private final Logger logger;
	private AcsLogger componentLogger;
	private final POA clientPOA;
	private POA offshootPoa;
	private Policy[] offshootPolicies;
	
	/**
	 * NCPublisher can be instantiated only using reflection, because modules jcontnc comes after jmanager
	 */
	private final String CLASSNAME_NC_PUBLISHER  = "alma.acs.nc.refactored.NCPublisher";
	
	private final ThreadFactory threadFactory;
	
	/**
	 * @param orb
	 * @param dal
	 * @param logger
	 */
	public ManagerContainerServices(ORB orb, POA clientPOA, DAL dal, Logger logger)
	{
		this.orb = orb;
		this.clientPOA = clientPOA;
		this.offshootPoa = null; // on demand  
		this.dal = dal;
		this.logger = logger;
		this.threadFactory = new DaemonThreadFactory(ManagerContainerServices.class.getSimpleName());
	}
	
	/**
	 * @see alma.acs.container.ContainerServices#activateOffShoot(org.omg.PortableServer.Servant)
	 */
	public <T extends Servant & OffShootOperations > OffShoot activateOffShoot(T servant)
		throws AcsJContainerServicesEx
	{
		checkOffShootServant(servant);
		
		OffShoot shoot = null;
		try  {

			checkOffShootPOA();

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

	public void deactivateOffShoot(Object offshootImpl)
	throws AcsJContainerServicesEx
	{

		if( offshootImpl instanceof Servant ) {
			Servant servant = (Servant)offshootImpl;
			checkOffShootServant(servant);
			
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
		else {
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx();
			ex.setContextInfo("Not yet implemented");
			throw ex;
		}
	}
	
	/**
	 * Creates the shared offshoot poa on demand 
	 */
	public void checkOffShootPOA() throws AcsJContainerEx, AcsJUnexpectedExceptionEx {
		final String offshootPoaName = "offshootPoa";
		
		synchronized (clientPOA) {
			try {
				// can we reuse it?
				offshootPoa = clientPOA.find_POA(offshootPoaName, false);
			} catch (AdapterNonExistent e) {
				logger.finest("will have to create offshoot POA");

				if (offshootPolicies == null) {
					offshootPolicies = new Policy[4];

					offshootPolicies[0] = clientPOA.create_id_assignment_policy(IdAssignmentPolicyValue.SYSTEM_ID);

					offshootPolicies[1] = clientPOA.create_lifespan_policy(LifespanPolicyValue.TRANSIENT);

					offshootPolicies[2] = clientPOA
							.create_request_processing_policy(RequestProcessingPolicyValue.USE_ACTIVE_OBJECT_MAP_ONLY);

					offshootPolicies[3] = clientPOA
							.create_servant_retention_policy(ServantRetentionPolicyValue.RETAIN);
				}

				try {
					offshootPoa = clientPOA.create_POA(offshootPoaName, clientPOA.the_POAManager(), offshootPolicies);

					logger.finest("successfully created offshoot POA");
				} catch (InvalidPolicy ex) {
					AcsJContainerEx ex2 = new AcsJContainerEx(ex);
					ex2.setContextInfo("Attempted to create offshoot POA with invalid policies.");
					throw ex2;
				} catch (AdapterAlreadyExists ex) {
					// we sync on componentPOA, so this should never happen
					throw new AcsJUnexpectedExceptionEx(ex);
				}
			}
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
		return threadFactory;
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

	@Override
	public <T> AcsEventPublisher<T> createNotificationChannelPublisher(String channelName, Class<T> eventType) throws AcsJContainerServicesEx {
		return createNotificationChannelPublisher(channelName, null, eventType);
	}

	@Override
	public <T> AcsEventPublisher<T> createNotificationChannelPublisher(
			String channelName,
			String channelNotifyServiceDomainName, 
			Class<T> eventType) throws AcsJContainerServicesEx {
			AcsEventPublisher<T> publisher = null;

			try {
				// TODO: Matej please help, is there a more elegant way to get the naming service ref?
				org.omg.CORBA.Object objRef = orb.resolve_initial_references("NameService");
				NamingContext ncRef = NamingContextHelper.narrow(objRef);
				Object[] args = new Object[]{
						channelName,
						channelNotifyServiceDomainName,
						this,
						ncRef
				};
//				// TODO: Can we do this without the direct cast? The usual "asSubclass" is not enough 
				// because we don't create a subclass of Class<T> but rather of Class<U<T>>.
				// Also the getGenericInterfaces / ParameterizedType trick does not work because because we don't have a 
				// concrete parameterized type.
				Class<AcsEventPublisher<T>> clazz = (Class<AcsEventPublisher<T>>) Class.forName(CLASSNAME_NC_PUBLISHER);
				Constructor<? extends AcsEventPublisher<T>> constructor = clazz.getConstructor(String.class, String.class, ContainerServicesBase.class, NamingContext.class);
				publisher = constructor.newInstance(args);
			} catch(ClassNotFoundException e) {
				// TODO: maybe we could prevent future NCPublisher creation tries, since the class isn't and will not be loaded
				//       The same applies for the next "catch" block
				logger.log(AcsLogLevel.ERROR, "Cannot create NC publisher because the 'NCPublisher' class is not present in the classpath", e);
				AcsJContainerServicesEx ex = new AcsJContainerServicesEx(e);
				ex.setContextInfo("'" + CLASSNAME_NC_PUBLISHER + "' class not present in the classpath");
				throw ex;
			} catch(ClassCastException e) {
				logger.log(AcsLogLevel.ERROR, "Cannot create NC publisher because loaded class '" + CLASSNAME_NC_PUBLISHER + "' is not of type 'AcsEventPublisher", e);
				AcsJContainerServicesEx ex = new AcsJContainerServicesEx(e);
				ex.setContextInfo("'" + CLASSNAME_NC_PUBLISHER + "' class does not extend 'AcsEventPublisher'");
				throw ex;
			} catch(Throwable e) {
				logger.log(AcsLogLevel.ERROR, "Unexpected error while creating new AcsEventPublisher object", e);
				AcsJContainerServicesEx ex = new AcsJContainerServicesEx(e);
				throw ex;
			}

//			m_publishers.put( (channelNotifyServiceDomainName == null ? "" : channelNotifyServiceDomainName) + "/" + channelName, publisher);

			return publisher;
	}


	@Override
	public AcsEventSubscriber createNotificationChannelSubscriber(String channelName) throws AcsJContainerServicesEx {
		throw new AcsJContainerServicesEx(new AcsJNotImplementedEx("createNotificationChannelSubscriber not yet implemented in this special alarm service CS class."));
	}

	@Override
	public AcsEventSubscriber createNotificationChannelSubscriber(String channelName,
			String channelNotifyServiceDomainName) throws AcsJContainerServicesEx {
		throw new AcsJContainerServicesEx(new AcsJNotImplementedEx("createNotificationChannelSubscriber not yet implemented in this special alarm service CS class."));
	}

}
