/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2011
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *                 and Cosylab
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
package alma.alarmsystem.corbaservice;

import java.util.concurrent.ThreadFactory;
import java.util.logging.Level;

import org.omg.CORBA.ORB;
import org.omg.CORBA.Policy;
import org.omg.CosNaming.NamingContext;
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
import com.cosylab.CDB.DALHelper;

import alma.ACS.OffShoot;
import alma.ACS.OffShootHelper;
import alma.ACS.OffShootOperations;
import alma.ACSErrTypeCommon.wrappers.AcsJBadParameterEx;
import alma.ACSErrTypeCommon.wrappers.AcsJNotImplementedEx;
import alma.ACSErrTypeCommon.wrappers.AcsJUnexpectedExceptionEx;
import alma.JavaContainerError.wrappers.AcsJContainerEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.alarmsystem.corbaservice.AlarmSystemCorbaServer;
import alma.acs.container.AdvancedContainerServices;
import alma.acs.container.CleaningDaemonThreadFactory;
import alma.acs.container.ContainerServicesBase;
import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.AcsLogger;
import alma.acs.nc.AcsEventPublisher;
import alma.acs.nc.AcsEventSubscriber;
import alma.acs.nc.Helper;
import alma.acs.nc.refactored.NCPublisher;

public class AlarmSystemContainerServices implements ContainerServicesBase {
	
	/**
	 * The CORBA ORB
	 */
	private final ORB orb;
	
	/**
	 * The logger
	 */
	private final AcsLogger logger;
	
	/**
	 * Thread factory.
	 * @TODO: During service shutdown, {@link CleaningDaemonThreadFactory#cleanUp()} should be called.
	 */
	private final CleaningDaemonThreadFactory threadFactory;
	
	/**
	 * The CORBA server for the alarm system
	 */
	private AlarmSystemCorbaServer alSysCorbaServer;
	
	/**
	 * The name returned by <code>getName()</code>.
	 */
	private static final String name = "AlarmService";
	
	/**
	 * The implementation of the {@link AdvancedContainerServices}
	 */
	private final AlarmSystemAdvancedContainerServices advancedContainerServices;
	
	/**
	 * Constructor 
	 * 
	 * @param theOrb The ORB
	 * @param theLogger The logger
	 */
	public AlarmSystemContainerServices(AlarmSystemCorbaServer alSysCorbaServer, AcsLogger theLogger) {
		if (alSysCorbaServer==null) {
			throw new IllegalArgumentException("The AlarmSystemCorbaServer can't be null");
		}
		if (theLogger==null) {
			throw new IllegalArgumentException("The logger can't be null");
		}
		threadFactory = new CleaningDaemonThreadFactory(name, theLogger);
		this.alSysCorbaServer=alSysCorbaServer;
		this.orb=alSysCorbaServer.getORB();
		logger=theLogger;
		advancedContainerServices= new AlarmSystemAdvancedContainerServices(this);
	}

	@Override
	public <T extends Servant & OffShootOperations> OffShoot activateOffShoot(T cbServant)
			throws AcsJContainerServicesEx {

		if (cbServant == null) {
			String msg = "activateOffShoot called with missing parameter.";
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx();
			ex.setContextInfo(msg);
			throw ex;
		}

		POA offshootPoa;
		try {
			offshootPoa = getPOAForOffshoots(alSysCorbaServer.getRootPOA());
		} catch (Exception e) {
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx(e);
			throw ex;
		}

		org.omg.CORBA.Object actObj = null;
		try {
			offshootPoa.activate_object(cbServant);
			actObj = offshootPoa.servant_to_reference(cbServant);
			actObj._hash(Integer.MAX_VALUE); // just to provoke an exc. if
												// something is wrong with our
												// new object
			logger.finer("offshoot of type '" + cbServant.getClass().getName()
					+ "' activated as a CORBA object.");
		} catch (Throwable thr) {
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx(thr);
			ex.setContextInfo("failed to activate offshoot of type '"
					+ cbServant.getClass().getName());
			throw ex;
		}

		return OffShootHelper.narrow(actObj);
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
			// TODO: try to get the naming service ref in a nicer way (from ORB etc)
			NamingContext namingService = Helper.getNamingServiceInitial(this);
			publisher = new NCPublisher<T>(channelName, channelNotifyServiceDomainName, this, namingService);
		} catch(Throwable e) {
			logger.log(AcsLogLevel.ERROR, "Unexpected error while creating new AcsEventPublisher object", e);
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx(e);
			throw ex;
		}

//		m_publishers.put( (channelNotifyServiceDomainName == null ? "" : channelNotifyServiceDomainName) + "/" + channelName, publisher);
		
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

	private Policy[] m_offshootPolicies;

	public POA getPOAForOffshoots(POA componentPOA) throws AcsJContainerEx,
	AcsJUnexpectedExceptionEx {
		final String offshootPoaName = "offshootPoa";
		POA offshootPoa = null;

		synchronized (componentPOA) {
			try {
				// can we reuse it?
				offshootPoa = componentPOA.find_POA(offshootPoaName, false);
			} catch (AdapterNonExistent e) {
				logger.finest("will have to create offshoot POA");

				if (m_offshootPolicies == null) {
					m_offshootPolicies = new Policy[4];

					m_offshootPolicies[0] = componentPOA
					.create_id_assignment_policy(IdAssignmentPolicyValue.SYSTEM_ID);

					m_offshootPolicies[1] = componentPOA
					.create_lifespan_policy(LifespanPolicyValue.TRANSIENT);

					m_offshootPolicies[2] = componentPOA
					.create_request_processing_policy(RequestProcessingPolicyValue.USE_ACTIVE_OBJECT_MAP_ONLY);

					m_offshootPolicies[3] = componentPOA
					.create_servant_retention_policy(ServantRetentionPolicyValue.RETAIN);
				}

				try {
					offshootPoa = componentPOA.create_POA(offshootPoaName,
							alSysCorbaServer.getRootPOA().the_POAManager(), m_offshootPolicies);

					logger.finest("successfully created offshoot POA");
				} catch (InvalidPolicy ex) {
					AcsJContainerEx ex2 = new AcsJContainerEx(ex);
					ex2
					.setContextInfo("Attempted to create offshoot POA with invalid policies.");
					throw ex2;
				} catch (AdapterAlreadyExists ex) {
					// we sync on componentPOA, so this should never happen
					throw new AcsJUnexpectedExceptionEx(ex);
				}
			}
		}
		return offshootPoa;
	}
	@Override
	public AdvancedContainerServices getAdvancedContainerServices() {
		return advancedContainerServices;
	}

	@Override
	public AcsLogger getLogger() {
		return logger;
	}

	@Override
	public String getName() {
		return name;
	}

	@Override
	public ThreadFactory getThreadFactory() {
		return threadFactory;
	}
	
	@Override
	public DAL getCDB() throws AcsJContainerServicesEx {
		try {
			org.omg.CORBA.Object dalObj = alSysCorbaServer.getServiceFromNameServer("CDB");
			DAL dal = DALHelper.narrow(dalObj);
			return dal;
		} catch (Throwable thr) {
			String msg = "Unexpectedly failed to get the CDB reference!";
			logger.log(Level.FINE, msg, thr);
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx(thr);
			ex.setContextInfo(msg);
			throw ex;
        }
	}

	/**
	 * 
	 * @return The Orb
	 */
	public ORB getOrb() {
		return orb;
	}

	@Override
	public void deactivateOffShoot(Object offshootImpl)
	throws AcsJContainerServicesEx
	{
		if( offshootImpl instanceof Servant ) {
			Servant cbServant = (Servant)offshootImpl;
			try {
				checkOffShootServant(cbServant);
				POA rootPOA = alSysCorbaServer.getRootPOA();
				if (cbServant == null || rootPOA == null) {
					String msg = "deactivateOffShoot called with missing parameter.";
					AcsJContainerEx ex = new AcsJContainerEx();
					ex.setContextInfo(msg);
					throw ex;
				}

				byte[] id = null;
				try {
					POA offshootPoa = getPOAForOffshoots(rootPOA);
					id = offshootPoa.servant_to_id(cbServant);
					offshootPoa.deactivate_object(id);
				}
				catch (AcsJContainerEx e) {
					throw e;
				}
				catch (Throwable thr) {
					String msg = "failed to deactivate offshoot of type '" + cbServant.getClass().getName() +
									"' (ID=" + String.valueOf(id) + ")";
					logger.log(Level.WARNING, msg, thr);
					AcsJContainerEx ex = new AcsJContainerEx(thr);
					ex.setContextInfo(msg);
					throw ex;
				}
			} catch (AcsJContainerEx ex) {
				throw new AcsJContainerServicesEx(ex);
			}
		}
		else {
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx();
			ex.setContextInfo("Not yet implemented");
			throw ex;
		}
	}

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

}
