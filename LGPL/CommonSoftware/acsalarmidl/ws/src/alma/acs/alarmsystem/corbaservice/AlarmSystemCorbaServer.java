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
package alma.acs.alarmsystem.corbaservice;

import java.io.StringReader;
import java.lang.reflect.Constructor;
import java.util.ConcurrentModificationException;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.jacorb.orb.acs.AcsORBProfiler;
import org.jacorb.orb.acs.AcsProfilingORB;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Object;
import org.omg.CORBA.Policy;
import org.omg.CORBA.ORBPackage.InvalidName;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContext;
import org.omg.CosNaming.NamingContextExtHelper;
import org.omg.CosNaming.NamingContextPackage.CannotProceed;
import org.omg.CosNaming.NamingContextPackage.NotFound;
import org.omg.PortableServer.IdAssignmentPolicyValue;
import org.omg.PortableServer.LifespanPolicyValue;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import org.omg.PortableServer.POAManager;
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
import alma.ACSErrTypeCommon.wrappers.AcsJUnexpectedExceptionEx;
//import alma.JavaContainerError.wrappers.AcsJContainerEx;
//import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.alarmsystem.acsimpl.AcsAlarmSystem;
import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;
import alma.acs.profiling.orb.AcsORBProfilerImplBase;
import alma.acs.util.ACSPorts;
import alma.alarmsystem.AlarmServiceOperations;
import alma.alarmsystem.alarmmessage.generated.AlarmSystemConfiguration;
import alma.alarmsystem.alarmmessage.generated.ConfigurationProperty;

/**
 * Class that provides default ACS CORBA service implementation.
 * <P>
 * The constructor activate the CORBA servant: the ACS or the CERN implementation,
 * depending on the value read from the CDB.
 * 
 * 
 * @author acaproni
 */
public class AlarmSystemCorbaServer implements Runnable {
	/**
	 * Object Request Broker (ORB) object.
	 */
	private ORB orb = null;

	/**
	 * Additional sync check (what if component is destroyed before thread is started).
	 */
	private volatile boolean destroyState = false;

	/**
	 * Root Portable Object Adapter (POA) object.
	 */
	private POA rootPOA = null;
	
	/**
	 * Alarm System POA.
	 */
	private POA asPOA = null;
	
	/**
	 * POA manager
	 */
	private POAManager poaManager;
	
	/**
	 * Logger.
	 */
	private final AcsLogger m_logger;
	
	private Policy[] m_offshootPolicies;
	
	private boolean isInitialized = false;
	
	/**
	 * Signal if the server has been closed
	 */
	private volatile boolean closed=false;
	
	/**
	 * The CERN CORBA servant
	 * <P>
	 * One and only one between <code>laserComponent</code> and <code>acsComponent</code> is not <code>null</code>.
	 * <P>
	 */
	private Servant laserComponent=null;
	
	/**
	 * The ACS CORBA servant
	 * <P>
	 * One and only one between <code>laserComponent</code> and <code>acsComponent</code> is not <code>null</code>.
	 */
	private Servant acsComponent=null;
	
	public static void main(String[] args) {
		AcsLogger logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("AlarmService", true);
		AlarmSystemCorbaServer server;
		try {
			server = new AlarmSystemCorbaServer(logger,args);
		} catch (Throwable t) {
			System.err.println("Error instantiating the alarm service: "+t.getMessage());
			t.printStackTrace();
			System.exit(-1);
		}
	}

	/**
	 * Constructor for DefaultCORBAService.
	 * 
	 * @param logger The logger
	 * @param args The arguments from the command line
	 */
	public AlarmSystemCorbaServer(AcsLogger logger, String[] args) throws Exception {
		if (logger==null) {
			throw new IllegalArgumentException("The logger can't be null");
		}
		Runtime.getRuntime().addShutdownHook(new Thread(new Runnable () {
			public void run() {
				if (laserComponent!=null) {
					((AlarmServiceOperations)laserComponent).shutdown();
				} else if (acsComponent!=null) {
					((AlarmServiceOperations)acsComponent).shutdown();
				} else {
					shutdown();
				}
			}
		},"JVM shutdown hook"));
		this.m_logger = logger;
		internalInitialize(args);
		// Check which implementation to use (true=ACS)
		boolean alarmType=true;
		try {
			alarmType=getAlarmSystemType();
		} catch (Throwable t) {}
		org.omg.CORBA.Object alarmObject;

		//create object id
		byte[] id = alma.alarmsystem.AlarmServiceName.value.getBytes();
		if (alarmType) {
			// ACS
			laserComponent=null;
			logger.log(AcsLogLevel.INFO,"Starting the ACS implementation of the alarm service");
			acsComponent=new AcsAlarmSystem(this);
			asPOA.activate_object_with_id(id, acsComponent);
			alarmObject = asPOA.servant_to_reference(acsComponent);
		} else {
			// CERN
			acsComponent=null;
			logger.log(AcsLogLevel.INFO,"Starting the CERN implementation of the alarm service");
			laserComponent=instantiateCernAS();
			asPOA.activate_object_with_id(id, laserComponent);
			alarmObject = asPOA.servant_to_reference(laserComponent);
		}
		registerToNamingService(alarmObject);
		new Thread(this, "AlarmSystemCORBAHelper").start();
		System.out.println("The alarm service is ready and waiting");
	}
	
	/**
	 * Instantiate the CERN alarm service.
	 * <P>
	 * At this point the CERN class are still unknown because they are built
	 * very late in ACS.
	 * Therefore the CERN alarm system is instantiated dynamically.
	 * 
	 * @return The CERN alarm service
	 */
	private Servant instantiateCernAS() throws Exception {
		Thread t = Thread.currentThread();
        ClassLoader loader = t.getContextClassLoader();
        Class cl =loader.loadClass("alma.alarmsystem.corbaservice.AlarmSystemContainerServices");
        
        Class[] classes = {AlarmSystemCorbaServer.class , AcsLogger.class };
        Constructor constructor = cl.getConstructor(classes);
        java.lang.Object contSvcs = constructor.newInstance(AlarmSystemCorbaServer.this,m_logger);
        m_logger.log(AcsLogLevel.DEBUG,"Built the alarm system implementation of ContainerServices");
		
		m_logger.log(AcsLogLevel.INFO,"Starting the CERN implementation of the alarm service");
		Class laserCL = loader.loadClass("com.cosylab.acs.laser.LaserComponent");
		Class[] laserClasses = { AlarmSystemCorbaServer.class, contSvcs.getClass()};
		Constructor ctor = laserCL.getConstructor(laserClasses);
		return (Servant)ctor.newInstance(AlarmSystemCorbaServer.this,contSvcs);
	}

	/**
	 * Initializes the CORBA.
	 * 
	 * @param args The command line arguments
	 */
	private void internalInitialize(String[] args) throws Exception {
		Properties properties = System.getProperties();
		int portNumber = Integer.parseInt(ACSPorts.getAlarmServicePort());
		boolean useJacORB = false; // default is JDK ORB
		for (int i = 0; i < args.length; i++) {
			if (args[i].equals("-OAport") || args[i].equals("-OAPort")) {
				if (i < args.length - 1) {
					portNumber = Integer.valueOf(args[++i]).intValue();
				}
			}

			if (args[i].equals("-OAIAddr"))
			    {
			    if (i < args.length - 1) {
			    properties.put("OAIAddr", args[++i]);
			    }
			    }
			if (args[i].equals("-orbacus")) {
				m_logger.log(AcsLogLevel.NOTICE, "ORBacus is no longer supported, switching to JacORB.");
				useJacORB = true;
			}
			if (args[i].equals("-jacorb")) {
				useJacORB = true;
			}
		}
		if (useJacORB) {
			if (Integer.getInteger("ACS.logstdout", 4) < 4)  {
			   	m_logger.log(AcsLogLevel.INFO, "ACS alarm service will use JacORB ORB");
		    }
			properties.put("org.omg.CORBA.ORBClass", "org.jacorb.orb.ORB");
			properties.put(
				"org.omg.CORBA.ORBSingletonClass",
				"org.jacorb.orb.ORBSingleton");

			// port
			properties.put("OAPort", Integer.toString(portNumber));

	    		// ORB implementation name
			properties.put("jacorb.implname", "ORB");

			/*
			 * by setting the following property, the ORB will
			 * accept client requests targeted at the object with
			 * key alma.alarmsystem.AlarmServiceName.value, so more readable corbaloc URLs
			 * can be used
			 */

			properties.put(
				"jacorb.orb.objectKeyMap." + alma.alarmsystem.AlarmServiceName.value,
				"ORB/asPOA/" + alma.alarmsystem.AlarmServiceName.value);

		} else {
			properties.put(
				"com.sun.CORBA.POA.ORBPersistentServerPort",
				Integer.toString(portNumber));
		}
		
		orb = ORB.init(args, properties);
		
		// ORB profiler
		if (orb instanceof AcsProfilingORB) {
			// This profiler will log ORB resource statistics every 10 seconds,
			AcsORBProfiler orbProfiler = new AcsORBProfilerImplBase(m_logger) {
				{
//	currently no additional call logging, could be added though via a new property
//					debugRequestStarted = true;
//					debugRequestFinished = true;
				}
			};
			((AcsProfilingORB)orb).registerAcsORBProfiler(orbProfiler);
		}
			
		// POA stanza, use rootPOA

		// resolve RootPOA
		rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
			
		org.omg.CORBA.Policy[] policies = new org.omg.CORBA.Policy[2];

		policies[0] =
			rootPOA.create_id_assignment_policy(
				IdAssignmentPolicyValue.USER_ID);
		if (useJacORB)
			policies[1] =
				rootPOA.create_lifespan_policy(
					LifespanPolicyValue.PERSISTENT);
		else
			policies[1] =
				rootPOA.create_lifespan_policy(
					LifespanPolicyValue.TRANSIENT);

		asPOA =
			rootPOA.create_POA(
				"asPOA",
				rootPOA.the_POAManager(),
				policies);

		for (int i = 0; i < policies.length; i++)
			policies[i].destroy();

		// activate POA
		poaManager = rootPOA.the_POAManager();
		poaManager.activate();
	}

	/**
	 * Overloads the destroy to first perform a ORB shutdown.
	 */
	public void destroy()
	{
		destroyState = true;
		
		// destory ORB
		if (orb != null)
		{
			try
			{
				// possible solution with orb.work_pending
				// but JacORB has no implementation of it
				
				// do not wait for completion
				orb.shutdown(false);
				
				// and finally destroy
				orb.destroy();
				orb = null;
			}
			catch (Throwable th) {
				// @TODO revisit org.omg.CORBA.COMM_FAILURE in ORB.shutdown() after JacORB upgrade
				//logger.log(Level.FINER, "Harmless exception caught while destroying ORB.", th);
			}
		}

	}
	
	/**
	 * Returns Object Request Broker (ORB) object.
	 * 
	 * @return		Object Request Broker (ORB) object
	 */
	public ORB getORB()
	{
		return orb;
	}

	/**
	 * Returns root Portable Object Adapter (POA) object.
	 * 
	 * @return		root Portable Object Adapter (POA) object
	 */
	public POA getRootPOA()
	{
		return rootPOA;
	}

	/**
	 * Main thread to handle CORBA requests.
	 * 
	 * @see java.lang.Runnable#run()
	 */
	public void run()
	{
		ORB localORBRef = orb;
		if (!destroyState && localORBRef != null) {
			localORBRef.run();
		}
		m_logger.log(AcsLogLevel.DEBUG,"ORB thread terminated");
	}
	
	/**
	 * Get the {@link NamingContext}
	 * 
	 * @return The {@link NamingContext}
	 */
	private NamingContext getNamingContext() throws InvalidName {
		org.omg.CORBA.Object obj=null;
		Properties props = System.getProperties();
		String nameServiceCorbaLoc=props.getProperty("ORBInitRef.NameService");
		if (nameServiceCorbaLoc!=null) {
			obj=orb.string_to_object(nameServiceCorbaLoc);
		}
		if (obj==null) {
			obj = orb.resolve_initial_references("NameService");
		}
		if (obj==null) {
			throw new NullPointerException("Error getting the reference to the NameService");
		}
		NamingContext context = NamingContextExtHelper.narrow(obj);
		if (context==null) {
			throw new NullPointerException("Got a null NamingContext while narrowing");
		}
		return context;
	}
	
	/**
	 * Get an object by browsing the name service
	 * 
	 * @param serviceName The name of the service to get from the server
	 * @return
	 * @throws org.omg.CosNaming.NamingContextPackage.InvalidName 
	 * @throws CannotProceed 
	 * @throws NotFound 
	 */
	public Object getServiceFromNameServer(final String serviceName) throws Exception {
		if (serviceName==null || serviceName.isEmpty()) {
			throw new IllegalArgumentException("Invalid null or emopty name");
		}
		try {
			NamingContext context= getNamingContext();
			NameComponent[] nameComponent= new NameComponent[1];
			nameComponent[0]=new NameComponent(serviceName,"");
			return context.resolve(nameComponent);
		} catch (Throwable t) {
			throw new Exception("Error getting "+serviceName,t);
		}
		
	}
	
	/**
     * Register the AlarmService name to the Naming Service.
     * <P>
     * The name to register is taken from the IDL interface.
     * 
     * @param laserObject The CORBA object to bind to the name
     */
    private void registerToNamingService(org.omg.CORBA.Object laserObject) throws Exception {
    	String name=alma.alarmsystem.AlarmServiceName.value;
    	m_logger.log(AcsLogLevel.DEBUG,"Registering into the naming service with name "+name);
   		NamingContext context = getNamingContext();
   		NameComponent[] nameComponent= new NameComponent[1];
   		nameComponent[0]=new NameComponent(name,"");
    		
   		context.rebind(nameComponent, laserObject);
    }
    
    /**
     * Unregister the AlarmService name to the Naming Service.
     * <P>
     * The name to unregister is taken from the IDL interface.
     */
    private void unregisterToNamingService() throws Exception {
    	String name=alma.alarmsystem.AlarmServiceName.value;
    	m_logger.log(AcsLogLevel.DEBUG,"Unregistering "+name+" from the naming service");
		NamingContext context = getNamingContext();
		NameComponent[] nameComponent= new NameComponent[1];
		nameComponent[0]=new NameComponent(name,"");
		
		context.unbind(nameComponent);
    }

    /**
     * 
     * @return The logger
     */
	public Logger getLogger() {
		return m_logger;
	}

//
//	/**
//	 * @param cbServant
//	 * @throws ContainerException
//	 */
//	private void checkOffShootServant(Servant servant) throws AcsJContainerServicesEx {
//		if (servant == null) {
//			AcsJBadParameterEx cause = new AcsJBadParameterEx();
//			cause.setParameter("servant");
//			cause.setParameterValue("null");
//			throw new AcsJContainerServicesEx(cause);
//		}		
//		
//		if (!(servant instanceof OffShootOperations)) {
//			String msg = "invalid offshoot servant provided. Must implement " + OffShootOperations.class.getName();
//			m_logger.fine(msg);
//			AcsJContainerServicesEx ex = new AcsJContainerServicesEx();
//			ex.setContextInfo(msg);
//			throw ex;
//		}
//	}
//	
//	public OffShoot activateOffShoot(Servant servant)
//			throws AcsJContainerEx, AcsJUnexpectedExceptionEx {
//		if (servant == null) {
//			String msg = "activateOffShoot called with missing parameter.";
//			AcsJContainerEx ex = new AcsJContainerEx();
//			ex.setContextInfo(msg);
//			throw ex;
//		}
//
//		POA offshootPoa = getPOAForOffshoots(rootPOA);
//
//		org.omg.CORBA.Object actObj = null;
//		try {
//			offshootPoa.activate_object(servant);
//			actObj = offshootPoa.servant_to_reference(servant);
//			actObj._hash(Integer.MAX_VALUE); // just to provoke an exc. if
//												// something is wrong with our
//												// new object
//			m_logger.finer("offshoot of type '" + servant.getClass().getName()
//					+ "' activated as a CORBA object.");
//		} catch (Throwable thr) {
//			AcsJContainerEx ex = new AcsJContainerEx(thr);
//			ex.setContextInfo("failed to activate offshoot of type '"
//					+ servant.getClass().getName());
//			throw ex;
//		}
//
//		return OffShootHelper.narrow(actObj);
//	}
//	
//	public void deactivateOffShoot(Servant cbServant) throws AcsJContainerServicesEx, AcsJContainerEx  {
//		checkOffShootServant(cbServant);
//		if (cbServant == null || rootPOA == null) {
//			String msg = "deactivateOffShoot called with missing parameter.";
//			AcsJContainerEx ex = new AcsJContainerEx();
//			ex.setContextInfo(msg);
//			throw ex;
//		}
//		
//		byte[] id = null;
//		try {
//			POA offshootPoa = getPOAForOffshoots(rootPOA);
//			id = offshootPoa.servant_to_id(cbServant);
//			offshootPoa.deactivate_object(id);
//		}
//		catch (AcsJContainerEx e) {
//			throw e;
//		}
//		catch (Throwable thr) {
//			String msg = "failed to deactivate offshoot of type '" + cbServant.getClass().getName() +
//							"' (ID=" + String.valueOf(id) + ")";
//			m_logger.log(Level.WARNING, msg, thr);
//			AcsJContainerEx ex = new AcsJContainerEx(thr);
//			ex.setContextInfo(msg);
//			throw ex;
//		}
//	}
//	
//	public POA getPOAForOffshoots(POA componentPOA) throws AcsJContainerEx,
//			AcsJUnexpectedExceptionEx {
//		final String offshootPoaName = "offshootPoa";
//		POA offshootPoa = null;
//
//		synchronized (componentPOA) {
//			try {
//				// can we reuse it?
//				offshootPoa = componentPOA.find_POA(offshootPoaName, false);
//			} catch (AdapterNonExistent e) {
//				m_logger.finest("will have to create offshoot POA");
//
//				if (m_offshootPolicies == null) {
//					m_offshootPolicies = new Policy[4];
//
//					m_offshootPolicies[0] = componentPOA
//							.create_id_assignment_policy(IdAssignmentPolicyValue.SYSTEM_ID);
//
//					m_offshootPolicies[1] = componentPOA
//							.create_lifespan_policy(LifespanPolicyValue.TRANSIENT);
//
//					m_offshootPolicies[2] = componentPOA
//							.create_request_processing_policy(RequestProcessingPolicyValue.USE_ACTIVE_OBJECT_MAP_ONLY);
//
//					m_offshootPolicies[3] = componentPOA
//							.create_servant_retention_policy(ServantRetentionPolicyValue.RETAIN);
//				}
//
//				try {
//					offshootPoa = componentPOA.create_POA(offshootPoaName,
//							poaManager, m_offshootPolicies);
//
//					m_logger.finest("successfully created offshoot POA");
//				} catch (InvalidPolicy ex) {
//					AcsJContainerEx ex2 = new AcsJContainerEx(ex);
//					ex2
//							.setContextInfo("Attempted to create offshoot POA with invalid policies.");
//					throw ex2;
//				} catch (AdapterAlreadyExists ex) {
//					// we sync on componentPOA, so this should never happen
//					throw new AcsJUnexpectedExceptionEx(ex);
//				}
//			}
//		}
//		return offshootPoa;
//	}
	
	/**
	 * Shuts down the CORBA services.
	 * <P>
	 * Note that this method is called by the alarm service as part of its shutdown
	 * sequence. 
	 * But it is also executed when the JVM shuts down (shutdown hook)
	 */
	public synchronized void shutdown() {
		if (closed) {
			return;
		}
		closed=true;
		try {
			unregisterToNamingService();
		} catch (Throwable t) {
			m_logger.log(AcsLogLevel.WARNING,"Error unbinding the alarm service from the name service",t);
		}
		try {
			m_logger.log(AcsLogLevel.DEBUG,"Shutting down the alarm service");
			if (laserComponent!=null) {
				((AlarmServiceOperations)laserComponent).shutdown();
			}
			if (acsComponent!=null) {
				((AlarmServiceOperations)acsComponent).shutdown();
			}
		} catch (Throwable t) {
			m_logger.log(AcsLogLevel.WARNING,"Error shutting down the alarm service",t);
		} finally {
			acsComponent=null;
			laserComponent=null;
		}
		m_logger.log(AcsLogLevel.DEBUG,"Shutting down ORB");
		try {
			orb.shutdown(true);
			m_logger.log(AcsLogLevel.DEBUG,"ORB shut down");
		} catch (ConcurrentModificationException ex) {
			System.out.println("ORB shutdown produced a ConcurrentModificationException (see also known JacORB bug http://www.jacorb.org/cgi-bin/bugzilla/show_bug.cgi?id=537)" + 
					" which may be related to surviving AlarmService instances we see in modular tests.");
		}
		poaManager=null;
		rootPOA=null;
		orb=null;
	}
	
	/**
	 * Get a reference to the DAL.
	 * 
	 * @return The DAL
	 * @throws Exception In case of error getting the DAL
	 */
	private DAL getCDB() throws Exception {
		NamingContext context=getNamingContext();
		NameComponent[] nameCom = new NameComponent[1];
		nameCom[0] = new NameComponent("CDB","");
		Object obj = context.resolve(nameCom);
		return DALHelper.narrow(obj);
	}
	
	/**
	 * Query the CDB to understand which one between ACS and CERN implementation
	 * has to be used
	 *  
	 * @return <code>true</code> if ACS implementation must be used
	 * @throws Exception In case of error
	 */
	private boolean getAlarmSystemType() throws Exception {
		DAL dal = getCDB();
		String str=dal.get_DAO("Alarms/Administrative/AlarmSystemConfiguration");
		StringReader strReader = new StringReader(str);
		AlarmSystemConfiguration configuration;
		try {
			configuration= AlarmSystemConfiguration.unmarshalAlarmSystemConfiguration(strReader);
		} catch (Throwable t) {
			m_logger.log(AcsLogLevel.ERROR,"Error parsing alarm configuration: using ACS alarm implementation");
			return true;
		}
		for (int propNum=0; propNum<configuration.getConfigurationPropertyCount(); propNum++) {
			ConfigurationProperty property=configuration.getConfigurationProperty(propNum);
			if (property.getName().equalsIgnoreCase("Implementation")) {
				if (property.getContent().equalsIgnoreCase("CERN")) {
					return false;
				}
			}
		}
		return true;
	}
}

