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
package alma.acs.container.corba;

import java.lang.reflect.Constructor;
import java.util.ConcurrentModificationException;
import java.util.Properties;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.jacorb.orb.acs.AcsORBProfiler;
import org.jacorb.orb.acs.AcsProfilingORB;
import org.omg.CORBA.Any;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Object;
import org.omg.CORBA.Policy;
import org.omg.CORBA.PolicyManager;
import org.omg.CORBA.PolicyManagerHelper;
import org.omg.CORBA.SetOverrideType;
import org.omg.Messaging.RELATIVE_RT_TIMEOUT_POLICY_TYPE;
import org.omg.PortableServer.IdAssignmentPolicyValue;
import org.omg.PortableServer.LifespanPolicyValue;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import org.omg.PortableServer.POAManager;
import org.omg.PortableServer.RequestProcessingPolicyValue;
import org.omg.PortableServer.Servant;
import org.omg.PortableServer.ServantRetentionPolicyValue;
import org.omg.PortableServer.POAManagerPackage.AdapterInactive;
import org.omg.PortableServer.POAManagerPackage.State;
import org.omg.PortableServer.POAPackage.AdapterAlreadyExists;
import org.omg.PortableServer.POAPackage.AdapterNonExistent;
import org.omg.PortableServer.POAPackage.InvalidPolicy;

import si.ijs.maci.ContainerHelper;
import alma.ACSErrTypeCommon.wrappers.AcsJUnexpectedExceptionEx;
import alma.JavaContainerError.wrappers.AcsJContainerEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.concurrent.DaemonThreadFactory;
import alma.acs.container.AcsContainer;
import alma.acs.container.ComponentServantManager;
import alma.acs.logging.AcsLogger;
import alma.acs.util.StopWatch;
import alma.acs.util.UTCUtility;


/**
 * This class contains methods to work with the ORB and POAs
 * on behalf of {@link alma.acs.container.AcsContainer} and other classes 
 * from the <code>alma.acs.container</code> or <code>alma.acs.component.client</code> package.
 * <p>
 * An instance of <code>AcsCorba</code> encapsulates an ORB instance, together with the POAs. 
 * <p>
 * POA structure:
 * <ul>
 * <li>RootPOA: does not directly host objects.
 * <li>ContainerPOA: child of RootPOA, responsible for the container.
 * <li>ComponentPOA: child of RootPOA, parent for the ComponentPOAxxx POAs, 
 *     does not directly host objects. 
 * <li>ComponentPOAxxx: child of ComponentPOA; the POA for exactly one component 
 * 	   (xxx stands for the component instance name). See {@link #createPOAForComponent(String)}.
 * <li>offshootPoa: child of a ComponentPOAxxx (created on demand); it is the POA for any number 
 *     of <code>OffShoot</code> objects for a given component. See {@link #getPOAForOffshoots(POA)}.
 * </ul>
 * <p>
 * This class must not be used directly by ACS applications such as components or component clients.
 * If you feel that you need to access ORB functionality, please request to have it provided
 * in {@link alma.acs.container.ContainerServices} or {@link alma.acs.container.AdvancedContainerServices}.
 * <p>
 * created on Nov 4, 2002 2:56:44 PM
 * @author hsommer
 */
public class AcsCorba 
{
	public static final String ORB_PROFILER_CLASS_PROPERTYNAME = "alma.acs.orb.profiler.class";

	private org.omg.CORBA.ORB m_orb;

	/**
	 * POA manager shared among RootPOA, ContainerPOA, component shared parent poa, offshoot poas, *BUT NOT with the component poas*.
	 */
	private POAManager sharedPoaManager;
	
	private POA m_rootPOA;
	private POA m_containerPOA;
	private POA m_componentPOA;
	private Policy[] m_compPolicies;
	private Policy[] m_offshootPolicies;
	

//	private boolean m_useRecoveryMode = false;
    
	private AcsLogger m_logger;
    
	private boolean m_isInitialized = false;

	private Integer orbPort;
	private Integer orbPortSearchRetry;
	private boolean isOrbChoosingPort = false;

	// copied from acscommandcenter. todo integrate better
	static final public String SYSPROP_FIRESTARTER_OAPORT = "acs.firestarter.oaport";
	protected int DEFAULT_PORT = 3075;
	protected int DEFAULT_RETRY = 5;

	
	/**
	 * ctor
	 */
	public AcsCorba(AcsLogger logger) {
		m_logger = logger;
	}

	/**
	 * 
	 * <p>
	 * Usage of parameters <code>orbPort</code> and <code>orbPortSearchRetry</code>:
	 * <pre>
	 *     orbPort | orbPortSearchRetry
	 * (a)  1234   |    null     : try port 1234-1238
	 * (b)  1234   |     3       : try port 1234-1236
	 * (c)  1234   |     0       : use port 1234
	 * (d)  1234   |    -1       : use port 1234
	 * 
	 * (e)  null   |    null     : try port 3075-3079
	 * (f)  null   |     3       : try port 3075-3077
	 * (g)  null   |     0       : use port 3075
	 * (h)  null   |    -1       : let orb choose port
	 * </pre>
	 * Notes: <ul>
	 * <li> 3075 can be overridden by system property SYSPROP_FIRESTARTER_OAPORT
	 * <li> (h) will fail miserably if an OAPort has been set in this virtual machine
	 * before
	 * </ul></p>
	 * The implementation of these port options has been moved here from acscommandcenter::Firestarter. 
	 * 
	 * @param orbPort use <code>null</code> for default
	 * @param orbPortSearchRetry use <code>null</code> for default
	 */
	public void setPortOptions(Integer orbPort, Integer orbPortSearchRetry) {
		this.orbPort = orbPort;
		this.orbPortSearchRetry = orbPortSearchRetry;

		// catch the special case
		if (this.orbPort == null && orbPortSearchRetry != null && -1 == orbPortSearchRetry.intValue()) {
			this.isOrbChoosingPort = true;
			return;
		}

		if (this.orbPort == null) {
			this.orbPort = Integer.getInteger(SYSPROP_FIRESTARTER_OAPORT, DEFAULT_PORT);
		}
		if (this.orbPortSearchRetry == null) {
			this.orbPortSearchRetry = new Integer(DEFAULT_RETRY);
		}
	}
	
	
    
    public synchronized void setLogger(AcsLogger logger) {
        m_logger = logger;
    }
    
    
//	/**
//	 * Must be called before initCorba if the container is supposed to host the CDB component
//	 * without the manager knowing about its component nature... 
//	 * see {@link alma.acs.container.ContainerServicesImplCDBHack}.
//	 * <p>
//	 * In CDB mode, the container (ORB) will use port 5 0 0 0, which violates the MACI convention
//	 * that the port for Containers must be between ($ACS_INSTANCE*100 + 3 0 5 0) and ($ACS_INSTANCE*100 + 3 0 9 9). 
//	 * Since the container initiates all communication, this should not cause any problems.
//	 * 
//	 * @see com.cosylab.cdb.jdal.Server
//	 */
//	public void prepareCDBHack()
//	{
//		// since this is a temporary hack, we don't bother about different ORBs here.
//		 
//		Properties properties = System.getProperties();
//		int portNumber = 5 0 0 0;
//		properties.put("OAPort", Integer.toString(portNumber));
//		properties.put("jacorb.implname", "ORB");
//		/*
//		 * by setting the following property, the ORB will
//		 * accept client requests targeted at the object with
//		 * key "CDB", so more readable corbaloc URLs
//		 * can be used
//		 */
//		properties.put(
//			"jacorb.orb.objectKeyMap.CDB",
//			"ORB/ComponentPOA/ComponentPOACDB/CDB");
//	}
//	
	
	/**
	 * Initializes the ORB and POAs to be used by a Java container.
	 * <p>
	 * Calls {@link #setPortOptions(Integer, Integer) setPortOptions} to ensure that for container ORBs
	 * the specified port gets used. Thus any previous settings of port finding options will be overwritten.
	 * 
	 * @param args  command line arguments for the ORB
	 * @param port  fixed port to be used by the ORB; this method will fail if the port is busy, 
	 *              since no retries with different ports will be done.
	 * @throws AcsJContainerServicesEx
	 */
	public synchronized void initCorba(String[] args, int port) throws AcsJContainerEx
	{		
		if (isInitialized()) {
			throw new IllegalStateException("Illegal call to initCorba. ORB/POAs have already been initialized.");
		}

		try {
			setPortOptions(new Integer(port), new Integer(0));
			prepareOrb(args);
			
			// for a container we need the following POAs:
			initPOAForContainer();
			initPOAForComponents();
			
			setInitialized(true);
		} catch (Throwable thr) {
			if (thr instanceof AcsJContainerEx) {
				throw (AcsJContainerEx) thr;
			}
			AcsJContainerEx ex = new AcsJContainerEx(thr);
			ex.setContextInfo("initCorba failed.");
			throw ex;
		}

		// ORB profiling setup
		try {
			// orbProfilerClassname can be set in env var "JAVA_OPTIONS_ORB_PROFILER", or acsStartJavaContainer script will use the default.
			String orbProfilerClassname = System.getProperty(ORB_PROFILER_CLASS_PROPERTYNAME); 
			if (orbProfilerClassname != null) {
				if (m_orb instanceof AcsProfilingORB) {
					Class<? extends AcsORBProfiler> orbProfilerClass = Class.forName(orbProfilerClassname).asSubclass(AcsORBProfiler.class);
					Constructor<? extends AcsORBProfiler> ctor = null;
					try {
						ctor = orbProfilerClass.getConstructor(AcsLogger.class);
					} catch (NoSuchMethodException ex) {
						ctor = orbProfilerClass.getConstructor(Logger.class);
					}
					AcsORBProfiler profiler = ctor.newInstance(m_logger);
					((AcsProfilingORB) m_orb).registerAcsORBProfiler(profiler);
					m_logger.finer("Orb profiling set up, using " + orbProfilerClassname);
				}
				else {
					m_logger.warning("Orb profiling was selected, but the currently used ORB " + m_orb.getClass().getName() + " does not support it.");
				}
			}
			else {
				m_logger.finer("Orb profiling was not selected.");
			}
		} catch (Throwable th) {
			m_logger.log(Level.WARNING, "Failed to set up ORB profiling, will run without it.", th);
		}
	}

	
	public synchronized boolean isInitialized() {
		return m_isInitialized;
	}

	private synchronized void setInitialized(boolean initialized) {
		m_isInitialized = initialized;
	}


	/**
	 * Initializes CORBA for the limited needs of a component client application.
	 * <p>
	 * If this class is used for client applications (outside a container),
	 * not all methods will be available (currently resulting in NPEs).
	 * Mainly <code>getORB()</code> is meant to be used.
	 * 
	 * @param isAdmin  if true, support for being an ACS administrator client will be created (TODO -- for Exec)
	 * @return the POA to be used by the client (happens to be the root POA)
	 * @throws Exception
	 * @see alma.acs.component.client.ComponentClient
	 */
	public synchronized POA initCorbaForClient(boolean isAdmin) 
				throws Exception
	{
		if (isInitialized()) {
			throw new IllegalStateException("Illegal call to initCorba. ORB/POAs have already been initialized.");
		}

		try {
			// todo: integrate better with #prepareORB
			OrbConfigurator orbConf = OrbConfigurator.getOrbConfigurator();
			setORB(ORB.init(orbConf.getOptions(), orbConf.getProperties()));
			initRootPoa(m_orb);
			sharedPoaManager.activate();
			
			// @TODO when switching from JacORB to some other Java ORB in the future, 
			// it may be necessary to create a new thread here and call ORB.run, 
			// as the spec recommends doing this although JacORB does not require it:
			// "Single threaded ORB implementations, and some multi-threaded ORB implementations, 
			//  need the use of the main thread in order to function properly.
			//  For maximum portability, an application should call either run or perform_work on
			//  its main thread. run may be called by multiple threads simultaneously."
			// In C++ ACS, the user is supposed to call SimpleClient.run,
			// but for Java it seems better for ACS to handle this internally.
			// Inside Java containers, this happens already via #blockOnORB
		} catch (Exception ex) {
			m_logger.log(Level.SEVERE, "failed to initialize CORBA.", ex);
			if (m_orb != null) {
				m_orb.destroy();
			}
			throw ex;
		}
		
		setInitialized(true);
		
		return m_rootPOA;
	}
	


//	/**
//	 * Initializes the <code>m_rootPOA</code> and <code>m_poaManager</code> fields 
//	 * with the root POA and its POA manager.
//	 * @throws AcsJContainerServicesEx
//	 */
//	private void initRootPoa() throws AcsJContainerServicesEx
//	{
//		m_rootPOA = null;
//		try {
//			m_rootPOA = POAHelper.narrow(m_orb.resolve_initial_references("RootPOA"));
//			
//			if (m_rootPOA != null) {
//				m_logger.finest("RootPOA initialized.");
//			}
//			else {
//				throw new AcsJContainerServicesEx("RootPOA == null");
//			}
//
//			// POA manager, same for all POAs
//			m_poaManager = m_rootPOA.the_POAManager();			
//			if (m_poaManager == null) {
//				throw new AcsJContainerServicesEx("POAManager == null");
//			}
//		}
//		catch (AcsJContainerServicesEx ex) {
//			throw ex;
//		}
//		catch (Exception ex) {
//			throw new  AcsJContainerServicesEx("Cannot resolve RootPOA: ", ex);
//		}
//
//	}

	/**
	 * Initializes the root poa on the ORB provided as an argument.
	 * Also obtains the POA manager, but does not yet initialize it. See {@link #runCorba()}.
	 * <p>
	 * Note that this method does not work on <code>m_orb</code> member variable,
	 * since in method <code>trialAndError</code> we use tentative OBRs.
	 * 
	 * @param orb
	 * @throws IllegalStateException if allocation failed, likely due to an unavailable port.
	 *         TODO: use a better fitting exception instead
	 */
	protected void initRootPoa (ORB orb) {

		m_rootPOA = null;

		try {
			m_logger.fine("calling orb.resolve_initial_references");
			
			Object obj = orb.resolve_initial_references("RootPOA");
			m_rootPOA = POAHelper.narrow(obj);

		} catch (org.omg.CORBA.ORBPackage.InvalidName exc) {
			throw new IllegalStateException("couldn't retrieve RootPOA: " + exc);
		} catch (RuntimeException exc) {
			throw new IllegalStateException("port taken?, couldn't retrieve RootPOA:" + exc);
		}


		try {
			sharedPoaManager = m_rootPOA.the_POAManager();

		} catch (Exception exc) {
			throw new IllegalStateException("Failed to get POA manager: " + exc);
		}

		m_logger.fine("POA activated");		
	}
	

	private void initPOAForContainer()
		throws AcsJContainerEx
	{
		if (m_containerPOA != null) {
			return;
		}
		
		Policy[] contPolicies = null;
		try {
			contPolicies = new Policy[4];
			
			contPolicies[0] =
				m_rootPOA.create_id_assignment_policy(IdAssignmentPolicyValue.USER_ID);
			
			contPolicies[1] =
				m_rootPOA.create_lifespan_policy(LifespanPolicyValue.PERSISTENT);
			
			contPolicies[2] =
				m_rootPOA.create_request_processing_policy(
					RequestProcessingPolicyValue.USE_ACTIVE_OBJECT_MAP_ONLY);
			
			contPolicies[3] =
				m_rootPOA.create_servant_retention_policy(ServantRetentionPolicyValue.RETAIN);
			
			m_containerPOA = m_rootPOA.create_POA("ContainerPOA", sharedPoaManager, contPolicies);
			
			if (m_containerPOA == null) {
				throw new NullPointerException("ContainerPOA reference == null");
			}
			m_logger.finest("ContainerPOA created.");
		}
		catch (Throwable thr) {
			AcsJContainerEx ex = new AcsJContainerEx(thr);
			ex.setContextInfo("ContainerPOA creation failed");
			throw ex;
		}
		finally {
			if (contPolicies != null) {
				for (Policy policy : contPolicies) {
					if (policy != null) {
						policy.destroy();
					}
				}
			}
		}
	}


	/**
	 * Creates <code>m_componentPOA</code> as a child of the root POA.
	 * This POA will be the parent of the POAs for the individual components. 
	 * Uses <code>LifespanPolicyValue.PERSISTENT</code>.
	 * 
	 * @throws AcsJContainerServicesEx
	 */
	private void initPOAForComponents()
		throws AcsJContainerEx
	{
		if (m_componentPOA != null) {
			return;
		}
		
		try {
			
			m_compPolicies = new Policy[4];
			
			m_compPolicies[0] =
				m_rootPOA.create_id_assignment_policy(IdAssignmentPolicyValue.USER_ID);
			
			m_compPolicies[1] =
				m_rootPOA.create_lifespan_policy(LifespanPolicyValue.PERSISTENT);
			
			m_compPolicies[2] =
				m_rootPOA.create_servant_retention_policy(ServantRetentionPolicyValue.RETAIN);
			
			m_compPolicies[3] =
				m_rootPOA.create_request_processing_policy(RequestProcessingPolicyValue.USE_SERVANT_MANAGER); 

			m_componentPOA = m_rootPOA.create_POA("ComponentPOA", sharedPoaManager, m_compPolicies);

			if (m_componentPOA == null) {
				throw new NullPointerException("ComponentPOA reference == null");
			}
			m_logger.finest("ComponentPOA created.");
		}
		catch (Throwable thr) {
			AcsJContainerEx ex = new AcsJContainerEx(thr);
			ex.setContextInfo("ComponentPOA creation failed");
			throw ex;
		}
	}
	

	/**
	 * Runs the ORB and POAs.
	 * Currently it calls <code>activate</code> on the poa manager.
	 * @throws AcsJContainerServicesEx
	 */
	public void runCorba() throws AcsJContainerEx
	{
		try {
			sharedPoaManager.activate();
		}
		catch (Throwable thr) {
			AcsJContainerEx ex = new AcsJContainerEx(thr);
			ex.setContextInfo("running CORBA failed");
			throw ex;
		}
	}

	/**
	 * Calls {@link ORB#run()} which from the point of view of the caller blocks the thread until the ORB has shut down,
	 * optionally performing tasks for the ORB in the meantime (see Corba spec 2.4 , chap. 4.2.3.3)
	 */
	public void blockOnORB()
	{
		getORB().run();
	}

	/**
	 * Calls <code>m_orb.shutdown(wait_for_completion)</code>.
	 * <p>
	 * If this method is invoked from an ORB thread (as it happens in the Java container in case of shutdown being
	 * commanded by the manager), it returns immediately, independently of the <code>wait_for_completion</code>
	 * parameter. The <code>Orb#shutdown</code> call will then run asynchronously. Note that shutting down the ORB from
	 * within an ORB thread would result in a deadlock (or BAD_INV_ORDER exception) if <code>wait_for_completion</code>
	 * is true. The Java container will nonetheless wait for the ORB shutdown, because its main thread blocks on ORB#run
	 * and only continues when the ORB is shut down.
	 * 
	 * @Todo: As a workaround for a JacORB bug (http://www.jacorb.org/cgi-bin/bugzilla/show_bug.cgi?id=537), we
	 *        currently skim off the <code>ConcurrentModificationException</code> that the unsynchronized HashMap of
	 *        <code>ClientConnectionManager.shutdown</code> may throw. This should be removed when we upgrade JacORB.
	 * @param wait_for_completion
	 *            blocks this call until ORB has shut down. Will be effectively <code>false</code> if
	 *            <code>isOrbThread == true</code>.
	 * @param isOrbThread
	 *            must be <code>true</code> if the calling thread services a Corba invocation, e.g. to
	 *            {@link AcsContainer#shutdown}. 
	 *            Note that JacORB has the proprietary method <code>isInInvocationContext</code>, but we need the explicit flag
	 *            in order to stay ORB-independent.
	 */
	public void shutdownORB(final boolean wait_for_completion, boolean isOrbThread) {
		if (m_orb != null) {
			if (isOrbThread) {
				Runnable cmd = new Runnable() {
					public void run() {
						try {
							// since m_orb.shutdown runs asynchronously anyway, there is no advantage in passing
							// wait_for_completion even if that flag is set to true.
							m_orb.shutdown(false);
						} catch (ConcurrentModificationException ex) {
							// ignore, see javadoc
						}
					}
				};
				(new DaemonThreadFactory("ShutdownORB")).newThread(cmd).start();
			} else {
				// not an ORB thread, thus we can call shutdown directly
				try {
					if (wait_for_completion) {
						// There appears to be a bug in JacORB that makes "orb.shutdown(true)" return too early (before shutdown completed),
						// which then depending on timing can make a subsequent call to orb#destroy hang.
						// Here we try out some additional synch'ing through Orb#run, and use a timeout to avoid blocking forever.
						// However the premature return could be caused by the occasional ConcurrentModificationException (see comment on JacORB bug)
						// in which case the additional orb shutdown synchronization on return from orb.run is useless.
						// For ACS 8.0 we use both strategies though, the additional synch'ing and a sleep delay when catching ConcurrentModificationException.
						final CountDownLatch synch1 = new CountDownLatch(1);
						final CountDownLatch synch2 = new CountDownLatch(1);
						Runnable cmd = new Runnable() {
							public void run() {
								synch1.countDown();
								m_orb.run();
								synch2.countDown();
							}
						};
						// Start this ORB-blocking thread, and wait until it actually runs.
						(new DaemonThreadFactory("WorkaroundBlockOnOrbTillShutdownCompletes")).newThread(cmd).start();
						try {
							synch1.await();
							// sleep one second to minimize the risk that orb.run() has not made it to the point where it blocks
							// when we call orb.shutdown next
							Thread.sleep(1000);
						} catch (InterruptedException ex1) {
							// loggers probably don't work any more even though we have not called orb.shutdown yet,
							// but the log manager and handlers has likely been flushed and shut down already
							System.out.println("Failed to wait for the thread that should call orb.run to synch with ORB shutdown. " + ex1.toString());
						}

						// this call should only return when the ORB has shut down, but it may return too early
						m_orb.shutdown(true);

						// second line of defense, hoping that orb.run is not called prematurely even if
						// orb.shutdown(true) returned too soon.
						try {
							boolean orbRunUnblocked = synch2.await(30, TimeUnit.SECONDS);
							if (!orbRunUnblocked) {
								// Loggers cannot be expected to work at this point, thus printing to stdout
								System.out.println("Failed to return within 30 s from orb.run() after ORB shutdown.");
							}
						} catch (InterruptedException ex) {
							// Loggers cannot be expected to work at this point, thus printing to stdout
							System.out.println("InterruptedException waiting for orb.run() to return after ORB shutdown. " + ex.toString());
						}
					} else { // don't wait for orb shutdown to complete...
						m_orb.shutdown(false);
					}
				} catch (ConcurrentModificationException ex) {
					System.out.println("Caught a ConcurrentModificationException from ORB shutdown. "
									+ "This should be harmless, see known JacORB bug http://www.jacorb.org/cgi-bin/bugzilla/show_bug.cgi?id=537");
					if (wait_for_completion) {
						// we try to compensate for leaving orb.shutdown too early by sleeping for a few seconds.
						// It is not clear though if this will improve the chances that a subsequent orb.destroy will not hang.
						// If it still hangs, we could try to sleep longer, or to throw a "shutdown failed" exception 
						// which the client could interpret to not call orb.destroy
						try {
							Thread.sleep(2000);
						} catch (InterruptedException ex1) {
							System.out.println("InterruptedException while sleeping after the ConcurrentModificationException in orb.shutdown");
						}
					}
				}
			}
		}
	}

	
	/**
	 * Calls <code>m_orb.destroy()</code>. 
	 * This method must not be invoked from an ORB invocation thread!
	 * <p>
	 * The Corba spec v. 2.4 says in section 4.2.3:
	 * <ul>
	 * <li> {@link ORB#shutdown(boolean)} causes all object adapters to be destroyed, since they cannot exist 
	 * in the absence of an ORB. Shut down is complete when all ORB processing 
	 * (including request processing and object deactivation or other operations associated with object adapters) 
	 * has completed and the object adapters have been destroyed.
	 * In the case of the POA, this means that all object etherealizations have finished and root POA has been destroyed 
	 * (implying that all descendent POAs have also been destroyed).
	 * <li> {@link ORB#destroy()} destroys the ORB so that its resources can be reclaimed by the application.
	 * <li> For maximum portability and to avoid resource leaks, an application should always call shutdown and destroy 
	 *      on all ORB instances before exiting.
	 * <li> Once an ORB has been destroyed, another call to ORB_init with the same ORBid will return a reference to a newly constructed ORB.
	 * <li> If destroy is called on an ORB that has not been shut down, it will start the shut down
     *      process and block until the ORB has shut down before it destroys the ORB.
	 * </ul>
	 * <p>
	 * With ACS 8.0, this method no longer attempts to destroy policy objects, trusting that either an explicit call
	 * to {@linkplain AcsCorba#shutdownORB(boolean, boolean)} or the abovementioned implicit call to {@linkplain ORB#shutdown(boolean)}
	 * takes care of this.
	 * <p> 
	 * See also problems reported in COMP-2632.
	 */
	public void doneCorba() {
		if (m_orb != null) {
			// Loggers cannot be expected to work at this point, thus any printing would have to go to stdout.
			// For backward compatibility we don't print these messages though, which before ACS 8.0 were dysfunctional logs.
			//System.out.println("about to destroy the ORB");
			try {
				m_orb.destroy();
				//System.out.println("ORB destroyed successfully.");
			} catch (Exception ex) {
				System.out.println("Exception occured during destruction of the ORB.\n" + ex.toString());
			}
		}
	}

	private void setORB(ORB orb) throws IllegalArgumentException {
		if (orb == null) {
			throw new IllegalStateException("ORB reference must not be null.");
		}
		this.m_orb = orb;
	}
	
	/**
	 * Returns the ORB instance.
	 * @return ORB
	 * @throws IllegalStateException if the ORB reference is missing or has not been initialized before. 
	 */
	public ORB getORB()
	{
		if (m_orb == null) {
			throw new IllegalStateException("ORB reference does not exist.");
		}
		if (!isInitialized()) {
			throw new IllegalStateException("ORB has not been initialized.");
		}
		return m_orb;
	}
	
	
	/**
	 * @return  the root POA used by the encapsulated ORB
	 * @throws IllegalStateException if the ORB has not been initialized before. 
	 */
	public POA getRootPOA() {
		getORB(); // to check init status
		return m_rootPOA;
	}

	
	/**
	 * Activates the container using the respective POA, so that the container
	 * becomes a CORBA object.
	 * 
	 * @param container  the container servant
	 * @param name	 a name assigned to the container, used as the CORBA id.
	 * @return the container CORBA object, never null
	 * @throws AcsJContainerEx  if args are null or the activation fails for whatever reason.
	 */
	public org.omg.CORBA.Object activateContainer(AcsContainer container, String name)
		throws AcsJContainerEx
	{
		if (name == null || name.length() == 0 || container == null) {
			String msg = "activateContainer called with missing parameter.";
			AcsJContainerEx ex = new AcsJContainerEx();
			ex.setContextInfo(msg);
			throw ex;
		}
		m_logger.finer("entering activateContainer name=" + name);

		org.omg.CORBA.Object actObj = null;

		try {
			byte[] id = name.getBytes();
			// container is a CORBA Servant...
			m_containerPOA.activate_object_with_id(id, container);
			actObj = m_containerPOA.servant_to_reference(container);
			actObj._hash(Integer.MAX_VALUE); // just to provoke an exc. if something is wrong with our new object
		}
		catch (Throwable thr) {
			AcsJContainerEx ex = new AcsJContainerEx(thr);
			ex.setContextInfo("failed to activate container object " + name);
			throw ex;
		}

		return actObj;
	}

	/**
	 * Encapsulates {@link POA#servant_to_reference(Servant)} for the container servant.
	 * @since ACS 8.1 when {@link Servant#_this_object(ORB)} was replaced by {@link POA#servant_to_reference(Servant)}
	 *        in {@link AcsContainer#loginToManager} to avoid a second activation of the container servant, on the RootPOA.
	 */
	public si.ijs.maci.Container getContainerCorbaRef(AcsContainer container)
		throws AcsJContainerEx
	{
		try {
			return ContainerHelper.narrow(m_containerPOA.servant_to_reference(container));
		} catch (Exception ex) { // ServantNotActive, WrongPolicy (needs RETAIN and UNIQUE_ID or IMPLICIT_ACTIVATION)
			AcsJContainerEx ex2 = new AcsJContainerEx(ex);
			ex2.setContextInfo("failed to retrieve corba ref for container servant " + container.name());
			throw ex2;
		}
	}
	
	
	/**
	 * Creates a new POA that is responsible for exactly one component.
	 * Dependent CORBA objects (offshoots) are activated through a child POA. 
	 * The new POA is created as a child of the shared ComponentPOA.
	 * <p>
	 * The new component POA uses its own POA manager, which allows discarding requests for this component 
	 * (w/o discarding requests for other components, the container, or even this component's offshoots).  
	 * 
	 * @param compName  the name of the component; used to construct the POA name by prepending "ComponentPOA_"
	 * @return the new component POA; never null.
	 * @throws AcsJContainerServicesEx  if creation of the POA fails.
	 */
	public POA createPOAForComponent(String compName)
		throws AcsJContainerEx
	{
		if (m_componentPOA == null) {
			throw new IllegalStateException("Must call 'initPOAForComponents()' before 'createPOAForComponent'.");
		}
		POA compChildPOA = null;
		String compChildPOAName = "ComponentPOA_" + compName;
		try
		{
			try
			{
				// will create its own POAManager
				compChildPOA = m_componentPOA.create_POA(compChildPOAName, null, m_compPolicies);
			}
			catch (org.omg.PortableServer.POAPackage.AdapterAlreadyExists ex1)
			{
				// todo: perhaps better create a new POA, since the old POA of the same name may belong 
				// to a shutting down component for which etherealization timed out.
				// Perhaps mark a component POA as "useless" once manager calls deactivate on the container
				m_logger.warning(compChildPOAName + " already exists even though it should not. Will try to reuse it...");
				compChildPOA = m_componentPOA.find_POA(compChildPOAName, true);
			}
			compChildPOA.the_POAManager().activate(); 
		}
		catch (Throwable thr) {
			AcsJContainerEx ex = new AcsJContainerEx();
			ex.setContextInfo("failed to create POA for component " + compName);
			throw ex;
		}
		return compChildPOA;
	}

	/**
	 * Creates a servant manager of type <code>ComponentServantManager</code>,
	 * attaches it to the POA, and activates the servant manager.
	 * <p>
	 * This <code>ComponentServantManager</code> can be used as a semaphore to synchronize 
	 * with component etherealization during POA destruction.
	 * <p>
	 * Note that {@link org.omg.PortableServer.POAOperations#get_servant_manager()} will not return 
	 * the <code>ComponentServantManager</code> implementation class, but instead a proxy object (_ServantActivatorStub).
	 * @param componentPOA a component POA 
	 * 
	 * @return the new <code>ComponentServantManager</code>.
	 * @throws AcsJContainerServicesEx 
	 */
	public ComponentServantManager setServantManagerOnComponentPOA(POA componentPOA) throws AcsJContainerEx {
		ComponentServantManager servantManager;
		try {
			servantManager = new ComponentServantManager(m_logger);
			componentPOA.set_servant_manager(servantManager);
		} catch (Throwable thr) {
			String msg = "Failed to set a servant activator on the component POA " + componentPOA.the_name();
			m_logger.log(Level.FINE, msg, thr);
			AcsJContainerEx ex = new AcsJContainerEx(thr);
			ex.setContextInfo(msg);
			throw ex;
		}
		return servantManager;
	}

	
	/**
	 * Creates (or reuses) the non-persistent POA for offshoot objects as a child of the given POA.
	 * In spite of the suggestive name 'componentPOA', this POA can actually be any kind of POA,
	 * so that also client application can use the offshoot mechanism. 
	 * <p>
	 * This method is synchronized inside to avoid race conditions between several offshoot POA creation/retrievals,
	 * where otherwise the POA would fail to be created even though first it was not found for reuse.
	 * 
	 * @param componentPOA  the POA of the component that owns the offshoot object;
	 * 							 will become the parent of the offshoot POA.
	 * @return POA  non-persistent POA for offshoot objects for the current component.
	 * 
	 * @throws AcsJContainerServicesEx
	 */
	public POA getPOAForOffshoots(POA componentPOA)
		throws AcsJContainerEx, AcsJUnexpectedExceptionEx
	{
		final String offshootPoaName = "offshootPoa";
		POA offshootPoa = null;
		
		synchronized (componentPOA) {
			try {
				// can we reuse it? 
				offshootPoa = componentPOA.find_POA(offshootPoaName, false);
			}
			catch (AdapterNonExistent e) {
				m_logger.finest("will have to create offshoot POA");
				
				if (m_offshootPolicies == null) 
				{
					m_offshootPolicies = new Policy[4];
				
					m_offshootPolicies[0] =
					componentPOA.create_id_assignment_policy(IdAssignmentPolicyValue.SYSTEM_ID);
				
					m_offshootPolicies[1] =
					componentPOA.create_lifespan_policy(LifespanPolicyValue.TRANSIENT);
				
					m_offshootPolicies[2] =
					componentPOA.create_request_processing_policy(
							RequestProcessingPolicyValue.USE_ACTIVE_OBJECT_MAP_ONLY);
				
					m_offshootPolicies[3] =
					componentPOA.create_servant_retention_policy(ServantRetentionPolicyValue.RETAIN);
				}
	
				try {
					offshootPoa = componentPOA.create_POA(
						offshootPoaName, sharedPoaManager, m_offshootPolicies);
										
					m_logger.finest("successfully created offshoot POA");
				}
				catch (InvalidPolicy ex) {
					AcsJContainerEx ex2 = new AcsJContainerEx(ex);
					ex2.setContextInfo("Attempted to create offshoot POA with invalid policies.");
					throw ex2;
				} 
				catch (AdapterAlreadyExists ex) {
					// we sync on componentPOA, so this should never happen
					throw new AcsJUnexpectedExceptionEx(ex);
				}
			}
		}
		return offshootPoa;		
	}


	/**
	 * Activates a component using a given component POA.
	 * @param servant
	 * @param name
	 * @param compPOA
	 * @return the component as a CORBA object
	 * @throws AcsJContainerServicesEx
	 */
	public org.omg.CORBA.Object activateComponent(Servant servant, String name, POA compPOA)
		throws AcsJContainerEx
	{
		if (name == null || name.length() == 0 ||
			servant == null || compPOA == null)
		{
			AcsJContainerEx ex = new AcsJContainerEx();
			ex.setContextInfo("activateComponent called with missing parameter.");
			throw ex;
		}
		m_logger.finer("entering activateComponent: name=" + name);
	
		org.omg.CORBA.Object actObj = null;
		try {
			byte[] id = name.getBytes();
			compPOA.activate_object_with_id(id, servant);
			actObj = compPOA.servant_to_reference(servant);
			actObj._hash(Integer.MAX_VALUE); // just to provoke an exc. if something is wrong with our new object
			m_logger.finer("component '" + name + "' activated as CORBA object.");
		}
		catch (Throwable thr) {
			AcsJContainerEx ex = new AcsJContainerEx(thr);
			ex.setContextInfo("failed to activate component " + name);
			throw ex;
		}
	
		return actObj;
	}

	
	/**
	 * Deactivates a component's POA manager. 
	 * The effect is that no further calls will reach the component.
	 * This method returns immediately if the POA manager is already inactive.
	 * Otherwise it will only return when the active requests are done.
	 * <p>
	 * Note for JacORB (1.4 as well as 2.2.4): there seems to be a problem in 
	 * <code>org.jacorb.poa.RequestController#waitForCompletion</code> with local calls (e.g. collocated component). 
	 * Instead of duly waiting for completion, that method returns immediately when such calls are still executing. 
	 * Even worse, <code>RequestController#activeRequestTable</code> seems to never get touched in case of local calls.
	 * There is a currently commented out JUnit test that verifies this problem.
	 * <p>
	 * The purpose of this method is to allow the container to "drain" a component of requests,
	 * so that <code>cleanUp</code> can be called while no functional calls are running or can come in.
	 * An alternative to using the component POA manager could be to destroy the component POA before
	 * calling <code>cleanUp</code>. This has the disadvantage of also destroying the offshoot child POA,
	 * which is needed to properly clean up callback connections.
	 * <p>
	 * Note that {@link POAManager#deactivate(boolean, boolean)} is called in a separate thread,
	 * so that this method itself may well be called from an ORB thread.
	 * This method uses <code>etherealize_objects=false</code> and <code>wait_for_completion=true</code>.
	 * 
	 * @param compPOA the component POA
	 * @param compName component instance name
	 * @param timeoutMillis timeout in milliseconds after which this call returns even if the POA manager is not inactive yet.
	 * @return true if the POA manager is inactive.
	 */
	public boolean deactivateComponentPOAManager(POA compPOA, final String compName, int timeoutMillis) {
		final POAManager compPOAManager = compPOA.the_POAManager();
		if (compPOAManager.get_state() == State.INACTIVE) {
			return true;
		}
		final CountDownLatch deactivateSyncer = new CountDownLatch(1);
		// todo: use thread pool instead of always creating a thread for this purpose
		Thread discardRequestsThread = new Thread(new Runnable() {
			public void run() {
				try {
					// note that deactivate(wait_for_completion=true) must not be called from an ORB thread, 
					// thus we use a separate thread here. This is quite annoying because 
					// at least in JacORB's implementation, another new thread is created inside deactivate
					compPOAManager.deactivate(false, true);
//					compPOAManager.discard_requests(true);
					deactivateSyncer.countDown();
				} catch (AdapterInactive e) {
					m_logger.log(Level.INFO, "Failed to finish and reject requests for component " + compName, e);
				}
			}
		});		
		discardRequestsThread.setDaemon(true);
		discardRequestsThread.setName("deactivatePOAManager_" + compName);
		StopWatch stopWatch = null;
		if (m_logger.isLoggable(Level.FINEST)) {
			stopWatch = new StopWatch();
		}
		discardRequestsThread.start();
		boolean isInactive = false;
		try {
			isInactive = deactivateSyncer.await(timeoutMillis, TimeUnit.MILLISECONDS);
		} catch (InterruptedException e) {
			// isInactive == false
		} finally {
			if (m_logger.isLoggable(Level.FINEST)) {
				long deactivationTime = stopWatch.getLapTimeMillis();
				String msg = "POA manager deactivation for component '" + compName + "' was " + (isInactive ? "" : "*not* ") + 
				"successful and took " + deactivationTime + " ms. " +
				"The component can" + (isInactive ? "not" : "") + " receive further calls over CORBA.";
				m_logger.finest(msg);
			}

		}
		return isInactive;
	}
	
	
	/**
	 * Destroys the component POA (and its child the offshoot POA if it exists), 
	 * waiting at most <code>timeoutMillis</code> milliseconds for active requests to terminate. 
	 * 
	 * @param compPOA the component POA
	 * @param compServantManager the custom servant manager of that POA
	 * @param timeoutMillis the timeout in milliseconds after which the call returns even if the POA has not been destroyed.
	 * @return true if the component was etherealized within the given time, false otherwise
	 * @see ComponentServantManager#waitForEtherealize(int)
	 */
	public boolean destroyComponentPOA(POA compPOA, ComponentServantManager compServantManager, int timeoutMillis) {
		compServantManager.resetWaitForEtherealize();
		// wait_for_completion == false: we synchronize instead by using a special servant manager.
		// Note that wait_for_completion must be false when calling from an ORB thread (otherwise BAD_INV_ORDER with standard minor code 3), 
		// thus by sync'ing with etherialization we avoid creating an additional thread which would be needed to trick the ORB-thread-check.
		synchronized (compPOA) {
			compPOA.destroy(true, false);
		}
		// sync with component etherealization.
		// Note that a steady stream of calls to this component can effectively prevent deactivation,
		// therefore we use a timeout.
		return compServantManager.waitForEtherealize(timeoutMillis);
	}

	/**
	 * Activates an offshoot object (which is a regular CORBA object
	 * owned by a component).
	 * <p>
	 * All offshoot objects for a component are activated using a non-persistent "offshootPOA".
	 *  
	 * @param servant  the offshoot servant.
	 * @param compPOA  the POA responsible for the component which activates the offshoot.
	 * @return the activated offshoot corba object.
	 * @throws AcsJContainerServicesEx
	 */
	public org.omg.CORBA.Object activateOffShoot(Servant servant, POA compPOA)
		throws AcsJContainerEx, AcsJUnexpectedExceptionEx
	{
		if (servant == null || compPOA == null) {
			String msg = "activateOffShoot called with missing parameter.";
			AcsJContainerEx ex = new AcsJContainerEx();
			ex.setContextInfo(msg);
			throw ex;
		}
		
		POA offshootPoa = getPOAForOffshoots(compPOA);

		org.omg.CORBA.Object actObj = null;
		try
		{
			offshootPoa.activate_object(servant);
			actObj = offshootPoa.servant_to_reference(servant);
			actObj._hash(Integer.MAX_VALUE); // just to provoke an exc. if something is wrong with our new object
			m_logger.finer("offshoot of type '" + servant.getClass().getName() + 
				"' activated as a CORBA object.");
		}
		catch (Throwable thr)
		{
			AcsJContainerEx ex = new AcsJContainerEx(thr);
			ex.setContextInfo("failed to activate offshoot of type '" + 
				servant.getClass().getName());
			throw ex;
		}
	
		return actObj;
	}

	
	public void deactivateOffShoot(Servant servant, POA compPOA) throws AcsJContainerEx {

		if (servant == null || compPOA == null) {
			String msg = "deactivateOffShoot called with missing parameter.";
			AcsJContainerEx ex = new AcsJContainerEx();
			ex.setContextInfo(msg);
			throw ex;
		}
		
		byte[] id = null;
		try {
			POA offshootPoa = getPOAForOffshoots(compPOA);
			id = offshootPoa.servant_to_id(servant);
			offshootPoa.deactivate_object(id);
		}
		catch (AcsJContainerEx e) {
			throw e;
		}
		catch (Throwable thr) {
			String msg = "failed to deactivate offshoot of type '" + servant.getClass().getName() +
							"' (ID=" + String.valueOf(id) + ")";
			m_logger.log(Level.WARNING, msg, thr);
			AcsJContainerEx ex = new AcsJContainerEx(thr);
			ex.setContextInfo(msg);
			throw ex;
		}
	}


	/**
	 * Sets the roundtrip timeout for all calls going out through the ORB that is encapsulated by this class.
	 * For example, this applies to all corba calls made by a container and any of its components.
	 * @param timeoutSeconds
	 * @throws AcsJContainerServicesEx
	 * @see {@link #wrapForRoundtripTimeout(Object, double)}
	 */
	public void setORBLevelRoundtripTimeout(double timeoutSeconds) throws AcsJContainerServicesEx {
		
		if (!isInitialized()) {
			throw new IllegalStateException("Only call when this object has been initialized!");
		}
		
		try
		{
			Any rrtPolicyAny = m_orb.create_any();
			rrtPolicyAny.insert_ulonglong(UTCUtility.durationJavaMillisToOmg((long)timeoutSeconds*1000));
			Policy p = m_orb.create_policy(RELATIVE_RT_TIMEOUT_POLICY_TYPE.value, rrtPolicyAny);
			// about PolicyManager, see Corba spec (2.4) section 4.9.1
			PolicyManager pm = PolicyManagerHelper.narrow(m_orb.resolve_initial_references("ORBPolicyManager"));
			pm.set_policy_overrides(new Policy[]{ p }, SetOverrideType.SET_OVERRIDE);
			p.destroy();
		}
		catch (Throwable thr) {
			AcsJContainerServicesEx ex2 = new AcsJContainerServicesEx(thr);
			ex2.setContextInfo("Failed to set the ORB-level client-side corba roundtrip timeout to " + timeoutSeconds);
			throw ex2;
		}
		m_logger.fine("Set ORB level roundtrip timeout to " + timeoutSeconds + " seconds.");
	}

	
	/**
	 * <p>
	 * Impl note: The corba spec (v 2.4, 4.3.8.1) describes the difference between 
	 * SetOverrideType.SET_OVERRIDE and SetOverrideType.ADD_OVERRIDE. It is not clear to me (HSO) 
	 * which one should be used, or if there is no practical difference.
	 * @param corbaRef
	 * @param timeoutSeconds
	 * @return
	 * @throws AcsJContainerServicesEx
	 * @see {@link #setORBLevelRoundtripTimeout(double)}
	 */
	public org.omg.CORBA.Object wrapForRoundtripTimeout(org.omg.CORBA.Object corbaRef, double timeoutSeconds) throws AcsJContainerServicesEx {
		
		if (!isInitialized()) {
			throw new IllegalStateException("Only call when this object has been initialized!");
		}
		
		org.omg.CORBA.Object ret = null;
		try
		{
			Any rrtPolicyAny = m_orb.create_any();
			rrtPolicyAny.insert_ulonglong(UTCUtility.durationJavaMillisToOmg((long)timeoutSeconds*1000));
			Policy p = m_orb.create_policy(RELATIVE_RT_TIMEOUT_POLICY_TYPE.value, rrtPolicyAny);
			ret = corbaRef._set_policy_override (new Policy[]{ p }, SetOverrideType.SET_OVERRIDE);
			p.destroy();
		}
		catch (Throwable thr) {
			AcsJContainerServicesEx ex2 = new AcsJContainerServicesEx(thr);
			ex2.setContextInfo("Failed to set the object-level client-side corba roundtrip timeout to " + timeoutSeconds);
			throw ex2;
		}
		return ret;
	}

	
	
	/////////////////////////////////////////////////////////////////////////////////////////////
	////// code integrated from Exec::Firestarter.java 
	/////////////////////////////////////////////////////////////////////////////////////////////	

	/**
	 * Gets an ORB and initializes the root POA using {@link #initRootPoa(ORB)}.
	 * <p>
	 * If this method has been called before, the same ORB will be returned.
	 * Depending on the values given in {@link #setPortOptions(Integer, Integer)},
	 * a fixed port will be used (resulting in a failure if it can't be obtained),
	 * or the port will be chosen from a given range, or the ORB will be allowed to pick a port.
	 * <p>
	 * This class is <i>fail-fast</i> in that it tries to create an orb
	 * instantly on a call to this method. If this first call goes well, subsequent calls
	 * will succeed (aside from other general errors).
	 * 
	 * @param args 
	 * @throws AcsJContainerServicesEx 
	 */
	public void prepareOrb(String[] args) {
		
		if (m_orb == null) {

			// deal with the special case
			if (this.isOrbChoosingPort) {

				ORB orb = createOrb(args, null);
				initRootPoa(orb);
				setORB(orb);
				return;
			}

			if (this.orbPortSearchRetry.intValue() <= 0) {

				ORB orb = createOrb(args, this.orbPort);
				initRootPoa(orb);
				setORB(orb);

			} else {

				RVtrialAndError re = trialAndError(args, orbPort.intValue(), orbPortSearchRetry.intValue());
				setORB(re.orb);

			}
		}
	}


	/**
	 * Creates an ORB with a port determined by the values given in {@link #setPortOptions(Integer, Integer)}.
	 * <p> 
	 * Aside from the logger, no instance variables are used in this method.
	 *  
	 * @param args  command line arguments for the ORB
	 * @param port  If port == null, OAPort property will not be set. 
	 * @return 
	 */
	private ORB createOrb(String[] args, Integer port) {
		ORB orb = null;
		// sets CORBA options
		OrbConfigurator orbConf = OrbConfigurator.getOrbConfigurator();
		
		// IFR access is generally discouraged but needed if clients such as the sampling manager call "get_interface"
		// on a reference to a corba object inside this container.
		// @TODO: Don't set this reference if CDB value Container#useIFR is false
		orbConf.setORBInitRef("InterfaceRepository", System.getProperty("ACS.repository"));
		// orbConf.setORBInitRef("NameService", System.getProperty("ACS.???")); // currently the JacORB-specific property ORBInitRef.NameService is set instead (acsStartJava)
		
		orbConf.setOptions(args);
		if (port != null) {
			orbConf.setPort(port.intValue());
		}
		String[] orbOpts = orbConf.getOptions();

		/*
		 * We start an ORB, which needs its own port ('OAPort'). There are, however, no
		 * conventions in Acs which port to assign to our ORB (which will be used by a
		 * ComponentClient or a Supervisor client). There is, on the other hand, an
		 * automatism in an ORB that finds free ports.
		 * 
		 * Thus: Don't set the OAPort property, instead let ORB choose the port itself
		 * 
		 * Problem: jacorb won't choose a port by itself when an OAPort has ever been set in
		 * this VM before
		 */
        boolean suppressPortProperty = (port==null);
        Properties orbProps = orbConf.getProperties(suppressPortProperty);
        
        StringBuffer logBuf = new StringBuffer("ORB options ");
        for (int i = 0; i < orbOpts.length; i++) {
        	logBuf.append(orbOpts[i]).append(' ');
        }
        logBuf.append("  ORB properties: ");
        logBuf.append(orbProps.toString());
        m_logger.finer(logBuf.toString());
        
        orb = org.omg.CORBA.ORB.init(orbOpts, orbProps);
        if (orb == null) {
        	// seems like this should never happen, but got strange reports about NPE when archive MC creates Corba Any for alarms,
        	// so better check here and fail fast.
        	throw new NullPointerException("org.omg.CORBA.ORB.init returned null.");
        }
        m_logger.finer("ORB initialized.");
        return orb;
	}



	// ==========================================================
	// ORB - trial and error logic for finding a free port
	// ==========================================================


	protected class RVtrialAndError {

		public ORB orb;
		public int port;

		public RVtrialAndError(ORB o, int p) {
			orb = o;
			port = p;
		}
	}

	/** trial-and-error: try out some ports, break on success */
	protected RVtrialAndError trialAndError(String[] args, int first, int retries) {

		ORB orb = null;

		int next = first;
		int i;
		for (i = 0; i < retries; i++) {
			try {
				next = first + i;
				m_logger.info("trying to start an orb on port " + next);
				
				orb = createOrb(args, new Integer(next));
				initRootPoa(orb);
				
				break;

			} catch (RuntimeException exc) {
				m_logger.finer("failed due to "+exc);
				orb.shutdown(true);
				continue;
			}
		}

		if (i == retries) {
			throw new RuntimeException("tried ports " + first + " to " + next + ": couldn't start orb; giving up");
		}

		return new RVtrialAndError(orb, next);
	}



}

