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
package alma.acs.component.client;

import java.util.logging.Level;

import junit.framework.TestCase;

import org.omg.CORBA.ORB;
import org.omg.PortableServer.POA;

import si.ijs.maci.Client;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.concurrent.DaemonThreadFactory;
import alma.acs.container.AcsManagerProxy;
import alma.acs.container.CleaningDaemonThreadFactory;
import alma.acs.container.ContainerServices;
import alma.acs.container.ContainerServicesImpl;
import alma.acs.container.corba.AcsCorba;
import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;
import alma.acs.logging.engine.LogReceiver;
import alma.acs.util.ACSPorts;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;

/**
 * Base class for writing JUnit test clients for ACS components. 
 * Takes care of the communication with the ACS manager (local or provided in property <code>ACS.manager</code>).
 *  
 * Provides the {@link ContainerServices}.
 * 
 * @author hsommer Nov 21, 2002 5:53:05 PM
 */
public class ComponentClientTestCase extends TestCase
{
	protected AcsCorba acsCorba;
	
	private ContainerServicesImpl m_containerServices;
	
	/**
	 * The thread factory used by <code>m_containerServices</code>.
	 */
	private CleaningDaemonThreadFactory m_threadFactory;
	
	/**
	 * Special tests that need to call directly the manager API could use this proxy object.
	 * To be used sparingly, as we need to exercise (and extend if necessary) the regular 
	 * classes such as ContainerServices.  
	 */
	protected AcsManagerProxy m_acsManagerProxy;
	
	protected AcsLogger m_logger;
	private LogReceiver logReceiver;

	/** from property ACS.manager, or defaults to localhost */
	protected String m_managerLoc;

	// manager client object 
	private ManagerClient managerClientImpl;
	private Client m_managerClient;

	private String m_namePrefix;


	/**
	 * Subclasses must call this ctor.
	 * @param name  the name used for the test case, and to talk with the ACS manager
	 * @throws Exception
	 */
	public ComponentClientTestCase(String name) throws Exception
	{
		super(name);
		m_namePrefix = name;
	}
	
	
	/**
	 * Executes a single test method. 
	 * Stray exceptions are logged using the test logger, so that they show in system logs.
	 * @see junit.framework.TestCase#runTest()
	 * @since ACS 6.0
	 */
	protected void runTest() throws Throwable {
		try {
			super.runTest();			
		}
		catch (Throwable thr) {
			if (m_logger != null) {
				m_logger.log(Level.WARNING, "JUnit test error in " + getFullName(), thr);
			}
			throw thr;
		}
	}
	
	/**
	 * Starts CORBA in the client process and connects to the manager and logger.
     * <p>
     * <b>Subclasses that override this method must call <code>super.setUp()</code>,
     * likely before any other code in that method.</b>
	 *  
	 * @see junit.framework.TestCase#setUp()
	 */
	protected void setUp() throws Exception
	{
		m_logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(getFullName(), true);

		m_logger.info("-------------------------------");
		m_logger.info("ComponentClientTestCase#setUp()");

		POA rootPOA = null;
		try
		{
			acsCorba = new AcsCorba(m_logger);
			rootPOA = acsCorba.initCorbaForClient(false);

			connectToManager();

			m_threadFactory = new CleaningDaemonThreadFactory(m_namePrefix, m_logger);
			m_containerServices = new ContainerServicesImpl(m_acsManagerProxy, rootPOA, acsCorba,
										m_logger, m_acsManagerProxy.getManagerHandle(), 
										this.getClass().getName(), null, m_threadFactory) {
				public AcsLogger getLogger() {
					return m_logger;
				}
			};
			managerClientImpl.setContainerServices(m_containerServices);
			initRemoteLogging();
			ACSAlarmSystemInterfaceFactory.init(m_containerServices);
		}
		catch (Exception ex1)
		{
			m_logger.log(Level.SEVERE, "failed to initialize the ORB, or connect to the ACS Manager, " + 
										"or to set up the container services.", ex1);
            if (acsCorba != null) {
            	try {
	            	acsCorba.shutdownORB(true, false);
	            	acsCorba.doneCorba();
            	} catch (Exception ex2) {
            		// to JUnit we want to forward the original exception ex1, 
            		// not any other exception from cleaning up the orb 
            		// which we would not have done without the first exception.
            		// Thus we simply print ex2 but let ex1 fly
            		ex2.printStackTrace();
            	}
            }
			throw ex1;
		}
	}


	/**
	 * Connects to the ACS Manager using {@link AcsManagerProxy}.
	 * @throws Exception
	 */
	protected void connectToManager() throws Exception
	{
		
		if (System.getProperty("ACS.manager") != null) {
			m_managerLoc = System.getProperty("ACS.manager").trim();
		}
		else {
			// default = localhost
			String host = ACSPorts.getIP();
			m_managerLoc = "corbaloc::" + host + ":" + ACSPorts.getManagerPort() + "/Manager";
		}
					
		managerClientImpl = new ManagerClient(getFullName(), m_logger)
			{
				public void disconnect()
				{
					m_logger.info("disconnected from manager");
					m_acsManagerProxy.logoutFromManager();
					m_acsManagerProxy = null;
					throw new RuntimeException("disconnected from the manager");
				}
			};
			
		ORB orb = acsCorba.getORB();		
		m_managerClient = managerClientImpl._this(orb);		
		m_acsManagerProxy = new AcsManagerProxy(m_managerLoc, orb, m_logger);
		
		m_acsManagerProxy.loginToManager(m_managerClient, false);
	}


    protected String getFullName() {
        String fullName = m_namePrefix + "#" + getName();
        return fullName;
    }	


	/**
	 * Gives access to the {@link ContainerServices} interface.
	 * This class plays the part of the role of the Java container that has to do with 
	 * providing explicit services to the component, or test case respectively.
	 * <p>
	 *  
	 * @return ContainerServices
	 */
	protected ContainerServices getContainerServices()
	{
		return m_containerServices;
	}

	/**
	 * Releases all previously obtained components (using manager), logs out from the manager, 
	 * and terminates the CORBA ORB.
	 * <p>
	 * <b>Subclasses that override this method must call <code>super.tearDown()</code>, likely after any other code in that method.</b>
	 * 
	 * @see junit.framework.TestCase#tearDown()
	 */
	protected void tearDown() throws Exception {
		try {
			m_acsManagerProxy.shutdownNotify();
			m_containerServices.releaseAllComponents();
			if (logReceiver != null && logReceiver.isInitialized()) {
				logReceiver.stop();
			}
			m_containerServices.cleanUp();
			m_acsManagerProxy.logoutFromManager();

			ClientLogManager.getAcsLogManager().shutdown(true);
		} 
		catch (Exception e) {
			System.err.println("Exception in tearDown: ");
			e.printStackTrace(System.err);
			throw e;
		} 
		finally {
			ACSAlarmSystemInterfaceFactory.done();
			m_threadFactory.cleanUp();
			if (acsCorba != null) {
				// @todo investigate COMP-2632 which happened here.
				// Check if the wait_for_completion is buggy and returns too early.
				// Then the overlapping call to doneCorba() ( -> orb.destroy()) would block this thread,
				// and with bad timing luck it could block *after* the first shutdown called "shutdown_synch.notifyAll()"
				// in ORB#shutdown, which could explain that the main thread hangs there forever.
				acsCorba.shutdownORB(true, false);
				// as a workaround for this problem, for now we run this async with a timeout
				Thread destroyThread = (new DaemonThreadFactory("OrbDestroy")).newThread(new Runnable() {
					public void run() {
						acsCorba.doneCorba();
					}
				});
				destroyThread.start();
				destroyThread.join(20000);
			}
			// just in case... should give the OS time to reclaim ORB ports and so on
			Thread.sleep(1000);
		}
	}

	
    /**
     * Sets up the test client logger(s) to send log records to the remote log service.
     * Only one attempt to connect to the remote logger is made.
     * If it fails, remote logging will be disabled.
     * <p>
     * Override this method to prevent remote logging. 
     */
    protected void initRemoteLogging() 
    {
        String errMsg = null;
        try {
            if (!ClientLogManager.getAcsLogManager().initRemoteLogging(acsCorba.getORB(), 
                m_acsManagerProxy.getManager(), m_acsManagerProxy.getManagerHandle(), false) ) {
                errMsg = "ACS central logging not available. ";
            }
        }
        catch (Throwable thr) {
            errMsg = "ACS central logging not available. Failed with unexpected error " + thr.toString();
        }
        if (errMsg != null) {
            // if we can't get remote logging, then we disable it (saves memory to not add to the log record queue)
            ClientLogManager.getAcsLogManager().suppressRemoteLogging();
            m_logger.warning(errMsg);
        }
    }
	
    
    /**
	 * Gets a {@link LogReceiver} which can be used 
	 * to verify log messages from both local and remote processes.
	 * The returned <code>LogReceiver</code> is already initialized.
	 * If initialization fails, an exception is thrown.
	 * <p>
	 * To receive logs from the log service, use either 
	 * {@link LogReceiver#getLogQueue()} or {@link LogReceiver#startCaptureLogs(java.io.PrintWriter)}.
	 *  
	 * @throws AcsJContainerServicesEx if the LogReceiver fails to initialize within 20 seconds.
	 */
	protected LogReceiver getLogReceiver() throws AcsJContainerServicesEx {
		if (logReceiver == null) {
			boolean initOk = false;
			try {
				logReceiver = new LogReceiver();
				// logReceiver.setVerbose(true);
				initOk = logReceiver.initialize(acsCorba.getORB(), m_acsManagerProxy.getManager(), 20);
			}
			catch (Throwable thr) {
				AcsJContainerServicesEx ex = new AcsJContainerServicesEx(thr);
				ex.setContextInfo("Failed to obtain an initialized LogReceiver.");
				throw ex;
			}
			if (!initOk) {
				AcsJContainerServicesEx ex = new AcsJContainerServicesEx();
				ex.setContextInfo("LogReceiver failed to initialize within 20 seconds.");
				throw ex;
			}
		}
		return logReceiver;
	}
}
