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
import java.util.logging.Logger;

import org.omg.CORBA.ORB;
import org.omg.PortableServer.POA;

import edu.emory.mathcs.backport.java.util.concurrent.ExecutorService;
import edu.emory.mathcs.backport.java.util.concurrent.Executors;

import si.ijs.maci.Client;

import alma.acs.concurrent.DaemonThreadFactory;
import alma.acs.container.AcsManagerProxy;
import alma.acs.container.ContainerServices;
import alma.acs.container.ContainerServicesImpl;
import alma.acs.container.CleaningDaemonThreadFactory;
import alma.acs.container.corba.AcsCorba;
import alma.acs.logging.ClientLogManager;


/**
 * Class that facilitates writing client application that access ACS components. 
 * Takes care of the communication with the ACS manager. 
 * Provides the {@link ContainerServices}.
 * Can be used either as a base class or as a delegate object.
 * <p>
 * Note that this class is only meant to be used within a stand-alone application
 * that needs to communicate with ACS-based components. 
 * It is definitely not meant to be used by a component running inside an ACS container.
 * 
 * @author hsommer Nov 21, 2002 5:53:05 PM
 */
public class ComponentClient 
{
	private ContainerServicesImpl m_containerServices;
    private CleaningDaemonThreadFactory m_threadFactory;
		
	protected Logger m_logger;

	private String m_clientName;

    // manager client object 
	private Client m_managerClient;
    private AcsManagerProxy m_acsManagerProxy;
            
    /**
     * The instance of {@link AcsCorba} to be used by this class for all CORBA activities. 
     */
    protected AcsCorba acsCorba;

    private boolean ownAcsCorba;
    



	/////////////////////////////////////////////////////////////
	// initialization stuff
	/////////////////////////////////////////////////////////////



	/**
	 * Normal constructor.
	 * The component client will take care of creating ORB and POAs,
	 * and of setting up remote logging.
	 * 
	 * @param logger
	 * @param managerLoc
	 * @param clientName
	 * @throws Exception
	 * @see #initRemoteLogging()
	 */
	public ComponentClient(Logger logger, String managerLoc, String clientName) 
		throws Exception 
	{
		this(logger, managerLoc, clientName, null);
	}

	
	/**
	 * Special c'tor that provides an already initialized instance of <code>AcsCorba</code>
	 * which encapsulates the ORB.
	 * This may be useful for an application such as the ALMA executive, 
	 * which works with different ORBs, tries out available ports, and so on.
	 * <p>
	 * With this constructor, also initialization and termination of remote ACS logging is left to the caller. 
	 * 
	 * @param logger  the logger to be used. If <code>null</code>, one will be created.
	 * @param managerLoc  the corbaloc for the ACS manager, e.g. "corbaloc::myhost:xxxx/Manager"
	 * @param clientName  name to be used toward the ACS Manager
	 * @param externalAcsCorba  encapsulates initialized ORB and POAs. 
	 * @throws Exception  at the slightest provocation...
	 * @see #initRemoteLogging()
	 */
	public ComponentClient(Logger logger, String managerLoc, String clientName, AcsCorba externalAcsCorba) 
		throws Exception
	{
		if (logger == null) {
			logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(getClass().getName(), true);
		}
		
		m_logger = logger;
		m_clientName = clientName;

		POA myPOA = null;
		if (externalAcsCorba == null) {
			try {
	    		acsCorba = new AcsCorba(m_logger);
	    		myPOA = acsCorba.initCorbaForClient(false);
	    		ownAcsCorba = true;
			}
			catch (IllegalStateException e) {
				throw new IllegalStateException("Class '" + ComponentClient.class.getName() + 
						"' can only be used as a singleton and outside of ACS containers. " + e.getMessage());
			}
		}		
		else {
			acsCorba = externalAcsCorba;
			myPOA = acsCorba.getRootPOA();
    		ownAcsCorba = false;
		}
		
		initAcs(managerLoc, myPOA);
		if (ownAcsCorba) {
			initRemoteLogging();
		}
		
		m_logger.fine("ready to talk with components!");
	}

    

    private void initAcs(String managerLoc, POA rootPOA) throws Exception
	{
		try
		{
			ManagerClient clImpl = new ManagerClient(m_clientName, m_logger)
				{
					public void disconnect()
					{
						m_logger.info("disconnected from manager");
						m_acsManagerProxy.logoutFromManager();
						m_acsManagerProxy = null;
						throw new RuntimeException("disconnected from the manager");
					}
				};
			m_managerClient = clImpl._this(getORB());
		
			m_acsManagerProxy = new AcsManagerProxy(managerLoc, getORB(), m_logger);
			
			m_acsManagerProxy.loginToManager(m_managerClient, false);

            m_threadFactory = new CleaningDaemonThreadFactory(m_clientName, m_logger);

			m_containerServices = new ContainerServicesImpl(m_acsManagerProxy, rootPOA, acsCorba,
										m_logger, m_acsManagerProxy.getManagerHandle(), 
										this.getClass().getName(), null, m_threadFactory);
		}
		catch (Exception ex)
		{
//			m_logger.log(Level.SEVERE, "failed to connect to the ACS Manager " + 
//										"or to set up the container services.", ex);
			if (getORB() != null) {
				getORB().destroy();
			}
			throw ex;
		}
	}

    
   
	/**
     * Sets up the client logger(s) to send log records to the remote log service.
     * This method starts a separate thread and returns immediately.
     * <p>
     * Makes repeated attempts to connect to the remote logger.
     * If they fail unexpectedly, remote logging will be disabled. 
     * <p>
     * If the <code>ComponentClient</code> has been constructed without an external <code>AcsCorba</code> object
     * (the normal case), then this method is called automatically. <br>
     * Otherwise (with an external <code>AcsCorba</code> object provided) it is assumed that also 
     * remote logging is controlled from outside of this class. If nonetheless you want to 
     * initialize remote logging, you may exlicitly call this method for convenience.
     * <p>
     * Override this method to prevent remote logging (useful only if AcsCorba is is not provided externally).
	 */
	public void initRemoteLogging() 
    {
        Runnable cmd = new Runnable() {
            public void run() {
                boolean gotLogService = false;
                try {
                    gotLogService = ClientLogManager.getAcsLogManager().initRemoteLogging(
                            getORB(), 
                            m_acsManagerProxy.getManager(), 
                            m_acsManagerProxy.getManagerHandle(),
                            true);
                } catch (Exception e) {
                    // just log below
                }
                if (!gotLogService) {
                    m_logger.log(Level.WARNING, "ACS central logging not available!");
                    ClientLogManager.getAcsLogManager().suppressRemoteLogging();
                }
            }            
        };
        ExecutorService executor = Executors.newSingleThreadExecutor(new DaemonThreadFactory("InitRemoteLogging"));
        executor.execute(cmd);
	}

	

	/////////////////////////////////////////////////////////////
	// service methods
	/////////////////////////////////////////////////////////////


	/**
	 * Gives access to the {@link ContainerServices} interface.
	 * This class plays the part of the role of the Java container that has to do with 
	 * providing explicit services to the component, or test case respectively.
	 * <p>
	 * @return ContainerServices
	 */
	public ContainerServices getContainerServices()
	{
		return m_containerServices;
	}


	/**
	 * Use only when direct access to the ORB is absolutely necessary. 
	 * We try to not expose the ORB to applications. 
	 *  
	 * @return ORB
	 */
	public ORB getORB()
	{
		return acsCorba.getORB();
	}
	
	
	/////////////////////////////////////////////////////////////
	// finalization stuff
	/////////////////////////////////////////////////////////////

	/**
	 * Must be called by the application that uses this class, when it is done using ACS components.
	 * At the latest it should be called when the application itself terminates.
	 * <p>
	 * Releases all previously obtained components (using manager), and logs out from the manager.
	 * It also stops remote logging and terminates the CORBA ORB 
	 * unless an <code>AcsCorba</code> instance was provided in the c'tor;
	 * otherwise the application remains responsible to clean up logging and the ORB.
	 */
	public void tearDown() throws Exception
	{
		m_logger.fine("will disconnect ACS stuff...");
		try
		{
			m_acsManagerProxy.shutdownNotify();
			
			m_containerServices.releaseAllComponents();
						
			m_acsManagerProxy.logoutFromManager();            
			
			if (ownAcsCorba) {
	            ClientLogManager.getAcsLogManager().shutdown(false);
				acsCorba.shutdownORB(true);
			}
            
            m_threadFactory.cleanUp();
		}
		catch (org.omg.CORBA.OBJECT_NOT_EXIST ex)
		{
			// todo: fix the bug that causes this; 
			// since it does not really matter, for now we keep a low profile outputting messages...
			m_logger.warning("ORB#destroy caused an org.omg.CORBA.OBJECT_NOT_EXIST exception; ");
		}
		catch (Exception e)
		{
			System.err.println("Exception in tearDown: ");
			e.printStackTrace(System.err);
			throw e;
		}
	}

	
}
