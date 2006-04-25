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

import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.CORBA.IntHolder;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Object;

import si.ijs.maci.Client;
import si.ijs.maci.ClientInfo;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ComponentSpec;
import si.ijs.maci.ComponentSpecIncompatibleWithActiveComponent;
import si.ijs.maci.IncompleteComponentSpec;
import si.ijs.maci.InvalidComponentSpec;
import si.ijs.maci.Manager;
import si.ijs.maci.ManagerHelper;
import si.ijs.maci.NoDefaultComponent;
import si.ijs.maci.ulongSeqHolder;

import alma.acs.util.ACSPorts;


/**
 * Proxy class that encapsulates access to the ACS Manager.
 * <p>
 * Not supposed to be instantiated by components and other non-framework classes. 
 * Components should only access this class through <code>ContainerServices</code>.
 * 
 * @author hsommer Feb 18, 2003 9:20:14 AM
 */
public class AcsManagerProxy 
{
	private Manager m_manager;
	private String m_managerLoc;
	
	// handle representing the container or other client (assigned by Manager at login)
	private int m_mgrHandle = 0;

	private ORB m_orb;

	private Logger m_logger;
	
	private boolean m_shuttingDown;
	
	/** msc(2005-07) client:managerproxy is always 1:1 */
	private Client m_managerClient;
	
	
	/////////////////////////////////////////////////////////////
	// creation and connection
	/////////////////////////////////////////////////////////////		
	
	/**
	 * Constructor stores parameters, but takes no further action.
	 * 
	 * @param managerLoc  corbaloc string for the Manager
	 * @param orb  active ORB; provided explicitly rather than using {@link alma.acs.container.corba.AcsCorba}, 
	 * 				so we can easily reuse this class for clients outside of the container.
	 * @param logger 
	 */
	public AcsManagerProxy(String managerLoc, ORB orb, Logger logger)
	{
		if (managerLoc == null)
		{
			throw new IllegalArgumentException("argument 'managerLoc' must not be null.");
		}
		m_managerLoc = managerLoc;
		m_orb = orb;
		
		m_logger = logger;		
	}
	

	
	/**
	 * Gets the ACS Manager, either cached or freshly retrieved.
	 * Note that no login is done here, so call {@link #loginToManager(Client, boolean)} after this.
	 * 
	 * @return Manager 
	 * @throws ContainerException
	 */
	public synchronized Manager getManager() throws ContainerException
	{
		if (m_manager == null)
		{
			if (m_shuttingDown)
			{
				String msg = "call to getManager() fails while shutting down.";
				m_logger.fine(msg);
				throw new ContainerException(msg);
			}
			
			m_logger.fine("Manager reference not available. Trying to resolve...");

			findManager(m_managerLoc, true);
		}
		
		return m_manager;
	}

	

	/////////////////////////////////////////////////////////////
	// -------- failure detection & automatic reconnect -------//
	/////////////////////////////////////////////////////////////

	/** 
	 * The lock should only be held for short periods, the threads executing
	 * our API need to be able to retrieve it quickly.
	 * 
	 * TODO: replace with lock from concurrent lib
	 */
	protected java.lang.Object connectorLock = new java.lang.Object();

	/**
	 * This thread keeps retrying to log in to the manager.
	 * Before each attempt, it checks whether some outside-world caller
	 * of loginToManager() has meanwhile accomplished to log in.
	 * 
	 * notify() on the lock will awake the thread,
	 * interrupt() will terminate the thread.
	 * 
	 * msc(2005-07)
	 */
	protected Thread connectorThread = new Thread("AcsManagerProxy.ConnectorThread") {

		public void run () {
			ALIVE: while (true) {

				synchronized (connectorLock) {
					try {
						connectorLock.wait();
					} catch (InterruptedException exc) {
						break ALIVE; // quit
					}
				}

				RECONNECT: while (true) {

					try {
						if (isLoggedIn(false)) {
							break RECONNECT; // somebody else succeeded for us
						}
						
						m_logger.finer("attempting (re-)login");						
						
						loginToManager(false);
						break RECONNECT; // managed to reconnect

					} catch (ContainerException e) {

						try {
							sleep(5000); // sleep, then retry
						} catch (InterruptedException exc) {
							break ALIVE; // quit
						}
					}
				}

			}
		}
	};
	
	/**
	 * Invoked by the manager methods on failure 
	 */
	protected void handleException (RuntimeException exc) {

		if (exc instanceof org.omg.CORBA.NO_PERMISSION				// manager changed, or has cut connection
				||exc instanceof org.omg.CORBA.OBJECT_NOT_EXIST		// manager is down
				||exc instanceof org.omg.CORBA.TRANSIENT)				// manager is down
		{
			
			// store the knowledge that we're unconnected:
			// this will make isLoggedIn() return false;
			m_mgrHandle = 0;
			
			// wake up connector thread
			synchronized (connectorLock) {
				connectorLock.notify();
			}
		} 
		else {
			m_logger.log(Level.INFO, "unexpected error occurred: please report this", exc);
			return;
		}
	}

	
	
	/**
	 * Finds the Manger using the supplied <code>managerLoc</code>.
	 * If the manager can't be found and <code>keepTrying</code> is true, 
	 * this method enters a loop and tries again after every 5 seconds. 
	 * 
	 * @param managerLoc  the corbaloc string that uniquely identifies the manager 
	 * 						as a CORBA service.
	 * @param keepTrying  if true, this method will only return
	 * 						when it has successfully contacted the manager service;
	 * 					  if false, this method will simply throw a <code>ContainerException</code>
	 * 						if it fails to resolve the manager. 
	 * @throws ContainerException  if anything goes wrong.
	 */
	private synchronized void findManager(String managerLoc, boolean keepTrying) 
		throws ContainerException
	{
		do
		{
			try
			{
				org.omg.CORBA.Object object = m_orb.string_to_object(m_managerLoc);
				m_logger.finest("manager corbaloc '" + managerLoc + "' resolved.");
				m_manager = ManagerHelper.narrow(object);
				if (m_manager == null)
				{
					throw new ContainerException("received null reference to ACS Manager.");
				}
				m_logger.finest("manager narrow successful.");
				keepTrying = false; 
			}
			catch (Exception e)
			{
				String msg = "Failed to obtain the manager reference from the corbaloc '" + 
								m_managerLoc + "'. ";
				if (keepTrying)
				{
					m_logger.log(Level.INFO, msg + "Will keep trying.");
					try
					{
						Thread.sleep(5000);
					}
					catch (InterruptedException e1)
					{
						// nada
					}
				}
				else
				{
					m_logger.log(Level.WARNING, msg + e.getMessage());
					throw new ContainerException(msg, e);
				}
			}
		}
		while (keepTrying);		
	}

	/**
	 * Logs in to the manager, using the provided manager client CORBA object.
	 *  
	 * @param managerClient  the IDL-defined client of the manager (see maci.idl), 
	 * 						 of which the container is a subclass.
	 * @param keepTrying  refers to multiple attempts for both finding the manager and logging in to the manager.  
	 * @throws ContainerException 
	 */
	public synchronized void loginToManager(Client managerClient, boolean keepTrying) throws ContainerException {
		this.m_managerClient = managerClient; 
		try {
			loginToManager(keepTrying);
		}
		finally {
			if (keepTrying && !connectorThread.isAlive()) {
				// start check&relogin loop 
				connectorThread.setDaemon(true);
				connectorThread.start();
			}
		}
	}

	/**
	 * Logs in to the Manager.
	 * Only to be called from within this class (see connectorThread), when the  
	 * 
	 * @param keepTrying  used for the call to {@link #findManager(String, boolean)}.
	 * @throws ContainerException
	 */
	private synchronized void loginToManager(boolean keepTrying) 
		throws ContainerException
	{
		if (m_shuttingDown)
		{
			String msg = "call to loginToManager(..) fails while shutting down.";
			m_logger.fine(msg);
			throw new ContainerException(msg);
		}
			
		if (isLoggedIn(false))
		{
			m_logger.info("login to Manager requested while being logged in - will first log out...");
			logoutFromManager();
		}
		
		if (m_manager == null)
		{
			m_logger.fine("manager reference not yet available in method loginToManager; " 
					+ "will try to find the manager first...");
					
			findManager(m_managerLoc, keepTrying);
		}
		
		try
		{
			// login
			ClientInfo ci = m_manager.login(m_managerClient);
			if (ci == null)
			{
				throw new NullPointerException("received null from manager.login()");
			}
			m_mgrHandle = ci.h;
		}
		catch (Exception e)
		{
			m_mgrHandle = 0;
			String msg = "Failed to login to manager.";
			m_logger.log(Level.WARNING, msg);
			throw new ContainerException(msg, e);
		}

		m_logger.fine("Manager login done, handle '" + m_mgrHandle + "' obtained.");
	}


	boolean isLoggedIn(boolean checkPing)
	{
		boolean loggedIn = ( m_manager != null && m_mgrHandle != 0 );
		
		if (loggedIn && checkPing)
		{
			loggedIn = pingManager(2000);
		}
		return loggedIn;
	}


	/**
	 * Pings the manager.
	 * Uses a separate thread so that life can go on if the manager does not respond,
	 * and the ORB timeout is too long for this purpose.
	 * <p>
	 * Currently calls <code>get_client_info</code> to see if the manager responds. 
	 * Todo: should be replaced by a call to Manager.ping when this is available.
	 * 
	 * @param timeoutMillis  timout in milliseconds that can restrict the general ORB timeout.
	 * @return  true if the manager responded within the given time, false otherwise
	 * @see #isLoggedIn(boolean)
	 */
	public boolean pingManager(long timeoutMillis)
	{
		boolean responding = false;
		long startTime = System.currentTimeMillis();
		
		PingManagerThread pingThread = new PingManagerThread();
		pingThread.start();
		
		do 
		{
			responding = pingThread.isResponding();
			
			try
			{
				// to save some CPU time
				Thread.sleep(5);
			}
			catch (InterruptedException e)
			{//nada
			}
		}
		while (System.currentTimeMillis() - startTime < timeoutMillis);
		
		return responding;
	}	
	
	private class PingManagerThread extends Thread
	{
		boolean m_responding = false;
		
		PingManagerThread()
		{
			super();
			setDaemon(true);
		}
		
		public void run()
		{
			try
			{
				// try something to see if the manager responds...
				ClientInfo[] cinfos = m_manager.get_client_info(m_mgrHandle, 
										new int[]{m_mgrHandle}, "");
				m_responding = true;
			}
			catch (Throwable thr)
			{
				// ignore and leave loggedIn false
				
				// todo remove debug message
				System.err.println("isLoggedIn - m_manager.get_client_info problem " + thr);
			}
		}
		
		boolean isResponding()
		{
			return m_responding;
		}
	}
	
	public synchronized void logoutFromManager()
	{
		try
		{
			if (isLoggedIn(false))
			{
				m_logger.fine("about to log out from manager...");
				m_manager.logout(m_mgrHandle);
				m_logger.fine("logged out from manager.");
			}
		}
		catch (Exception ex)
		{
			m_logger.log(Level.WARNING, "failed to log out gracefully from the manager...", ex);
		}
		finally
		{
			m_mgrHandle = 0;
			m_manager = null;
		}
	}
	


	/////////////////////////////////////////////////////////////
	// encapsulated methods of the manager
	/////////////////////////////////////////////////////////////

	/**
	 * From maci IDL comments:
	 * <i>
	 * "Gets all the information that the Manager has about components. 
	 * To invoke this method, the caller must have INTROSPECT_MANAGER access rights, 
	 * or it must have adequate privileges to access the component (the same as with the get_service method).
	 * Information about all components is returned, unless the active_only parameter is set to True, 
	 * in which case only information about those components that are currently registered with the Manager 
	 * and activated is returned.
	 * Calling this function does not affect the internal state of the Manager."
	 * </i>
	 * 
	 * @param componentHandles  Handles of the components whose information is requested. 
	 * 						If this is an empty sequence, the name_wc and type_wc parameters are used.
	 * @param name_wc  Wildcard that the component's name must match in order for its information to be returned.
	 * @param type_wc  Wildcard that the component's type must match in order for its information to be returned.
	 * @param active_only
	 * @return si.ijs.maci.ComponentInfo[]  A sequence of ComponentInfo structures containing the entire 
	 * 			Manager's knowledge about the components. If access is denied to a subset of objects, 
	 * 			the handles to those objects are set to 0.
	 */
	public ComponentInfo[] get_component_info(
		int[] componentHandles,
		String name_wc,
		String type_wc,
		boolean active_only)
	{
		try {
			return m_manager.get_component_info(m_mgrHandle, componentHandles, name_wc, type_wc, active_only);
			
		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}


	/**
	 * From maci IDL comments:
	 * <i>
	 * Gets a service, activating it if necessary (components). 
	 * The client represented by id (the handle) must have adequate access rights to access the service.
	 * This is untrue of components: components always have unlimited access rights to other components."
	 * </i>
	 * @param service_url  CURL of the service whose reference is to be retrieved.
	 * @param activate  True if the service is to be activated in case it does not exist. 
	 * 					If set to False, and the service does not exist, a nil reference is returned 
	 * 					and status is set to <code>Manager.COMPONENT_NOT_ACTIVATED</code>.
	 * @param status  Status of the request. One of <code>Manager.COMPONENT_ACTIVATED</code>, 
	 * 					<code>Manager.COMPONENT_NONEXISTENT</code> and <code>Manager.COMPONENT_NOT_ACTIVATED</code>.
	 * @return org.omg.CORBA.Object  Reference to the service. If the service could not be activated, 
	 * 			a nil reference is returned, and the status contains an error code detailing the cause of failure 
	 * 			(one of the component_* constants).
	 */
	public Object get_service(
		String service_url,
		boolean activate,
		IntHolder status)
	{
		try {
			return m_manager.get_service(m_mgrHandle, service_url, activate, status);
		
		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}

	/**
	 * From maci IDL comments:
	 * <i>
	 * "Used for retrieving several components with one call. See get_service."
	 * </i>
	 * 
	 * @param service_urls
	 * @param activate
	 * @param status
	 * @return org.omg.CORBA.Object[]  A sequence of requested services.
	 */
	public Object[] get_services(
		String[] service_urls,
		boolean activate,
		ulongSeqHolder status)
	{
		try {
			return m_manager.get_services(m_mgrHandle, service_urls, activate, status);
			
		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}


	/**
	 * The more restricted version of {@link #get_service(String, boolean, IntHolder) get_service}, 
	 * only good for getting components.
	 * 
	 * @param clientHandle  handle of requesting component or other kind of client
	 * @param component_url  to specify the requested component  
	 * @param activate
	 * @param status  status out-parameter
	 * @return  the component reference (must still be narrowed using the appropriate Corba helper)
	 * @see ContainerServices#getComponent(String)
	 */
	public Object get_component(int clientHandle, String component_url, boolean activate,
			IntHolder status) 
	{
		try {
			return m_manager.get_component(clientHandle, component_url, activate, status);

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}


	/**
	 * @param clientHandle  handle of requesting component or other kind of client
	 * @param componentIDLType  the IDL type
	 * @return @throws si.ijs.maci.NoDefaultComponent
	 * @see ContainerServices#getDefaultComponent(String)
	 */
	public ComponentInfo get_default_component(int clientHandle, String componentIDLType)
		throws NoDefaultComponent
	{
		try {
			return m_manager.get_default_component(clientHandle, componentIDLType);

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}


	/**
	 * @param clientHandle  handle of requesting component or other kind of client
	 * @param c  dynamic version of deployment info, may be incomplete, see ACS documentation
	 * @param mark_as_default  if true, make the new component instance the default for its type
	 * @return  a struct which contains the corba reference, as well as name, type, etc.
	 * 
	 * @throws si.ijs.maci.IncompleteComponentSpec
	 * @throws si.ijs.maci.InvalidComponentSpec
	 * 				Thrown if there is no valid dynamic component found for given resulting structure.
	 * @throws si.ijs.maci.ComponentSpecIncompatibleWithActiveComponent
	 * 				Thrown if the resulting structure is incompatible with a component of the same name 
	 * 				already active.
	 */
	public ComponentInfo get_dynamic_component(int clientHandle, ComponentSpec c, boolean mark_as_default) 
		throws IncompleteComponentSpec, InvalidComponentSpec, ComponentSpecIncompatibleWithActiveComponent 
	{
		try {
			return m_manager.get_dynamic_component(clientHandle, c, mark_as_default);
			
		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}

	/**
	 * Encapsulates {@link si.ijs.maci.ManagerOperations#get_collocated_component(int, si.ijs.maci.ComponentSpec, boolean, java.lang.String)}.
	 * 
	 * @param clientHandle  handle of requesting component or other kind of client
	 * @param c dynamic version of deployment info, may be incomplete, see ACS documentation
	 * @param mark_as_default  if true, make the new component instance the default for its type
	 * @param target_component_url
	 * @return ComponentInfo struct 
	 * @throws IncompleteComponentSpec
	 * @throws InvalidComponentSpec
	 * @throws ComponentSpecIncompatibleWithActiveComponent
	 */
	public ComponentInfo get_collocated_component(int clientHandle, ComponentSpec c, boolean mark_as_default, String target_component_url) 
		throws IncompleteComponentSpec, InvalidComponentSpec, ComponentSpecIncompatibleWithActiveComponent 
{
	try {
		return m_manager.get_collocated_component(clientHandle, c, mark_as_default, target_component_url);
		
	} catch (RuntimeException exc) {
		handleException(exc);
		throw exc;
	}
}


	/**
	 * From maci IDL comments: <i>"Registers a CORBA object as a component,
	 * assigning it a CURL and making it accessible through the Manager. The
	 * component is treated as an immortal component." </i>
	 * 
	 * @param component_url
	 * @param type
	 * @param component
	 * @return int
	 */
	public int register_component(String component_url, String type, Object component)
	{
		try {
			return m_manager.register_component(m_mgrHandle, component_url, type, component);

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}


	/**
	 * From maci IDL comments:
	 * <i>
	 * "Releases a component. In order for this operation to be possible, the caller 
	 * represented by the id must have previously successfuly requested the component 
	 * via a call to {@link #get_service(String, boolean, IntHolder)}.
     * Releasing a component more times than requesting it should be avoided, 
     * but it produces no errors."
	 * </i>
	 * 
	 * @param clientHandle  handle of requesting component or other kind of client
	 * @param component_url  
	 * @return int  Number of clients that are still using the component 
	 * 			after the operation completed. This is a useful debugging tool.
	 */
	public int release_component(int clientHandle, String component_url)
	{
		try {
			int clientNumber = m_manager.release_component(clientHandle, component_url);
			return clientNumber;

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}


	/**
	 * Releases several components at a time.
	 * @param clientHandle  handle of requesting component or other kind of client
	 * @param component_urls
	 * @see si.ijs.maci.ManagerOperations#release_components(int, java.lang.String[])
	 * @see #release_component(int, String)
	 */
	public void release_components(int clientHandle, String[] component_urls)
	{
		try {
			m_manager.release_components(clientHandle, component_urls);

		} catch (RuntimeException exc) {
			handleException(exc);
			throw exc;
		}
	}

	

	/////////////////////////////////////////////////////////////
	// other (interaction with container)
	/////////////////////////////////////////////////////////////		
	
	/**
	 * Returns the handle that the ACS Manager has assigned to this client.
	 * This handle is kept inside the Manager proxy class, but applies to the whole container.
	 * 
	 * @return int
	 */
	public int getManagerHandle()
	{
		return m_mgrHandle;
	}

	/**
	 * Notification that the container is in the process of shutting down.
	 */
	public synchronized void shutdownNotify()
	{
		m_shuttingDown = true;
		connectorThread.interrupt(); // make thread quit
	}

	public static String getLocalManagerCorbaloc() 
	{
		// default = localhost
		String host = ACSPorts.getIP();
		String managerLoc = "corbaloc::" + host + ":" + ACSPorts.getManagerPort() + "/Manager";
		return managerLoc;
	}
}
