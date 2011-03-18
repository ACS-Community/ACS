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
 *    This library is distributed in the hope thaowt it will be useful,
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
import si.ijs.maci.Manager;
import si.ijs.maci.ManagerHelper;
import si.ijs.maci.ManagerOperations;

import alma.ACS.CBDescIn;
import alma.ACS.CBlong;
import alma.ACSErrTypeCommon.wrappers.AcsJUnexpectedExceptionEx;
import alma.JavaContainerError.wrappers.AcsJContainerEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.util.ACSPorts;
import alma.maciErrType.CannotGetComponentEx;
import alma.maciErrType.CannotRegisterComponentEx;
import alma.maciErrType.ComponentConfigurationNotFoundEx;
import alma.maciErrType.ComponentNotAlreadyActivatedEx;
import alma.maciErrType.ComponentSpecIncompatibleWithActiveComponent;
import alma.maciErrType.ComponentSpecIncompatibleWithActiveComponentEx;
import alma.maciErrType.IncompleteComponentSpec;
import alma.maciErrType.IncompleteComponentSpecEx;
import alma.maciErrType.InvalidComponentSpec;
import alma.maciErrType.InvalidComponentSpecEx;
import alma.maciErrType.NoDefaultComponentEx;
import alma.maciErrType.NoPermissionEx;
import alma.maciErrType.wrappers.AcsJCannotGetComponentEx;
import alma.maciErrType.wrappers.AcsJCannotRegisterComponentEx;
import alma.maciErrType.wrappers.AcsJComponentConfigurationNotFoundEx;
import alma.maciErrType.wrappers.AcsJComponentNotAlreadyActivatedEx;
import alma.maciErrType.wrappers.AcsJComponentSpecIncompatibleWithActiveComponentEx;
import alma.maciErrType.wrappers.AcsJIncompleteComponentSpecEx;
import alma.maciErrType.wrappers.AcsJInvalidComponentSpecEx;
import alma.maciErrType.wrappers.AcsJNoDefaultComponentEx;
import alma.maciErrType.wrappers.AcsJNoPermissionEx;

/**
 * Proxy class that encapsulates access to the ACS Manager.
 * See maci.idl for the various types of client, which include container and admin client.
 * <p>
 * This class is stateful, so there must be one <code>AcsManagerProxy</code> used per client 
 * if a process has more than one client to the manager. 
 * <p>
 * Warning: This class is not supposed to be instantiated by components and other non-framework classes. 
 * Components should only access this class through <code>ContainerServices</code>.
 * 
 * @author hsommer Feb 18, 2003 9:20:14 AM
 */
public class AcsManagerProxy 
{
	private Manager m_manager;
	private final String m_managerLoc;
	
	/** 
	 * Handle representing the container or other client (assigned by Manager at login).
	 * Note that every component has a separate "component handle".
	 */
	private volatile int m_mgrHandle = 0;

	private final ORB m_orb;

	private final Logger m_logger;
	
	private volatile boolean m_shuttingDown;
	
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
	 * @throws AcsJContainerServicesEx
	 */
	public synchronized Manager getManager() throws AcsJContainerEx
	{
		if (m_manager == null)
		{
			if (m_shuttingDown) {
				String msg = "call to getManager() fails while shutting down.";
				m_logger.fine(msg);
				AcsJContainerEx ex = new AcsJContainerEx();
				ex.setContextInfo(msg);
				throw ex;
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
	protected final Thread connectorThread = new Thread("AcsManagerProxy.ConnectorThread") {

		public void run () {
			ALIVE: while (true) {

				synchronized (connectorLock) {
					try {
						connectorLock.wait(); // until connectorLock.notify or interrupt 
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

					} catch (AcsJContainerEx e) {

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
	

	protected void activateReloginToManagerIfDisconnected() {
		if (!connectorThread.isAlive()) {
			// start check&relogin loop 
			connectorThread.setDaemon(true);
			connectorThread.start();
		}
	}

	/**
	 * Invoked by the manager methods on failure.
	 * The "handling" of the exception refers to changing the connection status, logging the exception, etc.
	 * This method will not throw the given exception, which means that the calling client has to do this herself.
	 */
	protected void handleRuntimeException (RuntimeException exc) {

		if (exc instanceof org.omg.CORBA.NO_PERMISSION				// manager changed, or has cut connection
				||exc instanceof org.omg.CORBA.OBJECT_NOT_EXIST		// manager is down
				||exc instanceof org.omg.CORBA.TRANSIENT)			// manager is down
		{
			
			// store the knowledge that we're unconnected:
			// this will make isLoggedIn() return false;
			m_mgrHandle = 0;
			
			// wake up connector thread
			synchronized (connectorLock) {
				connectorLock.notify();
			}
		} 
		else if (exc instanceof org.omg.CORBA.TIMEOUT) {
			// the timeout usually does not indicate a general problem of the manager, but affects
			// only this one invocation. We don't do anything here and let the caller throw the exception.
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
	 * 					  if false, this method will simply throw a <code>AcsJContainerServicesEx</code>
	 * 						if it fails to resolve the manager. 
	 * @throws AcsJContainerEx  if anything goes wrong.
	 */
	private synchronized void findManager(String managerLoc, boolean keepTrying) 
		throws AcsJContainerEx
	{
		do
		{
			if (m_shuttingDown) {
				// 
				AcsJContainerEx ex = new AcsJContainerEx();
				ex.setContextInfo("Abandoned because we are shutting down.");
				throw ex;
			}
			
			try
			{
				org.omg.CORBA.Object object = m_orb.string_to_object(m_managerLoc);
				m_logger.finest("manager corbaloc '" + managerLoc + "' resolved.");
				m_manager = ManagerHelper.narrow(object);
				if (m_manager == null) {
					AcsJContainerEx ex = new AcsJContainerEx();
					ex.setContextInfo("received null reference to ACS Manager.");
					throw ex;
				}
				m_logger.finest("manager narrow successful.");
				keepTrying = false; 
			}
			catch (Throwable thr) {
				String msg = "Failed to obtain the manager reference from the corbaloc '" + m_managerLoc + "'. ";
				if (keepTrying) {
					m_logger.log(Level.INFO, msg + "Will keep trying.");
					try {
						Thread.sleep(5000);
					}
					catch (InterruptedException e1) {
						// nada
					}
				}
				else {
					m_logger.log(Level.WARNING, msg + thr.getMessage());
					AcsJContainerEx ex = new AcsJContainerEx(thr);
					ex.setContextInfo(msg);
					throw ex;
				}
			}
		}
		while (keepTrying);
	}

	/**
	 * Logs in to the manager, using the provided manager client CORBA object.
	 * This method only returns when the login succeeded. Otherwise an exception is thrown.
	 *  
	 * @param managerClient  the IDL-defined client of the manager (see maci.idl), 
	 * 						 of which the container is a subclass.
	 * @param keepTrying  refers to multiple attempts for both finding the manager and logging in to the manager. 
	 * 				If true, a background thread is started to re-login if the connection breaks.
	 * @throws AcsJContainerServicesEx 
	 */
	public synchronized void loginToManager(Client managerClient, boolean keepTrying) throws AcsJContainerEx {
		this.m_managerClient = managerClient;
		
		loginToManager(keepTrying);
		
		// if login has succeeded and keepTrying==true, then also activate the re-login thread
		// which sleeps unless handleRuntimeException reports a problem.
		if (keepTrying) {
			activateReloginToManagerIfDisconnected();
		}
	}



	/**
	 * Logs in to the Manager.
	 * Only to be called from within this class (see connectorThread), when the  
	 * 
	 * @param keepTrying  used for the call to {@link #findManager(String, boolean)}.
	 * @throws AcsJContainerServicesEx
	 */
	private synchronized void loginToManager(boolean keepTrying) 
		throws AcsJContainerEx
	{
		if (m_shuttingDown) {
			String msg = "call to loginToManager(..) fails while shutting down.";
			m_logger.fine(msg);
			AcsJContainerEx ex = new AcsJContainerEx();
			ex.setContextInfo(msg);
			throw ex;
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
			if (ci == null) {
				throw new NullPointerException("received null from manager.login()");
			}
			if (ci.h <= 0) {
				AcsJContainerEx ex = new AcsJContainerEx();
				ex.setContextInfo("Got invalid handle from manager login: " + ci.h);
				throw ex;
			}
			m_mgrHandle = ci.h;
		}
		catch (Throwable thr) {
			m_mgrHandle = 0;
			String msg = "Failed to login to manager.";
			m_logger.log(Level.WARNING, msg);
			AcsJContainerEx ex = new AcsJContainerEx(thr);
			ex.setContextInfo(msg);
			throw ex;
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
				m_manager.ping();
				m_responding = true;
			}
			catch (Throwable thr)
			{
				// ignore and leave loggedIn false
				
				// todo remove debug message
//				System.err.println("isLoggedIn - m_manager.get_client_info problem " + thr);
			}
		}
		
		boolean isResponding()
		{
			return m_responding;
		}
	}
	
	/**
	 * Logs out from the manager. 
	 * If this fails, a warning is logged, but no exception will be thrown by this method.
	 */
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
		throws AcsJNoPermissionEx
	{
		ComponentInfo[] compInfos = null;
		try {
			compInfos = m_manager.get_component_info(checkAndGetManagerHandle(), componentHandles, name_wc, type_wc, active_only);
		} catch (NoPermissionEx ex) {
			throw AcsJNoPermissionEx.fromNoPermissionEx(ex);
		} catch (RuntimeException exc) {
			handleRuntimeException(exc);
			throw exc;
		}
		
		// not sure if compInfos == null can ever happen, but we don't want clients to check this case 
		if (compInfos == null) {
			throw new NullPointerException("Manager returned null ComponentInfo[]"); 
		}
		
		return compInfos;
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
	public Object get_service(String service_url, boolean activate) 
		throws AcsJComponentNotAlreadyActivatedEx, AcsJCannotGetComponentEx, AcsJComponentConfigurationNotFoundEx, AcsJNoPermissionEx
	{
		try {
			return m_manager.get_service(checkAndGetManagerHandle(), service_url, activate);		
		} catch (RuntimeException exc) {
			handleRuntimeException(exc);
			throw exc;
		} catch (NoPermissionEx ex) {
			throw AcsJNoPermissionEx.fromNoPermissionEx(ex);
		} catch (ComponentNotAlreadyActivatedEx ex) {
			throw AcsJComponentNotAlreadyActivatedEx.fromComponentNotAlreadyActivatedEx(ex);
		} catch (CannotGetComponentEx ex) {
			throw AcsJCannotGetComponentEx.fromCannotGetComponentEx(ex);
		} catch (ComponentConfigurationNotFoundEx ex) {
			throw AcsJComponentConfigurationNotFoundEx.fromComponentConfigurationNotFoundEx(ex);
		}
	}

	/**
	 * The more restricted version of {@link #get_service(String, boolean, IntHolder) get_service}, 
	 * only good for getting components.
	 * 
	 * @param clientHandle  handle of requesting component or other kind of client 
	 *        (for non-container clients this will always be the login-handle)
	 * @param component_url  to specify the requested component  
	 * @param activate
	 * @param status  status out-parameter
	 * @return  the component reference (must still be narrowed using the appropriate Corba helper)
	 * @see ContainerServices#getComponent(String)
	 */
	public Object get_component(int clientHandle, String component_url, boolean activate)
	    throws AcsJCannotGetComponentEx, AcsJComponentNotAlreadyActivatedEx, AcsJComponentConfigurationNotFoundEx, AcsJNoPermissionEx
	{
		try {
			return m_manager.get_component(clientHandle, component_url, activate);
		} catch (RuntimeException exc) {
			handleRuntimeException(exc);
			throw exc;
		} catch (NoPermissionEx ex) {
			throw AcsJNoPermissionEx.fromNoPermissionEx(ex);
		} catch (ComponentNotAlreadyActivatedEx ex) {
			throw AcsJComponentNotAlreadyActivatedEx.fromComponentNotAlreadyActivatedEx(ex);
		} catch (CannotGetComponentEx ex) {
			throw AcsJCannotGetComponentEx.fromCannotGetComponentEx(ex);
		} catch (ComponentConfigurationNotFoundEx ex) {
			throw AcsJComponentConfigurationNotFoundEx.fromComponentConfigurationNotFoundEx(ex);
		}
	}

	
	public Object get_component_non_sticky(int clientHandle, String component_url)
    	throws AcsJCannotGetComponentEx, AcsJComponentNotAlreadyActivatedEx, AcsJNoPermissionEx
    {
	try {
		return m_manager.get_component_non_sticky(clientHandle, component_url);
	} catch (RuntimeException exc) {
		handleRuntimeException(exc);
		throw exc;
	} catch (NoPermissionEx ex) {
		throw AcsJNoPermissionEx.fromNoPermissionEx(ex);
	} catch (ComponentNotAlreadyActivatedEx ex) {
		throw AcsJComponentNotAlreadyActivatedEx.fromComponentNotAlreadyActivatedEx(ex);
	} catch (CannotGetComponentEx ex) {
		throw AcsJCannotGetComponentEx.fromCannotGetComponentEx(ex);
	}
}


	/**
	 * Gets a component as a "weak client" who does not prevent component unloading in case
	 * the other real clients release their references. 
	 * Otherwise similar to <code>get_component</code> with <code>activate==false</code>. 
	 * 
	 * @param clientHandle
	 * @param component_url
	 * @return  the component reference (must still be narrowed using the appropriate Corba helper)
	 * @throws AcsJComponentNotAlreadyActivatedEx  if the requested component has not already been activated by some other client 
	 * @throws AcsJCannotGetComponentEx
	 * @throws AcsJUnexpectedExceptionEx if the remote call failed with a (CORBA) runtime exception
	 * @since ACS 6.0
	 */
	public Object getComponentNonSticky(int clientHandle, String component_url) 
		throws AcsJComponentNotAlreadyActivatedEx, AcsJCannotGetComponentEx, AcsJNoPermissionEx
	{
		try {
			return m_manager.get_component_non_sticky(clientHandle, component_url);
		} catch (RuntimeException rtex) {
			handleRuntimeException(rtex);
			throw rtex;
		} catch (ComponentNotAlreadyActivatedEx ex) {
			throw AcsJComponentNotAlreadyActivatedEx.fromComponentNotAlreadyActivatedEx(ex);
		} catch (CannotGetComponentEx ex) {
			throw AcsJCannotGetComponentEx.fromCannotGetComponentEx(ex);
		} catch (NoPermissionEx ex) {
			throw AcsJNoPermissionEx.fromNoPermissionEx(ex);
		}
	}

	/**
	 * @param clientHandle  handle of requesting component or other kind of client
	 *        (for non-container clients this will always be the login-handle)
	 * @param componentIDLType  the IDL type
	 * @see ContainerServices#getDefaultComponent(String)
	 */
	public ComponentInfo get_default_component(int clientHandle, String componentIDLType)
	    throws AcsJNoDefaultComponentEx, AcsJCannotGetComponentEx, AcsJNoPermissionEx
	{
		try {
			return m_manager.get_default_component(clientHandle, componentIDLType);
		} catch (RuntimeException exc) {
			handleRuntimeException(exc);
			throw exc;
		} catch (NoPermissionEx ex) {
			throw AcsJNoPermissionEx.fromNoPermissionEx(ex);
		} catch (NoDefaultComponentEx ex) {
			throw AcsJNoDefaultComponentEx.fromNoDefaultComponentEx(ex);
		} catch (CannotGetComponentEx ex) {
			throw AcsJCannotGetComponentEx.fromCannotGetComponentEx(ex);
		}
	}


	/**
	 * @param clientHandle  handle of requesting component or other kind of client
	 *        (for non-container clients this will always be the login-handle)
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
	    throws AcsJIncompleteComponentSpecEx, AcsJInvalidComponentSpecEx,  AcsJComponentSpecIncompatibleWithActiveComponentEx, AcsJCannotGetComponentEx, AcsJNoPermissionEx
	{
		try {
			return m_manager.get_dynamic_component(clientHandle, c, mark_as_default);
			
		} catch (RuntimeException exc) {
			handleRuntimeException(exc);
			throw exc;
		} catch (NoPermissionEx ex) {
			throw AcsJNoPermissionEx.fromNoPermissionEx(ex);
		} catch (IncompleteComponentSpecEx ex) {
			throw AcsJIncompleteComponentSpecEx.fromIncompleteComponentSpecEx(ex);
		} catch (InvalidComponentSpecEx ex) {
			throw AcsJInvalidComponentSpecEx.fromInvalidComponentSpecEx(ex);
		} catch (CannotGetComponentEx ex) {
			throw AcsJCannotGetComponentEx.fromCannotGetComponentEx(ex);
		} catch (ComponentSpecIncompatibleWithActiveComponentEx ex) {
			throw AcsJComponentSpecIncompatibleWithActiveComponentEx.fromComponentSpecIncompatibleWithActiveComponentEx(ex);
		}
	}

	/**
	 * Encapsulates {@link si.ijs.maci.ManagerOperations#get_collocated_component(int, si.ijs.maci.ComponentSpec, boolean, java.lang.String)}.
	 * 
	 * @param clientHandle  handle of requesting component or other kind of client
	 *        (for non-container clients this will always be the login-handle)
	 * @param c dynamic version of deployment info, may be incomplete, see ACS documentation
	 * @param mark_as_default  if true, make the new component instance the default for its type
	 * @param target_component_url
	 * @return ComponentInfo struct 
	 * @throws IncompleteComponentSpec
	 * @throws InvalidComponentSpec
	 * @throws ComponentSpecIncompatibleWithActiveComponent
	 */
	public ComponentInfo get_collocated_component(int clientHandle, ComponentSpec c, boolean mark_as_default, String target_component_url) 
	    throws AcsJIncompleteComponentSpecEx, AcsJInvalidComponentSpecEx, AcsJComponentSpecIncompatibleWithActiveComponentEx, AcsJCannotGetComponentEx, AcsJNoPermissionEx
	{
		try {
			return m_manager.get_collocated_component(clientHandle, c, mark_as_default, target_component_url);
			
		} catch (RuntimeException exc) {
			handleRuntimeException(exc);
			throw exc;
		} catch (NoPermissionEx ex) {
			throw AcsJNoPermissionEx.fromNoPermissionEx(ex);
		} catch (IncompleteComponentSpecEx ex) {
			throw AcsJIncompleteComponentSpecEx.fromIncompleteComponentSpecEx(ex);
		} catch (InvalidComponentSpecEx ex) {
			throw AcsJInvalidComponentSpecEx.fromInvalidComponentSpecEx(ex);
		} catch (CannotGetComponentEx ex) {
			throw AcsJCannotGetComponentEx.fromCannotGetComponentEx(ex);
		} catch (ComponentSpecIncompatibleWithActiveComponentEx ex) {
			throw AcsJComponentSpecIncompatibleWithActiveComponentEx.fromComponentSpecIncompatibleWithActiveComponentEx(ex);
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
	    throws AcsJCannotRegisterComponentEx, AcsJNoPermissionEx
	{
		try {
			return m_manager.register_component(checkAndGetManagerHandle(), component_url, type, component);

		} catch (RuntimeException exc) {
			handleRuntimeException(exc);
			throw exc;
		} catch (NoPermissionEx ex) {
			throw AcsJNoPermissionEx.fromNoPermissionEx(ex);
		} catch (CannotRegisterComponentEx ex) {
			throw AcsJCannotRegisterComponentEx.fromCannotRegisterComponentEx(ex);
		}
	}


	/**
	 * From maci IDL comments:
	 * <i>
	 * "Asynchronously release a component. In order for this operation to be possible,
	 *  the caller represented by the id must have previously successfully requested the
	 *  component via a call to get_component.
	 *  Releasing a component more times than requesting it should be avoided,
	 *  but it produces no errors. 
	 *  This method will return before the manager/target container have deactivated the component.
	 *  If the caller wants to synchronize with component deactivation, it must supply
	 *  the optional CBlong callback."
	 * </i>
	 * 
	 * @param clientHandle  handle of requesting component or other kind of client
	 *        (for non-container clients this will always be the login-handle)
	 * @param component_url  
	 * @param callback  to synch on actual component release, receive possible exceptions, 
	 *        and returned number of remaining clients (which could be a useful debugging tool).
	 * @throws AcsJNoPermissionEx If this client does not hold a valid reference to the target component.
	 * @throws RuntimeException such as BAD_PARAM, NO_PERMISSION, NO_RESOURCES, OBJECT_NOT_EXIST, TIMEOUT, TRANSIENT UNKNOWN
	 */
	public void release_component(int clientHandle, String component_url, CBlong callback) throws AcsJNoPermissionEx {
		try {
			// currently we use a dummy CBDescIn. Should take care of the "tag" if we want to reuse the CBlong object.
			m_manager.release_component_async(clientHandle, component_url, callback, new CBDescIn());
		} catch (RuntimeException exc) {
			handleRuntimeException(exc);
			throw exc;
		} catch (NoPermissionEx ex) {
			throw AcsJNoPermissionEx.fromNoPermissionEx(ex);
		}
	}


	/**
	 * Calls {@link ManagerOperations#force_release_component(int, String)}. This call must not be used by regular
	 * component code. It is only provided for setting up tests, writing specialized operator tools, and similar purposes.
	 * @return  number of clients of the force-released components.
	 * @since ACS 5.0.4
	 */
	public int force_release_component(int clientHandle, String curl) throws AcsJNoPermissionEx
	{
		try {
			int clientNumber = m_manager.force_release_component(clientHandle, curl);
			return clientNumber;

		} catch (RuntimeException exc) {
			handleRuntimeException(exc);
			throw exc;
		} catch (NoPermissionEx ex) {
			throw AcsJNoPermissionEx.fromNoPermissionEx(ex);
		}
	}


	/////////////////////////////////////////////////////////////
	// other (interaction with container)
	/////////////////////////////////////////////////////////////		
	
	/**
	 * Returns the handle that the ACS Manager has assigned to the container/client during login.
	 * The handle should always be retrieved from here instead of storing its value outside of this class,
	 * so that a change in handle after a reconnect can be propagated.
	 * 
	 * @return handle, or 0 if not logged in
	 */
	public int getManagerHandle()
	{
		return m_mgrHandle;
	}

	/**
	 * Gets the current client handle assigned by the manager, or throws an
	 * exception if this client is not logged in.
	 * @throws AcsJNoPermissionEx
	 * @see {@link #getManagerHandle()}
	 */
	int checkAndGetManagerHandle() throws AcsJNoPermissionEx {
		int managerHandle = m_mgrHandle; // copy to avoid confusion from concurrent change
		if (managerHandle <= 0) {
			AcsJNoPermissionEx ex = new AcsJNoPermissionEx();
			ex.setReason("Currently not logged in with the acs manager.");
			ex.setID("client handle: " + managerHandle);
			throw ex;
		}
		return managerHandle;
	}


	/**
	 * Notification that the container is in the process of shutting down.
	 * <p>
	 * With ACS 7.0.2 this method is no longer synchronized because it deadlocked 
	 * the shutdown hook thread when the manager proxy was in a login loop. See COMP-2602.
	 */
	public void shutdownNotify()
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



	/**
	 * Creates a new instance, using the same manager, orb and logger.
	 * The new instance can then be used to log in to the manager (again) independently of this instance.
	 * <p>
	 * There is no communication between this instance and the new instance. 
	 * For example, a call to {@link #shutdownNotify()} will not be forwarded.
	 */
	public AcsManagerProxy createInstance() {
		AcsManagerProxy inst = new AcsManagerProxy(m_managerLoc, m_orb, m_logger);
		return inst;
	}


}
