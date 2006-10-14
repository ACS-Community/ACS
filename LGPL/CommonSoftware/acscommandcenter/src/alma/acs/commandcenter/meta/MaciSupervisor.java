/*
 * Created on Oct 13, 2003 by mschilli
 */
package alma.acs.commandcenter.meta;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.logging.Logger;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeNode;

import org.omg.CORBA.IntHolder;
import org.omg.CORBA.ORB;

import alma.acs.util.AcsLocations;

import si.ijs.maci.Administrator;
import si.ijs.maci.AdministratorPOA;
import si.ijs.maci.ClientInfo;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ContainerInfo;
import si.ijs.maci.Manager;
import si.ijs.maci.ManagerHelper;
import si.ijs.maci.ManagerOperations;

import alma.maciErrType.CannotGetComponentEx;
import alma.maciErrType.ComponentNotAlreadyActivatedEx;
import alma.maciErrType.ComponentConfigurationNotFoundEx;
import alma.maciErrType.NoPermissionEx;

import alma.maciErrType.wrappers.AcsJCannotGetComponentEx;
import alma.maciErrType.wrappers.AcsJNoPermissionEx;

/**
 * @author mschilli
 */
public class MaciSupervisor implements IMaciSupervisor {


   /**
    * Creates a MaciSupervisor running on the given ORB, it will connect to
    * the specified manager.
    * 
    * @param managerLoc  the manager corbaloc
    * @param orb the orb to use 
    */
	protected MaciSupervisor(String managerLoc, ORB orb, Logger log) {
		this("AcsCommandCenter", managerLoc, orb, log);
	}
	
   /**
    * Creates a MaciSupervisor running on the given ORB, it will connect to
    * the specified manager.
    * 
    * @param clientName name-prefix like "AcsCommandCenter" or "OMC"
    * @param managerLoc  the manager corbaloc
    * @param orb the orb to use 
    */
	protected MaciSupervisor(String clientName, String managerLoc, ORB orb, Logger log) {
		this.name = clientName + ".MaciSupervisor";
		this.managerLoc = managerLoc;
		this.orb = orb;
		this.log = log;
	}	
	 	
   /**
    * start() must be called after construction, before usage.
    */
	public synchronized void start() {

      if (acImpl == null) {
      	acImpl = new AdministratorImplementation();
      }
      
      // set up invariable tree structure
      if (managerNode == null) {
      	managerNode = createNode("Manager");
	      managerNode.add(containerNode = createNode(new FolderInfo("Containers")));
	      managerNode.add(clientNode = createNode(new FolderInfo("Client Applications")));
	      managerNode.add(componentNode = createNode(new FolderInfo("Components")));
      }

      if (!isConnected()) {
      	this.connectToManager();
      }
		
	}
	


   /**
    * Invoking this method is equal to invoking disconnect().
    */
   public synchronized void stop() {
       disconnectFromManager();
   }
   
   
   /** 
    * msc (2004-11-09): This was requested, suggested, and agreed upon as a workaround for
    * the situation where a manager has gone down (and the application using this macisupervisor
    * knows that the manager is down, e.g. because it made it go down) while this macisupervisor
    * does not yet know that the manager is down. It will only realize that the next time it tries
    * to access it. Doing so will provoke some no_permission error messages in the manager log.
    * To enable the application to avoid these error messages this API method was added. 
    */
    public synchronized void dismissManager() {
    	uninitialize();
    }

    
    public boolean isConnected() {
   	 return administratorClientInfo != null;
    }
    
   
   // =====  Getters / Setters  =====


   /**
    * Some people are interested in the weirdest things..
    */
   public String getManagerLocation() {
      return this.managerLoc;
   }

   /**
    * @since (2004-07-14)
    * @return the orb
    * 
    * @deprecated since 4.1.1
    */
   public ORB getOrb() {
   	return orb;
   }


   
   
   ///////////////////////////////////////////////////////////////////////
   ///--------------------------- Internal ----------------------------///
   ///////////////////////////////////////////////////////////////////////

   
   
   // =============   Connecting / Disconnecting  ===============

   
   /**
    *
    */   
   protected void connectToManager() {

      // isConnecting = true;
      try {

         org.omg.CORBA.Object object = orb.string_to_object(managerLoc);
         managerRef = ManagerHelper.narrow(object);

      } catch (Exception exc) {
         throw new RuntimeException("could not retrieve manager reference", exc);
      }
		
		if (managerRef == null) {
			throw new RuntimeException("orb delivered null-reference for manager-location "+managerLoc);
		}

      try {

         Administrator adminIF = acImpl._this(orb); // <-- throws "port occupied"
         administratorClientInfo = myManagerReference().login(adminIF);

      } catch (Exception exc) {
         throw new RuntimeException("could not login to manager as a supervisor", exc);
      }

      // isConnecting = false;
      // isConnected = true;

      log.info("connected to manager (" + getManagerLocation() + ")");
   }

   /**
    *   
    */   
   protected void disconnectFromManager() {
   	
		try {
		   myManagerReference().logout(myOwnMaciHandle());
		
		} catch (NotConnectedToManagerException exc) {
			// msc (2006-04-28): lowered loglevel (certain users felt confused)
		   log.finer("couldn't log out from manager: " + exc);
		   
		} catch (Exception exc) {
			//	 msc (2006-09-12): lowered loglevel 
			log.finer("couldn't log out from manager: " + exc);
		}
   	
   	uninitialize();

      // isConnected = false;
      // isConnecting = false;
   }


  
   protected void uninitialize() {
      administratorClientInfo = null;
   }
  
   
   // ============  Sending requests to the Manager  ============

  /**
   *   
   */   
   public void shutdownManager() throws NotConnectedToManagerException {
      int hhhhh = myOwnMaciHandle();
      log.fine("sending shutdown request to manager (" + getManagerLocation() + ")");
      myManagerReference().shutdown(hhhhh, 0);
   }

   /**
    *   
    */   
   public void logoutContainer(ContainerInfo info) throws NotConnectedToManagerException {
      log.fine("sending logout request to manager to log out container '" + info.name + "'");
      try {
         myManagerReference().logout(info.h);
      } catch (NoPermissionEx e) {
           AcsJNoPermissionEx ex = new AcsJNoPermissionEx(e);
	   ex.log(log);
      }
   }

   /**
    *   
    */   
   public void logoutClient(ClientInfo info) throws NotConnectedToManagerException {
      log.fine("sending logout request to manager to log out client '" + info.name + "'");
      try {
           myManagerReference().logout(info.h);
      } catch (NoPermissionEx e) {
           AcsJNoPermissionEx ex = new AcsJNoPermissionEx(e);
	   ex.log(log);
      }
   }

   
   
   // ============  Sending requests to Containers  ==============

   /**
    * Sends a shutdown request to every container. Note that the request is asynchronous: when this
    * method returns the container is likely to not have terminated yet.
    * 
    * @since 3.0.2
    */
   public void shutdownContainers() {
      ContainerInfo[] infos = getContainerInfos();
      int code = 2 * 256 + 0; // hibyte: 2 (= EXIT), lobyte: 0 (exitcode towards the OS)
      for (int i = 0; i < infos.length; i++) {
         log.fine("sending shutdown request to container '" + infos[i].name + "'");
         infos[i].reference.shutdown(code);
      }
   }

   /**
    * Resolves the specified name and forwards to {@link #shutdownContainer(ContainerInfo)}.
    * 
    * @param name the case-insensitive name (no wildcards) of a container
    * @since 3.0.2
    */
   public void shutdownContainer(String name) {

      ContainerInfo info = resolveContainerName(name);
      if (info != null)
         shutdownContainer(info);
      else
         log.warning("can't send shutdown request: container unknown: '" + name + "'");
   }

   /**
    * Sends a shutdown request to the container described by the specified ContainerInfo. Note that
    * the request is asynchronous: when this method returns the container is likely to not have
    * terminated yet.
    * 
    * @since 3.1
    */
   public void shutdownContainer(ContainerInfo info) {
      int action = 2 * 256 + 0; // hibyte: 2 (= EXIT), lobyte: 0 (exitcode towards the OS)

      log.fine("sending shutdown request to container '" + info.name + "'");
      info.reference.shutdown(action);
   }

   /**
    * Resolves the specified name and forwards to {@link #disconnectContainer(ContainerInfo)}.
    * 
    * @param name the case-insensitive name (no wildcards) of a container
    * @since 3.0.2
    */
   public void disconnectContainer(String name) {

      ContainerInfo info = resolveContainerName(name);
      if (info != null)
         disconnectContainer(info);
      else
         log.warning("can't send disconnect request: container unknown: '" + name + "'");
   }

   /**
    * Sends a disconnect request to the container described by the specified ContainerInfo.
    * 
    * @since 3.1
    */
   public void disconnectContainer(ContainerInfo info) {
      log.fine("sending disconnect request to container '" + info.name + "'");
      info.reference.disconnect();
   }

   /**
    * Resolves the specified name and forwards to {@link #pingContainer(ContainerInfo)}.
    * 
    * @param name the case-insensitive name (no wildcards) of a container
    * @since 3.0.2
    */
   public void pingContainer(String name) {

   ContainerInfo info = resolveContainerName(name);
      if (info != null)
         pingContainer(info);
      else
         log.warning("can't send ping request: container unknown: '" + name + "'");
   }

   /**
    * Sends a ping request to the container described by the specified ContainerInfo.
    * 
    * @since 3.1
    */
   public void pingContainer(ContainerInfo info) {
      log.fine("sending ping request to container '" + info.name + "'");
      info.reference.ping();
   }

   
   /**
    * @param info the ContainerInfo
    * @param msgType one of MSG_ERROR, MSG_INFORMATION
    * @param msg the message
    */
   public void sendMessageToContainer(ContainerInfo info, short msgType, String msg) {
      log.fine("sending message to container '" + info.name + "'");
      info.reference.message(msgType, msg);
   }
   
   
   
   // ===============  Sending requests to Clients  ====================


   /**
    * Sends a disconnect request to the client described by the specified ClientInfo.
    * 
    * @since 3.1
    */
   public void disconnectClient(ClientInfo info) {
      log.fine("sending disconnect request to client '" + info.name + "'");
      info.reference.disconnect();
   }

   /**
    * Sends a ping request to the client described by the specified ClientInfo.
    * 
    * @since 3.1
    */
   public void pingClient(ClientInfo info) {
      log.fine("sending ping request to client '" + info.name + "'");
      info.reference.ping();
   }

   /**
    * @param info the ClientInfo
    * @param msgType one of MSG_ERROR, MSG_INFORMATION
    * @param msg the message
    */
   public void sendMessageToClient(ClientInfo info, short msgType, String msg) {
      log.fine("sending message to client '" + info.name + "'");
      info.reference.message(msgType, msg);
   }
   
   
   
   // ==================  Retrieving Info from the Manager  =================

   
   /**
    * The containerHandles argument cannot be specified here. Reason: There's apparently a bug in
    * Manager.get_activator_info() in ACS2.x (nothing known about ACS3.x): The containerHandles
    * argument is not evaluated properly, instead empty (therefore useless) ContainerInfos are
    * returned.
    * 
    * @param name_wildcard not verified that it works as expected, recommended to use '*'.
    * @return
    */
   public ContainerInfo[] retrieveContainerInfo(String name_wildcard) throws NotConnectedToManagerException {
   	
      int hhhhh = myOwnMaciHandle();
      int[] containerHandles = new int[0]; // bug workaround, see above
      ContainerInfo[] containerInfo = null;

      try {
         containerInfo = myManagerReference().get_container_info(hhhhh, containerHandles, name_wildcard);
      } catch (NoPermissionEx e) {
           AcsJNoPermissionEx ex = new AcsJNoPermissionEx(e);
	   ex.log(log);
      }
      return containerInfo;
   }

   /**
    *   
    */   
   public ClientInfo[] retrieveClientInfo() throws NotConnectedToManagerException{

   	int hhhhh = myOwnMaciHandle();
	ClientInfo[] clientInfo = null;

	try {
	   clientInfo = myManagerReference().get_client_info(hhhhh, new int[0], "*");
	} catch (NoPermissionEx e) {
	   AcsJNoPermissionEx ex = new AcsJNoPermissionEx(e);
	   ex.log(log);
	}
	return clientInfo;
   }

   /**
    *   
    */   
	public ComponentInfo[] retrieveComponentInfo(int[] cobHandles, String name_wildcard, String type_wildcard, boolean active_only) throws NotConnectedToManagerException {

   	int hhhhh = myOwnMaciHandle();
	ComponentInfo[] componentInfo = null;

	try {
	   componentInfo = myManagerReference().get_component_info(hhhhh, cobHandles, name_wildcard, type_wildcard, active_only);
	} catch (NoPermissionEx e) {
           AcsJNoPermissionEx ex = new AcsJNoPermissionEx(e);
	   ex.log(log);
	}
	return componentInfo;
   }
   

   public org.omg.CORBA.Object getComponent (String curl) throws NotConnectedToManagerException {

		Manager mgr = myManagerReference(); // may throw NotConnectedToManager
		int hhhhh = myOwnMaciHandle(); // may throw NotConnectedToManager

		/**
		 * @todo GCH 2006-10-09 Here we simply catch the exceptions as it was int the
		 *       original code when the signature was simply returning a null object int
		 *       case of error. The exceptions are then just logged. To be verified if this
		 *       is the correct behavior or if we need to pass the exception to the caller
		 *       level.
		 */
		try {
			org.omg.CORBA.Object stub = null;
			try {
			   stub = mgr.get_component(hhhhh, curl, true);
			} catch (NoPermissionEx e) {
			   AcsJNoPermissionEx ex = new AcsJNoPermissionEx(e);
			ex.log(log);
			}


			log.fine("successfully retrieved component '" + curl + "'");
			return stub;
			
		} catch (CannotGetComponentEx e) {
			AcsJCannotGetComponentEx je = new AcsJCannotGetComponentEx(e);
			je.setCURL(curl);
			je.log(log);
		} catch (ComponentNotAlreadyActivatedEx e) {
			AcsJCannotGetComponentEx je = new AcsJCannotGetComponentEx(e);
			je.setCURL(curl);
			je.log(log);
		} catch (ComponentConfigurationNotFoundEx e) {
			AcsJCannotGetComponentEx je = new AcsJCannotGetComponentEx(e);
			je.setCURL(curl);
			je.log(log);
		}

		return null;
	}

   public void forceReleaseComponent(String curl) {
   	
   	int hhhhh = myOwnMaciHandle();

	try {
   	   myManagerReference().force_release_component(hhhhh, curl);
	} catch (NoPermissionEx e) {
           AcsJNoPermissionEx ex = new AcsJNoPermissionEx(e);
	   ex.log(log);
	}

   }
   
   
   public void releaseComponents(String[] curls) throws NotConnectedToManagerException {
   	
   	int hhhhh = myOwnMaciHandle();

	try {
   	   myManagerReference().release_components(hhhhh, curls);
	} catch (NoPermissionEx e) {
           AcsJNoPermissionEx ex = new AcsJNoPermissionEx(e);
	   ex.log(log);
	}
   }
   
   
   /**
    * Searches the local cache to find the {@link ContainerInfo} of
    * the container that currently runs the specified component.
    * 
    * @return info about the container for a specified component
    */
   public ContainerInfo getContainerInfoForComponent(String componentName) throws IllegalArgumentException {
      if (this.managerNode.getChildCount() == 0)
         throw new LocalCacheEmptyException("MaciSupervisor not yet initialized; do start()");

      ComponentInfo compInfo = resolveComponentName(componentName);

      if (compInfo.container == 0) {
      	throw new ComponentNotActivatedInLocalCacheException(compInfo.name);
      }

      // searches by handle
      for (Enumeration en = containerNode.children(); en.hasMoreElements();) {
         DefaultMutableTreeNode next = (DefaultMutableTreeNode) en.nextElement();
         ContainerInfo info = (ContainerInfo) next.getUserObject();
         if (info.h == compInfo.container)
            return info;
      }

      throw new ContainerUnknownInLocalCacheException(componentName+" claims to run on container " + compInfo.container+" which is unknown");
   }

   /**
    * Searches the local cache to find the {@link ComponentInfo} for 
    * the specified component.
    * 
    * @return info about the specified component
    */
   protected ComponentInfo resolveComponentName(String componentName) throws IllegalArgumentException {
   	synchronized (refreshComponentInfoLock) {
	      for (Enumeration en = componentNode.children(); en.hasMoreElements();) {
	         DefaultMutableTreeNode next = (DefaultMutableTreeNode) en.nextElement();
	         ComponentInfo info = (ComponentInfo) next.getUserObject();
	         if (info.name.equalsIgnoreCase(componentName))
	            return info;
	      }
	      throw new ComponentUnknownInLocalCacheException(componentName);
   	}
   }
   
   
   /**
    * Finds the Container info struct belonging to the container with the given name. Used by some
    * of the API methods.
    * 
    * Searches the local cache.
    * 
    * @param name the case-insensitive name (no wildcards) of a container
    * @return the containerinfo for the specified container or <code>null</code>
    */
   protected ContainerInfo resolveContainerName(String name) {
   	// msc(2005-07-23): from Acs 4.1.2, this method no longer queries the
   	// manager but looks up the requested info in the local maci cache-tree
      /*
   	ContainerInfo[] infos = retrieveContainerInfo("*");
      for (int i = 0; i < infos.length; i++)
         if (name.equalsIgnoreCase(infos[i].name))
            return infos[i];
      return null;
      */
   	synchronized (refreshContainerInfoLock) {
	      for (Enumeration en = containerNode.children(); en.hasMoreElements();) {
	         DefaultMutableTreeNode next = (DefaultMutableTreeNode) en.nextElement();
	         ContainerInfo info = (ContainerInfo) next.getUserObject();
	         if (info.name.equalsIgnoreCase(name))
	            return info;
	      }
	      throw new ContainerUnknownInLocalCacheException(name);
   	}
   }
   
   
   /**
    * Finds all Container info structs.
    * 
    * Searches the local cache.
    * 
    * @return the containerinfos
    */
   protected ContainerInfo[] getContainerInfos() {

   	ArrayList list;
   	
   	synchronized (refreshContainerInfoLock) {
   		 list = Collections.list(containerNode.children());
   	}
   	
		ContainerInfo ret[] = new ContainerInfo[list.size()];
		list.toArray(ret);
		return ret;
   }   
   
   protected DefaultTreeModel treeModel;

   /**
    * The returned TreeModel's root is a TreeNode that is the root of a hierarchy that is a
    * combination of the info retrievable through the other get_XXX_info() methods.
    * 
    * The returned treeModel reference will never become invalid during the lifetime of this
    * MaciSupervisor instance (and it will be automatically updated when this instance is notified
    * by the manager that the MaciInfo has changed).
    * 
    * @throws RuntimeException containing the root cause
    */
   public DefaultTreeModel getMaciInfo() throws RuntimeException {
      
      Throwable err = refreshAll();
      if (err != null)
      	throw new RuntimeException(err);
      
      if (treeModel == null)
         treeModel = new DefaultTreeModel(this.managerNode);
      return treeModel;
   }

   /**
    * Note: The process is not stopped if an exception arises.
    * @return the first exception during the process
    */
   protected Throwable refreshAll () {
   	Throwable t1 =null, t2 = null, t3 = null;
		
		// fills containerNode
		t1 = refreshContainerInfo(true);
		
		// fills clientNode
		t2 = refreshClientInfo();
		
		// fills componentNode
		t3 = refreshComponentInfo();
		
		if (t1 != null)
		   return t1;
		
		if (t2 != null)
		   return t2;

		if (t3 != null)
		   return t3;
		
		return null;
   }

   protected void refreshSome() {

   	if (componentInfoMayBeOutdated) {
         refreshComponentInfo();
         componentInfoMayBeOutdated = false;
         
         refreshContainerInfo(true);
         containerInfoMayBeOutdated = false;
      }
      
      if (containerInfoMayBeOutdated) {
         refreshContainerInfo(true);
         containerInfoMayBeOutdated = false;

         refreshComponentInfo();
         componentInfoMayBeOutdated = false;
      }
      
      if (clientInfoMayBeOutdated) {
         refreshClientInfo();
         clientInfoMayBeOutdated = false;
      }
		
   }
   

   /**
    * Assigns new children (containers) to the containerNode and, optionally, new children (active
    * components) to the containers.
    * The treemodel will be updated when things go well and also when things fail.
    * 
    * @param shouldAssociateComponents
    * @return any error that occured during refresh or <code>null</code>
    */
   protected Throwable refreshContainerInfo(boolean shouldAssociateComponents) {
      
      synchronized (refreshContainerInfoLock) {
      	
	      ContainerInfo[] containers = null;
	      ComponentInfo[] linkedComponents = null;
	
	      containerNode.removeAllChildren();
	
	      log.finer("Searching for Containers...");
			//new Exception().printStackTrace();      
				
	      try {
	         containers = this.retrieveContainerInfo("*");
	
	         //
	         // ---- look up components held by the container ---
	         if (shouldAssociateComponents) {
	            try {
	               log.finer("Searching for active Components...");
	               linkedComponents = this.retrieveComponentInfo(new int[0], "*", "*", true);
	            } catch (Exception exc) {
	            	linkedComponents = new ComponentInfo[]{};
	            }
	         } // -----------------------------------------------
	         //
	
	         for (int i = 0; i < containers.length; i++) {
	            DefaultMutableTreeNode currentNode = createNode(containers[i]);
	            containerNode.add(currentNode);
	
	            //
	            // ---- look up components held by the container ---
	            if (shouldAssociateComponents) {
	
	               // find components containing that handle
	               int handle = containers[i].h;
	               for (int j = 0; j < linkedComponents.length; j++) {
	                  if (linkedComponents[j].container == handle) {
	                     currentNode.add(createNode(linkedComponents[j], false));
	                  }
	               }
	            } // -----------------------------------------------
	            //
	         }
	         
	         containerNode.sortChildrenByInfoDetail("name");
	
	      } catch (Exception exc) {
	         log.info("couldn't refresh container info: " + exc.getClass().getName());
	         return exc;
	      }
	
	      fireTreeModelChange(containerNode);
	      return null;
	   }
   }

   /**
    * Tries to refresh the client-section of the info tree model.
    * The treemodel will be updated when things go well and also when things fail.
    * 
    * @return any error that occured during refresh or <code>null</code>
    */
   protected Throwable refreshClientInfo() {
      
      synchronized (refreshClientInfoLock) {

      ClientInfo[] clients;
      // --- find any client

      log.finer("Searching for Clients...");

      clientNode.removeAllChildren();
      try {
         clients = this.retrieveClientInfo();
         for (int i = 0; i < clients.length; i++)
            clientNode.add(createNode(clients[i]));

         clientNode.sortChildrenByInfoDetail("name");
         
      } catch (Exception exc) {
         log.info("couldn't refresh client info: " + exc.getClass().getName());
         return exc;
      }
      
      fireTreeModelChange(clientNode);
      return null;
   }
   
   }
   
  /**
   * Tries to refresh the component-section of the info tree model.
   * The treemodel will be updated when things go well and also when things fail.
   * 
    * @return any error that occured during refresh or <code>null</code>
   */
   protected Throwable refreshComponentInfo() {
      
   	synchronized (refreshComponentInfoLock) {
   	
      ComponentInfo[] components;
      // --- find any component

      log.finer("Searching for Components...");

      componentNode.removeAllChildren();
      try {
         components = this.retrieveComponentInfo(new int[0], "*", "*", false);

         for (int i = 0; i < components.length; i++)
            componentNode.add(createNode(components[i]));

         componentNode.sortChildrenByInfoDetail("name");
         
      } catch (Exception exc) {
         log.info("couldn't refresh component info: " + exc.getClass().getName());
         return exc;
      };
      
      fireTreeModelChange(componentNode);
      return null;
   }
   
   }


   
      

   /**
    *   
    */   
   protected int myOwnMaciHandle () throws NotConnectedToManagerException {
		if (administratorClientInfo == null)
   		throw new NotConnectedToManagerException("not logged in to manager");
		
		return administratorClientInfo.h;
	}

   protected Manager myManagerReference() throws NotConnectedToManagerException {
		if (managerRef == null)
   		throw new NotConnectedToManagerException("no reference to the manager");
		
   	return managerRef;
   }
   
   /** 
    * assigned in constructor
    */
   protected String name = null;
   
   /**
    * assigned in constructor
    */
   protected String managerLoc = null;
   /**
    * The OAPort for this client (may be null) assigned in constructor
    */
   protected String port = null;
   /**
    *  
    */
   protected Logger log = null;

   /**
    * assigned in initialize()
    */
   protected ORB orb = null;
   
   /**
    * assigned in connectToManager()
    */
   protected Manager managerRef = null;
   
    
   /**
    * Overridden to provide pretty description.
    */
   public String toString() {
   	String s1 = managerLoc;
   	if (s1 == null) {
   		s1 = "<no manager location>";
   	} 
   	else 
   	if (s1.length() > 30){ // limit to 30 chars
   		s1 = s1.substring(0, 22) + "..." + s1.substring(s1.length()-5, s1.length());
   	}
   	return getClass().getName()+"[mgr="+s1+"]";
   }
   
   
   ///////////////////////////////////////////////////////////////////////
   ///---------------------- Maci Info Cache --------------------------///
   ///////////////////////////////////////////////////////////////////////

   /**
    * assigned in start()
    */
   protected SortingTreeNode componentNode = null;
   /**
    * assigned in start()
    */
   protected SortingTreeNode containerNode = null;
   /**
    * assigned in start()
    */
   protected SortingTreeNode clientNode = null;

   /**
    * children are added in start()
    */
   protected SortingTreeNode managerNode;

   /** 
    * For synchronization
    */
   private Object refreshContainerInfoLock = new Object();
   
   /**
    * For synchronization
    */
   private Object refreshComponentInfoLock = new Object();
   
   /**
    * For synchronization
    */
   private Object refreshClientInfoLock = new Object();
   
  /**
   * The ComponentInfo of a component C needs to be refreshed when the
   * container hosting C logged off and on again with a different handle.
   */
   protected boolean componentInfoMayBeOutdated;
   
   /**
    * Yet another lazy-refresh flag attempting to decrease refreshment effort.
    */
   protected boolean containerInfoMayBeOutdated;

   /**
    * Yet another lazy-refresh flag attempting to decrease refreshment effort.
    */
   protected boolean clientInfoMayBeOutdated;
   
   


   /** Factory method */
   protected SortingTreeNode createNode(Object info) {
   	return createNode(info, true);
   }
   
   /** Factory method,
    *  the boolean parameter supresses auto-generated sub-nodes
    */
   protected SortingTreeNode createNode(Object info, boolean allowInfoDetails) {
      SortingTreeNode ret = new SortingTreeNode();
      
      if (info instanceof ContainerInfo) {
         ret.setUserObject(info);
         ContainerInfo casted = (ContainerInfo)info;
         if (allowInfoDetails) {
	         ret.add(createNode(new InfoDetail("location", extractLocation(casted.reference))));
         }         
         if (casted.h != 0)
         	ret.representedHandles = new int[]{casted.h};
      }
      else if (info instanceof ClientInfo) {
         ret.setUserObject(info);
         // --- add some "detail" nodes
         ClientInfo casted = (ClientInfo)info;
         if (allowInfoDetails) {
	         ret.add(createNode(new InfoDetail("location", extractLocation(casted.reference))));
	         ret.add(createNode(new InfoDetail("reference", casted.reference)));
	         ret.add(createNode(new InfoDetail("access", casted.access, false)));
	         ret.add(createNode(new InfoDetail("components", casted.components, true)));
	      }
         if (casted.h != 0)
         	ret.representedHandles = new int[]{casted.h};
      }
      else if (info instanceof ComponentInfo) {
         ret.setUserObject(info);
         // --- add some "detail" nodes
         ComponentInfo casted = (ComponentInfo)info;
         if (allowInfoDetails) {
	         ret.add(createNode(new InfoDetail("type", casted.type)));
	         ret.add(createNode(new InfoDetail("code", casted.code)));
	         ret.add(createNode(new InfoDetail("container_name", casted.container_name)));
	         ret.add(createNode(new InfoDetail("container", casted.container, true)));
	         ret.add(createNode(new InfoDetail("reference", casted.reference)));
	         ret.add(createNode(new InfoDetail("access", casted.access, false)));
	         ret.add(createNode(new InfoDetail("interfaces", casted.interfaces)));
	         ret.add(createNode(new InfoDetail("clients", casted.clients, true)));
         }
         if (casted.h != 0)
         	ret.representedHandles = new int[]{casted.h};
      }
      else if (info instanceof InfoDetail) {
      	InfoDetail casted = (InfoDetail)info;
         ret.setUserObject(info);
         ret.representedHandles = casted.representedHandles;
      }
      else if (info instanceof FolderInfo) {
         ret.setUserObject(info);  
      }
      else {
       ret.setUserObject(info);  
      }
      return ret;
   }
   
   /** 
    * Helper for createNode() 
    * @return <code>{host, port}</code>, or <code>null</code> on failure 
    */
   protected String extractLocation(org.omg.CORBA.Object reference) {
   	String location;
   	try {
   	   location = orb.object_to_string(reference);
   	   String[] hostport = AcsLocations.convert(location);
   	   location = hostport[0] + ":" + hostport[1];
   	} catch(Exception e){
   		location = null;
   	}
   	return location;
   }
   
   /**
    * Will notify all tree listeners that the tree under the specified node has massively changed.
    * 
    * @param node the root node of a modified subtree
    */
   protected void fireTreeModelChange(TreeNode node) {
      if (treeModel != null)
         treeModel.nodeStructureChanged(node);
   }


   
   // wraps a single piece of info from ComponentInfo, each piece is to be shown in its own node.
   static public class InfoDetail {
      public String key;
      public String value;
      
		/** 
		 * Many tree nodes represent 
		 * a) something that has a handle or 
		 * b) something that references other things that have a handle
		 */
		public int[] representedHandles = new int[]{};
      
      
      protected InfoDetail(String key, String[] value) {
         if (value.length == 0)
               this.value = "none";
         else {
            StringBuffer list = new StringBuffer();
            for (int i=0; i<value.length ; i++)
               list.append(String.valueOf(value[i])).append(",");
            list.setLength(list.length() - ",".length());
            this.value = list.toString();
         }
         this.key = key;
      }
      protected InfoDetail(String key, int[] value, boolean intRepresentsHandle) {
         StringBuffer list = new StringBuffer();
         if (value.length == 0)
               list.append("none");
         else {
            for (int i=0; i<value.length ; i++)
               list.append(String.valueOf(value[i])).append(",");
            list.setLength(list.length() - ",".length());
         }
         this.key = key;
         this.value = list.toString();
         if (intRepresentsHandle)
         	this.representedHandles = value;
      }
      protected InfoDetail(String key, Object value) {
         this.key = key;
         this.value = String.valueOf(value);
      }
      protected InfoDetail(String key, int value, boolean intRepresentsHandle) {
         this.key = key;
         this.value = String.valueOf(value);
         if (intRepresentsHandle && value != 0)
         	this.representedHandles = new int[]{value};
      }

      public String toString() {
         return  "Detail \""+key+"="+value+"\"";
       }
   }
   
  /**
   * Used as the userobject for the nodes "containers", "cliental applications", "components"
   */
   static public class FolderInfo {

      public String name;  
      
      protected FolderInfo(String name) {
       this.name = name;  
      }
      
      public String toString() {
        return  "Folder \""+name+"\"";
      }
   }
   
   static public class SortingTreeNode extends DefaultMutableTreeNode {
      
      protected SortingTreeNode() {
         super();
      }
		protected SortingTreeNode(Object userObject) {
			super(userObject);
		}
		
		public int[] representedHandles = new int[]{};
		
      public void sortChildrenByInfoDetail (final String key) {

      	if (children == null)
      		return; // nothing to do
      	
         synchronized(children) {
         	// make a working copy
            Object[] copy = new Object[children.size()];
            children.copyInto(copy);
            // create a comparator for detail-values
            Comparator c = new Comparator(){
               public int compare(Object a, Object b) {
                  String valA = ((SortingTreeNode)a).detailValue(key);
                  String valB = ((SortingTreeNode)b).detailValue(key);
                  return valA.compareTo(valB);
               }
            };
            // sort working copy, then replace original data
            Arrays.sort(copy, c);
            for (int i = 0; i < copy.length; i++) {
					children.set(i, copy[i]);
				}
         }
         
      }

      /**
       * Looks through child nodes with info detail for the InfoDetail with the specified name.
       * Used by the sort-method above.
       */
      protected String detailValue(String key) {

         if (key.equals("name")) {

            if (userObject instanceof ContainerInfo) {
             return ((ContainerInfo)userObject).name;  
            }
            else
            if (userObject instanceof ClientInfo) {
               return ((ClientInfo)userObject).name;  
            }
            else
            if (userObject instanceof ComponentInfo) {
               return ((ComponentInfo)userObject).name;  
            }
            else return "";

         } else {

            for (Enumeration en = children(); en.hasMoreElements();) {

            	try{
               SortingTreeNode elem = (SortingTreeNode) en.nextElement();
               InfoDetail info = (InfoDetail) elem.getUserObject();
               if (key.equals(info.key))
               	return info.value;
               }catch(Exception exc){/*child is not a InfoDetail node, just continue*/}
   			}
         	return ""; // no InfoDetail nodes at all, or none with that key
         }
      }
      
      /** support for merge operations */
      public boolean equals (SortingTreeNode other) {
      	
      	Object otherObject = other.getUserObject();
      	if (!otherObject.getClass().equals(userObject))
      		return false;

      	if (userObject instanceof ContainerInfo) {
      		return ((ContainerInfo)userObject).name.equals( ((ContainerInfo)otherObject).name);
      	
      	} else if (userObject instanceof ClientInfo) {
      		return ((ClientInfo)userObject).h == ((ClientInfo)otherObject).h;
      	
      	} else if (userObject instanceof ComponentInfo) {
      		return ((ComponentInfo)userObject).name.equals( ((ComponentInfo)otherObject).name);
      	
      	} else {
      		return false;
      	}
      }

   }
   
   
   
   
   ///////////////////////////////////////////////////////////////////////
   ///------------- Administrator Client Implementation ---------------///
   ///////////////////////////////////////////////////////////////////////

   /**
    * assigned in connectToManager(). 
    * unassigned in disconnectFromManager().
    * This means, this field indicates the "connected" status.
    */
   protected ClientInfo administratorClientInfo = null;
   
  /**
   * Instantiated in constructor.
   * The retrieved descriptor will be stored in field administratorClientInfo. 
   */
   protected AdministratorImplementation acImpl;
   
      
   /**
    *  Implementation of Administrator Interface
    */
   protected class AdministratorImplementation extends AdministratorPOA {

      
      // =====  Incoming Notifications  =====

      
      public void client_logged_in(ClientInfo arg0) {
         log.finer("client_logged_in() received");
         
         clientInfoMayBeOutdated = true;
         refreshSome();
      }

      public void client_logged_out(int arg0) {
         log.finer("client_logged_out() received");
         
         clientInfoMayBeOutdated = true;
         refreshSome();
      }

      public void container_logged_in(ContainerInfo arg0) {
         log.finer("container_logged_in() received");

         containerInfoMayBeOutdated = true;
         refreshSome();
      }

      public void container_logged_out(int arg0) {
         log.finer("container_logged_out() received");

         containerInfoMayBeOutdated = true;
         refreshSome();
      }

      public void components_requested(int[] arg0, int[] arg1) {
         log.finer("components_requested() received");

         componentInfoMayBeOutdated = true;
         refreshSome();
      }

      public void components_released(int[] arg0, int[] arg1) {
         log.finer("components_released() received");

         componentInfoMayBeOutdated = true;
         refreshSome();
      }

      public void components_available(ComponentInfo[] arg0) {
         log.finer("components_available() received");
         
         componentInfoMayBeOutdated = true;
         refreshSome();
      }

      public void components_unavailable(String[] arg0) {
         log.finer("components_unavailable() received");
         
         componentInfoMayBeOutdated = true;
         refreshSome();
      }

      
      // =====  Requests from the Manager  =====

      
      public String name() {
         log.finer("name() received");
         return name;
      }

      public String authenticate(String arg0) {
         log.finer("authenticate() received");
         return "S";
      }

      public void message(short arg0, String arg1) {
         log.finer("message() received");
         log.info("Incoming Maci Network Message: '" + arg1 + "'");
      }

      public boolean ping() {
         log.finest("ping() received");
         return true;
      }

      public void disconnect() {
         log.finer("disconnect() received");
         disconnectFromManager();
      }
   	
   }
   
   
}
