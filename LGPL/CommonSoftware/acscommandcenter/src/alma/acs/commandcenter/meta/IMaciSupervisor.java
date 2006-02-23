/*
 * Created on Jul 20, 2005 by mschilli
 */
package alma.acs.commandcenter.meta;

import javax.swing.tree.DefaultTreeModel;

import org.omg.CORBA.ORB;

import si.ijs.maci.AdministratorPOA;
import si.ijs.maci.ClientInfo;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ContainerInfo;



public interface IMaciSupervisor {

	static public final short MSG_INFORMATION = AdministratorPOA.MSG_INFORMATION;
	static public final short MSG_ERROR = AdministratorPOA.MSG_ERROR;

	/**
	 * start() must be called after construction, before usage.
	 */
	public void start ();

	/**
	 * Invoking this method is equal to invoking disconnect().
	 */
	public void stop ();

	/** 
	 * msc (2004-11-09): This was requested, suggested, and agreed upon as a workaround for
	 * the situation where a manager has gone down (and the application using this macisupervisor
	 * knows that the manager is down, e.g. because it made it go down) while this macisupervisor
	 * does not yet know that the manager is down. It will only realize that the next time it tries
	 * to access it. Doing so will provoke some no_permission error messages in the manager log.
	 * To enable the application to avoid these error messages this API method was added. 
	 */
	public void dismissManager ();

	
	/** 
	 * Whether this is connected to a manager
	 */
	public boolean isConnected();
	
	
	/**
	 * Some people are interested in the weirdest things..
	 */
	public String getManagerLocation ();

	/**
	 *   
	 */
	public void shutdownManager () throws NotConnectedToManagerException ;

	/**
	 *   
	 */
	public void logoutContainer (ContainerInfo info) throws NotConnectedToManagerException ;

	/**
	 *   
	 */
	public void logoutClient (ClientInfo info) throws NotConnectedToManagerException ;

	/**
	 * Sends a shutdown request to every container. Note that the request is asynchronous: when this
	 * method returns the container is likely to not have terminated yet.
	 * 
	 * @since 3.0.2
	 */
	public void shutdownContainers ();

	/**
	 * Resolves the specified name and forwards to {@link #shutdownContainer(ContainerInfo)}.
	 * 
	 * @param name the case-insensitive name (no wildcards) of a container
	 * @since 3.0.2
	 */
	public void shutdownContainer (String name);

	/**
	 * Sends a shutdown request to the container described by the specified ContainerInfo. Note that
	 * the request is asynchronous: when this method returns the container is likely to not have
	 * terminated yet.
	 * 
	 * @since 3.1
	 */
	public void shutdownContainer (ContainerInfo info);

	/**
	 * Resolves the specified name and forwards to {@link #disconnectContainer(ContainerInfo)}.
	 * 
	 * @param name the case-insensitive name (no wildcards) of a container
	 * @since 3.0.2
	 */
	public void disconnectContainer (String name);

	/**
	 * Sends a disconnect request to the container described by the specified ContainerInfo.
	 * 
	 * @since 3.1
	 */
	public void disconnectContainer (ContainerInfo info);

	/**
	 * Resolves the specified name and forwards to {@link #pingContainer(ContainerInfo)}.
	 * 
	 * @param name the case-insensitive name (no wildcards) of a container
	 * @since 3.0.2
	 */
	public void pingContainer (String name);

	/**
	 * Sends a ping request to the container described by the specified ContainerInfo.
	 * 
	 * @since 3.1
	 */
	public void pingContainer (ContainerInfo info);

	/**
	 * @param info the ContainerInfo
	 * @param msgType one of MSG_ERROR, MSG_INFORMATION
	 * @param msg the message
	 */
	public void sendMessageToContainer (ContainerInfo info, short msgType, String msg);

	/**
	 * Sends a disconnect request to the client described by the specified ClientInfo.
	 * 
	 * @since 3.1
	 */
	public void disconnectClient (ClientInfo info);

	/**
	 * Sends a ping request to the client described by the specified ClientInfo.
	 * 
	 * @since 3.1
	 */
	public void pingClient (ClientInfo info);

	/**
	 * @param info the ClientInfo
	 * @param msgType one of MSG_ERROR, MSG_INFORMATION
	 * @param msg the message
	 */
	public void sendMessageToClient (ClientInfo info, short msgType, String msg);

	/**
	 * The containerHandles argument cannot be specified here. Reason: There's apparently a bug in
	 * Manager.get_activator_info() in ACS2.x (nothing known about ACS3.x): The containerHandles
	 * argument is not evaluated properly, instead empty (therefore useless) ContainerInfos are
	 * returned.
	 * 
	 * @param name_wildcard not verified that it works as expected, recommended to use '*'.
	 * @return
	 */
	public ContainerInfo[] retrieveContainerInfo (String name_wildcard) throws NotConnectedToManagerException ;

	/**
	 * @throws NotConnectedToManagerException 
	 *   
	 */
	public ClientInfo[] retrieveClientInfo () throws NotConnectedToManagerException;

	/**
	 * @throws NotConnectedToManagerException 
	 *   
	 */
	public ComponentInfo[] retrieveComponentInfo (int[] cobHandles, String name_wildcard, String type_wildcard, boolean active_only) throws NotConnectedToManagerException;
	
	
	/**	
	 * 
	 * @param curl
	 * @return
	 * @throws NotConnectedToManagerException 
	 */
   public org.omg.CORBA.Object getComponent(String curl) throws NotConnectedToManagerException;

   /**
    * 
    * @param curl
    * @throws NotConnectedToManagerException
    * @since Acs 5.0
    */
   public void forceReleaseComponent(String curl) throws NotConnectedToManagerException;

	/**
	 * 
	 * @param curls
	 * @throws NotConnectedToManagerException 
	 */
	public void releaseComponents(String[] curls) throws NotConnectedToManagerException;
		
	
	/**
	 * @return info about the container for a specified component
	 */
	public ContainerInfo getContainerInfoForComponent (String componentName);

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
	public DefaultTreeModel getMaciInfo () throws RuntimeException;

	
	
	public static class ComponentUnknownInLocalCacheException extends IllegalArgumentException {

		protected ComponentUnknownInLocalCacheException() {
			super();
		}

		protected ComponentUnknownInLocalCacheException(String s) {
			super(s);
		}
		
	}
	
	public static class ContainerUnknownInLocalCacheException extends IllegalArgumentException {

		protected ContainerUnknownInLocalCacheException() {
			super();
		}

		protected ContainerUnknownInLocalCacheException(String s) {
			super(s);
		}
		
	}
	

	public static class ComponentNotActivatedInLocalCacheException extends IllegalArgumentException {

		protected ComponentNotActivatedInLocalCacheException() {
			super();
		}

		protected ComponentNotActivatedInLocalCacheException(String s) {
			super(s);
		}
		
	}
	
	
	public static class NotConnectedToManagerException extends IllegalStateException {
		
		protected NotConnectedToManagerException() {
			super();
		}

		protected NotConnectedToManagerException(String s) {
			super(s);
		}
		
	}

	
	public static class LocalCacheEmptyException extends IllegalStateException {
		
		protected LocalCacheEmptyException() {
			super();
		}

		protected LocalCacheEmptyException(String s) {
			super(s);
		}
		
	}
	
	
}



