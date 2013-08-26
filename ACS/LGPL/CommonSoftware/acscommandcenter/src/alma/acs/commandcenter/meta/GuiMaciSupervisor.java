/*
 * Created on Oct 10, 2006 by mschilli
 */
package alma.acs.commandcenter.meta;

import java.util.List;
import java.util.logging.Logger;

import org.omg.CORBA.ORB;

import si.ijs.maci.ClientInfo;
import si.ijs.maci.ContainerInfo;

import alma.maciErrType.CannotDeactivateComponentEx;
import alma.maciErrType.CannotGetComponentEx;
import alma.maciErrType.ComponentConfigurationNotFoundEx;
import alma.maciErrType.ComponentDeactivationFailedEx;
import alma.maciErrType.ComponentDeactivationUncleanEx;
import alma.maciErrType.ComponentNotAlreadyActivatedEx;
import alma.maciErrType.NoPermissionEx;
import alma.maciErrType.wrappers.AcsJCannotDeactivateComponentEx;
import alma.maciErrType.wrappers.AcsJComponentDeactivationFailedEx;
import alma.maciErrType.wrappers.AcsJComponentDeactivationUncleanEx;


/**
 * This MaciSupervisor sub class is particularly suitable for Guis like the
 * DeploymentTree, it offers various convenience methods that do not exist in the standard
 * MaciSupervisor.
 * <p>
 * Like the standard MaciSupervisor it translates various unchecked exceptions (=
 * RuntimeExceptions) coming from the Orb into checked exceptions so a client of this
 * class knows what it has to handle.
 * 
 * @author mschilli
 */
public class GuiMaciSupervisor extends MaciSupervisor {

	
	public GuiMaciSupervisor(String clientName, String managerLoc, ORB orb, Logger log) {
		super(clientName, managerLoc, orb, log);
	}
	
	// ============ Sending requests to the Manager ============

	
	/**
	 * Ping Manager.
	 * 
	 * @throws NotConnectedToManagerException
	 * @throws CorbaTransientException 
	 * @throws CorbaNotExistException 
	 * @throws UnknownErrorException 
	 */
	public void managerPing() throws NotConnectedToManagerException, CorbaTransientException, CorbaNotExistException, UnknownErrorException {
		try {
			log.fine("sending ping request to manager (" + getManagerLocation() + ")");
			myManagerReference().ping();
			
		} catch (NotConnectedToManagerException exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw exc;
			
		} catch (org.omg.CORBA.TRANSIENT exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw new CorbaTransientException(exc);

		} catch (org.omg.CORBA.OBJECT_NOT_EXIST exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw new CorbaNotExistException(exc);

		} catch (RuntimeException exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw new UnknownErrorException(exc);
		}
	}
	
	/**
	 * Shutdown Manager.
	 * 
	 * @throws NotConnectedToManagerException 
	 * @throws CorbaNoPermissionException 
	 * @throws CorbaTransientException 
	 * @throws CorbaNotExistException 
	 * @throws UnknownErrorException 
	 */
	public void managerShutdown () throws NotConnectedToManagerException, CorbaNoPermissionException, CorbaTransientException, CorbaNotExistException, UnknownErrorException {
		try {
			log.fine("sending shutdown request to manager (" + getManagerLocation() + ")");
			int hhhhh = myMaciHandle();
			myManagerReference().shutdown(hhhhh, 0);

			
		} catch (NotConnectedToManagerException exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw exc;
			
		} catch (org.omg.CORBA.NO_PERMISSION exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw new CorbaNoPermissionException(exc);
			
		} catch (org.omg.CORBA.TRANSIENT exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw new CorbaTransientException(exc);

		} catch (org.omg.CORBA.OBJECT_NOT_EXIST exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw new CorbaNotExistException(exc);

		} catch (RuntimeException exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw new UnknownErrorException(exc);
		}
	}

	/**
	 * Logout Container.
	 * 
	 * @throws NotConnectedToManagerException 
	 * @throws NoPermissionEx 
	 * @throws CorbaTransientException 
	 * @throws CorbaNotExistException 
	 * @throws UnknownErrorException 
	 */
	public void managerLogout (ContainerInfo info) throws NotConnectedToManagerException, NoPermissionEx, CorbaTransientException, CorbaNotExistException, UnknownErrorException {
		try {
			log.fine("sending logout request to manager to log out container '" + info.name + "'");
			myManagerReference().logout(info.h);

			
		} catch (NotConnectedToManagerException exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw exc;

		} catch (NoPermissionEx exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw exc;

		} catch (org.omg.CORBA.TRANSIENT exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw new CorbaTransientException(exc);

		} catch (org.omg.CORBA.OBJECT_NOT_EXIST exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw new CorbaNotExistException(exc);

		} catch (RuntimeException exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw new UnknownErrorException(exc);
		}
	}

	/**
	 * Logout Client.
	 * 
	 * @throws NotConnectedToManagerException 
	 * @throws NoPermissionEx 
	 * @throws CorbaTransientException 
	 * @throws CorbaNotExistException 
	 * @throws UnknownErrorException 
	 */
	public void managerLogout (ClientInfo info) throws NotConnectedToManagerException, NoPermissionEx, CorbaTransientException, CorbaNotExistException, UnknownErrorException  {
		try {
			log.fine("sending logout request to manager to log out client '" + info.name + "'");
			myManagerReference().logout(info.h);

			
		} catch (NotConnectedToManagerException exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw exc;

		} catch (NoPermissionEx exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw exc;

		} catch (org.omg.CORBA.TRANSIENT exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw new CorbaTransientException(exc);

		} catch (org.omg.CORBA.OBJECT_NOT_EXIST exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw new CorbaNotExistException(exc);

		} catch (RuntimeException exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw new UnknownErrorException(exc);
		}
	}


	/**
	 * Retrieve Component by name.
	 * @return a component
	 * 
	 * @throws ComponentConfigurationNotFoundEx 
	 * @throws CannotGetComponentEx 
	 * @throws ComponentNotAlreadyActivatedEx 
	 * @throws NotConnectedToManagerException 
	 * @throws NoPermissionEx 
	 * @throws CorbaTransientException 
	 * @throws CorbaNotExistException 
	 * @throws UnknownErrorException 
	 */
	public org.omg.CORBA.Object managerGetComponent (String curl) throws ComponentNotAlreadyActivatedEx, CannotGetComponentEx, ComponentConfigurationNotFoundEx, NotConnectedToManagerException, NoPermissionEx, CorbaTransientException, CorbaNotExistException, UnknownErrorException {
		try {
			log.fine("sending get_component request for '" + curl + "'");
			int hhhhh = myMaciHandle();
			org.omg.CORBA.Object stub = myManagerReference().get_component(hhhhh, curl, true);
			
			log.fine("successfully retrieved component '" + curl + "'");
			return stub;

			
		} catch (NotConnectedToManagerException exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw exc;

		} catch (NoPermissionEx exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw exc;

		} catch (org.omg.CORBA.TRANSIENT exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw new CorbaTransientException(exc);

		} catch (org.omg.CORBA.OBJECT_NOT_EXIST exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw new CorbaNotExistException(exc);

		} catch (RuntimeException exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw new UnknownErrorException(exc);
		}
	}

	/**
	 * Retrieve Component by name, non-sticky.
	 * @return a component
	 * 
	 * @throws ComponentNotAlreadyActivatedEx 
	 * @throws CannotGetComponentEx 
	 * @throws NotConnectedToManagerException 
	 * @throws NoPermissionEx 
	 * @throws CorbaTransientException 
	 * @throws CorbaNotExistException 
	 * @throws UnknownErrorException 
	 */
	public org.omg.CORBA.Object managerGetComponentNonSticky (String curl) throws ComponentNotAlreadyActivatedEx, CannotGetComponentEx, NotConnectedToManagerException, NoPermissionEx, CorbaTransientException, CorbaNotExistException, UnknownErrorException {
		try {
			log.fine("sending get_component_non_sticky request for '" + curl + "'");
			int hhhhh = myMaciHandle();
			org.omg.CORBA.Object stub = myManagerReference().get_component_non_sticky(hhhhh, curl);
			
			log.fine("successfully retrieved component '" + curl + "' as non_sticky");
			return stub;


		} catch (NotConnectedToManagerException exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw exc;

		} catch (NoPermissionEx exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw exc;

		} catch (org.omg.CORBA.TRANSIENT exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw new CorbaTransientException(exc);

		} catch (org.omg.CORBA.OBJECT_NOT_EXIST exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw new CorbaNotExistException(exc);

		} catch (RuntimeException exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw new UnknownErrorException(exc);
		}
	}
	

	/**
	 * Release components by name.
	 * 
	 * @throws UnknownErrorException 
	 * @throws NotConnectedToManagerException 
	 * @throws NoPermissionEx 
	 * @throws CorbaTransientException 
	 * @throws CorbaNotExistException 
	 * @throws CorbaUnknownException 
	 */
	public void managerReleaseComponents (String[] curls) 
		throws UnknownErrorException, NotConnectedToManagerException, NoPermissionEx, CorbaTransientException, CorbaNotExistException, CorbaUnknownException, 
		AcsJCannotDeactivateComponentEx, AcsJComponentDeactivationUncleanEx, AcsJComponentDeactivationFailedEx
	{
		try {
			log.fine("sending release_component requests to manager");
			int hhhhh = myMaciHandle();
			for (int i = 0; i < curls.length; i++) {
				myManagerReference().release_component(hhhhh, curls[i]);
			}
			
			/* the first bunch of errors are not manager-communication
			 * problems, so we do not pass them to the mce-handler. */
			
		} catch (org.omg.CORBA.UNKNOWN exc) {
			/* may be thrown by manager if component is unknown.
			 * thus, this is not a manager-communication problem. */
			throw new CorbaUnknownException(exc);

		} catch (CannotDeactivateComponentEx ex) { // @TODO remove this catch once we remove this ex from maci.idl
			throw AcsJCannotDeactivateComponentEx.fromCannotDeactivateComponentEx(ex);
		
		} catch (ComponentDeactivationUncleanEx ex) {
			throw AcsJComponentDeactivationUncleanEx.fromComponentDeactivationUncleanEx(ex);
		
		} catch (ComponentDeactivationFailedEx ex) {
			throw AcsJComponentDeactivationFailedEx.fromComponentDeactivationFailedEx(ex);
		
		} catch (NotConnectedToManagerException exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw exc;

		} catch (NoPermissionEx exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw exc;

		} catch (org.omg.CORBA.TRANSIENT exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw new CorbaTransientException(exc);

		} catch (org.omg.CORBA.OBJECT_NOT_EXIST exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw new CorbaNotExistException(exc);

		} catch (RuntimeException exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw new UnknownErrorException(exc);
		}
	}


	/**
	 * Force-release component by name.
	 * 
	 * @throws NotConnectedToManagerException 
	 * @throws NoPermissionEx 
	 * @throws CorbaTransientException 
	 * @throws CorbaNotExistException 
	 * @throws UnknownErrorException 
	 * @throws CorbaNotExistException 
	 * @throws UnknownErrorException 
	 */
	public void managerForceReleaseComponent (String curl) throws NotConnectedToManagerException, NoPermissionEx, CorbaTransientException, CorbaNotExistException, UnknownErrorException {
		try {
			log.fine("sending force_release_component request to manager");
			int hhhhh = myMaciHandle();
			myManagerReference().force_release_component(hhhhh, curl);

			
		} catch (NotConnectedToManagerException exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw exc;

		} catch (NoPermissionEx exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw exc;

		} catch (org.omg.CORBA.TRANSIENT exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw new CorbaTransientException(exc);

		} catch (org.omg.CORBA.OBJECT_NOT_EXIST exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw new CorbaNotExistException(exc);

		} catch (RuntimeException exc) {
			mcehandler.handleExceptionTalkingToManager(exc);
			throw new UnknownErrorException(exc);
		}
	}


	// ============ Sending requests to Containers ==============


	/**
	 * Sends a disconnect request to a container.
	 * 
	 * @throws CorbaTransientException 
	 * @throws UnknownErrorException 
	 */
	public void containerDisconnect (ContainerInfo info) throws CorbaTransientException, UnknownErrorException {
		try {
			log.fine("sending disconnect request to container '" + info.name + "'");
			info.reference.disconnect();
		
			
		} catch (org.omg.CORBA.TRANSIENT exc) {
			throw new CorbaTransientException(exc);
	
		} catch (RuntimeException exc) {
			throw new UnknownErrorException(exc);
		}
	}


	/**
	 * Sends a ping request to a container.
	 * 
	 * @throws CorbaTransientException 
	 * @throws UnknownErrorException 
	 */
	public void containerPing (ContainerInfo info) throws CorbaTransientException, UnknownErrorException {
		try {
			log.fine("sending ping request to container '" + info.name + "'");
			info.reference.ping();
			
			
		} catch (org.omg.CORBA.TRANSIENT exc) {
			throw new CorbaTransientException(exc);

		} catch (RuntimeException exc) {
			throw new UnknownErrorException(exc);
		}
	}


	/**
	 * Sends a shutdown request to a container.
	 * Note that the request is asynchronous: when this method returns the container is
	 * likely to not have terminated yet.
	 * 
	 * @throws CorbaTransientException 
	 * @throws UnknownErrorException 
	 */
	public void containerShutdown (ContainerInfo info) throws CorbaTransientException, UnknownErrorException {
		try {
			log.fine("sending shutdown request to container '" + info.name + "'");
			// hibyte: 2 (= EXIT), lobyte: 0 (exitcode towards the OS)
			int action = 2 * 256 + 0;
			info.reference.shutdown(action);
			
			
		} catch (org.omg.CORBA.TRANSIENT exc) {
			throw new CorbaTransientException(exc);

		} catch (RuntimeException exc) {
			throw new UnknownErrorException(exc);
		}
	}


	/**
	 * Sends a message to a container.
	 * 
	 * @param info the ContainerInfo
	 * @param msgType one of MSG_ERROR, MSG_INFORMATION
	 * @param msg the message
	 * 
	 * @throws CorbaTransientException 
	 * @throws UnknownErrorException 
	 */
	public void containerMessage (ContainerInfo info, short msgType, String msg) throws CorbaTransientException, UnknownErrorException {
		try {
			log.fine("sending message to container '" + info.name + "'");
			info.reference.message(msgType, msg);
			
			
		} catch (org.omg.CORBA.TRANSIENT exc) {
			throw new CorbaTransientException(exc);

		} catch (RuntimeException exc) {
			throw new UnknownErrorException(exc);
		}
	}



	/**
	 * Sends a shutdown request to every container. Note that the request is asynchronous:
	 * when this method returns the container is likely to not have terminated yet.
	 * 
	 * @throws UnknownErrorException 
	 * @throws CorbaTransientException 
	 */
	public void containersShutdown () throws CorbaTransientException, UnknownErrorException {
		List<ContainerInfo> cc = maciInfo.getContainers();
		for (ContainerInfo c : cc) {
			containerShutdown(c);
		}
	}


	// =============== Sending requests to Clients ====================


	/**
	 * Sends a disconnect request to a client.
	 * 
	 * @param info the ClientInfo
	 * 
	 * @throws CorbaTransientException 
	 * @throws UnknownErrorException 
	 */
	public void clientDisconnect (ClientInfo info) throws CorbaTransientException, UnknownErrorException {
		try {
			log.fine("sending disconnect request to client '" + info.name + "'");
			info.reference.disconnect();
			
			
		} catch (org.omg.CORBA.TRANSIENT exc) {
			throw new CorbaTransientException(exc);

		} catch (RuntimeException exc) {
			throw new UnknownErrorException(exc);
		}
	}

	/**
	 * Sends a ping request to a client.
	 * 
	 * @param info the ClientInfo
	 * 
	 * @throws CorbaTransientException 
	 * @throws UnknownErrorException 
	 */
	public void clientPing (ClientInfo info) throws CorbaTransientException, UnknownErrorException {
		try {
			log.fine("sending ping request to client '" + info.name + "'");
			info.reference.ping();
			
			
		} catch (org.omg.CORBA.TRANSIENT exc) {
			throw new CorbaTransientException(exc);

		} catch (RuntimeException exc) {
			throw new UnknownErrorException(exc);
		}
	}

	/**
	 * Sends a message to a client.
	 * 
	 * @param info the ClientInfo
	 * @param msgType one of MSG_ERROR, MSG_INFORMATION
	 * @param msg the message
	 * 
	 * @throws CorbaTransientException 
	 * @throws UnknownErrorException 
	 */
	public void clientMessage (ClientInfo info, short msgType, String msg) throws CorbaTransientException, UnknownErrorException {
		try {
			log.fine("sending message to client '" + info.name + "'");
			info.reference.message(msgType, msg);
			
			
		} catch (org.omg.CORBA.TRANSIENT exc) {
			throw new CorbaTransientException(exc);

		} catch (RuntimeException exc) {
			throw new UnknownErrorException(exc);
		}
	}



}
