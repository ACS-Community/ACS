/*
 * Created on Jul 20, 2005 by mschilli
 */
package alma.acs.commandcenter.meta;


import org.omg.CORBA.NO_PERMISSION;
import org.omg.CORBA.OBJECT_NOT_EXIST;
import org.omg.CORBA.TRANSIENT;
import org.omg.CORBA.UNKNOWN;

import si.ijs.maci.AdministratorPOA;
import alma.maciErrType.NoPermissionEx;



public interface IMaciSupervisor {

	static public final short MSG_INFORMATION = AdministratorPOA.MSG_INFORMATION;
	static public final short MSG_ERROR = AdministratorPOA.MSG_ERROR;

	/**
	 * Start must be called after construction, before usage.
	 * 
	 * @return whether start worked
	 * @throws CannotRetrieveManagerException 
	 * @throws NoPermissionEx 
	 * @throws CorbaTransientException 
	 * @throws CorbaNotExistException 
	 * @throws UnknownErrorException 
	 */
	public void start () throws CannotRetrieveManagerException, NoPermissionEx, CorbaTransientException, CorbaNotExistException, UnknownErrorException;	

	/**
	 * Stop this instance. It's possible to start again later on.
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
	 * Returns the handle given by the manager.
	 * 
	 * @return the handle given by the manager.
	 * @throws NotConnectedToManagerException if no handle available
	 */
	public int myMaciHandle() throws NotConnectedToManagerException;


	/**
	 * Returns a maciInfo instance as retrieved from the manager.
	 * 
	 * @return maciInfo
	 * @throws NotConnectedToManagerException 
	 * @throws NoPermissionEx 
	 * @throws UnknownErrorException 
	 * @throws CorbaNotExistException 
	 * @throws CorbaTransientException 
	 */
	public MaciInfo getMaciInfo () throws NoPermissionEx, NotConnectedToManagerException, CorbaTransientException, CorbaNotExistException, UnknownErrorException;

	
	/**
	 * Base class of the exceptions we're creating ourselves.
	 */
	public static class MaciSupervisorException extends Exception {
		protected MaciSupervisorException(String s) {
			super(s);
		}
		protected MaciSupervisorException(String s, Throwable t) {
			super(s, t);
		}
	} 
	
	/**
	 * Can happen at every access to the manager. 
	 */
	public static class NotConnectedToManagerException extends MaciSupervisorException {
		protected NotConnectedToManagerException(String s) {
			super(s);
		}
	}

	/**
	 * Can happen at every access to the manager. 
	 */
	public static class CorbaTransientException extends MaciSupervisorException {
		protected CorbaTransientException (TRANSIENT exc) {
			super("remote object has disappeared", exc);
		}
	}

	/**
	 * Can happen when trying to shutdown the manager.
	 * (all other manager methods allegedly will throw a NoPermissionEx but since
	 * Manager::shutdown is oneway it is not possible to throw a UserException from
	 * it, Mail Matej 2006-10-17)  
	 */
	public static class CorbaNoPermissionException extends MaciSupervisorException {
		protected CorbaNoPermissionException (NO_PERMISSION exc) {
			super("remote object refuses communication", exc);
		}
	}
	
	/**
	 * Can happen when releasing a component that wasn't activated before.
	 */
	public static class CorbaUnknownException extends MaciSupervisorException {
		protected CorbaUnknownException (UNKNOWN exc) {
			super("no such remote object known", exc);
		}
	}
	
	/**
	 * Can happen when trying to log in to the manager. 
	 */
	public static class CorbaNotExistException extends MaciSupervisorException {
		protected CorbaNotExistException (OBJECT_NOT_EXIST exc) {
			super("remote object doesn't exist", exc);
		}
	}
	
	/**
	 * Can happen when trying to log in to the manager.
	 */
	public static class CannotRetrieveManagerException extends MaciSupervisorException {
		public CannotRetrieveManagerException(String s) {
			super(s);
		}
		protected CannotRetrieveManagerException(String s, Throwable t) {
			super(s, t);
		}
	}

   // not sure i should use this
	public static class UnknownErrorException extends MaciSupervisorException {
		protected UnknownErrorException (RuntimeException exc) {
			super("unknown error", exc);
		}
	}
	
	
	
	
}



