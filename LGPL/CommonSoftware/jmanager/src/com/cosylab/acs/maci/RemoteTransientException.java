/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci;

/**
 * Defined specialized <code>RemoteException</code>, this implementation
 * defines an transient remote exception (e.g. CORBA::TRANSIENT()).
 * Remote exception is thrown when a processing of a request fails because of an
 * underlying failure in the remote layer. For example, if some request fails to execute
 * on the remote layer and the remote server returns an error response, that response 
 * should be packed into the <code>RemoteException</code>.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class RemoteTransientException extends RemoteException
{

	/**
	 * Serial version UID. 
	 */
	private static final long serialVersionUID = -1093411214533539296L;

	/**
	 * 
	 */
	public RemoteTransientException() {
		super();
	}

	/**
	 * @param message
	 * @param t
	 */
	public RemoteTransientException(String message, Throwable t) {
		super(message, t);
	}

	/**
	 * @param s
	 */
	public RemoteTransientException(String s) {
		super(s);
	}
	
}
