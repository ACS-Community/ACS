/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci;

/**
 * Timeout Remote exception.
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class TimeoutRemoteException extends RemoteException
{
	/**
	 * Serial version UID. 
	 */
	private static final long serialVersionUID = -237154508894374528L;

	/**
	 * Constructor for NoResourcesException.
	 */
	public TimeoutRemoteException()
	{
		super();
	}

	/**
	 * Constructor for NoResourcesException.
	 * @param s
	 */
	public TimeoutRemoteException(String s)
	{
		super(s);
	}

	/**
	 * Constructor for NoResourcesException.
	 * @param message
	 * @param t
	 */
	public TimeoutRemoteException(String message, Throwable t)
	{
		super(message, t);
	}

	
}
