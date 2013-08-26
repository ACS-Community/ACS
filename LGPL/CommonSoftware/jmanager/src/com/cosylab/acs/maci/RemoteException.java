/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci;

/**
 * Remote exception.
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class RemoteException extends RuntimeException
{
	/**
	 * Serial version UID. 
	 */
	private static final long serialVersionUID = -8010272337298228962L;

	/**
	 * Constructor for NoResourcesException.
	 */
	public RemoteException()
	{
		super();
	}

	/**
	 * Constructor for NoResourcesException.
	 * @param s
	 */
	public RemoteException(String s)
	{
		super(s);
	}

	/**
	 * Constructor for NoResourcesException.
	 * @param message
	 * @param t
	 */
	public RemoteException(String message, Throwable t)
	{
		super(message, t);
	}

	
}
