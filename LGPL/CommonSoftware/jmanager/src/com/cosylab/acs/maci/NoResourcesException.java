/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci;

/**
 * This exception is thrown when requestor did not have enough
 * access rights to perform a request.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class NoResourcesException extends RuntimeException
{
	/**
	 * Serial version UID. 
	 */
	private static final long serialVersionUID = 9049082656705515827L;

	/**
	 * Constructor for NoResourcesException.
	 */
	public NoResourcesException()
	{
		super();
	}

	/**
	 * Constructor for NoResourcesException.
	 * @param s
	 */
	public NoResourcesException(String s)
	{
		super(s);
	}

	/**
	 * Constructor for NoResourcesException.
	 * @param message
	 * @param t
	 */
	public NoResourcesException(String message, Throwable t)
	{
		super(message, t);
	}

}
