/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci;

import abeans.core.AssertionFailed;
import abeans.core.Identifiable;

/**
 * This exception is thrown when requestor did not have enough
 * access rights to perform a request.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class NoResourcesException extends AssertionFailed
{
	/**
	 * Constructor for NoResourcesException.
	 */
	public NoResourcesException()
	{
		super();
	}

	/**
	 * Constructor for NoResourcesException.
	 * @param instance
	 * @param s
	 */
	public NoResourcesException(Identifiable instance, String s)
	{
		super(instance, s);
	}

	/**
	 * Constructor for NoResourcesException.
	 * @param instance
	 * @param message
	 * @param t
	 */
	public NoResourcesException(
		Identifiable instance,
		String message,
		Throwable t)
	{
		super(instance, message, t);
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
