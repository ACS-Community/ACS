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
public class NoPermissionException extends AssertionFailed
{
	/**
	 * Constructor for NoPermissionException.
	 */
	public NoPermissionException()
	{
		super();
	}

	/**
	 * Constructor for NoPermissionException.
	 * @param instance
	 * @param s
	 */
	public NoPermissionException(Identifiable instance, String s)
	{
		super(instance, s);
	}

	/**
	 * Constructor for NoPermissionException.
	 * @param instance
	 * @param message
	 * @param t
	 */
	public NoPermissionException(
		Identifiable instance,
		String message,
		Throwable t)
	{
		super(instance, message, t);
	}

	/**
	 * Constructor for NoPermissionException.
	 * @param s
	 */
	public NoPermissionException(String s)
	{
		super(s);
	}

	/**
	 * Constructor for NoPermissionException.
	 * @param message
	 * @param t
	 */
	public NoPermissionException(String message, Throwable t)
	{
		super(message, t);
	}

}
