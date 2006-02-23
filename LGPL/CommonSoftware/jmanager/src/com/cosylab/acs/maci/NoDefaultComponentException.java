/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci;

import abeans.core.AssertionFailed;
import abeans.core.Identifiable;

/**
 * This exception is thrown when there is no default component.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class NoDefaultComponentException extends AssertionFailed
{
	/**
	 * Constructor for NoDefaultComponentException.
	 */
	public NoDefaultComponentException()
	{
		super();
	}

	/**
	 * Constructor for NoDefaultComponentException.
	 * @param instance
	 * @param s
	 */
	public NoDefaultComponentException(Identifiable instance, String s)
	{
		super(instance, s);
	}

	/**
	 * Constructor for NoDefaultComponentException.
	 * @param instance
	 * @param message
	 * @param t
	 */
	public NoDefaultComponentException(
		Identifiable instance,
		String message,
		Throwable t)
	{
		super(instance, message, t);
	}

	/**
	 * Constructor for NoDefaultComponentException.
	 * @param s
	 */
	public NoDefaultComponentException(String s)
	{
		super(s);
	}

	/**
	 * Constructor for NoDefaultComponentException.
	 * @param message
	 * @param t
	 */
	public NoDefaultComponentException(String message, Throwable t)
	{
		super(message, t);
	}

}
