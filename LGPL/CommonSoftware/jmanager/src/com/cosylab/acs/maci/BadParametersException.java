/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci;

import abeans.core.AssertionFailed;
import abeans.core.Identifiable;

/**
 * This exception is throwns when requestor did not have enough
 * access rights to perform a request.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class BadParametersException extends AssertionFailed
{
	/**
	 * Constructor for BadParametersException.
	 */
	public BadParametersException()
	{
		super();
	}

	/**
	 * Constructor for BadParametersException.
	 * @param instance
	 * @param s
	 */
	public BadParametersException(Identifiable instance, String s)
	{
		super(instance, s);
	}

	/**
	 * Constructor for BadParametersException.
	 * @param instance
	 * @param message
	 * @param t
	 */
	public BadParametersException(
		Identifiable instance,
		String message,
		Throwable t)
	{
		super(instance, message, t);
	}

	/**
	 * Constructor for BadParametersException.
	 * @param s
	 */
	public BadParametersException(String s)
	{
		super(s);
	}

	/**
	 * Constructor for BadParametersException.
	 * @param message
	 * @param t
	 */
	public BadParametersException(String message, Throwable t)
	{
		super(message, t);
	}

}
