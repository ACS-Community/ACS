/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci;

/**
 * This exception is throwns when requestor did not have enough
 * access rights to perform a request.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class BadParametersException extends RuntimeException
{
	/**
	 * Serial version UID. 
	 */
	private static final long serialVersionUID = -1912473918645274498L;

	/**
	 * Constructor for BadParametersException.
	 */
	public BadParametersException()
	{
		super();
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
