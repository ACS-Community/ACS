/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci;

/**
 * This exception is thrown when there is no default component.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class NoDefaultComponentException extends Exception
{
	/**
	 * Serial version UID. 
	 */
	private static final long serialVersionUID = 6353311481636278718L;

	/**
	 * Constructor for NoDefaultComponentException.
	 */
	public NoDefaultComponentException()
	{
		super();
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
