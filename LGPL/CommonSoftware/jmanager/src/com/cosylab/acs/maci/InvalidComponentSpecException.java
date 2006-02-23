/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci;

import abeans.core.AssertionFailed;
import abeans.core.Identifiable;

/**
 * This exception is thrown when given <code>ComponentSpec</code> is not complete.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class InvalidComponentSpecException extends AssertionFailed
{
	/**
	 * Incomplete specification.
	 */
	ComponentSpec invalidSpec;
	
	/**
	 * Constructor for InvalidComponentSpecException.
	 * @param invalidSpec
	 */
	public InvalidComponentSpecException(ComponentSpec invalidSpec)
	{
		super();
		this.invalidSpec = invalidSpec;
	}

	/**
	 * Constructor for InvalidComponentSpecException.
	 * @param instance
	 * @param s
	 * @param invalidSpec
	 */
	public InvalidComponentSpecException(Identifiable instance, String s, ComponentSpec invalidSpec)
	{
		super(instance, s);
		this.invalidSpec = invalidSpec;
	}

	/**
	 * Constructor for InvalidComponentSpecException.
	 * @param instance
	 * @param message
	 * @param t
	 * @param invalidSpec
	 */
	public InvalidComponentSpecException(
		Identifiable instance,
		String message,
		Throwable t,
		ComponentSpec invalidSpec)
	{
		super(instance, message, t);
		this.invalidSpec = invalidSpec;
	}

	/**
	 * Constructor for InvalidComponentSpecException.
	 * @param s
	 * @param invalidSpec
	 */
	public InvalidComponentSpecException(String s, ComponentSpec invalidSpec)
	{
		super(s);
		this.invalidSpec = invalidSpec;
	}

	/**
	 * Constructor for InvalidComponentSpecException.
	 * @param message
	 * @param t
	 * @param invalidSpec
	 */
	public InvalidComponentSpecException(String message, Throwable t, ComponentSpec invalidSpec)
	{
		super(message, t);
		this.invalidSpec = invalidSpec;
	}

	/**
	 * Get invalid specification.
	 * @return	invalid specification.
	 */
	public ComponentSpec getInvalidSpec()
	{
		return invalidSpec;
	}

	/**
	 * Set invalid specification.
	 * @param spec	invalid specification.
	 */
	public void setInvalidSpec(ComponentSpec spec)
	{
		invalidSpec = spec;
	}

}
