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
public class IncompleteComponentSpecException extends AssertionFailed
{
	/**
	 * Incomplete specification.
	 */
	ComponentSpec incompleteSpec;
	
	/**
	 * Constructor for IncompleteComponentSpecException.
	 * @param incompleteSpec
	 */
	public IncompleteComponentSpecException(ComponentSpec incompleteSpec)
	{
		super();
		this.incompleteSpec = incompleteSpec;
	}

	/**
	 * Constructor for IncompleteComponentSpecException.
	 * @param instance
	 * @param s
	 * @param incompleteSpec
	 */
	public IncompleteComponentSpecException(Identifiable instance, String s, ComponentSpec incompleteSpec)
	{
		super(instance, s);
		this.incompleteSpec = incompleteSpec;
	}

	/**
	 * Constructor for IncompleteComponentSpecException.
	 * @param instance
	 * @param message
	 * @param t
	 * @param incompleteSpec
	 */
	public IncompleteComponentSpecException(
		Identifiable instance,
		String message,
		Throwable t,
		ComponentSpec incompleteSpec)
	{
		super(instance, message, t);
		this.incompleteSpec = incompleteSpec;
	}

	/**
	 * Constructor for IncompleteComponentSpecException.
	 * @param s
	 * @param incompleteSpec
	 */
	public IncompleteComponentSpecException(String s, ComponentSpec incompleteSpec)
	{
		super(s);
		this.incompleteSpec = incompleteSpec;
	}

	/**
	 * Constructor for IncompleteComponentSpecException.
	 * @param message
	 * @param t
	 * @param incompleteSpec
	 */
	public IncompleteComponentSpecException(String message, Throwable t, ComponentSpec incompleteSpec)
	{
		super(message, t);
		this.incompleteSpec = incompleteSpec;
	}

	/**
	 * Get incomplete specification.
	 * @return	incomplete specification.
	 */
	public ComponentSpec getIncompleteSpec()
	{
		return incompleteSpec;
	}

	/**
	 * Set incomplete specification.
	 * @param spec	incomplete specification.
	 */
	public void setIncompleteSpec(ComponentSpec spec)
	{
		incompleteSpec = spec;
	}

}
