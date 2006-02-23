/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci;

import abeans.core.AssertionFailed;
import abeans.core.Identifiable;

/**
 * This exception is thrown when there is a activated - requested component request collision.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class ComponentSpecIncompatibleWithActiveComponentException extends AssertionFailed
{
	/**
	 * Active component specification.
	 */
	ComponentSpec activeComponentSpec;
	
	/**
	 * Constructor for ComponentSpecIncompatibleWithActiveComponentException.
	 * @param activeComponentSpec
	 */
	public ComponentSpecIncompatibleWithActiveComponentException(ComponentSpec activeComponentSpec)
	{
		super();
		this.activeComponentSpec = activeComponentSpec;
	}

	/**
	 * Constructor for ComponentSpecIncompatibleWithActiveComponentException.
	 * @param instance
	 * @param s
	 * @param activeComponentSpec
	 */
	public ComponentSpecIncompatibleWithActiveComponentException(Identifiable instance, String s, ComponentSpec activeComponentSpec)
	{
		super(instance, s);
		this.activeComponentSpec = activeComponentSpec;
	}

	/**
	 * Constructor for ComponentSpecIncompatibleWithActiveComponentException.
	 * @param instance
	 * @param message
	 * @param t
	 * @param activeComponentSpec
	 */
	public ComponentSpecIncompatibleWithActiveComponentException(
		Identifiable instance,
		String message,
		Throwable t,
		ComponentSpec activeComponentSpec)
	{
		super(instance, message, t);
		this.activeComponentSpec = activeComponentSpec;
	}

	/**
	 * Constructor for ComponentSpecIncompatibleWithActiveComponentException.
	 * @param s
	 * @param activeComponentSpec
	 */
	public ComponentSpecIncompatibleWithActiveComponentException(String s, ComponentSpec activeComponentSpec)
	{
		super(s);
		this.activeComponentSpec = activeComponentSpec;
	}

	/**
	 * Constructor for ComponentSpecIncompatibleWithActiveComponentException.
	 * @param message
	 * @param t
	 * @param activeComponentSpec
	 */
	public ComponentSpecIncompatibleWithActiveComponentException(String message, Throwable t, ComponentSpec activeComponentSpec)
	{
		super(message, t);
		this.activeComponentSpec = activeComponentSpec;
	}

	/**
	 * Get active component specification.
	 * @return	active component specification.
	 */
	public ComponentSpec getActiveComponentSpec()
	{
		return activeComponentSpec;
	}

	/**
	 * Set active component specification.
	 * @param spec	active component specification.
	 */
	public void setActiveComponentSpec(ComponentSpec spec)
	{
		activeComponentSpec = spec;
	}

}
