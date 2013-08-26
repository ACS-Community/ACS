/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci;

/**
 * Status of the Component object.
 * Can be one of: <code>COMPONENT_ACTIVATED</code>, <code>COMPONENT_DOES_NO_EXIST</code> and <code>COMPONENT_NOT_ACTIVATED</code>.
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public final class ComponentStatus
{

	/**
	 * Status of activated Component.
	 */
	public static final ComponentStatus COMPONENT_ACTIVATED = new ComponentStatus("COMPONENT_ACTIVATED", "Component is activated.");

	/**
	 * Status of non-existant Component.
	 */
	public static final ComponentStatus COMPONENT_DOES_NO_EXIST = new ComponentStatus("COMPONENT_DOES_NO_EXIST", "Component does not exists.");

	/**
	 * Status of non-activated Component.
	 */
	public static final ComponentStatus COMPONENT_NOT_ACTIVATED = new ComponentStatus("COMPONENT_NOT_ACTIVATED", "Component is not activated.");

	/**
	 * The name of the status (e.g. COMPONENT_ACTIVATED).
	 */
	private String name;

	/**
	 * The description of the status.
	 */
	private String description;

	/**
	 * Creates a new status with its name and description.
	 * Contructor is <code>protected</code> to deny creation of unsupported statuses
	 * @param	name		name of the status, non-<code>null</code>
	 * @param	description	description of the status, non-<code>null</code>
	 */
	protected ComponentStatus(String name, String description)
	{
		assert (name != null);		
		assert (description != null);		
		
		this.name = name;
		this.description = description;
	}

	/**
	 * Returns the name of the status.
	 * @return String	name of the status
	 */
	public String getName()
	{
		return name;
	}

	/**
	 * Returns the description of the status.
	 * @return String	description of the status
	 */
	public String getDescription()
	{
		return description;
	}

	/**
	 * Returns a single-line rendition of this instance into text.
	 * 
	 * @return internal state of this instance
	 */
	public String toString()
	{
		StringBuffer sbuff = new StringBuffer();
		sbuff.append("ComponentStatus = { ");
		sbuff.append("name = '");
		sbuff.append(name);
		sbuff.append("', description = '");
		sbuff.append(description);
		sbuff.append("' }");
		return new String(sbuff);
	}

}

