/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci;

/**
 * Type of the message.
 * Can be either <code>MSG_ERROR</code> or <code>MSG_INFORMATION</code>.
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public final class MessageType
{

	/**
	 * Error message.
	 */
	public static final MessageType MSG_ERROR = new MessageType("MSG_ERROR", "Error message.");

	/**
	 * Informational message.
	 */
	public static final MessageType MSG_INFORMATION = new MessageType("MSG_INFORMATION", "Informational message.");

	/**
	 * The name of the type (e.g. MSG_ERROR).
	 */
	private String name;

	/**
	 * The description of the type.
	 */
	private String description;

	/**
	 * Creates a new message type with its name and description.
	 * Contructor is <code>protected</code> to deny creation of unsupported types
	 * @param	name		name of the type, non-<code>null</code>
	 * @param	description	description of the type, non-<code>null</code>
	 */
	protected MessageType(String name, String description)
	{
		assert (name != null);		
		assert (description != null);		
		
		this.name = name;
		this.description = description;
	}

	/**
	 * Returns the name of the type.
	 * @return String	name of the type
	 */
	public String getName()
	{
		return name;
	}

	/**
	 * Returns the description of the type.
	 * @return String	description of the type
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
		sbuff.append("MessageType = { ");
		sbuff.append("name = '");
		sbuff.append(name);
		sbuff.append("', description = '");
		sbuff.append(description);
		sbuff.append("' }");
		return new String(sbuff);
	}

}
