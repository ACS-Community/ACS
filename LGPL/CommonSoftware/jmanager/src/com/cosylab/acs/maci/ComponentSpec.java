/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci;

import java.util.StringTokenizer;

/**
 * Structure containing all fields necessary to describe a component and its deployment information.
 * Used for dynamic component instatiation.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class ComponentSpec
{
	/**
	 * Any ComponentSpec string.
	 */
	public static final String COMPSPEC_ANY = "*";

	/**
	 * CURL of the component.
	 */
	//private URI curl;
	private String name;

	/**
	 * The type of the component.
	 */
	private String type;
	
	/**
	 * The code of the component.
	 * Code is a identifying component's executable code (library/class name). 
	 */
	private String code;

	/**
	 * Name of the container which hosts the component.
	 */
	private String container;
	
	/**
	 * Creates an instance of ComponentInfo with all necesarry data.
	 * @//param curl		curl of the component.
	 * @param name		name of the component.
	 * @param type		type of the component.
	 * @param code		code of the component.
	 * @param container	container hosting the component.
	 */
	//public ComponentSpec(URI curl, String type, String code, String container)
	public ComponentSpec(String name, String type, String code, String container)
	{
		//this.curl = curl;
		this.name = name;
		this.type = type;
		this.code = code;
		this.container = container;
	}

	/**
	 * Creates an instance of ComponentInfo from stringified ComponentSpec.
	 * @param componentSpec		stringified ComponentSpec, non-<code>null</code>
	 * @throws BadParametersException
	 * @see #parseComponentSpec
	 */
	public ComponentSpec(String componentSpec) throws IllegalArgumentException
	{
		parseComponentSpec(componentSpec);
	}

	/**
	 * Creates an instance of ComponentInfo from stringified ComponentSpec.
	 * Format:
	 * <pre>
	 * 		name + "|" + type + "|" + code + "|" + containerName
	 * </pre>
	 * 
	 * Empty fields are allowed.
	 * 
	 * @param componentSpec		stringified ComponentSpec
	 * @throws BadParametersException
	 */
	private void parseComponentSpec(String componentSpec) throws IllegalArgumentException
	{
		if (componentSpec == null)
			throw new IllegalArgumentException("Non-'null' string expected.");

		StringTokenizer tokenizer = new StringTokenizer(componentSpec, "|");

		final String ERROR_STRING =  " element expected."; //, format: 'name + \"|\" + type + \"|\" + code + \"|\" + containerName'.";

		// read name		
		if (!tokenizer.hasMoreTokens())
			throw new IllegalArgumentException("'name'" + ERROR_STRING);
		name = tokenizer.nextToken();

		// read type		
		if (!tokenizer.hasMoreTokens())
			throw new IllegalArgumentException("'type'" + ERROR_STRING);
		type = tokenizer.nextToken();

		// code type		
		if (!tokenizer.hasMoreTokens())
			throw new IllegalArgumentException("'code'" + ERROR_STRING);
		code = tokenizer.nextToken();

		// containerName type		
		if (!tokenizer.hasMoreTokens())
			throw new IllegalArgumentException("'containerName'" + ERROR_STRING);
		container = tokenizer.nextToken();

		// just ignore if there is sth more		
	}

	/**
	 * Returns the container.
	 * @return String
	 */
	public String getContainer()
	{
		return container;
	}

	/**
	 * Returns the code.
	 * @return String
	 */
	public String getCode()
	{
		return code;
	}

	/**
	 * Returns the type.
	 * @return String
	 */
	public String getType()
	{
		return type;
	}

	/**
	 * Returns the CURL.
	 * @return java.net.URI
	 */
	/*
	public URI getCURL()
	{
		return curl;
	}
	*/

	/**
	 * Returns the name.
	 * @return String
	 */
	public String getName()
	{
		return name;
	}

	/**
	 * Sets the container.
	 * @param container The container to set
	 */
	public void setContainer(String container)
	{
		this.container = container;
	}

	/**
	 * Sets the code.
	 * @param code The component's code.
	 */
	public void setCode(String code)
	{
		this.code = code;
	}

	/**
	 * Sets the type.
	 * @param type The type to set
	 */
	public void setType(String type)
	{
		this.type = type;
	}

	/**
	 * Sets the CURL.
	 * @param curl The CURL to set.
	 */
	/*
	public void setCURL(URI curl)
	{
		this.curl = curl;
	}
	*/

	/**
	 * Sets the name.
	 * @param name The name to set
	 */
	public void setName(String name)
	{
		this.name = name;
	}

	/**
	 * Returns a single-line rendition of this instance into text.
	 * 
	 * @return internal state of this instance
	 */
	public String toString()
	{
		StringBuffer sbuff = new StringBuffer();
		sbuff.append("ComponentSpec = { ");
		//sbuff.append("curl = '");
		//sbuff.append(curl);
		sbuff.append("name = '");
		sbuff.append(name);
		sbuff.append("', type = '");
		sbuff.append(type);
		sbuff.append("', code = '");
		sbuff.append(code);
		sbuff.append("', container = '");
		sbuff.append(container);
		sbuff.append("' }");
		return new String(sbuff);
	}

}
