/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci;

import java.io.Serializable;

/**
 * Structure in which the Manager stores information about an container.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class ContainerInfo implements Serializable
{
	
	/**
	 * Serial version UID. 
	 */
	private static final long serialVersionUID = -2545764366629134129L;

	/**
	 * Container's handle (in the range 0x04000000 to 0x04FFFFFF).
	 */
	private int handle;
	
	/**
	 * Container's name.
	 */
	private String name;
	
	/**
	 * Reference to the Container.
	 */
	private Container container;
	
	/**
	 * Handles of all components hosted by the Container.
	 */
	private IntArray components;

    /**
     * Manage the implementation language name
     */
    private ImplLang implLang;
    
    /**
     * Ping interval in ms.
     */
    private long pingInterval;

	/**
	 * Creates an instance of ContainerInfo with all necesarry data.
	 * @param handle	handle of the container.
	 * @param name		name of the container.
	 * @param container	container itself.
	 * @param pingInterval ping interval in ms.
	 * 
	 * @see #handle
	 * @see #name 
	 * @see #container
	 */
	public ContainerInfo(int handle, String name, Container container, long pingInterval)
	{
		this.handle = handle;
		this.name = name;
		this.container = container;
		this.pingInterval = pingInterval;
		this.components = new IntArray();
        this.implLang = ImplLang.not_specified;
	}

	/**
	 * Returns the container.
	 * @return Container
	 */
	public Container getContainer()
	{
		return container;
	}

	/**
	 * Returns the components.
	 * @return ArrayList
	 */
	public IntArray getComponents()
	{
		return components;
	}

	/**
	 * Returns the handle.
	 * @return int
	 */
	public int getHandle()
	{
		return handle;
	}

	/**
	 * Returns the implementation language.
	 * @return ImplLang
	 */
	public ImplLang getImplLang()
	{
		return implLang;
	}

	/**
	 * Returns the name.
	 * @return String
	 */
	public String getName()
	{
		return name;
	}
	
    /**
	 * Sets the implementation language.
	 * @param container The language to set
	 */
	public void setImplLang(ImplLang implLang)
	{
		this.implLang = implLang;
	}

	/**
	 * Sets the container.
	 * @param container The container to set
	 */
	public void setContainer(Container container)
	{
		this.container = container;
	}

	/**
	 * Sets the handle.
	 * @param handle The handle to set
	 */
	public void setHandle(int handle)
	{
		this.handle = handle;
	}

	/**
	 * Sets the name.
	 * @param name The name to set
	 */
	public void setName(String name)
	{
		this.name = name;
	}

	/**
	 * Sets the components.
	 * @param components The components to set
	 */
	public void setComponents(IntArray components)
	{
		this.components = components;
	}

	/**
	 * @return the pingInterval
	 */
	public long getPingInterval() {
		return pingInterval;
	}

	/**
	 * @param pingInterval the pingInterval to set
	 */
	public void setPingInterval(long pingInterval) {
		this.pingInterval = pingInterval;
	}

	/**
	 * Returns a single-line rendition of this instance into text.
	 * 
	 * @return internal state of this instance
	 */
	public String toString()
	{
		StringBuffer sbuff = new StringBuffer();
		sbuff.append("ContainerInfo = { ");
		sbuff.append("name = '");
		sbuff.append(name);
		sbuff.append("', ");
		sbuff.append(HandleHelper.toString(handle));
		sbuff.append(" }");
		return new String(sbuff);
	}

}
