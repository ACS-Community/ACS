/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci;

import java.io.Serializable;

/**
 * Structure in which the Manager (and Container) stores information about a component.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class ComponentInfo implements Serializable, Comparable<ComponentInfo>
{

	/**
	 * Serial version UID. 
	 */
	private static final long serialVersionUID = -4376414924118239593L;

	/**
	 * Component's handle.
	 * The handle is automatically assigned by the Manager.
	 */
	private int handle;
	
	/**
	 * Name of the component (without the domain).
	 * The name of the component must be unique within the domain, and can be used to identify the component.
	 * The name can imply hierarchy, which it does by separating name components with a '/' (slash).
	 */
	private String name;

	/**
	 * The type of the component.
	 * Uniquely identifies the code-base which the component's servant is executing.
	 * Given the type name and a hypothetical type library it is possible to infer supported interfaces,
	 * version information, etc.
	 */
	private String type;
	
	/**
	 * The code of the component.
	 * Path to the executable file (a DLL, a shared library, or Java class name) in which the component's code resides.
	 * Can be <code>null</code>, if unknown.
	 */
	private String code;

	/**
	 * Reference to the component, <code>null</code> if the component has not yet been activated.
	 */
	private Component component;
	
	/**
	 * Specifies the clients that have requested and successfuly obtained a reference to this component from the Manager.
	 * If a client has done so more than once, its handle is not repeated.
	 * For immortal and startup components this list always contains at least one handle - the Manager.
	 * Thus, the only way to deactivate these components is by deactivating the Manager.
	 */
	private IntArray clients;
	
	/**
	 * Specifies the components that have been requested and successfuly obtained a reference by this component from the Manager.
	 * If a component has done so more than once, its handle is not repeated.
	 */
	private IntArray components;

	/**
	 * Handle to the container which hosts the component.
	 * This handle is 0 if the component has not been activated by an component, but by some other means,
	 * and has only been registered with the Manager through register_component.
	 */
	private int container;
	
	/**
	 * Name of the container which hosts the component.
	 */
	private String containerName;

	/**
	 * Required access rights to access this component.
	 * When a client attempts to access the component (via Manager's get_component), the bitwise AND of client's access
	 * and component's access must yield component's access in order for the client to have the permission to access the component.
	 */
	private int accessRights;
	
	/**
	 * A list of all interfaces supported by the component.
	 * The first interface in the list is the default interface.
	 */
	private String[] interfaces;

	/**
	 * Dynamic component flag.
	 * NOTE: this attribute is internal.
	 */
	private boolean isDynamic;

	/**
	 * Name of the container on which dynamic component was activated.
	 * NOTE: this attribute is internal.
	 */
	private String dynamicContainerName;

	/**
	 * Keep alive time of a component (needed for dynamic components).
	 * NOTE: this attribute is internal.
	 */
	private int keepAliveTime;

	/**
	 * Creates an instance of ComponentInfo with all necesarry data.
	 * @param handle	handle of the component.
	 * @param name		name of the component.
	 * @param type		type of the component.
	 * @param code		code of the component.
	 * @param component	component itself.
	 * 
	 * @see #handle
	 * @see #name 
	 * @see #type
	 * @see #component 
	 */
	public ComponentInfo(int handle, String name, String type, String code, Component component)
	{
		this.handle = handle;
		this.name = name;
		this.type = type;
		this.code = code;
		this.component = component;
		this.clients = new IntArray();
		this.components = new IntArray();
		this.isDynamic = false;
		this.keepAliveTime = ManagerConstants.RELEASE_TIME_UNDEFINED;
	}

	/**
	 * Returns the container.
	 * @return int
	 */
	public int getContainer()
	{
		return container;
	}

	/**
	 * Returns the clients.
	 * @return ArrayList
	 */
	public IntArray getClients()
	{
		return clients;
	}

	/**
	 * Returns the component.
	 * @return Component
	 */
	public Component getComponent()
	{
		return component;
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
	 * Returns the interfaces.
	 * @return String[]
	 */
	public String[] getInterfaces()
	{
		return interfaces;
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
	 * Returns the type.
	 * @return String
	 */
	public String getType()
	{
		return type;
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
	 * Returns the accessRights.
	 * @return int
	 */
	public int getAccessRights()
	{
		return accessRights;
	}

	/**
	 * Sets the container.
	 * @param container The container to set
	 */
	public void setContainer(int container)
	{
		this.container = container;
	}

	/**
	 * Sets the component.
	 * @param component The component to set
	 */
	public void setComponent(Component component)
	{
		this.component = component;
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
	 * Sets the interfaces.
	 * @param interfaces The interfaces to set
	 */
	public void setInterfaces(String[] interfaces)
	{
		this.interfaces = interfaces;
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
	 * Sets the type.
	 * @param type The type to set
	 */
	public void setType(String type)
	{
		this.type = type;
	}

	/**
	 * Sets the code.
	 * @param code The code to set
	 */
	public void setCode(String code)
	{
		this.code = code;
	}

	/**
	 * Sets the access rights.
	 * @param accessRights The access rights to set
	 */
	public void setAccessRights(int accessRights)
	{
		this.accessRights = accessRights;
	}

	/**
	 * Sets the clients.
	 * @param clients The clients to set
	 */
	public void setClients(IntArray clients)
	{
		this.clients = clients;
	}

	/**
	 * Get array of component handles that this component requested.
	 * @return array of component handles that this component requested.
	 */
	public IntArray getComponents() {
		return components;
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
	 * Get container name which hosts the component.
	 * @return Returns the container name.
	 */
	public String getContainerName() {
		return containerName;
	}
	/**
	 * Set container name.
	 * @param containerName The container name to set.
	 */
	public void setContainerName(String containerName) {
		this.containerName = containerName;
	}

	/**
	 * Returns a single-line rendition of this instance into text.
	 * 
	 * @return internal state of this instance
	 */
	public String toString()
	{
		StringBuffer sbuff = new StringBuffer();
		sbuff.append("ComponentInfo = { ");
		sbuff.append("name = '");
		sbuff.append(name);
		sbuff.append("', type = '");
		sbuff.append(type);
		sbuff.append("', code = '");
		sbuff.append(code);
		sbuff.append(", handle = '");
		sbuff.append(HandleHelper.toString(handle));
		sbuff.append("', container = '");
		sbuff.append(container);
		sbuff.append("', containerName = '");
		sbuff.append(containerName);
		sbuff.append("', component = '");
		sbuff.append(component);
		sbuff.append("' }");
		return new String(sbuff);
	}

	// ---- INTERNAL ---- //

	/**
	 * Get dynamic flag.
	 * @return dynamic flag.
	 */
	public boolean isDynamic() {
		return isDynamic;
	}

	/**
	 * Set dynamic flag.
	 * @param b dynamic flag.
	 */
	public void setDynamic(boolean b) {
		isDynamic = b;
	}

	/**
	 * Get container on which dynamic component was activated.
	 * @return container on which dynamic component was activated.
	 */
	public String getDynamicContainerName() {
		return dynamicContainerName;
	}

	/**
	 * Set container on which dynamic component was activated.
	 * @param string container on which dynamic component was activated.
	 */
	public void setDynamicContainerName(String string) {
		dynamicContainerName = string;
	}

	/**
	 * Get component keep alive time.
	 * @return component keep alive time.
	 */
	public int getKeepAliveTime() {
		return keepAliveTime;
	}

	/**
	 * Set component keep alive time.
	 * @param keepAliveTime the keepAliveTime to set.
	 */
	public void setKeepAliveTime(int keepAliveTime) {
		this.keepAliveTime = keepAliveTime;
	}

	/* (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	public int compareTo(ComponentInfo o) {
		return name.compareTo(o.name);
	}

}
