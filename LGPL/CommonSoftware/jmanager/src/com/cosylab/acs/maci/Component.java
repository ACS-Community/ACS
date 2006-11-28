/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci;

/**
 * Component interface.
 * 
 * Interface contains two methods: <code>construct()</code> and <code>destruct()</code>, which Component may choose to implement
 * to allow construction and destruction outside of the constructor/destructor.
 * This is mandatory if a component refers to other components, which could in turn refer back to this component.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public interface Component 
{
	/**
	 * Called by the Manager immediately after the object has been constructed and registered with the Manager.
	 * This is a good place to resolve references to dependent components.
	 */
	public void construct() throws RemoteException;

	/**
	 * Called immediately before the object is destructed. Here references to other components should be released.
	 * After this method completes, the Manager unregisters the component.
	 */
	public void destruct() throws RemoteException;

	/**
	 * Checks if component implements given type.
	 * @param	type	implementation of the type to be checked.
	 * @returns		<code>true</code> if component implements <code>type</code> type, otherwise <code>false</code>.
	 */
	public boolean doesImplement(String type);

	/**
	 * Returns list of intefaces implemented by this component.
	 * @returns		list of intefaces implemented by this component.
	 */
	public String[] implementedInterfaces();

	/**
	 * Returns implementation of component object.
	 * @returns		implementation of component object.
	 */
	public Object getObject();

}
