/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci.test;

import com.cosylab.acs.maci.Component;
import com.cosylab.acs.maci.RemoteException;

/**
 * Test implementation of Component.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class TestComponent implements Component
{

	/**
	 * Component name.
	 */
	String name;

	/**
	 * Component handle.
	 */
	int handle = 0;

	/**
	 * If <code>true</code> construct method will throw exception.
	 */
	boolean simulateConstructFailure = false;

	/**
	 * If <code>true</code> desctruct method will throw exception.
	 */
	boolean simulateDestructFailure = false;

	/**
	 * Constructor for TestComponent.
	 * @param	name name of the cob
	 */
	public TestComponent(String name)
	{
		this.name = name;
	}

	/**
	 * Constructor for TestComponent, tuning construct/destruct failures.
	 * @param	name name of the cob
	 * @param 	simulateConstructFailure	if <code>true</code> construct method will throw exception
	 * @param	simulateDestructFailure		if <code>true</code> desctruct method will throw exception
	 */
	public TestComponent(String name, boolean simulateConstructFailure, boolean simulateDestructFailure)
	{
		this(name);
		
		this.simulateConstructFailure = simulateConstructFailure;
		this.simulateDestructFailure = simulateDestructFailure;
	}

	/**
	 * TectContainer should call this method on activation.
	 */
	public void activate() throws Exception
	{
		// noop
	}

	/**
	 * TectContainer should call this method on deactivation.
	 */
	public void deactivate()
	{
		// noop
	}

	/**
	 * @see com.cosylab.acs.maci.Component#construct()
	 */
	public void construct() throws RemoteException
	{
		if (simulateConstructFailure)
		{
			RemoteException re = new RemoteException("Simulated exception.");
			throw re;
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Component#destruct()
	 */
	public void destruct() throws RemoteException
	{
		if (simulateDestructFailure)
		{
			RemoteException re = new RemoteException("Simulated exception.");
			throw re;
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Component#implementedInterfaces()
	 */
	public String[] implementedInterfaces()
	{
		return new String[]{ Component.class.getName() };
	}

	/**
	 * @see com.cosylab.acs.maci.Component#doesImplement(String)
	 */
	public boolean doesImplement(String type)
	{
		return true;
	}

	/**
	 * @see com.cosylab.acs.maci.Component#getObject()
	 */
	public Object getObject()
	{
		// dummy impl.
		return this;
	}

	/**
	 * Get component handle.
	 * @return component handle.
	 */
	public int getHandle() {
		return handle;
	}

	/**
	 * Set component handle.
	 * @param h component handle.
	 */
	public void setHandle(int h) {
		handle = h;
	}

}
