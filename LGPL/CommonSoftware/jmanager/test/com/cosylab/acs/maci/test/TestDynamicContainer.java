/*
 * @@COPYRIGHT@@
 */
package com.cosylab.acs.maci.test;

import abeans.pluggable.RemoteException;
import com.cosylab.acs.maci.ComponentInfo;

/**
 * Test implementation of an container.
 * 
 * @author Matej Sekoranja
 * @version @@VERSION@@
 */
public class TestDynamicContainer extends TestContainer {

	/**
	 * Constructor for TestContainer.
	 * @param name
	 * @param reply
	 */
	public TestDynamicContainer(String name, String reply) {
		super(name, reply);
	}

	/**
	 * @param name
	 */
	public TestDynamicContainer(String name) {
		super(name);
	}


	/**
	 * @see com.cosylab.acs.maci.Container#activate_COB(int, java.lang.String, java.lang.String, java.lang.String)
	 */
	public ComponentInfo activate_component(
		int handle,
		String name,
		String exe,
		String type)
		throws RemoteException {

		supportedComponents.put(name, new TestComponent(name));
		
		return super.activate_component(handle, name, exe, type);
	}
}
