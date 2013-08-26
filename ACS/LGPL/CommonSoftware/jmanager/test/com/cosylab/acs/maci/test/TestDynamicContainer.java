/*
 * @@COPYRIGHT@@
 */
package com.cosylab.acs.maci.test;

import alma.maciErrType.wrappers.AcsJNoPermissionEx;

import com.cosylab.acs.maci.ClientType;
import com.cosylab.acs.maci.ComponentInfo;
import com.cosylab.acs.maci.Manager;
import com.cosylab.acs.maci.RemoteException;

/**
 * Test implementation of an container.
 * 
 * @author Matej Sekoranja
 * @version @@VERSION@@
 */
public class TestDynamicContainer extends TestContainer {

	private Manager manager = null;
	

	public TestDynamicContainer(String name, ClientType type, boolean recover) {
		super(name, type, recover);
	}

	public TestDynamicContainer(String name) {
		super(name);
	}

	/**
	 * @param name
	 */
	public TestDynamicContainer(String name, Manager manager) {
		super(name);
		this.manager = manager;
	}

	/**
	 * @see com.cosylab.acs.maci.Container#activate_COB(int, long, java.lang.String, java.lang.String, java.lang.String)
	 */
	public ComponentInfo activate_component(
		int handle,
		long executionId,
		String name,
		String exe,
		String type)
		throws RemoteException {

		supportedComponents.put(name, new TestComponent(name));
		
		return super.activate_component(handle, executionId, name, exe, type);
	}

	/**
	 * @see com.cosylab.acs.maci.Container#shutdown(int)
	 */
	public void shutdown(int action) throws RemoteException {
		try {
			if (manager != null)
				manager.logout(handle);
		} catch (AcsJNoPermissionEx e) {
			/// @TODO What do to here?
			e.printStackTrace();
		}
	}

}
