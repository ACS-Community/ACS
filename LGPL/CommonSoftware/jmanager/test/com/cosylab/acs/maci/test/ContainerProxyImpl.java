/*
 * Created on May 14, 2003
 *
 * To change this generated comment go to 
 * Window>Preferences>Java>Code Generation>Code Template
 */
package com.cosylab.acs.maci.test;

import org.omg.CORBA.Object;

import si.ijs.maci.ContainerPOA;
import si.ijs.maci.ComponentInfo;

/**
 * @author almamgr
 */
public class ContainerProxyImpl extends ContainerPOA {

	protected String name;
	
	/**
	 * 
	 */
	public ContainerProxyImpl( String name ) {
		super();
		this.name = name;
	}

	/* (non-Javadoc)
	 * @see si.ijs.maci.ContainerOperations#activate_COB(int, java.lang.String, java.lang.String, java.lang.String)
	 */
	public ComponentInfo activate_component(int arg0, String arg1, String arg2, String arg3) {
		return null;
	}

	/* (non-Javadoc)
	 * @see si.ijs.maci.ContainerOperations#deactivate_components(int[])
	 */
	public void deactivate_components(int[] arg0) {
	}

	/* (non-Javadoc)
	 * @see si.ijs.maci.ContainerOperations#shutdown(int)
	 */
	public void shutdown(int arg0) {
	}

	/* (non-Javadoc)
	 * @see si.ijs.maci.ContainerOperations#get_COB_info(int[])
	 */
	public ComponentInfo[] get_component_info(int[] arg0) {
		return null;
	}

	/* (non-Javadoc)
	 * @see si.ijs.maci.ClientOperations#name()
	 */
	public String name() {
		return name;
	}

	/* (non-Javadoc)
	 * @see si.ijs.maci.ClientOperations#disconnect()
	 */
	public void disconnect() {

	}

	/* (non-Javadoc)
	 * @see si.ijs.maci.ClientOperations#authenticate(java.lang.String)
	 */
	public String authenticate(String arg0) {
		return "A";

	}

	/* (non-Javadoc)
	 * @see si.ijs.maci.ClientOperations#message(short, java.lang.String)
	 */
	public void message(short arg0, String arg1) {
	}

	/* (non-Javadoc)
	 * @see si.ijs.maci.ClientOperations#ping()
	 */
	public boolean ping() {
		return true;
	}

	/* (non-Javadoc)
	 * @see si.ijs.maci.ClientOperations#components_available(si.ijs.maci.ComponentInfo[])
	 */
	public void components_available(ComponentInfo[] arg0) {

	}

	/* (non-Javadoc)
	 * @see si.ijs.maci.ClientOperations#components_unavailable(java.lang.String[])
	 */
	public void components_unavailable(String[] arg0) {
	}

	/* (non-Javadoc)
	 * @see si.ijs.maci.ContainerOperations#restart_component(int)
	 */
	public Object restart_component(int arg0) {
		return null;
	}

	/* (non-Javadoc)
	 * @see si.ijs.maci.ContainerOperations#set_component_shutdown_order(int[])
	 */
	public void set_component_shutdown_order(int[] h) {
	}
}
