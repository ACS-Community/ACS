package com.cosylab.acs.maci;

/**
 * Predefined access rights values.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public interface AccessRights
{
	/**
	 * None.
	 */
	public static final int NONE = 0x00000000;

	/**
	 * Clients must have the INTROSPECT_MANAGER access right to be able to gain access to Manager's internal state.
	 * This access is usually given to administrator clients.
	 */
	public static final int INTROSPECT_MANAGER = 0x08000000;
	
	/**
	 * Clients must have the SHUTDOWN_SYSTEM access right to be able to call Manager's shutdown method,
	 * thus shutting down the entire control system.
	 */
	public static final int SHUTDOWN_SYSTEM = 0x04000000;

	/**
	 * Clients must have the REGISTER_COMPONENT access right to be able to call Manager's register_component and unregister_component methods.
	 */
	public static final int REGISTER_COMPONENT = 0x02000000;
	
}
