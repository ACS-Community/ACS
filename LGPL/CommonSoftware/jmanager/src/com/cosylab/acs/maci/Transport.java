/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci;

/**
 * Abstract transport (CORBA, etc.) IF.
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public interface Transport
{

	/**
	 * Get stringified manager reference.
	 * @return stringified manager reference.
	 */
	String getManagerReference();
	
	/**
	 * Get daemon on host.
	 * @param host remote host.
	 * @return daemon instance, <code>null</code> if failed.
	 */
	Daemon getDaemon(String host);

	/**
	 * Get service daemon on host.
	 * @param host remote host.
	 * @return service daemon instance, <code>null</code> if failed.
	 */
	ServiceDaemon getServiceDaemon(String host);
}
