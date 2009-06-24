/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci;

/**
 * ACS service daemon IF.
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public interface Daemon
{

	/**
	 * Start container.
	 * @param containerType	container type (e.g. cpp, java, python).
	 * @param containerName	name of the container to start.
	 * @param instanceNumber	instance number.
	 * @param flags	additiona flags (command line).
	 * @throws RemoteException
	 */
	void startContainer(String containerType, String containerName, short instanceNumber, String flags) throws RemoteException;

}
