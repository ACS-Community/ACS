/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci;

/**
 * ACS service daemon IF.
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public interface ServiceDaemon
{

	/**
	 * Set manager reference.
	 * @param reference manager reference (corbaloc or IOR).
	 * @throws RemoteException
	 */
	void setManagerReference(String reference) throws RemoteException;

}
