/*
 * @@COPYRIGHT@@
 */

package com.cosylab.cdb.client;

/**
 * DAO proxy connection listener inteface.
 * @author msekoranja
 */
public interface DAOProxyConnectionListener {

	/**
	 * Connected event.
	 * @param proxy
	 */
	void connected(DAOProxy proxy);
	
	/**
	 * Disconnected event.
	 * @param proxy
	 */
	void disconnected(DAOProxy proxy);

}
