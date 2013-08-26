/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci.manager;

/**
 * Simple interface defining <code>shutdown</code> method
 * to be called to shutdown whole application.
 *  
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public interface ManagerShutdown
{
	/**
	 * Shutdown the application.
	 * 
	 * @param sigInt should be set to <code>true</code> if CTRL+C is pressed 
	 */
	public void shutdown( boolean sigInt );

	/**
	 * Returns shutdown state.
	 */
	public boolean isShutdownInProgress();

}
