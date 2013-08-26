/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci;

/**
 * Manager constants.
 *  
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public interface ManagerConstants
{
	/**
	 * Release immediately keep-alive time.
	 */
	public static final int	RELEASE_IMMEDIATELY = 0;		// 0 secs

	/**
	 * Never release (immortal) immediately keep-alive time.
	 */
	public static final int	RELEASE_NEVER = -1;		// < 0 secs

	/**
	 * (Internal) undefined keep-alive time.
	 */
	public static final int	RELEASE_TIME_UNDEFINED = Integer.MIN_VALUE + 2;

}
