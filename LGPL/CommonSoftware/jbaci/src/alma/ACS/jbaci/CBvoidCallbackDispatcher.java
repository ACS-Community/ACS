/*******************************************************************************
 * ALMA - Atacama Large Millimiter Array
 * (c) European Southern Observatory, 2002
 * Copyright by ESO (in the framework of the ALMA collaboration)
 * and Cosylab 2002, All rights reserved
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */

package alma.ACS.jbaci;

import alma.ACS.CBDescOut;
import alma.ACS.CBvoid;
import alma.ACS.Callback;
import alma.ACSErr.Completion;

/**
 * Implementation of CBvoid callback dispatcher as singleton.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public class CBvoidCallbackDispatcher implements CallbackDispatcher {

	/**
	 * Singleton instance.
	 */
	private static CBvoidCallbackDispatcher instance = null;

	/**
	 * Protected constructor (singleton pattern). 
	 */
	protected CBvoidCallbackDispatcher() {
	}

	/**
	 * Singleton pattern.
	 */
	public static synchronized CBvoidCallbackDispatcher getInstance()
	{
		if (instance == null)
			instance = new CBvoidCallbackDispatcher();
		return instance;
	}

	/**
	 * @see alma.ACS.jbaci.CallbackDispatcher#dispatchCallback(int, java.lang.Object, alma.ACSErr.Completion, alma.ACS.CBDescOut)
	 */
	public boolean dispatchCallback(
		int type,
		Object value,
		Callback callback,
		Completion completion,
		CBDescOut desc) {
		try
		{	
			if (type == CallbackDispatcher.DONE_TYPE)
				((CBvoid)callback).done(completion, desc);
			else if (type == CallbackDispatcher.WORKING_TYPE)
				((CBvoid)callback).working(completion, desc);
			else 
				return false;
				
			return true;
		}
		catch (Throwable th)
		{
			return false;
		}
	}

}
