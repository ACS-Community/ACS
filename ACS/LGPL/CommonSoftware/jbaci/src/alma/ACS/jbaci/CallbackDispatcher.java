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
import alma.ACS.Callback;
import alma.ACSErr.Completion;

/**
 * Interface defining generic method to dispatch a callback.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public interface CallbackDispatcher {
	
	/**
	 * 'done' callback type constant.
	 */
	public static final int DONE_TYPE = 0;

	/**
	 * 'working' callback type constant.
	 */
	public static final int WORKING_TYPE = 1;
	
	/**
	 * Dispatches a callback.
	 * @param type		type of the callback (not strong typed to be flexible?!).
	 * @param value		value to be delivered.
	 * @param callback	callback.
	 * @param competion	completion.
	 * @param desc		callback out-descriptor.
	 * @return	<code>true</code> if callback was successfully delivered, <code>false</code> on failure. 
	 */
	public boolean dispatchCallback(int type, Object value,
							Callback callback, Completion completion, CBDescOut desc);

}
