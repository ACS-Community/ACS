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

import alma.ACSErr.CompletionHolder;

/**
 * Memory data access (what you set is what you get) implementation.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public class MemoryDataAccess extends DataAccessSupport {

	/**
	 * Value.
	 */
	protected Object value;
	
	/**
	 * This requires value to be initialized to non-null value.
	 * @see alma.ACS.DataAccess#initializeValue()
	 */
	public boolean initializeValue() {
		return true;
	}

	/**
	 * @see alma.ACS.DataAccess#get(alma.ACSErr.CompletionHolder)
	 */
	public Object get(CompletionHolder completionHolder) {
		// no-error completion generation is left to the caller
		return value;
	}

	/**
	 * @see alma.ACS.DataAccess#set(Object, alma.ACSErr.CompletionHolder)
	 */
	public void set(Object value, CompletionHolder completionHolder) {
		// no-error completion generation is left to the caller
		Object oldValue = this.value;
		this.value = value;
		// notify
		notify(oldValue, this.value);
	}

}
