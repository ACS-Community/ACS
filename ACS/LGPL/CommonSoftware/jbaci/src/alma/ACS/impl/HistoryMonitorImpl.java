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

package alma.ACS.impl;

import alma.ACS.jbaci.BACIPriority;
import alma.ACS.jbaci.CompletionUtil;
import alma.ACSErr.CompletionHolder;

/**
 * Implementation of history monitor - retrieveValueAndDispatch method
 * adds value to the history buffer. History monitor is IDLE priority task.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
// TODO should the also be comparable monitor (to full only on change)
public class HistoryMonitorImpl extends CommonMonitorImpl {

	/**
	 * Constructor.
	 * @param property	property to be monitored, non-<code>null</code>.
	 */
	public HistoryMonitorImpl(CommonPropertyImpl property) {
		
		if (property == null)
			throw new NullPointerException("property == null");
		
		this.property = property;
		this.startTime = 0;

		// initialize and start
		initialize();		
	}

	/**
	 * @see alma.ACS.jbaci.PrioritizedRunnable#getPriority()
	 */
	public BACIPriority getPriority() {
		return BACIPriority.IDLE;
	}

	/**
	 * Retrieve property value via cached <code>mnemonicValue</code>
	 * and add response to the history buffer. 
	 * @see alma.ACS.CommonMonitor#retrieveValueAndDispatch(long, boolean)
	 */
	protected void retrieveValueAndDispatch(long keyTime, boolean done)
	{
		// create new holder (done expeditiously)
		CompletionHolder completionHolder = CompletionUtil.createCompletionHolder();
		
		// retrieve value
		Object value = property.mnemonicValue(keyTime, completionHolder);

		// TODO check completion error state - what do to then?
		
		// add to history
		property.addValueToHistory(value, completionHolder.value.timeStamp);
	}

}
