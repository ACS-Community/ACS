/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.acs.eventbrowser.views;

import java.util.concurrent.atomic.AtomicLong;

/**
 * TODO: Expand to get the parameter type as well, so that value can be interpreted
 */

public class ArchiveEventData extends AbstractEventData {
	protected static AtomicLong totalEventsProcessed = new AtomicLong();
	protected static AtomicLong timeFirstEventProcessed = new AtomicLong();
	
	private final String device;
	private final String parameter;
	private final Object value;
	
	public ArchiveEventData(Long time, String device, String parameter, Object value) {
		super();
		timestamp = time;
		this.device = device;
		this.parameter = parameter;
		this.value = value;
		totalEventsProcessed.getAndIncrement();
		timeFirstEventProcessed.compareAndSet(0L, System.currentTimeMillis());
	}
	
	public static long getTotalEventsProcessed() {
		return totalEventsProcessed.get();
	}
	
	/**
	 * @return Time (in ms) of first event processed
	 */
	public static Long getTimeFirstEventProcessed() {
		return timeFirstEventProcessed.get();
	}
	
	/**
	 * @return Average number of events/s since the first one that was processed
	 */
	public static float getAverageRate() {
		return ((float)getTotalEventsProcessed()*1000.f)/((float)(System.currentTimeMillis()-getTimeFirstEventProcessed()));
	}

	public String getDevice() {
		return device;
	}


	public String getParameter() {
		return parameter;
	}

	public Object getValue() {
		return value;
	}
	
	public String toString() {
		return "Timestamp "+getTimestamp()+" Device "+getDevice()+" Property/parameter "+getParameter()+" Value "+getValue().toString();
	}

}
