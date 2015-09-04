/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2007
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acsplugins.alarmsystem.gui.table;

import java.util.concurrent.atomic.AtomicInteger;

/**
 * The counter for an alarm type
 * 
 * @author acaproni
 *
 */
public class AlarmCounter {
	
	private AtomicInteger count = new AtomicInteger(0);
	
	/**
	 * Increase and return the counter
	 * 
	 * @return The value of the increased counter
	 */
	public int incCounter() {
		return count.incrementAndGet();
	}
	
	/**
	 * Decrease and return the counter
	 * 
	 * @return The value of the decremented counter
	 */
	public int decCounter() {
		if (count.get()<=0) {
			throw new IllegalStateException("Error managing counters");
		}
		return count.decrementAndGet();
	}
	
	/**
	 * 
	 * @return The value of the counter
	 */
	public int getCount() {
		return count.get();
	}
	
}
