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

/**
 * The counter for an alarm type
 * 
 * @author acaproni
 *
 */
public class AlarmCounter {
	
	private volatile int count=0;
	
	/**
	 * Increase the counter
	 */
	public void incCounter() {
		if (count==Integer.MAX_VALUE) {
			count=1;
			return;
		}
		count++;
	}
	
	/**
	 * Decrease the counter
	 */
	public void decCounter() {
		if (count<=0) {
			throw new IllegalStateException("Error managing counters");
		}
		count--;
	}
	
	/**
	 * 
	 * @return The value of the counter
	 */
	public int getCount() {
		return count;
	}
	
}
