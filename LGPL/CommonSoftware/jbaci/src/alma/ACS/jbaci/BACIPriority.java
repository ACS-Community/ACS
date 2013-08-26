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

/**
 * BACI priority.
 * Priorities are defined as an interger value in range from 0 to 1000,
 * <b>where lower value means higher priority</b>. 
 * There are 3 predifined classes of priorities:
 * <ul>
 * 	<li><b>REALTIME</b> - 0 to 99</li>
 * 	<li><b>NORMAL</b> - 100 to 899</li>
 * 	<li><b>IDLE</b> - 900 to 1000</li>
 * </ul>
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public final class BACIPriority implements Comparable {

	/**
	 * Maximal priority value.
	 */
	public static final int MAX_PRIORITY_VALUE = 0; 

	/**
	 * Minimal priority value.
	 */
	public static final int MIN_PRIORITY_VALUE = 1000; 

	/**
	 * Maximal REALTIME class priority value.
	 */
	public static final int MAX_REALTIME_PRIORITY_VALUE = MAX_PRIORITY_VALUE; 

	/**
	 * Minimal REALTIME class priority value.
	 */
	public static final int MIN_REALTIME_PRIORITY_VALUE = 99; 

	/**
	 * Maximal NORMAL class priority value.
	 */
	public static final int MAX_NORMAL_PRIORITY_VALUE = MIN_REALTIME_PRIORITY_VALUE + 1; 

	/**
	 * Minimal NORMAL class priority value.
	 */
	public static final int MIN_NORMAL_PRIORITY_VALUE = 899; 

	/**
	 * Maximal IDLE class priority value.
	 */
	public static final int MAX_IDLE_PRIORITY_VALUE = MIN_NORMAL_PRIORITY_VALUE + 1; 

	/**
	 * Minimal IDLE class priority value.
	 */
	public static final int MIN_IDLE_PRIORITY_VALUE = MIN_PRIORITY_VALUE; 

	/**
	 * REALTIME priorty class.
	 */
	public static final BACIPriority REALTIME =
		new BACIPriority((MAX_REALTIME_PRIORITY_VALUE+MIN_REALTIME_PRIORITY_VALUE)/2); 
	
	/**
	 * NORMAL priorty class.
	 */
	public static final BACIPriority NORMAL =
		new BACIPriority((MAX_NORMAL_PRIORITY_VALUE+MIN_NORMAL_PRIORITY_VALUE)/2); 

	/**
	 * IDLE priorty class.
	 */
	public static final BACIPriority IDLE =
		new BACIPriority((MAX_IDLE_PRIORITY_VALUE+MIN_IDLE_PRIORITY_VALUE)/2); 

	/**
	 * Priority value.
	 */
	private int value;

	/**
	 * Creates a user defined priority. 
	 */
	public BACIPriority(int value) {
		
		// check ranges
		if (value < MAX_PRIORITY_VALUE ||
			value > MIN_PRIORITY_VALUE)
			throw new IllegalArgumentException("MAX_PRIORITY_VALUE < value < MIN_PRIORITY_VALUE");
			
		this.value = value;
	}

	/**
	 * Returns priority value.
	 * @return priority value.
	 */
	public int getValue() {
		return value;
	}

	/**
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	public int compareTo(Object o) {
		int ovalue = ((BACIPriority)o).getValue();
		// most likely to happen
		if (value == ovalue)
			return 0;
		else if (value < ovalue)
			return -1;
		else
			return 1; 
	}

}
