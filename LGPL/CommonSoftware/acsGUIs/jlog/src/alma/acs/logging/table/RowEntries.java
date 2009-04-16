/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.logging.table;

import java.util.ArrayList;

/**
 * The data structure containing all the rows of the table
 * (in the model coordinate space).
 * <P>
 * The entry in position <i>i</i> contains the key of the
 * log to show in the row <i>i</i> of the table.
 * 
 * @author acaproni
 *
 */
public class RowEntries extends ArrayList<Integer> {

	/**
	 * Constructor
	 * 
	 * @param initialCapacity the initial capacity of the list 
	 */
	public RowEntries(int initialCapacity) {
		super(initialCapacity);
	}
	
	/**
	 * Remove the first <code>numOfEntries</code> entries from the 
	 * array.
	 * 
	 * @param numOfEntries
	 */
	public void removeFirstEntries(int numOfEntries) {
		if (numOfEntries<=0) {
			throw new IllegalArgumentException("Invalid number of entries to remove: "+numOfEntries);
		}
		removeRange(0, numOfEntries);
	}
	
	/**
	 * Remove the first <code>numOfEntries</code> entries from the 
	 * array.
	 * 
	 * @param numOfEntries
	 */
	public void removeLastEntries(int numOfEntries) {
		if (numOfEntries<=0) {
			throw new IllegalArgumentException("Invalid number of entries to remove: "+numOfEntries);
		}
		removeRange(size()-numOfEntries,size());
	}
}
