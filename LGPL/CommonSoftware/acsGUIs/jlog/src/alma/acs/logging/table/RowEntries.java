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
import java.util.Collections;
import java.util.List;

/**
 * The data structure containing all the rows of the table
 * (in the model coordinate space).
 * <P>
 * The entry in position <i>i</i> contains the key of the
 * log to show in the row <i>i</i> of the table.
 * 
 * TODO: add at the beginning of the list results in very poor performance:
 *       we should always append new items and convert the indexes properly!
 *         
 * @author acaproni
 *
 */
public class RowEntries {
	
	/**
	 * The entries i.e. the keys.
	 */
	private final List<Integer> entries = Collections.synchronizedList(new ArrayList<Integer>(20000));

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
		synchronized (entries) {
			for (int t=0; t<numOfEntries; t++) {
				entries.remove(0);
			}
		}
	}
	
	/**
	 * Remove the last <code>numOfEntries</code> entries from the 
	 * array.
	 * 
	 * @param numOfEntries
	 */
	public void removeLastEntries(int numOfEntries) {
		if (numOfEntries<=0) {
			throw new IllegalArgumentException("Invalid number of entries to remove: "+numOfEntries);
		}
		synchronized (entries) {
			for (int t=0; t<numOfEntries && !entries.isEmpty(); t++) {
				entries.remove(entries.size()-1);
			}
		}
	}

	/**
	 * @return
	 * @see java.util.List#size()
	 */
	public int size() {
		return entries.size();
	}

	/**
	 * TODO: for better performance we should always add new elements at the end
	 * @param index
	 * @param element
	 * @see java.util.List#add(int, java.lang.Object)
	 */
	public void add(int index, Integer element) {
		entries.add(index, element);
	}

	/**
	 * @param index
	 * @return
	 * @see java.util.List#get(int)
	 */
	public Integer get(int index) {
		return entries.get(index);
	}

	/**
	 * 
	 * @see java.util.List#clear()
	 */
	public void clear() {
		entries.clear();
	}

	/**
	 * @param o
	 * @return
	 * @see java.util.List#indexOf(java.lang.Object)
	 */
	public int indexOf(Object o) {
		return entries.indexOf(o);
	}
}
