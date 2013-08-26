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
 * (in the model coordinate space). It encapsulate a list of keys.
 * <P>
 * For the user point of view, </CODE>RowEntries</CODE> contains the keys of the 
 * logs in the table: the keys in position <i>i</i> is the key of the log
 * shown in the table in position <i>i</i>.
 * <P>
 * <B>Implementation note.</B><BR>
 * Even if logically, the entry in position <i>i</i> contains the key of the
 * log to show in the row <i>i</i> of the table, to improve performances, 
 * new keys are appended in the tail of the vector instead
 * of being inserted in the head i.e. the key of the first log (in row 0) is 
 * is the last key of the vector (in position size-1). For this reason, {@link #get(int)}
 * convert from the passed index to the real position in the list.
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
	 * Remove the oldest <code>numOfEntries</code> entries from the 
	 * array.
	 * 
	 * @param numOfEntries
	 */
	public List<Integer> removeLastEntries(int numOfEntries) {
		if (numOfEntries<=0) {
			throw new IllegalArgumentException("Invalid number of entries to remove: "+numOfEntries);
		}
		List<Integer> temp = new ArrayList<Integer>();
		synchronized (entries) {
			for (int t=0; t<numOfEntries; t++) {
				temp.add(entries.get(0));
				entries.remove(0);
			}
		}
		return temp;
	}
	
	/**
	 * Remove the first <code>numOfEntries</code> entries from the 
	 * array.
	 * 
	 * @param numOfEntries
	 * @return the keys removed
	 */
	public List<Integer> removeFirstEntries(int numOfEntries) {
		if (numOfEntries<=0) {
			throw new IllegalArgumentException("Invalid number of entries to remove: "+numOfEntries);
		}
		List<Integer> temp = new ArrayList<Integer>();
		synchronized (entries) {
			for (int t=0; t<numOfEntries && !entries.isEmpty(); t++) {
				temp.add(entries.get(entries.size()-1));
				entries.remove(entries.size()-1);
				
			}
		}
		return temp;
	}

	/**
	 * @return
	 * @see java.util.List#size()
	 */
	public int size() {
		return entries.size();
	}
	
	/**
	 * Add the passed key to the list of entries.
	 * <P>
	 * To improve performances, the key is appended at the end of the vector
	 * 
	 * @param key The integer key to add to the list
	 */
	public void add(Integer key) {
		entries.add(key);
	}

	/**
	 * Return the key at the given row of the table.
	 * <P>
	 * For a deeper understanding of the algorithm, read the implementation note
	 * in {@link RowEntries}.
	 *  
	 * @param index The index of the key to get
	 * @return The key at passed row of the table
	 */
	public Integer get(int row) {
		// The index must be converted from the passed row 
		// to the right index in the list
		synchronized (entries) {
			int indxInTheList=entries.size()-1-row;
			return entries.get(indxInTheList);	
		}
	}

	/**
	 * 
	 * @see java.util.List#clear()
	 */
	public void clear() {
		entries.clear();
	}

	/**
	 * Return the position (i.e. the row in the table) that contains the passed key 
	 * <P>
	 * For a deeper understanding of the algorithm, read the implementation note
	 * in {@link RowEntries}.
	 * 
	 * @param key The key to look for in the table
	 * @return The position in the list of the passed key or
	 *         -1 if the list does not contain the passed key
	 */
	public int indexOf(Integer key) {
		// Get the real position in the list
		synchronized (entries) {
			// Get the real position in the list
			int pos=entries.indexOf(key);
			// return the converted index
			return pos==-1?pos:entries.size()-pos-1;
		}
	}
}
