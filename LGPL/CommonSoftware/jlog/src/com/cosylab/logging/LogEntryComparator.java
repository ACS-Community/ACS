/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
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
package com.cosylab.logging;

import com.cosylab.logging.engine.log.ILogEntry;

/**
 * This is specialized comparator to sort LogEntryXML objects using <code>Collections</code>.
 * Creation date: (11/19/2001 10:45:07)
 * @author: 
 */
public class LogEntryComparator implements java.util.Comparator<ILogEntry> {

	private short fieldIndex = 0;
	
	private boolean ascending = true;


	/**
	 * LCLogEntryComparator constructor comment.
	 */
	public LogEntryComparator(short index, boolean sortAscending) {
		super();
		fieldIndex = index;
		ascending = sortAscending;
	}
	
	public final int compare(ILogEntry log1, ILogEntry log2) {
		if ((log1 == null) || (log2 == null))
			return 0;
			
		Comparable item1 = (Comparable)(log1).getField(fieldIndex);
		Comparable item2 = (Comparable)(log2).getField(fieldIndex);
	
		int returnValue = 0;
	
		int nulltest = 0;
		if (item1 == null) nulltest += 1;
		if (item2 == null) nulltest += 2;
	
		switch (nulltest) {
			case 0 : returnValue = item1.compareTo(item2); break;
			case 1 : returnValue = -1; break;
			case 2 : returnValue = 1; break;
			case 3 : returnValue = 0; break;
		}
	
		return (ascending ? returnValue : -returnValue);
	
	}
	/**
	 * Insert the method's description here.
	 * <p>
	 * Creation date: (1/1/2002 14:57:38)
	 * @return int
	 */
	public int getFieldIndex() {
		return fieldIndex;
	}
	/**
	 * Insert the method's description here.
	 * <p>
	 * Creation date: (1/1/2002 14:59:15)
	 * @return boolean
	 */
	public boolean isAscending() {
		return ascending;
	}
}
