/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2006
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
package com.cosylab.logging.engine.cache;

/**
 * An entry of the cache.
 * It contains the name of the file where the entry is stored together with the
 * position of the entry.
 * 
 * Having the name of the file allows to open and close the file when needed.
 * In a previous version there was a <code>RandomAccessFile</code> instead of the name
 * but it ended up with an error because the number of open file was exceeding
 * the maximum allowed.
 * 
 * 
 * @author acaproni
 *
 */
public class CacheEntry {
	/**
	 * The key of the file where the entry is stored
	 */
	public final Integer key;
	/**
	 * The starting position of the entry in the file
	 */
	public final long start;
	/**
	 * The ending position of the entry in the file
	 */
	public final long end;
	
	/**
	 * Constructor
	 * 
	 * @param key The key of the file where the entry is stored
	 * @param startPos The starting position of the entry in the file
	 * @param endPos The ending position of the entry in the file
	 */
	public CacheEntry(Integer key, long startPos, long endPos) {
		if (key==null) {
			throw new IllegalArgumentException("The file name can't be null nor empty");
		}
		if (startPos<0 || endPos<=0 || startPos>=endPos) {
			throw new IllegalArgumentException("Invalid start/end positions: "+startPos+", "+endPos);
		}
		this.key=key;
		start=startPos;
		end=endPos;
	}
}
