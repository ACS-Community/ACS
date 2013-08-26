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
	 * The length of the bytes representing each entry.
	 * <P>
	 * This number is useful while caching the entries on a file.
	 * <P>
	 * 40 is 8 for the integer and 2 time 16 for the longs.
	 * 
	 * @see toBytes()
	 */
	public static final int ENTRY_LENGTH = 40; 
	
	/**
	 * The key of the file where the entry is stored
	 */
	public final int key;
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
		if (key==null || key<0) {
			throw new IllegalArgumentException("The key must be not null and greater or equal to 0");
		}
		if (startPos<0 || endPos<=0 || startPos>=endPos) {
			throw new IllegalArgumentException("Invalid start/end positions: "+startPos+", "+endPos);
		}
		this.key=key.intValue();
		start=startPos;
		end=endPos;
	}
	
	/**
	 * Constructor.
	 * <P>
	 * Build the entry by its hexadecimal representation
	 * 
	 * @param hexadecimal
	 * 
	 * @see toHexadecimal()
	 */
	public CacheEntry(String hexadecimal) {
		key=Integer.decode("0X"+hexadecimal.substring(0, 8));
		start=Long.decode("0X"+hexadecimal.substring(8, 24));
		end=Long.decode("0X"+hexadecimal.substring(24, 40));
	}
	
	/**
	 * Translate the content of the entry in an hexadecimal string composed
	 * of the key, the start and end.
	 * <P>
	 * The array returned by this method can be written on a file.
	 * Its length is constant for each possible entry so the fields are not
	 * separated.
	 * 
	 * @return An hexadecimal string representing the content of this entry
	 */
	public String toHexadecimal() {
		StringBuilder str = new StringBuilder();
		String hexKey=String.format("%08X",key);
		String hexStart=String.format("%016X",start);
		String hexEnd = String.format("%016X",end);
		str.append(hexKey);
		str.append(hexStart);
		str.append(hexEnd);
		
		return str.toString();
	}
}
