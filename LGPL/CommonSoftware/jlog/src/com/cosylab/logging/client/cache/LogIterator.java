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
package com.cosylab.logging.client.cache;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Set;

import com.cosylab.logging.engine.log.ILogEntry;

public class LogIterator implements Iterator<ILogEntry> {
	
	/**
	 * The map of the logs
	 */
	private ILogMap logs = null;
	
	/**
	 * The keys of the logs in the map
	 */
	private Set<Integer> keys = null;
	
	/**
	 * The iterator over the keys in the map
	 */
	private Iterator<Integer> keyIterator =null;
	
	/**
	 * The key read with the last call to next()
	 * This value is needed to implement the remove()
	 */
	private Integer lastKey=null;
	
	/**
	 * Constructor
	 * 
	 * @param logs The map containing the logs to iteratoe over
	 */
	public LogIterator(ILogMap logs) {
		if (logs==null) {
			throw new IllegalArgumentException("The map of logs can't be null");
		}
		this.logs=logs;
		keys = logs.keySet();
		if (keys!=null) {
			keyIterator = keys.iterator();
		}
	}

	/**
	 * @see java.util.Iterator
	 */
	public boolean hasNext() {
		return keyIterator.hasNext();
	}

	/**
	 * @see java.util.Iterator
	 */
	public ILogEntry next() {
		lastKey = keyIterator.next();
		if (lastKey==null) {
			throw new NoSuchElementException("No log for null key ");
		}
		try {
			ILogEntry log = logs.getLog(lastKey);
			return log;
		} catch (LogCacheException e) {
			throw new NoSuchElementException("No log for key "+lastKey);
		}
	}

	/**
	 * The remove implemented in this way doesn't work because it modifies
	 * the elements in the set and the keyIterator throws a ConcurrentModificationException
	 * 
	 * @see java.util.Iterator
	 */
	public void remove() {
		if (lastKey==null) {
			throw new IllegalStateException("No log to remove (key is null)");
		} else {
			try {
				logs.deleteLog(lastKey);
			} catch (LogCacheException e) {
				System.err.println("The log is not in the cache.");
				System.err.println("Probably the cache has been modified while reading");
				e.printStackTrace();
			}
			// Set the key to null to avoid to delete the same log again
			lastKey=null;
		}
	}

}
