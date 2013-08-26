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

import com.cosylab.logging.engine.log.ILogEntry;

public class LogIterator implements Iterator<ILogEntry> {
	
	/**
	 * The map of the logs
	 */
	private ILogMap logs = null;
	
	/**
	 * The key read with the last call to next()
	 * This value is needed to implement the remove()
	 */
	private Integer lastReadKey=null;
	
	// The first and last key in the map
	// Both of them can change if the map of logs changes while iterating
	private Integer firstKey=null;
	private Integer lastKey= null;
	
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
	}

	/**
	 * @see java.util.Iterator
	 */
	public boolean hasNext() {
		if (logs.getSize()==0) {
			return false;
		} else if (lastReadKey==null) {
			return true;
		} else {
			lastKey=logs.getLastLog();
			return lastReadKey<lastKey;
		}
	}

	/**
	 * @see java.util.Iterator
	 */
	public ILogEntry next() {
		if (lastReadKey==null) {
			firstKey = logs.getFirstLog();
			try {
				lastReadKey=firstKey;
				return logs.getLog(firstKey);
			} catch (LogCacheException e) {
				throw new NoSuchElementException("Log not found");
			}
		}
		for (Integer t = lastReadKey+1; t<=logs.getLastLog(); t++) {
			if (logs.keySet().contains(t)) {
				lastReadKey=t;
				try {
					return logs.getLog(lastReadKey);
				} catch (LogCacheException e) {
					throw new NoSuchElementException("Log not found");
				}
			}
		}
		throw new NoSuchElementException("No more elements");
	}

	/**
	 * The remove implemented in this way doesn't work because it modifies
	 * the elements in the set and the keyIterator throws a ConcurrentModificationException
	 * 
	 * @see java.util.Iterator
	 */
	public void remove() {
		if (lastReadKey==null) {
			throw new IllegalStateException("No log to remove (call next before remove)");
		} else {
			try {
				logs.deleteLog(lastReadKey);
			} catch (LogCacheException e) {
				throw new IllegalStateException("Log with key "+lastReadKey+" already deleted");
			}
		}
	}

}
