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

import java.util.Collection;
import java.util.Iterator;
import java.util.Set;

import com.cosylab.logging.engine.log.ILogEntry;

/**
 * The interface with the methods for the map of logs.
 * The cache is implementing this interface.
 * 
 * @author acaproni
 */
public interface ILogMap {
	/**
	 * Clear the Map i.e. remove all the logs and keys from the map
	 * 
	 * @throws LogCacheException
	 */
	public void clear() throws LogCacheException;
	
	/**
	 * Remove a log from the Map
	 * @param key
	 * @throws LogCacheException
	 */
	public void deleteLog(Integer key) throws LogCacheException;
	
	/**
	 * Delete a set of logs
	 * 
	 * @param keys The keys of logs to delete
	 */
	public void deleteLogs(Collection<Integer> keys) throws LogCacheException;
	
	/**
	 * 
	 * @return The number of logs in the map
	 */
	public int getSize();
	
	/**
	 * The keys in the map
	 * 
	 * @return The key in the map
	 */
	public Set<Integer> keySet();
	
	/**
	 * Return a log eith the given key
	 * 
	 * @param key The key of the logs
	 * @return The log with the given key
	 * @throws LogCacheException
	 */
	public ILogEntry getLog(Integer key) throws LogCacheException;
	
	/**
	 * Add a log in the map
	 * 
	 * @param log The log to add in the map
	 * @return The key of the added log
	 * 
	 * @throws LogCacheException
	 */
	public int add(ILogEntry log) throws LogCacheException;
	
	/**
	 * Replace the log in the given position with the new one
	 
	 * @param position The position of the log to replace
	 * @param log The key (identifier) ot the log
	 */
	public void replaceLog(Integer key, ILogEntry log) throws LogCacheException;
	
	/**
	 * Return an Iterator to browse the logs in the map.
	 * The order the iterator returns the logs is that of the keys.
	 * 
	 * @return an Iterator over the elements in this map
	 */
	public Iterator<ILogEntry> iterator();
	
	/**
	 * Return the key of the last valid log (FIFO)
	 * The key of the last log is the key of the last inserted log
	 * but it can cheang if such log has been deleted
	 * 
	 * @return The key of the last inserted log
	 *         null if th cache is empty
	 */
	public Integer getLastLog();
	
	/**
	 * Return the key of the first valid log (FIFO).
	 * The key of the first log is 0 but it can change if the log 0 has
	 * been deleted.
	 * 
	 * @return The key of the first log
	 *         null if the cache is empty
	 */
	public Integer getFirstLog();
	
	/**
	 * Append at most n keys from the first valid logs to the collection.
	 * First here means first in the FIFO policy.
	 * 
	 * The number of added keys can be less then n if the cache doesn't
	 * contain enough logs.
	 * 
	 * @param n The desired number of keys of first logs
	 * @param keys The collection to add they keys to
	 * @return The number of keys effectively added
	 */
	public int getFirstLogs(int n, Collection<Integer> keys);
}
