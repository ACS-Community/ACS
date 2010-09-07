/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
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
package alma.acs.container;

import java.util.*;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Synchronized type-safe map for loaded components.
 * Keys are component handles, values are <code>ComponentAdapter</code>s.
 * The map entries are sorted by insertion order, or by the handle sequence given to {@link #sort(int[])}. 
 * <p>
 * Note that it is not sufficient to use something like 
 * <code>Collections.synchronizedMap(new HashMap())</code>,
 * since reading out the keys or values of such a map with an iterator
 * is still backed by the original instance and therefore not thread-safe.
 * See {@link Collections#synchronizedMap(java.util.Map)}.
 *    
 * @author hsommer
 * created Oct 30, 2003 10:04:28 AM
 */
class ComponentMap
{
	/**
	 * The map that backs this class. 
	 */
	private LinkedHashMap<Integer, ComponentAdapter> m_map;
	
	private Logger logger;
	
	ComponentMap(Logger logger) {
		this.logger = logger;
		m_map = new LinkedHashMap<Integer, ComponentAdapter>();
	}
	
	
	/**
	 * Makes a reservation for a <code>ComponentAdapter</code> based on the component handle.
	 * Can be used to avoid concurrent multiple activation of the same component (which also manager should prevent). 
	 * @param compHandle
	 * @return true if this handle could be reserved
	 */
	synchronized boolean reserveComponent(int compHandle) {
		boolean ret = false;
		Integer handleObj = new Integer(compHandle);
		// need to check if it's an old ComponentAdapter we want to overwrite, or a reservation (null)
		if (!m_map.containsKey(handleObj) || m_map.get(handleObj) != null ) {
			m_map.put(handleObj, null);
			ret = true;
		}
		return ret;
	}
	
	
	/**
	 * 
	 * @param compHandle
	 * @param compAdapter
	 * @return  previously stored adapter, possibly null 
	 * @see Map#put(java.lang.Object, java.lang.Object)
	 */
	synchronized ComponentAdapter put(int compHandle, ComponentAdapter compAdapter)
	{
		return ( m_map.put(compHandle, compAdapter) );
	}
	
	
	/**
	 * Sorts the entries in the order given by <code>sortedHandles</code>.
	 * The sorted component handles will be received from the manager.
	 * Before this method is called, the iteration order is that of insertion order.  
	 * <p>
	 * @param sortedHandles
	 */
	synchronized void sort(int[] sortedHandles) {
		Map<Integer, ComponentAdapter> tmpMap = new LinkedHashMap<Integer, ComponentAdapter>(m_map.size());
		try {
			for (int i = 0; i < sortedHandles.length; i++) {
				ComponentAdapter compAdapter = get(sortedHandles[i]);
				if (compAdapter != null) {
					Integer keyObj = new Integer(sortedHandles[i]);
					tmpMap.put(keyObj, compAdapter);
					m_map.remove(keyObj);
				}
				else {
					logger.warning("Invalid component handle '" + sortedHandles[i] + "' provided for component sorting. " + 
							"This means that ACS manager wrongly believes this container hosts a component with this handle.");
				}
			}
			if (!m_map.isEmpty()) {
				String missingHandles = "";
				for (int handle : m_map.keySet()) {
					missingHandles += handle + " ";
				}
				logger.info("Not enough component handles provided for sorting. Missing handles: " + missingHandles);
			}
		} catch (Exception ex) {
			// we don't expect an exception here, but really don't want this sorting to disturb anything else
			logger.log(Level.WARNING, "Failed to sort components into correct shutdown order!", ex);
		} finally {
			m_map.putAll(tmpMap);
		}
	}
	
	/**
	 * Removes a component adapter from the map.
	 * 
	 * @param compHandle  handle of component (adapter) to be removed
	 * @return  old <code>ComponentAdapter</code> with that handle, or null
	 * @see Map#remove(java.lang.Object)
	 */
	synchronized ComponentAdapter remove(int compHandle)
	{
		return m_map.remove(new Integer(compHandle));
	}


	/**
	 * Returns the component adapter for a given component handle, 
	 * or <code>null</code> if either the component handle is unknown, or if the corresponding
	 * component is still under construction (which means that the handle is just for a 'reservation'.
	 *  
	 * @param compHandle
	 * @return  component adapter, or null 
	 * @see Map#get(java.lang.Object)
	 */
	synchronized ComponentAdapter get(int compHandle)
	{
		return m_map.get(new Integer(compHandle));
	}

	
	/**
	 * Gets component adapters with the specified handles out of the map.
	 * Ignores handles for which no components are stored.
	 * The order of the returned component adapters matches the order of the handles.  
	 * <p>
	 * Unlike {@link Map#values()}, the returned array contains 
	 * a snapshot of the map contents,
	 * rather than a view backed by the map.
	 *  
	 * @param compHandles handles of those components whose adapters should be returned. 
	 * @return  adapters for all components in the map whose handles match <code>compHandles</code>. 
	 * 			Possibly an empty array (if compHandles is null or empty or contains wrong handles), but never null.
	 * @see Map#values()
	 */
	synchronized ComponentAdapter[] getComponentAdapters(int[] compHandles)
	{
		List<ComponentAdapter> adapters = new ArrayList<ComponentAdapter>();
		
		if (compHandles != null)
		{
			for (int i = 0; i < compHandles.length; i++)
			{
				ComponentAdapter adapter = get(compHandles[i]);
				if (adapter != null)
				{
					adapters.add(adapter);
				}			
			}
		}
		ComponentAdapter[] ret = adapters.toArray(new ComponentAdapter[adapters.size()]);		
					
		return ret;
	}
	
	
	/**
	 * Tries to find a component with the given name and type, regardless of the handle
	 * which is the "primary key".
	 * 
	 * @param name component instance name (curl)
	 * @param type  IDL type name
	 * @return the component adapter that matches name and type, or null if none is found.
	 */
	ComponentAdapter getComponentByNameAndType(String name, String type)
	{
		// todo-: check performance; perhaps iterate directly over underlying map, then synchronized
		ComponentAdapter existingCompAdapter = null;
		ComponentAdapter[] compAdapters = getAllComponentAdapters();
		for (int i = 0; i < compAdapters.length; i++)
		{
			ComponentAdapter otherAdapter = compAdapters[i];
			if (otherAdapter.getName().equals(name) && otherAdapter.getType().equals(type))
			{
				existingCompAdapter = otherAdapter;
				break;
			}
		}
		return existingCompAdapter;
	}
	
	
	/**
	 * Gets all component adapters stored in the map.
	 * Unlike {@link Map#values()}, the returned array contains a snapshot of the map contents,
	 * rather than a live view backed by the map. 
	 * <p>
	 * Notice that <code>null</code> adapters for {@link #reserveComponent(int) reserved} components
	 * are not included in the returned array of adapters.
	 *  
	 * @return  adapters for all components in the map 
	 * @see Map#values()
	 */
	synchronized ComponentAdapter[] getAllComponentAdapters()
	{
		List<ComponentAdapter> nonNullAdapters = new ArrayList<ComponentAdapter>();
		for (ComponentAdapter compAdapter : m_map.values()) {
			if (compAdapter != null) {
				nonNullAdapters.add(compAdapter);
			}
		}
		ComponentAdapter[] ret = nonNullAdapters.toArray(new ComponentAdapter[nonNullAdapters.size()]);		
		return ret;
	}
	

	/**
	 * Gets all component handles, which are the keys of the map.
	 * Unlike {@link Map#keySet()}, the returned array contains a snapshot of the map contents,
	 * rather than a view backed by the map. 
	 * 
	 * <p>
	 * TODO: check if it's ok to include handles for reserved component adapters (value still null)
	 *  
	 * @return  keys for all component (adapters) in the map 
	 * @see Map#keySet()
	 */
	synchronized int[] getAllHandles()
	{
		Collection<Integer> keys = m_map.keySet();
		int[] ret = new int[keys.size()];
		
		int index=0;
		for (Integer key : keys) {
			ret[index] = key.intValue();
			index++;
		}

		return ret;
	}
	

}
