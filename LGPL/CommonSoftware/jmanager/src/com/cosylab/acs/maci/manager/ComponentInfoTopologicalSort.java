/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci.manager;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import com.cosylab.acs.maci.ComponentInfo;
import com.cosylab.acs.maci.HandleConstants;

/**
 * An implementation of topological sort operating on <code>ComponentInfo</code> handle data store.
 */
@SuppressWarnings("unchecked")
public class ComponentInfoTopologicalSort {
	
	/**
	 * The tree nodes, packed into an array.
	 */
	protected ComponentInfoVertex[] nodes;

	/**
	 * Number of used slots.
	 */
	protected int count = 0;

	/**
	 * Handle to hash index lookups.
	 */
	protected IntHashMap handleToHashIndexMap;

	/**
	 * Handle data store.
	 */
	protected HandleDataStore handleDataStore;

	/**
	 * Create a Heap with the given initial capacity and comparator.
	 * @param capacity initial heap capacity.
	 * @param cmp comparator used to compare objects, if <code>null</code> natural ordering is used
	 * @exception IllegalArgumentException if capacity less than zero
	 */
	public ComponentInfoTopologicalSort(int capacity) throws IllegalArgumentException {
		if (capacity < 0)
			throw new IllegalArgumentException();
		
		nodes = new ComponentInfoVertex[capacity];

		// large load factor, no rehashing needed
		handleToHashIndexMap = new IntHashMap(capacity, 2);
	}

	/**
	 * Constructor.
	 * @param dataStore	<code>ComponentInfo</code> handle data store instance to sort.
	 */
	public ComponentInfoTopologicalSort(HandleDataStore dataStore) {
		this(dataStore.size());
		
		HashSet immortalChainMap = generateImmortalChainMap(dataStore);

		int pos = 0;
		for (int h = dataStore.first(); h != 0; h = dataStore.next(h))
		{
			ComponentInfo componentInfo = (ComponentInfo)dataStore.get(h);
			this.insert(new ComponentInfoVertex(componentInfo, pos++, immortalChainMap.contains(componentInfo)));
		}
		
		this.handleDataStore = dataStore;
	}

	/**
	 * Generate immortal chain.
	 * @param dataStore	<code>ComponentInfo</code> handle data store instance to sort.
	 * @return set of components (immportal or components which clients are immortal components).
	 */
	private HashSet generateImmortalChainMap(HandleDataStore dataStore)
	{
		HashSet set = new HashSet();
		// TODO Manager.getHandle should be used here
		markImmortalChain(set, dataStore, HandleConstants.MANAGER_MASK);
		return set;
	}

	/**
	 * DSF algrithm to generate immortal chain.
	 * @param immortalChainMap set of components (immportal or components which clients are immortal components).
	 * @param dataStore	<code>ComponentInfo</code> handle data store instance to sort.
	 * @param marker	handle of the object which identifies member of immortal chain.
	 */
	private void markImmortalChain(HashSet immortalChainMap, HandleDataStore dataStore, int marker)
	{
		for (int h = dataStore.first(); h != 0; h = dataStore.next(h))
		{
			ComponentInfo componentInfo = (ComponentInfo)dataStore.get(h);

			int[] clients = componentInfo.getClients().toArray();
			for (int i = 0; i < clients.length; i++)
			{
				if (clients[i] == marker && !immortalChainMap.contains(componentInfo))
				{
					immortalChainMap.add(componentInfo);
					markImmortalChain(immortalChainMap, dataStore, componentInfo.getHandle());
				}
			}
		}
		
	}
	/**
	 * Return parent node of the child.
	 * @param k index of a child node.
	 */
	protected final int parent(int k) {
		return (k - 1) / 2;
	}

	/**
	 * Return left child node.
	 * @param k index of a parent node.
	 */
	protected final int left(int k) {
		return 2 * k  + 1;
	}

	/**
	 * Return right child node.
	 * @param k index of a parent node.
	 */
	protected final int right(int k) {
		return 2 * (k + 1);
	}

	/**
	 * Insert an element, resize if necessary.
	 * @param element object to be insterted.
	 */
	public void insert(ComponentInfoVertex element) {
		
		// resize if necessary
		if (count >= nodes.length) {
			int newcap = 3 * nodes.length / 2 + 1;
			ComponentInfoVertex[] newnodes = new ComponentInfoVertex[newcap];
			System.arraycopy(nodes, 0, newnodes, 0, nodes.length);
			nodes = newnodes;
		}

		// downheap
		int k = count;
		++count;
		downheap(element, k);
	}

	/**
	 * @param element
	 * @param k
	 */
	protected void downheap(ComponentInfoVertex element, int k) {
		while (k > 0) {
			int par = parent(k);
			if (element.compareTo(nodes[par]) < 0) {
				nodes[k] = nodes[par];
				handleToHashIndexMap.put(nodes[par].getComponentInfo().getHandle(), k);
				k = par;
			} else
				break;
		}
		nodes[k] = element;
		handleToHashIndexMap.put(element.getComponentInfo().getHandle(), k);
	}

	/**
	 * Return and remove least element, or null if empty.
	 * @return least element (min).
	 */
	protected ComponentInfoVertex heapExtract() {
		if (count < 1)
			return null;

		// take element at root
		int k = 0;
		ComponentInfoVertex least = nodes[k];
		handleToHashIndexMap.remove(least.getComponentInfo().getHandle());
		--count;
		
		// take last element, put it to root and do upheap
		ComponentInfoVertex x = nodes[count];
		nodes[count] = null;
		for (;;) {
			int l = left(k);
			if (l >= count)
				break;
			else {
				int r = right(k);
				int child = (r >= count || nodes[l].compareTo(nodes[r]) < 0) ? l : r;
				if (x.compareTo(nodes[child]) > 0) {
					nodes[k] = nodes[child];
					handleToHashIndexMap.put(nodes[child].getComponentInfo().getHandle(), k);
					k = child;
				} else
					break;
			}
		}
		nodes[k] = x;
		handleToHashIndexMap.put(x.getComponentInfo().getHandle(), k);
		return least;
	}

	/**
	 * Return and remove least element, or null if empty.
	 * @return least element (min).
	 */
	public ComponentInfoVertex extract() {
		ComponentInfoVertex civ = heapExtract();
		
		// for each child decrement indegree and heapify
		if (!civ.getComponentInfo().getComponents().isEmpty())
		{
			int[] clients = civ.getComponentInfo().getComponents().toArray();
			int civHandle = civ.getComponentInfo().getHandle();
			for (int i = 0; i < clients.length; i++)
			{
				int index = handleToHashIndexMap.get(clients[i]);
				if (index != -1)
				{
					if (nodes[index].getComponentInfo().getClients().contains(civHandle))
					{
						nodes[index].decrementIndegree();
						downheap(nodes[index], index);
					}
				}
			}
		}
		
		return civ;
	}

	/**
	 * Return least element without removing it, or null if empty
	 * @return least element (min).
	 **/
	public ComponentInfoVertex peek() {
		if (count > 0)
			return nodes[0];
		else
			return null;
	}

	/**
	 * Return number of elements.
	 * @return number of elements
	 **/
	public int size() {
		return count;
	}

	/**
	 * Remove all elements.
	 **/
	public void clear() {
		for (int i = 0; i < count; ++i)
			nodes[i] = null;
		count = 0;
	}
	

	/**
	 * Topological sort on <code>ComponentInfo</code> handle data store.
	 * @param handleDataStore	<code>ComponentInfo</code> handle data store
	 * @return topologically sorted list.
	 */
	public static List sort(HandleDataStore handleDataStore) {

		ComponentInfoTopologicalSort ts = new ComponentInfoTopologicalSort(handleDataStore);
		ArrayList list = new ArrayList(ts.size());
		
		while (ts.size() > 0)
		{
			ComponentInfoVertex civ = ts.extract();
			// if civ.getIndegree() != 0 then we have cycle (but we allow them)
			list.add(civ.getComponentInfo());
		}
		
		return list;
	}
	
	/**
	 * Wrapper class around <code>ComponentInfo</code>.
	 */
	private static class ComponentInfoVertex implements Comparable {
		
		/**
		 * Immortal credit (to increase indregree).
		 * Such components should be released after all non-importal components
		 * which are not subcomponents of moral components. 
		 */
		private static final int IMMORTAL_CREDIT = Integer.MAX_VALUE / 2;
		
		/**
		 * ComponentInfo instance itself.
		 */
		private ComponentInfo componentInfo;
		
		/**
		 * Indegree (dependency) count.
		 */
		private int indegree = 0;
		
		/**
		 * Activation order (position).
		 * Used to reconstruct reverse deactivation order.
		 */
		private int order;

		/**
		 * Constructor.
		 * @param componentInfo	instance to wrap.
		 */
		public ComponentInfoVertex(ComponentInfo componentInfo, int order, boolean immortalChain)
		{
			this.componentInfo = componentInfo;
			this.order = order;
			
			if (immortalChain)
				indegree += IMMORTAL_CREDIT;
				
			int[] clients = componentInfo.getClients().toArray();
			for (int i = 0; i < clients.length; i++)
			{
				int type = clients[i] & HandleConstants.TYPE_MASK;
				if (type == HandleConstants.COMPONENT_MASK)
					indegree++;
			}
		}
		
		/**
		 * Decrement (remove) one dependecy.
		 */
		public void decrementIndegree() {
			indegree--;
		}
		
		/**
		 * @see java.lang.Comparable#compareTo(java.lang.Object)
		 */
		public int compareTo(Object obj) {
			int otherIndegree = ((ComponentInfoVertex)obj).indegree;
			if (indegree < otherIndegree)
				return -1;
			else if (indegree == otherIndegree)
			{
				// reverse order
				int otherOrder = ((ComponentInfoVertex)obj).order;
				if (order < otherOrder)
					return 1;
				else if (order == otherOrder)
					return 0;
				else
					return -1;
			}
			else
				return 1;
		}

		/**
		 * Get wraped instance of <code>ComponentInfo</code>.
		 * @return returns the componentInfo.
		 */
		public ComponentInfo getComponentInfo() {
			return componentInfo;
		}

		/**
		 * @return Returns the indegree.
		 */
		public int getIndegree() {
			return indegree;
		}
	}
	
	
}