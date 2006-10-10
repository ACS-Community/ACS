/*******************************************************************************
 * ALMA - Atacama Large Millimiter Array
 * (c) European Southern Observatory, 2002
 * Copyright by ESO (in the framework of the ALMA collaboration)
 * and Cosylab 2002, All rights reserved
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */

package alma.ACS.jbaci;

import java.util.Comparator;

/**
 * A heap-based priority queue.
 * The class currently uses a standard array-based heap, as described
 * in, for example, Sedgewick's Algorithms text. All methods are fully synchronized.
 **/
public class Heap {

	/**
	 * The tree nodes, packed into an array.
	 */
	protected Object[] nodes_; 

	/**
	 * Number of used slots.
	 */
	protected int count_ = 0; // number of used slots

	/**
	 * Ordering comparator.
	 */
	protected final Comparator cmp_;

	/**
	 * Create a Heap with the given initial capacity and comparator
	 * @param capacity initial capacity.
	 * @param cmp comparator instance.
	 * @exception IllegalArgumentException if capacity less or equal to zero
	 **/
	public Heap(int capacity, Comparator cmp) throws IllegalArgumentException {
		if (capacity <= 0)
			throw new IllegalArgumentException();
		nodes_ = new Object[capacity];
		cmp_ = cmp;
	}

	/**
	 * Create a Heap with the given capacity, and relying on natural ordering.
	 * @param capacity initial capacity.
	 **/
	public Heap(int capacity) {
		this(capacity, null);
	}

	/**
	 * Perform element comparisons using comparator or natural ordering.
	 **/
	protected int compare(Object a, Object b) {
		if (cmp_ == null)
			return ((Comparable) a).compareTo(b);
		else
			return cmp_.compare(a, b);
	}

	/**
	 * Get parent index.
	 */
	protected final int parent(int k) {
		return (k - 1) / 2;
	}

	/**
	 * Get left child.
	 */
	protected final int left(int k) {
		return 2 * k + 1;
	}

	/**
	 * Get right child.
	 */
	protected final int right(int k) {
		return 2 * (k + 1);
	}

	/**
	 * Insert an element, resize if necessary.
	 * @param x object to insert.
	 **/
	public synchronized void insert(Object x) {
		if (count_ >= nodes_.length) {
			int newcap = 3 * nodes_.length / 2 + 1;
			Object[] newnodes = new Object[newcap];
			System.arraycopy(nodes_, 0, newnodes, 0, nodes_.length);
			nodes_ = newnodes;
		}

		int k = count_;
		++count_;
		while (k > 0) {
			int par = parent(k);
			if (compare(x, nodes_[par]) < 0) {
				nodes_[k] = nodes_[par];
				k = par;
			} else
				break;
		}
		nodes_[k] = x;
	}

	/**
	 * Return and remove least element, or null if empty.
	 * @return extraced least element.
	 **/
	public synchronized Object extract() {
		if (count_ < 1)
			return null;

		int k = 0; // take element at root;
		Object least = nodes_[k];
		--count_;
		Object x = nodes_[count_];
		nodes_[count_] = null;
		for (;;) {
			int l = left(k);
			if (l >= count_)
				break;
			else {
				int r = right(k);
				int child = (r >= count_ || compare(nodes_[l], nodes_[r]) < 0) ? l
						: r;
				if (compare(x, nodes_[child]) > 0) {
					nodes_[k] = nodes_[child];
					k = child;
				} else
					break;
			}
		}
		nodes_[k] = x;
		return least;
	}

	/**
	 * Return least element without removing it, or null if empty.
	 * @return least element.
	 **/
	public synchronized Object peek() {
		if (count_ > 0)
			return nodes_[0];
		else
			return null;
	}

	/**
	 * Return number of elements.
	 * @return number of elements.
	 **/
	public synchronized int size() {
		return count_;
	}

	/**
	 * Remove all elements.
	 **/
	public synchronized void clear() {
		for (int i = 0; i < count_; ++i)
			nodes_[i] = null;
		count_ = 0;
	}

}
