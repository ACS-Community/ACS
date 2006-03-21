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
package com.cosylab.logging.client;

import java.util.List;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Comparator;
import java.util.Collections;

import java.awt.Cursor;

import com.cosylab.logging.engine.log.LogEntryXML;
import com.cosylab.logging.LoggingClient;
import com.cosylab.logging.client.cache.LogFileCache;

/**
 * Linear list that allows grouping of similar elements based on a comparator object.
 * Creation date: (11/30/2001 13:54:29)
 * @author: Ales Pucelj (ales.pucelj@kgb.ijs.si)
 */
public final class GroupedList {
	
	// The cache with all the logs
	// In this implementation we keep the indexes of the position
	// in the cache of the visible log entries insted of a copy of the
	// LogEntryXML themself.
	private LogFileCache cache;
	
	private final List<GroupedListItem> elements = new ArrayList<GroupedListItem>();

	private Comparator<LogEntryXML> sortComparator = null;
	private final GroupedListComparator groupComparator =
		new GroupedListComparator();
	private final GroupedListIndexComparator indexComparator =
		new GroupedListIndexComparator();

	private final GroupedListItem temp = new GroupedListItem();

	private GroupedListCallback changeCallback = null;
	private int updating = 0;

	private int lastAbsoluteIndex = -1;
	private int lastGroupIndex = -1;

	private UpdateQueue updateQueue = new UpdateQueue();

	/**
	 * Updates queue used to add or remove elements from the list. Works in
	 * a separate thread to reduce the lag on the user interface.
	 * <p>
	 * Creation date: (12/25/2001 11:27:11)
	 * @author: Ales Pucelj (ales.pucelj@kgb.ijs.si)
	 */
	private final class UpdateQueue extends Thread {
		private LinkedList<UpdateAction> queue = new LinkedList<UpdateAction>();
		private LinkedList<UpdateAction> uiQueue = new LinkedList<UpdateAction>();

		private static final byte ITEM_ADD = 0;
		private static final byte ITEM_REMOVE = 1;
		private static final byte ITEM_EXPAND = 2;
		private static final byte ITEM_COLLAPSE = 3;

		private final class UpdateAction {
			public byte action;
			public Integer data;
			public int index;
			
	        public UpdateAction(byte a, Integer d, int i) {
		        action = a;
		        data = d;
		        index = i;
	        }
		}
		/**
		 * Constructor.
		 */
		public UpdateQueue() {
			super();
		}
		/**
		 * Posts a request to add item to the list.
		 * <p>
		 * @param item  
		 */
		public synchronized void addItem(Integer item) {
            synchronized(queue) {
		//System.out.print(" UpdateAction action for item "+item);
		//System.out.println(" Thread state "+this.getState());
    	        queue.add(new UpdateAction(ITEM_ADD, item, 0));
		}

    	        		notifyAll();
		}
		/**
		 * Posts a request to remove item from the list.
		 * <p>
		 * @param item  
		 */
		public synchronized void removeItem(Integer item) {
            synchronized(queue) {
                queue.add(new UpdateAction(ITEM_REMOVE, item, 0));
            }
                notifyAll();
		}
		/**
		 * Posts a request for expanding of an item.
		 * <p>
		 * @param item  
		 */
		public synchronized void expandItem(int index) {
            synchronized(uiQueue) {
    	        uiQueue.add(new UpdateAction(ITEM_EXPAND, null, index));
            }
    	        notifyAll();
		}
		/**
		 * Posts a request for collapsing of an item (the opposite of exapanding).
		 * <p>
		 * @param item  
		 */
		public synchronized void collapseItem(int index) {
            synchronized(uiQueue) {
    	        uiQueue.add(new UpdateAction(ITEM_COLLAPSE, null, index));
            }
    	        notifyAll();
		}

		public final void run() {
			while (true) {
				// if (!GroupedList.this.isUpdating()) {
				synchronized (uiQueue) {
					if (!uiQueue.isEmpty()) {
						UpdateAction ua = uiQueue.get(0);
						uiQueue.remove(0);

						int i = internalGet(ua.index);
						if (i >= 0) {
							GroupedListItem group = GroupedList.this
									.getGroup(i);
							group.setExpanded(ua.action == ITEM_EXPAND);
						}
					}
				}
				synchronized (queue) {
					if (!queue.isEmpty()) {
						UpdateAction ua = queue.get(0);
						queue.remove(0);

						if (ua.action == ITEM_ADD) {
							GroupedList.this.addInternal(ua.data);
						} else if (ua.action == ITEM_REMOVE) {
							GroupedList.this.removeInternal(ua.data);
						}
					}
				}
				// Heiko and I investigated the reason while this thread never
				// wakes up form the wait in ste-gns (even if the notifyAll was executed
				// several times)
				// We didn't find a real error in the code but executing the test
				// in this way seems to fix
				synchronized (this) {
					if (queue.isEmpty() && uiQueue.isEmpty()) {
						try {
								wait();
						} catch (InterruptedException e) {
						}
					}
				}
				// }
			}
		}
	}

	/**
	 * This is wrapper class for <code>GroupedList</code> items. It provides
	 * efficient grouping support for linear lists. It is designed to offer good
	 * performance for both large and single element groups.
	 * <p>
	 * Items contained in this group are maintained by List. However, if the
	 * group contains only one object, the groupList is null to avoid excesive
	 * memory consumption, and the item is stored in a single variable. The
	 * class provides this functionality transparently.
	 * <p>
	 * Creation date: (11/30/2001 13:21:50)
	 * 
	 * @author: Ales Pucelj (ales.pucelj@kgb.ijs.si)
	 */
	public final class GroupedListItem {

		private int absoluteIndex = 0;
		private boolean expanded = false;
		private boolean updated = false;
		
		// A list of indexes of log entries in the cache
		private List<Integer> indexOfGroupedItems = null;

		private Integer indexOfFirstItem = null;

		private GroupedList owner = null;

		/**
		 * GroupedListItem constructor comment.
		 * Creation date: (11/30/2001 14:12:41) 
		 */
		public GroupedListItem() {
			super();
			owner = GroupedList.this;
		}
		
		/**
		 * GroupedListItem constructor comment.
		 * Creation date: (11/30/2001 14:12:41)
		 * @param item The index in the cache of all the logs of a LogEntryXML  
		 */
		public GroupedListItem(Integer item) {
			super();
			indexOfFirstItem = item;
			owner = GroupedList.this;
		}
		
		/**
		 * Adds the item to the group. 
		 * Creation date: (11/30/2001 13:24:39)
		 * @param item  The index in the cache of all the logs of the log to add
		 */
		public final void add(Integer item) {
			int index = getAbsoluteIndex();

			if (indexOfFirstItem == null) {
				indexOfFirstItem = item;
				owner.fireDataInserted(index, index);
				return;
			}

			if (indexOfGroupedItems == null) {
				indexOfGroupedItems = createDefaultList();
				indexOfGroupedItems.add(indexOfFirstItem);
			}
			indexOfGroupedItems.add(item);

			owner.reIndex(index);

			int last = index + size() - 1;

			if (!expanded) {
				updated = true;
				owner.fireDataChanged(index, index);
			} else {
				owner.fireDataInserted(index, index);
				sort();
				owner.fireDataChanged(index, last);
			}
		}
		
		/**
		 * Creates the List to be used for this group. This is a convinience method to
		 * provide centralized class management.
		 * Creation date: (11/30/2001 13:32:14)
		 * @return java.util.List
		 */
		private final List<Integer> createDefaultList() {
			return new ArrayList<Integer>();
		}
		
		/**
		 * Finds the index that corresponds to an item.
		 * Creation date: (11/30/2001 13:32:14)
		 * @return int
		 */
		public int find(Integer item) {
			if (indexOfGroupedItems == null) {
				if ((indexOfFirstItem != null) && (indexOfFirstItem.equals(item)))
					return absoluteIndex;
				else
					return -1;
			}

			int n = indexOfGroupedItems.size();
			boolean found = false;
			int i = 0;

			while ((i < n) && (!found)) {
				if (indexOfGroupedItems.get(i).equals(item))
					found = true;
				else
					i++;
			}
			if (!found)
				return -1;
			else
				return i + getAbsoluteIndex();
		}
		/**
		 * Returns the item at absolute position indicated by index. The return value
		 * is <code>groupedItems.get(index - absoluteIndex)</code>.
		 * Creation date: (11/30/2001 13:36:31)
		 * 
		 * @param index The position of the item to get
		 * @return  The integer in the index position
		 * 
		 */
		public final Integer getAbsolute(int index) {
			index -= absoluteIndex;
			if ((indexOfGroupedItems == null) && (index > 0)) {
				throw new IndexOutOfBoundsException();
			}
			return indexOfGroupedItems.get(index);
		}
		/**
		 * Returns this items absolute index.
		 * Creation date: (11/30/2001 13:47:35)
		 * @return int
		 */
		public final int getAbsoluteIndex() {
			return absoluteIndex;
		}

		/**
		 * Returns the item at absolute position indicated by index. The return value
		 * is <code>groupedItems.get(index - absoluteIndex)</code>.
		 * Creation date: (11/30/2001 13:36:31)
		 * @return  
		 * @param index int
		 */
		public final Integer getFirst() {
			return indexOfFirstItem;
		}
		/**
		 * Get an item at a specified index after a check on whether
		 * the list is not empty is performed.
		 * Creation date: (12/1/2001 12:30:05)
		 * @param index int
		 */
		public final Integer getRelative(int index) {
			if ((indexOfGroupedItems== null) && (index > 0)) {
				throw new IndexOutOfBoundsException();
			}
			return indexOfGroupedItems.get(index);
		}
		/**
		 * Returns if this group is expandable i.e. if it contains more than one element.
		 * Creation date: (11/30/2001 13:34:18)
		 * @return boolean
		 */
		public final boolean isExpandable() {
			return ((indexOfGroupedItems != null) && (indexOfGroupedItems.size() > 1));
		}
		/**
		 * Returns true if the group is currently expanded.
		 * Creation date: (11/30/2001 13:47:35)
		 * @return boolean
		 */
		public final boolean isExpanded() {
			return expanded;
		}
		/**
		 * Returns true if the group is collapsed and new data has been added to it.
		 * Always returns false if the group is expanded.
		 * Creation date: (11/30/2001 22:32:52)
		 * @return boolean
		 */
		public final boolean isUpdated() {
			return updated;
		}
		/**
		 * Removes the item at the absolute index from the group.
		 * Creation date: (11/30/2001 13:33:20)
		 * @param item  
		 */
		public final void removeAbsolute(int index) {

			int localIndex = index - absoluteIndex;

			if (indexOfGroupedItems == null) {
				if (localIndex > 0)
					//					return;
					throw new IndexOutOfBoundsException();
				else {
					owner.elements.remove(index);
					owner.reIndex(0);
					fireDataRemoved(index, index);
				}
				return;
			}

			indexOfGroupedItems.remove(localIndex);
			int newSize = indexOfGroupedItems.size();
			if (newSize > 0) {
				indexOfFirstItem = indexOfGroupedItems.get(0);
				if (newSize == 1)
					indexOfGroupedItems = null;
			} else {
				indexOfFirstItem = null;
			}
			owner.reIndex(0);
			fireDataRemoved(index, index);
		}
		/**
		 * Sets the absolute index for this group. This is the index of the first element
		 * in this group as it is referenced by the owner list.
		 * Creation date: (11/30/2001 13:47:35)
		 * @param newAbsoluteIndex int
		 */
		public final void setAbsoluteIndex(int newAbsoluteIndex) {
			absoluteIndex = newAbsoluteIndex;
		}
		/**
		 * Sets the new expanded state.
		 * Creation date: (11/30/2001 13:47:35)
		 * @param newExpanded boolean
		 */
		public final void setExpanded(boolean newExpanded) {
			if (newExpanded == expanded)
				return;

			int oldSize = size();

			expanded = newExpanded;

			owner.reIndex(absoluteIndex);

			if (expanded) {
				sort();
				owner.fireDataInserted(absoluteIndex + 1, absoluteIndex + size() - 1);
			} else {
				if (oldSize > 1)
					owner.fireDataRemoved(absoluteIndex + 1, absoluteIndex + oldSize - 1);
			}
			updated = false;
		}
		
		/**
		 * Sets the new updated state.
		 * Creation date: (11/30/2001 22:32:52)
		 * @param newUpdated boolean
		 */
		public final void setUpdated(boolean newUpdated) {
			updated = newUpdated;
		}
		
		/**
		 * Returns the number of items in this group. This value depends on the expanded
		 * state of the group. If the group is not expanded, it is treated as if it were
		 * a single element group. If it is expanded, it returns the actual number of
		 * elements.
		 * Creation date: (11/30/2001 13:42:32)
		 * @return int
		 */
		public final int size() {
			if (indexOfFirstItem == null)
				return 0;
			if ((!expanded) || (indexOfGroupedItems == null))
				return 1;
			return indexOfGroupedItems.size();
		}
		
		/**
		 * Sorts the group according to the comparator. Also adjusts first item if
		 * neccessary. After sorting the group, tha caller may need to adjust the
		 * reference to the first item.
		 * Creation date: (11/30/2001 13:28:13)
		 * @param comparator Comparator
		 */
		public final void sort() {
			if ((expanded) && (size() > 1)) {
				Comparator<LogEntryXML> comparator = GroupedList.this.getSortComparator();

				if (comparator != null) {
					//Collections.sort(indexOfGroupedItems, comparator);
					indexOfFirstItem = indexOfGroupedItems.get(0);
				}
			}
		}
	}

	/**
	 * GroupedList constructor comment.
	 * 
	 * @param allLogsCache The cache of the logs
	 */
	public GroupedList(LogFileCache allLogsCache) {
		super();
		cache=allLogsCache;
		updateQueue.start();
	}
	
	/**
	 * Adds object to the grouped list.
	 * <p>Creation date: (11/30/2001 14:10:56)
	 * @return int
	 * @param item  
	 */
	public final void add(Integer item) {
		updateQueue.addItem(item);
	}
	
	/**
	 * Replace an object in the list with another one
	 * 
	 * @param oldObj The object to replace
	 * @param newObj The new object
	 */
	public void replace(GroupedListItem oldItem, GroupedListItem newItem) {
		int pos = elements.indexOf(oldItem);
		if (pos>-1) {
            elements.set(pos,newItem);
        }
	}
	
	/**
	 * Adds a grouped list item at a certain position 
	 * specified by the index parameter.
	 * <p>
	 * Creation date: (12/23/2001 11:58:33)
	 * @param item com.cosylab.logging.client.GroupedListItem
	 * @param index int
	 */
	protected final void addAsNewGroup(GroupedListItem item, int index) {
		
		// index = -index - 1;
		
		item.setAbsoluteIndex(index);
		
		elements.add(index, item);
		reIndex(index);

		index = item.getAbsoluteIndex();
		
		fireDataInserted(index, index);
	}
	
	/**
	 * Adds an item to the grouped list at a proper position.
	 * <p>
	 * Creation date: (12/25/2001 11:36:48)
	 * @param item The index of the LogEntryXML to add
	 */
	protected final synchronized void addInternal(Integer item) {
		int index = 0;
	
		beginUpdate();
	
		GroupedListItem curr = new GroupedListItem(item);
	
		// Elements are not grouped	
		if (groupComparator.getComparator() == null) {
			if (sortComparator == null) {
				// Elements are not grouped or sorted
				// Add to the end of the list
				addAsNewGroup(curr, elements.size());
			} else {
				// Elements are not grouped but are sorted
				GroupedListComparator glc = new GroupedListComparator();
				glc.setComparator(sortComparator);
	
				// Find the proper position for the element using
				// the sort comparator
				index = Collections.binarySearch(elements, curr, glc);
	
				if (index >= 0) {
					addAsNewGroup(curr, index);
				} else {
					addAsNewGroup(curr, -index - 1);
				}
			}
			endUpdate();
			return;
		}
	
		// Elements are grouped.
		// We cannot assume anything about sort order, so we need to search
		// all top level groups for a match.
		int i = 0;
		int n = elements.size();
		while (i < n) {
			if (groupComparator.compare(curr, getGroup(i)) == 0) {
				// If we find it, we add element to it.
				addToExistingGroup(item, i);
				reIndex(0);
				endUpdate();
				return;
			}
			i++;
		}
	
		// Elements are not sorted
		if (sortComparator == null) {
		    addAsNewGroup(curr, n);
		    endUpdate();
		    return;
		}
		    
		// The element was not found, so we need to add it to
		// the position designated by sort comparator.
	
		GroupedListComparator glc = new GroupedListComparator();
		glc.setComparator(sortComparator);
	
		// Find the proper position for the element using
		// the sort comparator.
		index = Collections.binarySearch(elements, curr, glc);
	
		if (index >= 0) {
			addAsNewGroup(curr, index);
		} else {
			addAsNewGroup(curr, -index - 1);
		}
	
		//	index = Collections.binarySearch(elements, curr, sortComparator);
	
		//	if (index >= 0) {
		//		addToExistingGroup(item, index);
		//	} else {
	//    addAsNewGroup(curr, n);
		//	
	
		endUpdate();
	}
	
	/**
	 * Adds an item to an existing group from a specified position.
	 * Creation date: (12/23/2001 11:53:47)
	 * <p>
	 * @param item The index in the cache of the log to add
	 * @param index int
	 */
	protected final void addToExistingGroup(Integer item, int index) {
		GroupedListItem group = (GroupedListItem) (elements.get(index));
		group.add(item);
//		reIndex(index);
//		index = group.getAbsoluteIndex() + group.size() - 1;
//		fireDataInserted(index, index);
/*
		if (group.isExpanded()) {
			if (sortComparator != null) {
//				group.sort(sortComparator);
				index = group.getAbsoluteIndex();
				fireDataChanged(index, index + group.size() - 1);
			}
		} else {
			index = group.getAbsoluteIndex();
			fireDataChanged(index, index);
		}
*/
	}
	
	/**
	 * Clears the list.
	 * Creation date: (11/30/2001 18:10:57)
	 */
	public synchronized final void clear() {
		int oldSize = elements.size();
		if (oldSize > 0) {
			elements.clear();
			fireDataRemoved(0, oldSize - 1);
		}
	}
	
	/**
	 * Specifies which item is to be collapsed.
	 * Creation date: (12/1/2001 12:40:07)
	 * @param index int
	 */
	public final void collapse(int index) {
	/*
		int i = internalGet(index);
		if (i >= 0) {
			GroupedListItem group = getGroup(i);
			group.setExpanded(false);
		}
	*/
		updateQueue.collapseItem(index);
	
	}
	
	/**
	 * Creates the default List Object type for this list.
	 * Creation date: (11/30/2001 13:55:12)
	 * @return java.util.List
	 */
	protected final static List createDefaultList() {
		return new ArrayList();
	}
	
	/**
	 * Expands an item.
	 * Creation date: (11/30/2001 22:43:22)
	 * @param index int
	 */
	public final void expand(int index) {
	
	//    int i = internalGet(index);
	//    if (i >= 0) {
	//        GroupedListItem group = getGroup(i);
	//        group.setExpanded(true);
	//    }
	
		updateQueue.expandItem(index);
	
	}
	
	/**
	 * Performs an update.
	 * Creation date: (11/30/2001 22:16:40)
	 * @param start int
	 * @param end int
	 */
	protected final void fireDataChanged(int start, int end) {
		performCallback(GroupedListCallback.GL_UPDATED, start, end);
	}
	
	/**
	 * Performs an insert.
	 * Creation date: (11/30/2001 22:13:39)
	 * @param start int
	 * @param end int
	 */
	protected final void fireDataInserted(int start, int end) {
		performCallback(GroupedListCallback.GL_INSERT, start, end);
	}
	
	/**
	 * Performs a removal.
	 * Creation date: (12/1/2001 12:35:14)
	 * @param start int
	 * @param end int
	 */
	protected final void fireDataRemoved(int start, int end) {
		performCallback(GroupedListCallback.GL_DELETE, start, end);
	}
	
	/**
	 * Returns the item at absolute index. The actual item returned is based on the
	 * current group item visibility.
	 * Creation date: (11/30/2001 17:06:21)
	 * @return  
	 * @param index int
	 */
	public synchronized final LogEntryXML get(int index) {

		int i = internalGet(index);

		if (i >= 0)
			return cache.getLog(getGroup(i).getFirst());
		else {
			i = -i - 1;
			return cache.getLog(getGroup(i - 1).getAbsolute(index));
		}
	}
	
	/**
	 * Gets changeCallback.
	 * Creation date: (11/30/2001 22:12:47)
	 * @return com.cosylab.logging.client.GroupedListCallback
	 */
	public final GroupedListCallback getChangeCallback() {
		return changeCallback;
	}
	
	/**
	 * Returns group at a specified index.
	 * Creation date: (11/30/2001 17:30:20)
	 * @param index int
	 */
	protected final GroupedListItem getGroup(int index) {
		return elements.get(index);
	}
	
	/**
	 * Gets current group comparator.
	 * Creation date: (11/30/2001 16:44:54)
	 * @return com.cosylab.logging.client.GroupedListComparator
	 */
	public final Comparator getGroupComparator() {
		return groupComparator.getComparator();
	}
	
	/**
	 * Returns number of groups in this list.
	 * Creation date: (11/30/2001 17:32:34)
	 * @return int
	 */
	public final int getGroupCount() {
		return elements.size();
	}
	
	/**
	 * Gets current sort comparator.
	 * Creation date: (12/1/2001 12:50:41)
	 * @return java.util.Comparator
	 */
	public final Comparator<LogEntryXML> getSortComparator() {
		return sortComparator;
	}
	
	/**
	 * Internal helper routine.
	 * Creation date: (12/1/2001 12:26:01)
	 * @return int
	 * @param index int
	 */
	protected synchronized final int internalGet(int index) {
/*
		if ((cacheHits + cacheMisses) % 100 == 0) {
			System.out.println(
				"GroupedList: Hits: "
					+ cacheHits
					+ ", Misses: "
					+ cacheMisses
					+ ", H/M ratio = "
					+ (100 * cacheHits / (cacheHits + cacheMisses + 1)));
		}
*/
		if (index == lastAbsoluteIndex) {
//			cacheHits++;
			return lastGroupIndex;
		}
//		cacheMisses++;

		temp.setAbsoluteIndex(index);
		lastAbsoluteIndex = index;
		lastGroupIndex = Collections.binarySearch(elements, temp, indexComparator);

		return lastGroupIndex;
	}
	
	/**
	 * Invalidates cache by defining an integer value.
	 * Creation date: (12/1/2001 14:41:02)
	 */
	protected void invalidateIndexCache() {
		lastAbsoluteIndex = Integer.MIN_VALUE;
	}
	
	/**
	 * Returns true, if the group can be expanded.
	 * Creation date: (12/1/2001 14:34:53)
	 * @return boolean
	 * @param index int
	 */
	public boolean isExpandable(int index) {
		int i = internalGet(index);
		if (i >= 0)
			return getGroup(i).isExpandable();

		return false;
	}
	
	/**
	 * Returns current expanded state.
	 * Creation date: (12/1/2001 14:35:57)
	 * @param index int
	 */
	public boolean isExpanded(int index) {
		int i = internalGet(index);

		if (i >= 0)
			return getGroup(i).isExpanded();

		return false;
	}
	
	/**
	 * Returns if the index references a group or a specific item. 
	 * Creation date: (11/30/2001 18:15:05)
	 * @return boolean
	 * @param index int
	 */
	public final boolean isIndexVirtual(int index) {

		return (internalGet(index) < 0);
	}
	
	/**
	 * Returns if the group has new entries. This is only true if the group is
	 * not expanded.
	 * Creation date: (12/1/2001 14:37:09)
	 * @return boolean
	 */
	public boolean isUpdated(int index) {
		int i = internalGet(index);

		if (i >= 0)
			return getGroup(i).isUpdated();

		return false;
	}
	
	/**
	 * Checks whether the group is currently updating.
	 * Creation date: (11/30/2001 22:34:33)
	 * @return boolean
	 */
	public final boolean isUpdating() {
		return (updating > 0);
	}
	
	/**
	 * Insert the method's description here.
	 * Creation date: (11/30/2001 16:37:18)
	 * @param args java.lang.String[]
	 */
	public static void main(String[] args) {
/*
		GroupedList gl = new GroupedList();
		gl.setGroupComparator(new TestItemComparator());

		int i;

		int testCount = 2000;

		TestItem[] tis = new TestItem[testCount];
		for (i = 0; i < testCount; i++)
			tis[i] = new TestItem();

		long time = System.currentTimeMillis();

		for (i = 0; i < testCount; i++) {
			gl.add(tis[i]);
		}
		time = System.currentTimeMillis() - time;

		System.out.println("Time taken: " + time);

		GroupedListItem gli = (GroupedListItem) gl.elements.get(0);
		System.out.println(gli.size());
		System.out.println(gli.size());
		for (i = 0; i < gl.size(); i++) {
			System.out.println("Item " + i + " = " + gl.get(i));
		}

		for (i = 0; i < gl.elements.size(); i++) {
			gli = gl.getGroup(i);
			gli.setExpanded(true);
			gl.reIndex(i);
			System.out.println("Group " + i + ", index= " + gli.getAbsoluteIndex() + ", size = " + gli.size());

			//		System.out.println("Item "+i+" = "+gl.get(i));
		}
*/
	}
	
	/**
	 * Performs a callback
	 * Creation date: (11/30/2001 22:14:31)
	 * @param changeType int
	 * @param param1 int
	 * @param param2 int
	 */
	public final void performCallback(int changeType, int param1, int param2) {
		if (changeCallback != null)
			changeCallback.groupedListChanged(changeType, param1, param2);
	}
	
	/**
	 * Updates item indexes.
	 * Creation date: (11/30/2001 14:18:23)
	 * @param index int
	 */
	protected synchronized final void reIndex(int index) {

		beginUpdate();
		
		invalidateIndexCache();

		GroupedListItem group;
		int n = elements.size();
		int newIndex = 0;

		for (int i = 0; i < n; i++) {
			group = getGroup(i);
			group.setAbsoluteIndex(newIndex);
			newIndex += group.size();
		}
		endUpdate();

	}
	
	/**
	 * Resorts the items.
	 * Creation date: (12/1/2001 13:52:41)
	 */
	protected synchronized void reSort() {
		if (sortComparator != null) {
			LoggingClient mainWin = LoggingClient.getInstance();
			Cursor cursor = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR);
			Cursor originalCursor = mainWin.getCursor();
			mainWin.setCursor(cursor);
			
		    beginUpdate();
		    
			GroupedListComparator glc = new GroupedListComparator();
			glc.setComparator(sortComparator);
			
			Collections.sort(elements, glc);
				
		    fireDataChanged(0, elements.size()-1);
		    reIndex(0);
	
		    endUpdate();
		    
		    mainWin.setCursor(originalCursor);
		}
	}
	
	/**
	 * Sets change callback.
	 * Creation date: (11/30/2001 22:12:47)
	 * @param newChangeCallback com.cosylab.logging.client.GroupedListCallback
	 */
	public void setChangeCallback(GroupedListCallback newChangeCallback) {
		changeCallback = newChangeCallback;
	}
	
	/**
	 * Sets current group comparator.
	 * Creation date: (11/30/2001 16:44:54)
	 * @param GroupedListComparator
	 */
	public final void setGroupComparator(Comparator<LogEntryXML> newGroupComparator) {
		groupComparator.setComparator(newGroupComparator);
//		Collections.sort(elements, groupComparator);
	}
	
	/**
	 * Sets current sort comparator
	 * Creation date: (12/1/2001 12:50:41)
	 * @param newSortComparator java.util.Comparator
	 */
	public void setSortComparator(Comparator<LogEntryXML> newSortComparator) {
		sortComparator = newSortComparator;
		reSort();
	}

	/**
	 * Returns the number of currently visible elements in this list.
	 * Creation date: (11/30/2001 16:38:13)
	 * @return int
	 */
	public synchronized final int size() {
		int n = elements.size();
		if (n == 0)
			return 0;

		GroupedListItem lastItem = getGroup(n - 1);

		return lastItem.getAbsoluteIndex() + lastItem.size();
	}
	
	/**
	 * Comparator used when grouping the items in <code>GroupedList</code>.
	 * This comparator compares the items stores added to the list. If the
	 * comparator returns true, the items belong to the same group.
	 * Creation date: (11/30/2001 14:01:26)
	 * @author: Ales Pucelj (ales.pucelj@kgb.ijs.si)
	 */
	private final class GroupedListComparator implements Comparator<GroupedListItem> {
		private Comparator<LogEntryXML> comparator = null;

		private boolean sortAscending = true;

		public GroupedListComparator() {
		}

		public final int compare(GroupedListItem glist1, GroupedListItem glist2) {
			LogEntryXML log1 = cache.getLog(glist1.getFirst());
			LogEntryXML log2 = cache.getLog(glist2.getFirst());
			if (sortAscending)
				return comparator.compare(log1, log2);
			else
				return -comparator.compare(log1, log2);
		}
		public Comparator<LogEntryXML> getComparator() {
			return comparator;
		}
		public boolean isSortAscending() {
			return sortAscending;
		}
		public void setComparator(Comparator<LogEntryXML> newComparator) {
			comparator = newComparator;
		}
		public void setSortAscending(boolean newSortAscending) {
			sortAscending = newSortAscending;
		}
	}
	
	/**
	 * Wrapper for the index comparison. Used when accessing the list using index.
	 * Creation date: (11/30/2001 17:07:51)
	 * @author: Ales Pucelj (ales.pucelj@kgb.ijs.si)
	 */
	private final class GroupedListIndexComparator implements Comparator<GroupedListItem> {
		public GroupedListIndexComparator() {
			super();
		}
		public int compare(GroupedListItem glItem1, GroupedListItem glItem2) {
			return glItem1.getAbsoluteIndex() - glItem2.getAbsoluteIndex();
		}
	}
	
	/**
	 * Starts updating by setting the variable updating from 0 to 1.
	 * Creation date: (1/23/02 6:43:43 PM)
	 */
	public void beginUpdate() {
		updating++;	
	}
	
	/**
	 * Ends updating.
	 * Creation date: (1/23/02 6:45:02 PM)
	 */
	public void endUpdate() {
		if (updating > 0) 
			updating--;	
	}
	
	/**
	 * Looks for an item.
	 * Creation date: (1/23/02 6:55:14 PM)
	 * @return int
	 * @param item  
	 */
	public int find(Integer item) {
		int i = 0;
		int c = -1;
		int n = elements.size();
		boolean found = false;
		GroupedListItem group = null;
	
		while ((i<n) && (!found)) {
			group = getGroup(i);
			c = group.find(item);
			found = (c > -1);
			i++;	
		}
	
		return c;
	}
	
	/**
	 * Removes an item.
	 * Creation date: (1/23/02 6:47:32 PM)
	 * @param item  
	 */
	public void remove(Integer item) {
		updateQueue.removeItem(item);
	}
	
	/**
	 * Removes an item.
	 * Creation date: (1/23/02 6:47:32 PM)
	 * @param item  
	 */
	protected synchronized void removeInternal(Integer item) {
		beginUpdate();
	
		try {
			int i = 0;
			int c = -1;
			int n = elements.size();
			GroupedListItem group = null;
	
			while (i < n) {
				group = getGroup(i);
				c = group.find(item);
				if (c > -1) {
					group.removeAbsolute(c);
					i = n;
				}
				i++;
			}
		} catch (Exception e) {
		} finally {
			endUpdate();
		}
	}
}
