/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci.manager;

import java.io.Serializable;

/**
 * Data structure for maintaining a collection of elements that can be referred to using handles.
 * 
 * Stores data elements in an array-like structure. Individual elements are
 * addressed using handles, which can be though of as indices in the
 * array. It fulfills these requirements:
 *
 * <OL>
 * 	<LI><B>allocation</B> - a new element can be added and is assigned a unique handle.
 * 							Allocation is an O(1) operation.</LI>
 * 	<LI><B>deallocation</B> - an element can be removed from the registrar.
 * 							The handle is freed, and can be assigned to another element during allocation at
 * 							a later time. Deallocation is an O(1) operation.</LI>
 * 	<LI><B>retrieval</B> - a reference to the element can be retrieved for reading and writing.
 * 						   Retrieval is an O(1) operation.
 * 	<LI><B>enumeration</B> - elements stored can be traversed from first to last. 
 * 							 Costs of acquiring first, last, next and previous element of the array are O(1).
 * </OL>
 * 
 * This ADT is suitable for enumerating resources that are frequently allocated,
 * retrieved and deallocated without losing large amounts of memory and/or time.
 * 
 * This is essentially a doubly-linked list of elements, which are placed in an array.
 * Each element has assigned a handle (the index in the array), and handles of
 * the elements that come previous and next to it. There are actually two
 * chains of elements: the free element chain, which contains all elements
 * that have not yet been allocated, and the allocated element chain.
 * Free element chain is cyclic (passing the end resumes at the beginning),
 * and contains the element with the handle 0.
 * The allocated element chain is not cyclic: it starts with the element
 * that was first allocated, and ends with the one that was last allocated.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @author		Klemen Zagar (klemen.zagar@cosylab.com)
 * @version	@@VERSION@@
 */
public class HandleDataStore implements Serializable
{

	/**
	 * Serial version UID. 
	 */
	private static final long serialVersionUID = -6137572272422678754L;

	/** 
	 * Element of the <code>HandleDataStore</code>.
	 */
	private class Element implements Serializable
	{
		/**
		 * Serial version UID. 
		 */
		private static final long serialVersionUID = -6225830724337396211L;

		/**
		 * Previous element with the same <code>free</code>.
		 */
		int previous;
	
		/**
		 * Next element with the same <code>free</code>.
		 */
		int next;
	
		/**
		 * The actual data.
		 */
		Object data;
	
		/**
		 * <code>true</code> if the element is still unallocated.
		 */
		boolean free;
	};
	

	
	/**
	 * Capacity of this ADT.
	 */ 
	private int capacity;
	
	/**
	 * Default maximum capacity of this ADT.
	 */ 
	public static final int DEFAULT_MAX_CAPACITY = Integer.MAX_VALUE;

	/**
	 * Maximum capacity of this ADT.
	 */ 
	private int maxCapacity = DEFAULT_MAX_CAPACITY;

	/**
	 * Number of elements currently in the <code>HandleDataStore</code>.
	 */
	private int size;
	  
	/**
	 * Array of elements
	 */
	private Element[] elements;
	  
	/**
	 * Handle of the first non-free element.
	 */
	private int first;
	
	/**
	 * Handle of the last non-free element.
	 */
	private int last;
	  
	/**
	 * Amount by which to offset all handles.
	 */
	//private int offset;



	/**
	 * Constructs a <code>HandleDataStore</code> with zero offset and initial capacity of ten.
	 */
	public HandleDataStore()
	{
		this(10, DEFAULT_MAX_CAPACITY);
	}

	/**
	 * Constructs a <code>HandleDataStore</code> with zero offset.
	 * 
     * @param	initialCapacity   the initial capacity of the list.
     * @exception	IllegalArgumentException if the specified initial capacity is negative
	 */
	public HandleDataStore(int initialCapacity)
	{
		this(initialCapacity, DEFAULT_MAX_CAPACITY);
	}

	/**
	 * Constructs a <code>HandleDataStore</code>.
	 * 
	 * Creates a <code>HandleDataStore</code> and allocates enough space to hold <code>initialCapacity</code> elements.
	 * 
     * @param	initialCapacity   the initial capacity of the list.
     * @exception	IllegalArgumentException if the specified initial capacity is negative
	 */
	public HandleDataStore(int initialCapacity, int maxCapacity)
	{
        if (initialCapacity < 0)
            throw new IllegalArgumentException("Illegal capacity: "+ initialCapacity);

        if (maxCapacity < initialCapacity)
            throw new IllegalArgumentException("Illegal maxCapacity (less than capacity): "+ maxCapacity);

		this.maxCapacity = maxCapacity;

		setCapacity(initialCapacity);           
	}
	
    /**
     * Returns the number of elements in this ADT.
     *
     * @return  the number of elements in this ADT.
     */
    public int size()
    {
		return size;
    }

    /**
     * Returns the capacity of this ADT.
     *
     * Capacity is the maximum number of elements that this ADT can hold before resizing itself.
     * @return  the capacity of this ADT.
     */
    public int capacity()
    {
		return capacity - 1;
    }

    /**
     * Tests if this list has no elements.
     *
     * @return  <code>true</code> if this list has no elements, <code>false</code> otherwise.
     */
    public boolean isEmpty()
    {
		return size == 0;
    }

    /**
     * Returns the element with the specified handle.
     * 
     * NOTE: <code>handle</code> is not checked, if it is valid.
     *
     * @param	handle	handle of the element
     * @throws IndexOutOfBoundsException if handle is out of bounds.
     */
    public Object get(int handle)
    {
    	/*
        if (handle < 0 || handle >= capacity) 
            throw new IndexOutOfBoundsException(String.valueOf(handle) + " < " + offset + " or " +
            									 String.valueOf(handle) + " >= " + (offset+capacity));
        */

		return elements[handle].data;
    }
	
    /**
     * Sets the element with the specified handle.
     * 
     * NOTE: <code>handle</code> is not checked, if it is valid.
     *
     * @param	handle	handle of the element
     * @param	data	data to be set
     * @throws IndexOutOfBoundsException if handle is out of bounds.
     */
    public void set(int handle, Object data)
    {
    	if (data == null)
    		throw new IllegalArgumentException("data == null");
    	
    	/*
        if (handle < 0 || handle >= capacity) 
            throw new IndexOutOfBoundsException(String.valueOf(handle) + " < " + offset + " or " +
            									 String.valueOf(handle) + " >= " + (offset+capacity));
        */

		elements[handle].data = data;
    }

    /**
     * Sets the capacity of this <code>HandleDataStore</code> instance to
     * hold <code>newCapacity</code> elements;. 
     *
     * @param   newCapacity   the desired capacity.
     */
    public void setCapacity(int newCapacity)
    {

		// account for the extra element with handle 0
		newCapacity++;

		// It is quite difficult to reduce the size of the registrar, because
		// doing that could invalidate some of the outstanding handles. Instead
		// of inventing science, we chose to fail if shrinking is requested.
		if (newCapacity <= capacity || newCapacity > maxCapacity)
			return;

		// allocate memory for new elements
		Element[] newElements = new Element[newCapacity];
		
		// initialize the newly added elements so that they form a doubly linked list
		for(int i = capacity; i < newCapacity; i++)
		{
			newElements[i] = new Element();
			newElements[i].next = i+1;
			newElements[i].previous = i-1;
			newElements[i].free = true;
		}

		// copy existing elements
		for (int i = 0; i < capacity; i++)
			newElements[i] = elements[i];

		if(capacity != 0)
		{
			// join the newly added elements linked-list to the linked list of free elements
			newElements[newElements[0].previous].next = capacity;
			newElements[capacity].previous = newElements[0].previous;
			newElements[newCapacity-1].next = 0;
			newElements[0].previous = newCapacity-1;
		}
		else
		{
			newElements[0].previous = newCapacity-1;
			newElements[newCapacity-1].next = 0;
		}

		// save new data
		elements = newElements;
		capacity = newCapacity;
	}

	/**
	 * Return the handle of the first element in this ADT.
	 * 
	 * @return	the handle of the element that was the first one to get allocated and
	 * 			has not yet been deallocated.
	 * 			Useful for determining the starting point when enumerating the entire ADT.
	 * @see #next
	 * @see #previous
	 * @see #last
	 */
	public int first()
	{
		return first;
	}

	/**
	 * Return the handle of the last element in this ADT.
	 * 
	 * @return	the handle of the element that was the last one to get allocated and
	 * 			has not yet been deallocated.
	 * 			Useful for determining the starting point when enumerating the entire ADT.
	 * @see #next
	 * @see #previous
	 * @see #first
	 */
	public int last()
	{
		return last;
	}

	/**
	 * Return the handle of the next element in this ADT.
	 * 
	 * @param	handle	handle of the element whose sucessor's handle we wish to acquire.
	 * @return	the handle of the element that follows the one whose handle is <code>handle</code>.
	 * 			If <code>handle</code> is the last element, <code>0</code> is returned.
	 */
	public int next(int handle)
	{
		return elements[handle].next;
	}

	/**
	 * Return the handle of the previous element in this ADT.
	 * 
	 * @param	handle	handle of the element whose predecessor's handle we wish to acquire.
	 * @return	the handle of the element that precedes the one whose handle is <code>handle</code>.
	 * 			If <code>handle</code> is the first element, <code>0</code> is returned.
	 */
	public int previous(int handle)
	{
		return elements[handle].previous;
	}


	/**
	 * Determines whether a given handle is allocated.
	 * 
	 * @param	handle	the handle in question.
	 * @return	<code>true</code> if an element with handle <code>handle</code> already
	 * 			exists in this ADT, <code>false</code> otherwise.
	 */
	public boolean isAllocated(int handle)
	{
        if (handle <= 0 || handle >= capacity) 
        	return false;
       	else
			return !elements[handle].free;
	}

	/**
	 * Allocate an element in this <code>HandleDataStore</code>.
	 * 
	 * @return	newly allocated handle if allocation was successful, otherwise <code>0</code>
	 * @see	#deallocate
	 */
	public int allocate()
	{
		return allocate(elements[0].next, false);
	}
	
	/**
	 * Allocate an element in this <code>HandleDataStore</code>.
	 * 
	 * @param	handle	hanlde be allocated
	 * @return	newly allocated handle if allocation was successful, otherwise <code>0</code>
	 * @see	#deallocate
	 */
	public int allocate(int handle)
	{
		return allocate(handle, false);
	}

	/**
	 * Preallocate an element in this <code>HandleDataStore</code>.
	 * 
	 * @return	newly allocated handle if allocation was successful, otherwise <code>0</code>
	 * @see	#allocate
	 * @see	#deallocate
	 */
	public int preallocate()
	{
		return allocate(elements[0].next, true);
	}

	/**
	 * Allocate an element with given handle in this <code>HandleDataStore</code>.
	 * 
	 * Assures that this ADT is capable of holding yet another element
	 * and returns a handle to it.
	 * 
	 * @param	handle	handle to be allocated
	 * @param	preallocate	if <code>true</code> element is not really allocated (only reserved,
	 * 			listing through the ADT will skip preallocated elements), to completely allocate 
	 * 			preallocated elements use <code>ackAllocation(handle)</code> or canceled by
	 * 			<code>deallocate(handle, true)</code> method.
	 * @return	newly allocated handle if allocation was successful, otherwise <code>0</code>
	 * @see	#deallocate
	 */
	public int allocate(int handle, boolean preallocate)
	{
		// if the capacity is exceeded, double the capacity of this ADT
		if(handle == 0)
		{
			int newCapacity = Math.min(2*(capacity-1), maxCapacity-1);
			setCapacity(newCapacity);
			handle = elements[0].next;
		}
		else if (handle >= capacity && handle < maxCapacity)
		{
			setCapacity(handle+1);
		}

		// if handle isn't free, bail out
		if (handle == 0 || handle >= capacity || !elements[handle].free)
			return 0;

		// remove element from the free element chain
		elements[elements[handle].previous].next = elements[handle].next;
		elements[elements[handle].next].previous = elements[handle].previous;

		if (!preallocate)
			ackAllocation(handle);

		// mark it as allocated.
		elements[handle].free = false;

		// increase size
		size++;

		return handle;
	}

	/**
	 * Completes allocation of handle, to be used to completely allocate handle after preallocation.
	 * 
	 * @param	handle 	handle to be completely allocated
	 */
	public void ackAllocation(int handle)
	{
		// if this is the first element to get allocated, remember it
		if (first == 0)
			first = handle;

		// add newly allocated element to the end of the allocated element chain
		elements[handle].next = 0;
		elements[handle].previous = last;

		// remeber last
		if (last != 0)
			elements[last].next = handle;
		last = handle;
	}

	/**
	 * Deallocate an element with the given handle.
	 * 
	 * The element and its corresponding handle can be reused at a later call to <code>allocate</code>.
	 * 
	 * @param	handle	the handle of the element to deallocate.
	 * @see	#allocate
	 */
	public void deallocate(int handle)
	{
		deallocate(handle, false);
	}

	/**
	 * Deallocate an element with the given handle.
	 * 
	 * The element and its corresponding handle can be reused at a later call to <code>allocate</code>.
	 * 
	 * @param	handle	the handle of the element to deallocate.
	 * @param	depreallocate	has to be <code>true</code>, if handle was only preallocated
	 * @see	#allocate
	 */
	public void deallocate(int handle, boolean depreallocate)
	{
		// check if already free
		if (elements[handle].free)
			return;

		if (!depreallocate)
		{
			if (elements[handle].previous != 0)
				elements[elements[handle].previous].next = elements[handle].next;
			else
				first = elements[handle].next;
			
			if(elements[handle].next != 0)
				elements[elements[handle].next].previous = elements[handle].previous;
			else
				last = elements[handle].previous;
		}		

		// mark as free
		elements[handle].free = true;
		
		elements[elements[0].previous].next = handle;
		elements[handle].previous = elements[0].previous;
		
		elements[handle].next = 0;
		elements[0].previous = handle;
		
		// free pointer, to that GC can do its work
		elements[handle].data = null;

		// decrease size
		size--;
	}

	/**
	 * Returns a single-line rendition of this instance into text.
	 * 
	 * @return internal state of this instance
	 */
	public String toString()
	{
		StringBuffer sbuff = new StringBuffer();
		sbuff.append("HandleDataStore = { ");
		sbuff.append("size = '");
		sbuff.append(size);
		sbuff.append("', capacity = '");
		sbuff.append(capacity);
		sbuff.append("', elements = { ");
		int h = first();
		while (h != 0)
		{
			sbuff.append(get(h));
		
			h = next(h);
		
			if (h != 0)
				sbuff.append(", ");
		}
		sbuff.append(" }}");
		return new String(sbuff);
	}

}
