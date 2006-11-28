/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci;

import java.io.Serializable;
import java.io.ObjectOutputStream;
import java.io.ObjectInputStream;
import java.io.IOException;

/**
 * A resizable, array of integer primitives.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */

public class IntArray implements Serializable, Cloneable
{

    /**
	 * Serial version UID. 
	 */
	private static final long serialVersionUID = -8356201427005323616L;

	/**
     * Default capacity for the array.
     */
    private static final int DEFAULT_CAPACITY = 10;
    
	/**
	 * Array data.
	 * @serial 
     */
    protected transient int[] data;

    /**
     * Index of the first empty bucket (also size).
     * @serial 
	 */
    private transient int pos;


    /**
     * Constrcuts a new <code>IntArray</code> instance with the default capacity.
     */
    public IntArray()
    {
        this(DEFAULT_CAPACITY);
    }
    
    /**
     * Constrcuts a new <code>IntArray</code> instance with the specified capacity.
     *
     * @param	capacity an <code>int</code> value
     */
    public IntArray(int capacity)
    {
        data = new int[capacity];
        pos = 0;
    }

    /**
     * Constrcuts a new <code>IntArray</code> instance whose
     * capacity is the greater of the length of <tt>values</tt> and
     * DEFAULT_CAPACITY and whose initial contents are the specified
     * values.
     *
     * @param values an <code>int[]</code> value
     */
    public IntArray(int[] values)
    {
        this(Math.max(values.length, DEFAULT_CAPACITY));
        add(values);
    }

    /**
     * Grow the internal array as needed to accomodate the specified
     * number of elements.  The size of the array doubles on each
     * resize unless <tt>capacity</tt> requires more than twice the
     * current capacity.
     *
     * @param capacity an <code>int</code> value
     */
    public void ensureCapacity(int capacity)
    {
        if (capacity > data.length)
        {
            int newCap = Math.max(data.length << 1, capacity);
            int[] tmp = new int[newCap];
            System.arraycopy(data, 0, tmp, 0, data.length);
            data = tmp;
        }
    }

    /**
     * Returns the number of values in the list.
     *
     * @return the number of values in the list.
     */
    public int size()
    {
        return pos;
    }

    /**
     * Tests whether this list contains any values.
     *
     * @return true if the list is empty.
     */
    public boolean isEmpty()
    {
        return pos == 0;
    }

    /**
     * Sheds any excess capacity above and beyond the current size of the list.
     */
    public void trimToSize()
    {
        if (data.length > size()) {
            int[] tmp = new int[size()];
            toArray(tmp, 0, tmp.length);
            data = tmp;
        }
    }

    /**
     * Adds <tt>val</tt> to the end of the list, growing as needed.
     *
     * @param val an <code>int</code> value
     */
    public void add(int val)
    {
        ensureCapacity(pos + 1);
        data[pos++] = val;
    }

    /**
     * Adds the values in the array <tt>vals</tt> to the end of the
     * list, in order.
     *
     * @param vals an <code>int[]</code> value
     */
    public void add(int[] vals)
    {
        add(vals, 0, vals.length);
    }

    /**
     * Adds a subset of the values in the array <tt>vals</tt> to the
     * end of the list, in order.
     *
     * @param vals an <code>int[]</code> value
     * @param offset the offset at which to start copying
     * @param length the number of values to copy.
     */
    public void add(int[] vals, int offset, int length)
    {
        ensureCapacity(pos + length);
        System.arraycopy(vals, offset, data, pos, length);
        pos += length;
    }

    /**
     * Inserts <tt>value</tt> into the list at <tt>offset</tt>.  All
     * values including and to the right of <tt>offset</tt> are shifted
     * to the right.
     *
     * @param offset an <code>int</code> value
     * @param value an <code>int</code> value
     */
    public void insert(int offset, int value)
    {
        if (offset == pos)
        {
            add(value);
            return;
        }
        
        ensureCapacity(pos + 1);
        
        // shift right
        System.arraycopy(data, offset, data, offset + 1, pos - offset);
        
        // insert
        data[offset] = value;
        pos++;
    }

    /**
     * Inserts the array of <tt>values</tt> into the list at
     * <tt>offset</tt>.  All values including and to the right of
     * <tt>offset</tt> are shifted to the right.
     *
     * @param offset an <code>int</code> value
     * @param values an <code>int[]</code> value
     */
    public void insert(int offset, int[] values)
    {
        insert(offset, values, 0, values.length);
    }

    /**
     * Inserts a slice of the array of <tt>values</tt> into the list
     * at <tt>offset</tt>.  All values including and to the right of
     * <tt>offset</tt> are shifted to the right.
     *
     * @param offset an <code>int</code> value
     * @param values an <code>int[]</code> value
     * @param valOffset the offset in the values array at which to
     * start copying.
     * @param len the number of values to copy from the values array
     */
    public void insert(int offset, int[] values, int valOffset, int len)
    {
        if (offset == pos)
        {
            add(values, valOffset, len);
            return;
        }

        ensureCapacity(pos + len);
        
        // shift right
        System.arraycopy(data, offset, data, offset + len, pos - offset);
        
        // insert
        System.arraycopy(values, valOffset, data, offset, len);
        
        pos += len;
    }

    /**
     * Returns the value at the specified offset.
     *
     * @param offset an <code>int</code> value
     * @return an <code>int</code> value
     */
    public int get(int offset)
    {
        if (offset >= pos)
            throw new ArrayIndexOutOfBoundsException(offset);

        return data[offset];
    }

    /**
     * Sets the value at the specified offset.
     *
     * @param offset an <code>int</code> value
     * @param val an <code>int</code> value
     */
    public void set(int offset, int val)
    {
        if (offset >= pos)
            throw new ArrayIndexOutOfBoundsException(offset);

        data[offset] = val;
    }

    /**
     * Replace the values in the list starting at <tt>offset</tt> with
     * the contents of the <tt>values</tt> array.
     *
     * @param offset the first offset to replace
     * @param values the source of the new values
     */
    public void set(int offset, int[] values)
    {
        set(offset, values, 0, values.length);
    }

    /**
     * Replace the values in the list starting at <tt>offset</tt> with
     * <tt>length</tt> values from the <tt>values</tt> array, starting
     * at valOffset.
     *
     * @param offset the first offset to replace
     * @param values the source of the new values
     * @param valOffset the first value to copy from the values array
     * @param length the number of values to copy
     */
    public void set(int offset, int[] values, int valOffset, int length)
    {
        if (offset < 0 || offset + length >= pos)
            throw new ArrayIndexOutOfBoundsException(offset);

        System.arraycopy(data, offset, values, valOffset, length);
    }

    /**
     * Flushes the internal state of the list, resetting the capacity
     * to the default.
     */
    public void clear()
    {
        clear(DEFAULT_CAPACITY);
    }

    /**
     * Flushes the internal state of the list, setting the capacity of
     * the empty list to <tt>capacity</tt>.
     *
     * @param capacity an <code>int</code> value
     */
    public void clear(int capacity)
    {
        data = new int[capacity];
        pos = 0;
    }

    /**
     * Removes the value at <tt>offset</tt> from the list.
     *
     * @param offset an <code>int</code> value
     * @return the value previously stored at offset.
     */
    public int removeAt(int offset)
    {
        int old = get(offset);
        removeAt(offset, 1);
        return old;
    }

    /**
     * Removes the element with value <tt>value</tt> from the list.
     *
     * @param	value	value of the element to be removed
     * @return ofset of the removed element, -1 on failure.
     */
    public int remove(int value)
    {
		for (int i = pos; i-- > 0;)
			if (data[i] == value)
			{
		        removeAt(i, 1);
		        return i;
			}

        return -1;
    }

    /**
     * Removes <tt>length</tt> values from the list, starting at
     * <tt>offset</tt>
     *
     * @param offset an <code>int</code> value
     * @param length an <code>int</code> value
     */
    public void removeAt(int offset, int length)
    {
        if (offset < 0 || offset >= pos)
            throw new ArrayIndexOutOfBoundsException(offset);

        if (offset == 0)
        {
            // data at the front
            System.arraycopy(data, length, data, 0, pos - length);
        }
        else if (pos - length == offset)
        {
            // no copy to make, decrementing pos "deletes" values at the end
        }
        else
        {
            // data in the middle
            System.arraycopy(data, offset + length,
                             data, offset, pos - (offset + length));
        }
        
        pos -= length;

    }

    /**
     * Returns a clone of this list.  Since this is a primitive
     * collection, this will be a deep clone.
     *
     * @return a deep clone of the list.
     */
    public Object clone()
    {
        IntArray clone = null;
        
        try
        {
            clone = (IntArray)super.clone();
            clone.data = (int[])data.clone();
            
        } catch (CloneNotSupportedException e)
        {
            // it's supported
        }
        
        return clone;
    }

    /**
     * Copies the contents of the list into a native array.
     *
     * @return an <code>int[]</code> value
     */
    public int[] toArray()
    {
        return toArray(0, pos);
    }

    /**
     * Copies a slice of the list into a native array.
     *
     * @param offset the offset at which to start copying
     * @param len the number of values to copy.
     * @return an <code>int[]</code> value
     */
    public int[] toArray(int offset, int len)
    {
        int[] rv = new int[len];
        toArray(rv, offset, len);
        return rv;
    }

    /**
     * Copies a slice of the list into a native array.
     *
     * @param dest the array to copy into.
     * @param offset the offset of the first value to copy
     * @param len the number of values to copy.
     */
    public void toArray(int[] dest, int offset, int len)
    {
        if (len == 0)
            return;

        if (offset < 0 || offset >= pos)
            throw new ArrayIndexOutOfBoundsException(offset);

        System.arraycopy(data, offset, dest, 0, len);
    }

    // comparing
    
    /**
     * Compares this list to another list, value by value.
     *
     * @param other the object to compare against
     * @return true if other is a TIntArrayList and has exactly the
     * same values.
     */
    public boolean equals(Object other)
    {
        if (other == this)
            return true;
       	
       	if (other instanceof IntArray)
       	{
            IntArray that = (IntArray)other;
            if (that.size() != this.size())
                return false;
            else
            {
                for (int i = pos; i-- > 0;)
                    if (this.data[i] != that.data[i])
                        return false;

                return true;
            }
       	}
       	
        return false;
    }


    /**
     * Returns the hash code value for this list. <p>
     *
     * This implementation uses exactly the code that is used to define the
     * list hash function in the documentation for the <tt>List.hashCode</tt>
     * method.
     *
     * @return the hash code value for this list.
     */
    public int hashCode()
    {
        int h = 0;
        for (int i = pos; i-- > 0;)
            h += data[i];

        return h;
    }


    /**
     * Searches the list for <tt>value</tt>
     *
     * @param value an <code>int</code> value
     * @return true if value is in the list.
     */
    public boolean contains(int value)
    {
        for (int i = pos; i-- > 0;)
            if (data[i] == value)
                return true;

        return false;
    }

    /**
     * Returns a String representation of the list, front to back.
     *
     * @return a <code>String</code> value
     */
    public String toString()
    {
        StringBuffer buf = new StringBuffer("{");

        for (int i = 0; i < pos; i++)
        {
			buf.append(data[i]);
		    if (i != pos-1) buf.append(", ");
        }
            
        buf.append("}");
        
        return buf.toString();
    }

    /**
     * Save the state of the <tt>ArrayList</tt> instance to a stream (that
     * is, serialize it).
     *
     * @serialData The length of the array backing the <tt>ArrayList</tt>
     *             instance is emitted (int), followed by all of its elements
     *             (each an <tt>Object</tt>) in the proper order.
     */
    private void writeObject(ObjectOutputStream stream)
        throws IOException
    {
        	
        stream.defaultWriteObject();

        // write out array length
        stream.writeInt(pos);

		// write out all elements in the proper order.
		for (int i = 0; i < pos; i++)
	            stream.writeInt(data[i]);
    }



    /**
     * Reconstitute the <tt>ArrayList</tt> instance from a stream (that is,
     * deserialize it).
     */
    private void readObject(ObjectInputStream stream)
        throws IOException, ClassNotFoundException
    {
        stream.defaultReadObject();

        // read in array length and allocate array
        int size = stream.readInt();
		data = new int[size];

		// fill array
		for (int i = 0; i < size; i++)
            data[i] = stream.readInt();
        
        // set size
        pos = size;
    }
    
}
