/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci.test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import com.cosylab.acs.maci.IntArray;

import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * JUnit Test for IntArray.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class IntArrayTest extends TestCase
{

	/**
	 */
	public IntArrayTest(String name)
	{
		super(name);
	}
	
	/**
	 */
	public static TestSuite suite()
	{
		return new TestSuite(IntArrayTest.class);
	}
	
	/**
	 */
	public void testAllocation()
	{
		IntArray ia = new IntArray();
		int i = 0;
		for (; i <= 10000; i++)
		{
			ia.add(i);
			assertEquals(i, ia.get(i));
		} 
		
		assertEquals(i, ia.size());
	}

	/**
	 */
	public void testRemoval()
	{
		IntArray ia = new IntArray();
		int i = 0;
		for (; i <= 12345; i++)
		{
			ia.add(i);
			assertEquals(i, ia.get(i));
		} 
		
		for (--i; i >= 0; i--)
		{
			ia.removeAt(i);
		} 

		assertEquals(0, ia.size());

	}

	/**
	 */
	public void testContainment()
	{
		IntArray ia = new IntArray();
		int i = 0;
		for (; i <= 12; i++)
		{
			ia.add(i);
			assertEquals(i, ia.get(i));
		} 

		// remove odd
		for (i = 0; i < ia.size();)
			if (ia.get(i)%2==1)
				ia.removeAt(i);
			else
				 i++;

		for (i = 0; i < ia.size(); i++)
			assertEquals(i*2, ia.get(i));

		System.out.println(ia.toString());

	}

	/**
	 */
	public void testAddRemoveContains()
	{
		IntArray ia = new IntArray();
		
		ia.add(1);
		assertTrue(ia.contains(1));
		
		ia.add(3);
		assertTrue(ia.contains(1));
		assertTrue(ia.contains(3));

		ia.add(5);
		assertTrue(ia.contains(1));
		assertTrue(ia.contains(3));
		assertTrue(ia.contains(5));
		
		ia.remove(3);
		assertTrue(!ia.contains(3));
		
		ia.remove(1);
		assertTrue(!ia.contains(1));
		
		ia.remove(5);
		assertTrue(!ia.contains(5));
	}

	/**
	 * Test serialization of the object.
	 * @param testObject	object whose serialization will be tested.
	 */
	private void testSerialization(Object testObject)
	{
		try
		{
			ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
			ObjectOutputStream objectOutputStream = new ObjectOutputStream(outputStream);
			
			// serialize object
			objectOutputStream.writeObject(testObject);
			
			ByteArrayInputStream inputStream = new ByteArrayInputStream(outputStream.toByteArray());
			ObjectInputStream objectInputStream = new ObjectInputStream(inputStream);
			
			// deserialize object
			Object obj = objectInputStream.readObject();

			// compare
			assertEquals(testObject, obj);
		} 
		catch (Exception ex)
		{
			fail(ex.toString());
		}
	}

	/**
	 */
	public void testIntArraySerialization()
	{
		IntArray ia = new IntArray();
		
		final int TEST_LEN = 12;
		
		for (int i = 0; i < TEST_LEN; i++)
			ia.add(i);

		testSerialization(ia);
	}

}
