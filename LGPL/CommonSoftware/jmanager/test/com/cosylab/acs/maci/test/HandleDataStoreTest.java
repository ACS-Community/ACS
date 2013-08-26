/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci.test;

import com.cosylab.acs.maci.manager.HandleDataStore;

import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * JUnit Test for HandleDataStore.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class HandleDataStoreTest extends TestCase
{

	/**
	 */
	public HandleDataStoreTest(String name)
	{
		super(name);
	}
	
	/**
	 */
	public static TestSuite suite()
	{
		return new TestSuite(HandleDataStoreTest.class);
	}
	
	/**
	 */
	public void testDefaultAllocation()
	{
		HandleDataStore hds = new HandleDataStore();
		int i = 1;
		for (; i <= 10000; i++)
		{
			int handle = hds.allocate();
			assertEquals(i, handle);
		} 
		
		assertEquals(i-1, hds.size());
		assertEquals(i-1, hds.last());
		assertEquals(1, hds.first());

		int handle = hds.first();
		assertTrue(hds.isAllocated(handle));
		for (i = 2; i <= 10000; i++)
		{
			assertEquals(i, handle=hds.next(handle));
			assertTrue(hds.isAllocated(handle));
		} 
		assertEquals(0, handle=hds.next(handle));
		assertTrue(!hds.isAllocated(handle));

		handle = hds.last();
		assertTrue(hds.isAllocated(handle));
		for (i = 10000-1; i > 0; i--)
		{
			assertEquals(i, handle=hds.previous(handle));
			assertTrue(hds.isAllocated(handle));
		} 
		assertEquals(0, handle=hds.previous(handle));
		assertTrue(!hds.isAllocated(handle));
		
	}

	/**
	 */
	private void allocate(HandleDataStore hds, int handle)
	{
		assertTrue(hds.allocate(handle) != 0);
		assertTrue(hds.isAllocated(handle));
	}

	/**
	 */
	private void deallocate(HandleDataStore hds, int handle)
	{
		hds.deallocate(handle);
		assertTrue(!hds.isAllocated(handle));
	}

	/**
	 */
	public void testFreeAllocation()
	{
		HandleDataStore hds = new HandleDataStore();

		allocate(hds, 1);		
		allocate(hds, 3);		
		allocate(hds, 2);		
		allocate(hds, 10);		
		allocate(hds, 100);		
		allocate(hds, 1000);	

		deallocate(hds, 100);		

		deallocate(hds, 1);		
		deallocate(hds, 1000);		

		allocate(hds, 1003);	
		allocate(hds, 1000);	

		int h=hds.preallocate();
		assertEquals(0, hds.allocate(h));
		hds.deallocate(h, true);

		h = hds.allocate();
		hds.deallocate(h);

		int[] right = new int[] { 3, 2, 10, 1003, 1000 };
		h = hds.first(); int pos = 0;
		do {
			assertEquals(right[pos++], h);
			h = hds.next(h);
		} while (h != 0 && pos < right.length);
		
		assertEquals(0, h);
		assertEquals(right.length, pos);
		
	}
	
	/*
	public void testWrapAllocationDeallocation()
	{
		HandleDataStore hds = new HandleDataStore();
		long i = 1;
		for (; i <= 3*Integer.MAX_VALUE; i++)
		{
			int handle = hds.allocate();
			assertFalse(0 == handle);
			hds.deallocate(handle);
		} 
	}
	 */
	
}
