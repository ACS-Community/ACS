package com.cosylab.util;

import junit.framework.TestCase;

public class CircularArrayListTest extends TestCase {

	public void testClassicGrowth()
	{
		CircularArrayList<Integer> cal = new CircularArrayList<Integer>(10);
		
		assertTrue(cal.isEmpty());

		final int COUNT = 1000;
		for (int i = 0; i < COUNT; i++)
			cal.add(i);
		
		assertEquals(COUNT, cal.size());
		assertFalse(cal.isEmpty());

		for (int i = 0; i < COUNT; i++)
			assertEquals(i, ((Integer)cal.get(i)).intValue());
	}

	public void testCircular()
	{
		final int SIZE = 100;
		CircularArrayList<Integer> cal = new CircularArrayList<Integer>(SIZE);
		
		assertTrue(cal.isEmpty());

		for (int i = 0; i < SIZE - 3; i++)
			cal.add(i);

		for (int i = 0; i < SIZE - 5; i++)
			cal.remove(0);

		for (int i = 0; i < SIZE - 7; i++)
			cal.add(i);

		
		assertEquals(SIZE - 7 + 2, cal.size());

		assertEquals(SIZE - 5, ((Integer)cal.get(0)).intValue());
		assertEquals(SIZE - 4, ((Integer)cal.get(1)).intValue());
		
		for (int i = 0; i < SIZE - 7; i++)
			assertEquals(i, ((Integer)cal.get(i+2)).intValue());
	}

}
