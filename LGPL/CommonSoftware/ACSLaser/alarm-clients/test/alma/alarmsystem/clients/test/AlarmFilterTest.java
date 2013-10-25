/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2013 
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
package alma.alarmsystem.clients.test;

import junit.framework.TestCase;
import alma.alarmsystem.clients.alarm.AlarmFilter;

/**
 * Test if the filtering of alarms by their triplets work.
 * 
 * @see AlarmFilter
 */
public class AlarmFilterTest  extends TestCase {

	@Override
	protected void setUp() throws Exception {
		
		super.setUp();
	}
	
	@Override
	protected void tearDown() throws Exception {
		super.tearDown();
	}
	
	/**
	 * It is not possible to instantiate a filter without constraints
	 */
	public void testInvalidInstantiation() throws Exception {
		try {
			AlarmFilter filter = new AlarmFilter(null, null, null, null);
			System.out.println("THis line must not be in the output");
		} catch(Throwable t) {
			// This is ok
		}
		
		try {
			AlarmFilter filter = new AlarmFilter(null, null, 10, 5);
			System.out.println("Neither this line must be in the output");
		} catch(Throwable t) {
			// This is ok
		}
	}
	
	public void testFaultFamilyFiltering() throws Exception {
		AlarmFilter filter = new AlarmFilter("[a-z]+", null, null, null);
		assertNotNull("Invalid null filter", filter);
		
		// These all pass
		assertTrue(filter.matches("test", "Whatever", 1));
		assertTrue(filter.matches("alarm", "Whatever", 1));
		assertTrue(filter.matches("onealarm", "99", 0));
		
		// These must be rejected
		assertFalse(filter.matches("Test", "Whatever", 1));
		assertFalse(filter.matches("test2", "alone", 123));
		assertFalse(filter.matches("No alarm", "A99G", 0));
	}
	
	public void testFaultMemberFiltering() throws Exception {
		AlarmFilter filter = new AlarmFilter(null,"[a-z]+", null, null);
		assertNotNull("Invalid null filter", filter);
		
		// These all pass
		assertTrue(filter.matches("FF", "member", 1));
		assertTrue(filter.matches("Alarm06", "m", 156));
		assertTrue(filter.matches("1FF6 h", "test", 3));
		
		// These must be rejected
		assertFalse(filter.matches("Test", "Whatever", 1));
		assertFalse(filter.matches("test2", "alarm one", 123));
		assertFalse(filter.matches("No alarm 1", "A99G", 0));
		
	}
	
	public void testFaultCodeFiltering() throws Exception {
		// Test the min only
		AlarmFilter filter = new AlarmFilter(null,null, 5, null);
		assertNotNull("Invalid null filter", filter);
		
		// These pass
		assertTrue(filter.matches("FF0", "member", 5));
		assertTrue(filter.matches("FF ", "mem ", 6));
		assertTrue(filter.matches("FF2", "memb1r", 1254));
		
		// These do not pass
		assertFalse(filter.matches("FF", "mMMaaa", 4));
		assertFalse(filter.matches("FF", "member", 0));
		
		// Test the max only
		filter = new AlarmFilter(null,null, null, 10);
		assertNotNull("Invalid null filter", filter);
		
		// These pass
		assertTrue(filter.matches("FF0", "member", 10));
		assertTrue(filter.matches("FF ", "mem ", 6));
		assertTrue(filter.matches("FF2", "memb1r", 0));
		
		// These do not pass
		assertFalse(filter.matches("FF", "mMMaaa", 40));
		assertFalse(filter.matches("FF", "member", 11));
		
		// Test the [min,max] only
		filter = new AlarmFilter(null,null, 21, 100);
		assertNotNull("Invalid null filter", filter);
		
		// These pass
		assertTrue(filter.matches("FF0", "member", 100));
		assertTrue(filter.matches("FF ", "mem ", 21));
		assertTrue(filter.matches("FF2", "memb1r", 50));
		
		// These do not pass
		assertFalse(filter.matches("FF", "mMMaaa", 0));
		assertFalse(filter.matches("FF", "member", 200));
		assertFalse(filter.matches("FF", "mMMaaa", 20));
		assertFalse(filter.matches("FF", "member", 101));
		
	}
	
	/**
	 * Test a complete filtering
	 */
	public void testFiltering() throws Exception {
		// Test the min only
		AlarmFilter filter = new AlarmFilter("[A-Z]+","[a-z]+[0-9]+", 5, 10);
		assertNotNull("Invalid null filter", filter);
		
		// These pass
		assertTrue(filter.matches("FF", "member1", 7));
		assertTrue(filter.matches("FAMILY", "member2", 5));
		assertTrue(filter.matches("THEFAMILY", "alarm982", 10));
		
		// These do not pass
		assertFalse(filter.matches("F1", "member2", 0));
		assertFalse(filter.matches("FF", "member", 7));
		assertFalse(filter.matches("ALARM", "memberq2222", 122));
		assertFalse(filter.matches("FF", "2member2", 32));
		assertFalse(filter.matches("ALARM family", "member2", 1001));
	}
	
}
