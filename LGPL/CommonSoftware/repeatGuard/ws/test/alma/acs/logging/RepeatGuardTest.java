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
 *
 *    Created on May 25, 2007
 *
 */
 
  
// $Author: hsommer $
// $Date: 2008/12/11 10:23:27 $
// $Log: RepeatGuardTest.java,v $
// Revision 1.6  2008/12/11 10:23:27  hsommer
// Added some test cases and comments to the existing tests.
//
// Revision 1.5  2008/03/28 13:05:33  msekoran
// Java code cleanup.
//
// Revision 1.4  2007/07/23 10:03:47  nbarriga
// Default evaluation method is now OR, to be consistent with cpp implementation.
// Some minor bugs fixed(not reseting counters the first time, time drift).
// References in asserts fixed.
// Added filter to TesList.grep.
//
// Revision 1.3  2007/07/17 09:34:18  cparedes
// Added a little cleaning and always true for the first check
//
// Revision 1.2  2007/07/13 07:19:38  cparedes
// Adding the right reference file
//
// Revision 1.1  2007/07/11 07:54:00  hmeuss
// Added Java implementation, but for some reason TAT does not work for the test here. Needs repair!
// 
 
package alma.acs.logging;

import java.util.concurrent.TimeUnit;

import junit.framework.TestCase;

/**
 * 
 */
public class RepeatGuardTest extends TestCase {

	public RepeatGuardTest(String name) {
		super(name);
	}
	
	public void testRepeatGuard() throws InterruptedException {

		System.out.println("******************RepeatGuard(1sec, 10)***************");
		RepeatGuard rg = new RepeatGuard(1000, TimeUnit.SECONDS, 10);
		// here we expect only the counter to be relevant, as it does not take 1000 seconds to run the loop
		assertEquals(0, rg.counter());
		assertEquals(0, rg.counterAtLastExecution());
		int countSucceeded = 0;
		int total = 0;
		for (int i = 1; i <= 50; i++) {
			rg.increment();
			if (rg.check()) {
				total += rg.counterAtLastExecution();
				countSucceeded++;
			}
			assertEquals("counterAtLastExecution for i=" + i, (i < 11 ? 1 : 10), rg.counterAtLastExecution());
		}
		assertEquals(5, countSucceeded);
		total += rg.counter();
		assertEquals(50, total);

		System.out.println("******************RepeatGuard(0,10)***************");
		// here we expect only the counter to be relevant, as the time interval is 0 and therefore not considered.
		rg.reset(0, TimeUnit.SECONDS, 10);
		countSucceeded = 0;
		for (int i = 1; i <= 25; i++) {
			Thread.sleep(200); // 200 millisecs;
			if (rg.checkAndIncrement()) {
				countSucceeded++;
			}
		}
		assertEquals(3, countSucceeded); // at counts 1, 11, 21

		System.out.println("******************RepeatGuard(0,1)***************");
		// again only the counter
		rg.reset(0, TimeUnit.SECONDS, 1);
		countSucceeded = 0;
		for (int i = 1; i <= 25; i++) {
			Thread.sleep(200); // 200 millisecs;
			if (rg.checkAndIncrement()) {
				countSucceeded++;
			}
		}
		assertEquals(25, countSucceeded); // all

		System.out.println("******************RepeatGuard.reset()***************");
		// simple reset without changing the configuration
		rg.reset();
		countSucceeded = 0;
		for (int i = 1; i <= 25; i++) {
			Thread.sleep(200); // 200 millisecs;
			if (rg.checkAndIncrement()) {
				countSucceeded++;
			}
		}
		assertEquals(25, countSucceeded); // same as before

		System.out.println("******************RepeatGuard.reset(1sec,10,OR)***************");
		// again only the counter should matter as the loop takes less than 1 second. We test the explicit use of OR logic.
		rg.reset(1, TimeUnit.SECONDS, 10, RepeatGuard.Logic.OR);
		countSucceeded = 0;
		for (int i = 1; i <= 25; i++) {
			if (rg.checkAndIncrement()) {
				countSucceeded++;
			}
		}
		assertEquals(3, countSucceeded);

		System.out.println("******************RepeatGuard(1sec, 0)***************");
		// Here only the timer should matter because the counter is 0 and thus not considered.
		// Logic should be set to TIMER, and check() should then fire at multiples of 200 ms, 
		// regardless of the time spent in the loop between the calls to checkAndIncrement
		rg.reset(1, TimeUnit.SECONDS, 0);
		countSucceeded = 0;
		for (int i = 1; i <= 25; i++) {
			Thread.sleep(200); // 200 millisecs;
			if (rg.checkAndIncrement()) {
				assertEquals("execution for i = 1 + 5*n", 1, i%5);
				countSucceeded++;
			}
		}
		assertEquals(5, countSucceeded); // execution once a second, which means at tries 1, 6, 11, 16, 21 because of 200 ms sleep. 

		System.out.println("******************RepeatGuard(1sec,10,AND)***************");
		// Here we test the timer AND counter logic
		// Timer alone would fire for i = 1 + 5*n
		// Counter alone would fire for i = 1 + 10*n , which then determines the outcome of AND
		rg.reset(1, TimeUnit.SECONDS, 10, RepeatGuard.Logic.AND);
		countSucceeded = 0;
		for (int i = 1; i <= 25; i++) {
			Thread.sleep(200); // 200 millisecs;
			if (rg.checkAndIncrement()) {
				assertEquals("execution for i = 1 + 10*n", 1, i%10);
				countSucceeded++;
			}
		}
		assertEquals(3, countSucceeded); // 1, 11, 21

		System.out.println("******************RepeatGuard(1sec,4,AND)***************");
		// Here again we test the timer AND counter logic
		// Counter alone would fire for i = 1 + 4*n 
		// Timer alone would fire for i = 1 + 5*n , which then determines the outcome of AND
		rg.reset(1, TimeUnit.SECONDS, 4, RepeatGuard.Logic.AND);
		countSucceeded = 0;
		for (int i = 1; i <= 25; i++) {
			Thread.sleep(200); // 200 millisecs;
			if (rg.checkAndIncrement()) {
				countSucceeded++;
			}
		}
		assertEquals(5, countSucceeded); // 1, 6, 11, 16, 21
	}
	
	
	public void testErrorInput() {
		try {
			new RepeatGuard(0, TimeUnit.SECONDS, -1);
			fail("Expected IllegalArgumentException");
		} catch (IllegalArgumentException ex) {
			// good
		}
		RepeatGuard rg = new RepeatGuard(1000, TimeUnit.SECONDS, 10);
		try {
			rg.reset(-1, TimeUnit.MICROSECONDS, 0);
		} catch (IllegalArgumentException ex) {
			// good
		}

		// null time unit OK for counter-only setup
		new RepeatGuard(-1, null, 10);
		try {
			// with timer setup, unit must be given
			new RepeatGuard(1, null, 10);
			fail("Expected IllegalArgumentException for null timeUnit");
		} catch (IllegalArgumentException ex) {
			// good
		}
		
		try {
			// logic must always be given
			new RepeatGuard(1, TimeUnit.DAYS, 10, null);
			fail("Expected IllegalArgumentException for null 'logic'");
		} catch (IllegalArgumentException ex) {
			// good
		}
	}

//	public void testCounterTimerStartedByFirstCheck() {
//		RepeatGuard rg = new RepeatGuard(1000, TimeUnit.SECONDS, 10);
//		fail();//todo continue
//	}
}
