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
 
  
// $Author: nbarriga $
// $Date: 2007/07/23 10:03:47 $
// $Log: RepeatGuardTest.java,v $
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

import junit.framework.TestCase;

/**
 * @author hmeuss
 *
 */
public class RepeatGuardTest extends TestCase {

	public RepeatGuardTest(String name) {
		super(name);
	}
	
	public void testRepeatGuard() throws InterruptedException {
		RepeatGuard rg = new RepeatGuard(10000000,10);  // interval is 1 sec

        System.out.println("******************RepeatGuard(10000000,10)***************");
		int countSucceeded=0;
        int total = 0;
		for(int i=0;i<50;i++){
			rg.increment();
			if(rg.check()) {
                total+=rg.count();
				countSucceeded++;
			}
		}
        
        total+=rg.counter();
		assertEquals(50, total);
		assertEquals(5, countSucceeded);

        System.out.println("******************RepeatGuard(0,10)***************");
        rg.reset(0,10);
        countSucceeded=0;
        for (int i=0;i<25;i++) {
            Thread.sleep(200);  // 200 millisecs;
            if(rg.checkAndIncrement()) {
                countSucceeded++;
            }
        }
        assertEquals(3, countSucceeded);
		
        System.out.println("******************RepeatGuard.reset()***************");
        rg.reset();
		countSucceeded=0;
        for(int i=0;i<25;i++){
            Thread.sleep(200);  // 200 millisecs;
            if(rg.checkAndIncrement()) {
                countSucceeded++;
            }
        }
        assertEquals(3, countSucceeded);
        
        System.out.println("******************RepeatGuard.reset(10000000,10,RepeatGuard.OR)***************");
        rg.reset(10000000, 10, RepeatGuard.OR);
        countSucceeded=0;
        for(int i=0;i<25;i++) {
            if(rg.checkAndIncrement()) {
                countSucceeded++;
            }
        }
        assertEquals(3, countSucceeded);
            
        System.out.println("******************RepeatGuard(10000000,10)***************");
        rg.reset(10000000,0);
        countSucceeded=0;
        for (int i=0;i<25;i++) {
            Thread.sleep(200);  // 200 millisecs;
            if(rg.checkAndIncrement()) {
                countSucceeded++;
            }
        }
        assertEquals(5, countSucceeded);
        
        System.out.println("******************RepeatGuard(10000000,10,RepeatGuard.AND)***************");
        rg.reset(10000000,10,RepeatGuard.AND);
        countSucceeded=0;
        for (int i=0;i<25;i++) {
            Thread.sleep(200);  // 200 millisecs;
            if(rg.checkAndIncrement()) {
                countSucceeded++;
            }
        }
        assertEquals(3, countSucceeded);
        
        System.out.println("******************RepeatGuard(10000000,4,RepeatGuard.AND)***************");
        rg.reset(10000000,4,RepeatGuard.AND);
        countSucceeded=0;
        for (int i=0;i<25;i++) {
            Thread.sleep(200);  // 200 millisecs;
            if(rg.checkAndIncrement()) {
                countSucceeded++;
            }
        }
        assertEquals(5, countSucceeded);
	}

	/**
	 * @throws java.lang.Exception
	 */
	protected void setUp() throws Exception {
	}

	/**
	 * @throws java.lang.Exception
	 */
	protected void tearDown() throws Exception {
	}

}
