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
 
  
// $Author: msekoran $
// $Date: 2008/03/28 13:05:33 $
// $Log: RepeatGuardTest.java,v $
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
 * @author hmeuss
 *
 */
public class RepeatGuardTest extends TestCase {

	public RepeatGuardTest(String name) {
		super(name);
	}
	
	public void testRepeatGuard() throws InterruptedException {
		RepeatGuard rg = new RepeatGuard(1, TimeUnit.SECONDS, 10);

        System.out.println("******************RepeatGuard(1sec, 10)***************");
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
        rg.reset(0,TimeUnit.SECONDS,10);
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
        
        System.out.println("******************RepeatGuard.reset(1sec,10,OR)***************");
        rg.reset(1,TimeUnit.SECONDS,10,RepeatGuard.Logic.OR);
        countSucceeded=0;
        for(int i=0;i<25;i++) {
            if(rg.checkAndIncrement()) {
                countSucceeded++;
            }
        }
        assertEquals(3, countSucceeded);
            
        System.out.println("******************RepeatGuard(1sec,10)***************");
        rg.reset(1,TimeUnit.SECONDS,0);
        countSucceeded=0;
        for (int i=0;i<25;i++) {
            Thread.sleep(200);  // 200 millisecs;
            if(rg.checkAndIncrement()) {
                countSucceeded++;
            }
        }
        assertEquals(5, countSucceeded);
        
        System.out.println("******************RepeatGuard(1sec,10,AND)***************");
        rg.reset(1,TimeUnit.SECONDS,10,RepeatGuard.Logic.AND);
        countSucceeded=0;
        for (int i=0;i<25;i++) {
            Thread.sleep(200);  // 200 millisecs;
            if(rg.checkAndIncrement()) {
                countSucceeded++;
            }
        }
        assertEquals(3, countSucceeded);
        
        System.out.println("******************RepeatGuard(1sec,4,AND)***************");
        rg.reset(1,TimeUnit.SECONDS,4,RepeatGuard.Logic.AND);
        countSucceeded=0;
        for (int i=0;i<25;i++) {
            Thread.sleep(200);  // 200 millisecs;
            if(rg.checkAndIncrement()) {
                countSucceeded++;
            }
        }
        assertEquals(5, countSucceeded);
	}

}
