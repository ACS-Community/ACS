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
// $Log: RepeatGuardLoggerTest.java,v $
// Revision 1.2  2008/03/28 13:05:33  msekoran
// Java code cleanup.
//
// Revision 1.1  2007/07/13 07:51:45  cparedes
// Adding the rest of the files for repeatGuardLogger
//
// Revision 1.1  2007/07/11 07:54:00  hmeuss
// Added Java implementation, but for some reason TAT does not work for the test here. Needs repair!
// 
 
package alma.acs.logging;

import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

import junit.framework.TestCase;

/**
 * @author cparedes 
 *
 */
public class RepeatGuardLoggerTest extends TestCase {

	public RepeatGuardLoggerTest(String name) {
		super(name);
	}
	
	/* simple test only, RepeatGuard is throughly tested in RepeatGuardTest */
	public void testRepeatGuardLogger() throws InterruptedException {
	    Logger logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("RepeatGuardLoggerTest", true);
	    
	    RepeatGuardLogger guardbl = new RepeatGuardLogger(1, TimeUnit.SECONDS, 10);
        
        logger.log(Level.INFO,"Simple test.");

        for(int i=0;i<50;i++)
        {
	        guardbl.log(logger, Level.INFO, "Log A without incrementing");
	        guardbl.log(logger, Level.INFO, "Log B without incrementing");
	        guardbl.logAndIncrement(logger, Level.INFO, "Log C with incrementing");
	    }
	}

}
