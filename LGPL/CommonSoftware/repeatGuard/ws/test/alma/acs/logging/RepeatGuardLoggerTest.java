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
 
  
// $Author: cparedes $
// $Date: 2007/07/13 07:51:45 $
// $Log: RepeatGuardLoggerTest.java,v $
// Revision 1.1  2007/07/13 07:51:45  cparedes
// Adding the rest of the files for repeatGuardLogger
//
// Revision 1.1  2007/07/11 07:54:00  hmeuss
// Added Java implementation, but for some reason TAT does not work for the test here. Needs repair!
// 
 
package alma.acs.logging;

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
	
	public void testRepeatGuardLogger() throws InterruptedException {
        Logger logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("RepeatGuardLoggerTest", true);

        RepeatGuardLogger guardbl = new RepeatGuardLogger(10000000,10);
        
        logger.log(Level.INFO,"Simple test.");

        for(int i=0;i<50;i++)
        {
	        guardbl.log(logger, Level.INFO, "Log A without incrementing");
	        guardbl.log(logger, Level.INFO, "Log B without incrementing");
	        guardbl.logAndIncrement(logger, Level.INFO, "Log C with incrementing");
	    }
        
       // RepeatGuardLogger guardbl = new RepeatGuardLogger(10000000,10);
/*

    Logging::RepeatGuardLogger<Logging::TypeSafeLog> guard(10000000,10);

    repeatGuardLogTypeExample::simpleLog my_simpleLog(__FILE__,__LINE__,"main");
    my_simpleLog.log();


    guard.log(my_simpleLog);

    for(int i=0;i<50;i++)
	{
	guard.logAndIncrement(my_simpleLog);
	}



    Logging::RepeatGuardLogger<ACSErr::ACSbaseExImpl> guardex(10000000,10);

    ACSErrTypeCommon::GenericErrorExImpl displayMessageEx(
	__FILE__, __LINE__, "main");
    displayMessageEx.log();


    guardex.log(displayMessageEx);

    for(int i=0;i<50;i++)
	{
	guardex.logAndIncrement(displayMessageEx);
	}

       */
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
