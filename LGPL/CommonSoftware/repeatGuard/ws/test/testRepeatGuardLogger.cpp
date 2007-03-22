/*******************************************************************************
 * ALMA - Atacama Large Millimiter Array
 * (c) Associated Universities Inc., 2007 
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
 *
 * "@(#) $Id: testRepeatGuardLogger.cpp,v 1.3 2007/03/22 10:56:55 gchiozzi Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * nbarriga  2007-01-30  created
 */

// Uncomment this if you are using the VLT environment
// #include "vltPort.h"


#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static char *rcsId="@(#) $Id: testRepeatGuardLogger.cpp,v 1.3 2007/03/22 10:56:55 gchiozzi Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <maciSimpleClient.h>

#include <ACSErrTypeCommon.h>
#include <repeatGuardLogTypeExample.h>

#include "RepeatGuardLogger.h"

/**
 *
 * Test program and example for guarded logs.
 *
 */
int main(int argc, char *argv[])
{
    maci::SimpleClient client;

    if (client.init(argc,argv) == 0)
	{
	return -1;
	}
    else
	{
	// Log into the manager before doing anything
	client.login();
	}

    /*****************************************************************/
    /*
     * This example guards a log
     * produced using any subclass of Logging::BaseLog
     * These are the normal logs
     *
     * All examples follow this same pattern
     */

    /*
     * 1. The first step is to instantiate the guard template.
     * In this case the guarding is not done for multiple
     * executions of this function, since it is the main().
     * 
     * In that case we would have to declare the guard as static.
     * The same applies to all following examples
     *
     * This approach also allows to use the same guard for multiple logs
     * (as long as they are of the same logger type)
     */
    Logging::RepeatGuardLogger<Logging::BaseLog> guardbl(1,10);

    /*
     * 2. We get the standard logger for this "object"
     *    and we make a simple normal log, just to see that it works. 
     */
    Logging::Logger::LoggerSmartPtr logger = getLogger();
    logger->log(Logging::Logger::LM_INFO, "Simple test.",
	       __FILE__,__LINE__,
	       "main");

    /*
     * 3. Finally we make a loop with the guarded log
     */
    for(int i=0;i<50;i++)
	{
	guardbl.log(logger, Logging::Logger::LM_INFO, 
		    "Simple test.",
		    __FILE__,__LINE__,
		    "main");
	}

    /*****************************************************************/
    /*
     * This example guards a type-safe log
     * produced using any subclass of Logging::TypeSafeLog
     */

    Logging::RepeatGuardLogger<Logging::TypeSafeLog> guard(1,10);

    repeatGuardLogTypeExample::simpleLog my_simpleLog(__FILE__,__LINE__,"main");
    my_simpleLog.log();



    for(int i=0;i<50;i++)
	{
	guard.log(my_simpleLog);
	}

    /*****************************************************************/
    /*
     * This example guards the logging of an exception
     * produced using any subclass of ACSErr::ACSbaseExImpl
     */


    Logging::RepeatGuardLogger<ACSErr::ACSbaseExImpl> guardex(1,10);

    ACSErrTypeCommon::GenericErrorExImpl displayMessageEx(
	__FILE__, __LINE__, "main");
    displayMessageEx.log();



    for(int i=0;i<50;i++)
	{
	guardex.log(displayMessageEx);
	}

    /*****************************************************************/
    client.logout();

    return 0;

}








