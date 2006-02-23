/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) UNSPECIFIED - FILL IN, 2005 
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
* "@(#) $Id: testGenericLogger.cpp,v 1.1 2005/08/09 00:45:40 dfugate Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-03-31  created
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <iostream>

#include "loggingGenericLogger.h"
#include "logging.h"



static char *rcsId="@(#) $Id: testGenericLogger.cpp,v 1.1 2005/08/09 00:45:40 dfugate Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);


int main(int argc, char *argv[])
{
    Logging::Logger::setGlobalLogger(new Logging::GenericLogger("testLogger"));
    Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();

    printf("-------------------------------------------------------------------------\n");
    //simple test should print the message just once
    myLoggerSmartPtr->log(Logging::Logger::LM_INFO,
			  "Simple test...should see once.");
    {
    printf("-------------------------------------------------------------------------\n");
    printf("Should be a trace begin here...\n");
    Logging::LogTrace::LogTraceSmartPtr joe(new Logging::LogTrace(getLogger(),
								  "no method name",
								  __FILE__,
								  __LINE__));
    printf("...something in the middle...\n");
    } 
    printf("...and the trace ending should be done now.\n");
    printf("-------------------------------------------------------------------------\n");
    return 0;
}
