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
* "@(#) $Id: loggingStdoutHandler.cpp,v 1.3 2006/01/05 18:45:10 dfugate Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-04-04  created 
*/

#include "loggingStdoutHandler.h"
#include <stdio.h>

static char *rcsId="@(#) $Id: loggingStdoutHandler.cpp,v 1.3 2006/01/05 18:45:10 dfugate Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

namespace Logging {
    // ----------------------------------------------------------
    StdoutHandler::StdoutHandler(const std::string& soName) :
	sourceObjectName_m(soName)
    {
	int stdio;
	char *acsSTDIO = getenv("ACS_LOG_STDOUT");

	if (acsSTDIO && *acsSTDIO)
	    {
	    stdio = atoi(acsSTDIO);
	    //TODO - ensure stdio is a proper level
	    setLevel((Logging::BaseLog::Priority)stdio);
	    }
	else
	    {
	    setLevel(LM_INFO);
	    }
    }
    // ----------------------------------------------------------
    void
    StdoutHandler::log(const LogRecord& lr)
    {
	//TODO - do better than this!!!
	printf ("[%s] %s\n", lr.method.c_str(), lr.message.c_str());
    }
    // ----------------------------------------------------------
    std::string
    StdoutHandler::getName() const
    {
	return std::string("Stdout");
    }
    // ----------------------------------------------------------
};

/*___oOo___*/
