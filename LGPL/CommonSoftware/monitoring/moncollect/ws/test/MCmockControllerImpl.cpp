/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2009
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
* "@(#) $Id: MCmockControllerImpl.cpp,v 1.2 2011/03/14 14:08:15 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2009-02-11  created
*/



static char *rcsId="@(#) $Id: MCmockControllerImpl.cpp,v 1.2 2011/03/14 14:08:15 tstaig Exp $";


#include "MCmockControllerImpl.h"


using namespace MC;


mockControllerImpl::mockControllerImpl(const ACE_CString& name,
			     maci::ContainerServices * containerServices)
	: acscomponent::ACSComponentImpl(name, containerServices)
{
    AUTO_TRACE("mockControllerImpl::mockControllerImpl");
}//mockControllerImpl

mockControllerImpl::~mockControllerImpl()
{
    AUTO_TRACE("mockControllerImpl::~mockControllerImpl");

}//~mockControllerImpl


void mockControllerImpl::registerCollector(const char* componentName)
{
	AUTO_TRACE("mockControllerImpl::registerCollector");
}



void mockControllerImpl::deregisterCollector(const char* componentName)
{
	AUTO_TRACE("mockControllerImpl::deregisterCollector");
}

void mockControllerImpl::registerKnownCollectors(const char* componentName)
{
	AUTO_TRACE("mockControllerImpl::registerKnownCollectors");
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(mockControllerImpl)
/* ----------------------------------------------------------------*/


/*___oOo___*/
