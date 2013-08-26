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
* "@(#) $Id: MCmockBlobberImpl.cpp,v 1.1 2011/01/19 21:20:41 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2009-02-11  created
*/



static char *rcsId="@(#) $Id: MCmockBlobberImpl.cpp,v 1.1 2011/01/19 21:20:41 tstaig Exp $";


#include "MCmockBlobberImpl.h"


using namespace MC;


mockBlobberImpl::mockBlobberImpl(const ACE_CString& name,
			     maci::ContainerServices * containerServices)
	: acscomponent::ACSComponentImpl(name, containerServices)
{
    AUTO_TRACE("mockBlobberImpl::mockBlobberImpl");
}//mockBlobberImpl

mockBlobberImpl::~mockBlobberImpl()
{
    AUTO_TRACE("mockBlobberImpl::~mockBlobberImpl");

}//~mockBlobberImpl


MonitorArchiver::CollectorListStatus mockBlobberImpl::addCollector(const char* componentName)
{
	AUTO_TRACE("mockBlobberImpl::addCollector");
	return MonitorArchiver::ADDED;
}

MonitorArchiver::CollectorListStatus mockBlobberImpl::containsCollector(const char* componentName)
{
        AUTO_TRACE("mockBlobberImpl::containsCollector");
	return MonitorArchiver::KNOWN;
}
MonitorArchiver::CollectorListStatus mockBlobberImpl::removeCollector(const char* componentName)
{
        AUTO_TRACE("mockBlobberImpl::removeCollector");
	return MonitorArchiver::REMOVED;
}




/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(mockBlobberImpl)
/* ----------------------------------------------------------------*/


/*___oOo___*/
