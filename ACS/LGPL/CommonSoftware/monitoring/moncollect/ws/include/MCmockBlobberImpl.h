#ifndef MCmockbloberimpl_H
#define MCmockbloberimpl_H
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
* "@(#) $Id: MCmockBlobberImpl.h,v 1.1 2011/01/19 21:20:41 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2009-02-11  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <MonitorArchiverIFS.h>
#include <CollectorListStatusS.h>
#include <acscomponentImpl.h>

namespace MC
{

/**
 * It is just fake/mock implementation of blobber that is used for test purpose.
 */
class mockBlobberImpl :
public acscomponent::ACSComponentImpl,
public POA_MonitorArchiver::Blobber
{
public:
	mockBlobberImpl(const ACE_CString& name,
			maci::ContainerServices * containerServices);

	~mockBlobberImpl();

	MonitorArchiver::CollectorListStatus  addCollector(const char* inComponentName);

	MonitorArchiver::CollectorListStatus  containsCollector(const char* inComponentName);

	MonitorArchiver::CollectorListStatus  removeCollector(const char* inComponentName);

};//class mockBlobberImpl

};//namespace MC


#endif /*!_H*/
