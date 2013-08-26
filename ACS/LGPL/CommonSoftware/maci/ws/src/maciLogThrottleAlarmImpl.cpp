/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2012 
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
* "@(#) $Id: maciLogThrottleAlarmImpl.cpp,v 1.1 2012/02/21 10:48:27 acaproni Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2012-02-20  created 
*/

static char *rcsId="@(#) $Id: maciLogThrottleAlarmImpl.cpp,v 1.1 2012/02/21 10:48:27 acaproni Exp $"; 

#include <maciLogThrottleAlarmImpl.h>

using namespace maci;

LogThrottleAlarmImpl::LogThrottleAlarmImpl(const ContainerServices* containerSvcs, std::string name):
		LogThrottleAlarm("Logging",name,10),
		m_containerSvcs(containerSvcs)
{

}

void LogThrottleAlarmImpl::raiseLogThrottleAlarm()
{
	m_containerSvcs->getAlarmSource()->raiseAlarm(m_faultFamily,m_faultMember,m_faultCode);
}

void LogThrottleAlarmImpl::clearLogThrottleAlarm()
{
	m_containerSvcs->getAlarmSource()->clearAlarm(m_faultFamily,m_faultMember,m_faultCode);
}

/*___oOo___*/
