/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) Associated Universities Inc., 2004 
*
*This library is free software; you can redistribute it and/or
*modify it under the terms of the GNU Lesser General Public
*License as published by the Free Software Foundation; either
*version 2.1 of the License, or (at your option) any later version.
*
*This library is distributed in the hope that it will be useful,
*but WITHOUT ANY WARRANTY; without even the implied warranty of
*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*Lesser General Public License for more details.
*
*You should have received a copy of the GNU Lesser General Public
*License along with this library; if not, write to the Free Software
*Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: acsexmplCallbacksImpl.cpp,v 1.4 2008/10/01 04:30:47 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dave  2004-01-14  created 
*/

#include "acsexmplCallbacks.h"
#include <logging.h>

//------------------------------------------------------------------
void
MyAlarmdouble::alarm_raised (CORBA::Double value,
			     const ACSErr::Completion &c,
			     const ACS::CBDescOut &desc)
{
    ACS_SHORT_LOG ((LM_INFO, "(%s::Alarmdouble::alarm_raised) Value: %f", prop.c_str(), value));
}

void
MyAlarmdouble::alarm_cleared (CORBA::Double value,
			      const ACSErr::Completion &c,
			      const ACS::CBDescOut &desc)
{
    ACS_SHORT_LOG ((LM_INFO, "(%s::Alarmdouble::alarm_cleared) Value: %f", prop.c_str(), value));
}
//------------------------------------------------------------------
void 
MyCBdouble::working(CORBA::Double value, const ACSErr::Completion &c, const ACS::CBDescOut &desc)
{
    //To make the modular test deterministic, we have to ensure this message is only printed
    //to standard out exactly once!  This is accomplished by changing the priority of the 
    //logging statement after the working method has been invoked.
    if (m_count == 0)
	{
	ACS_SHORT_LOG ((LM_INFO, "(%s::CBdouble::working) Value: %f, desc.id_tag: %d", prop.c_str(), value, desc.id_tag));
	}
    else
	{
	ACS_SHORT_LOG ((LM_DEBUG, "(%s::CBdouble::working) Value: %f, desc.id_tag: %d", prop.c_str(), value, desc.id_tag));
	}
    m_count++;
}

void 
MyCBdouble::done (CORBA::Double value, const ACSErr::Completion &c, const ACS::CBDescOut &desc)
{
    ACS_SHORT_LOG ((LM_INFO, "(%s::CBdouble::done) Value: %f", prop.c_str(), value));
}
//------------------------------------------------------------------
void 
MyCBvoid::working (const ACSErr::Completion &c, const ACS::CBDescOut &desc) 
{ 
    ACS_SHORT_LOG ((LM_INFO, "(%s::CBvoid::working)", prop.c_str())); 
}

void 
MyCBvoid::done (const ACSErr::Completion &c, const ACS::CBDescOut &desc) 
{ 
    ACS_SHORT_LOG ((LM_INFO, "(%s::CBvoid::done)", prop.c_str()));
    ACS_SHORT_LOG ((LM_INFO, "Error code returned: %d", c.code));
}
//------------------------------------------------------------------

