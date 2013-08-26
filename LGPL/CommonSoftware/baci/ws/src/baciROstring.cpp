/*******************************************************************************
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
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: baciROstring.cpp,v 1.100 2008/10/01 02:26:45 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram    2003/02/13  change to use templates
* msekoran  2001/12/28  created
*/


#include "baciROstring.h"
#include "baciAlarm_T.i"
#include "baciAlarmSystemMonitorDisc_T.i"
#include "baciROdiscImpl_T.i"
#include "baciMonitor_T.i"

using namespace baci;

template class Monitor<const char*, CORBA::String_out, ACS::CBstring, POA_ACS::Monitorstring, BACIValue::type_string>;

template<>
ACS::Subscription_ptr ROdiscImpl<char*, ACS::CBstring, ACS::stringSeq, ACS::stringSeq_out, ACS::Monitor, MonitorstringImpl, ACE_CString, char*, ACE_CString, POA_ACS::ROstring, ACS::Alarmstring, POA_ACS::CBstring, const char*>::new_subscription_Alarm (ACS::Alarmstring *cb,
					const ACS::CBDescIn & desc
					)
{
    ACE_UNUSED_ARG(cb);
    ACE_UNUSED_ARG(desc);
    

    ACS_LOG(LM_RUNTIME_CONTEXT, "ROstring::new_subscription_Alarm",
		  (LM_ERROR, "Not implemented"));
    return ACS::Subscription::_nil();
}

template class ROdiscImpl<char*, ACS::CBstring, ACS::stringSeq, ACS::stringSeq_out, ACS::Monitor, MonitorstringImpl, ACE_CString, char*, ACE_CString, POA_ACS::ROstring, ACS::Alarmstring, POA_ACS::CBstring, const char*>;






