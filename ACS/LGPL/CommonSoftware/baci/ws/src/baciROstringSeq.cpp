/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2004 
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
* "@(#) $Id: baciROstringSeq.cpp,v 1.4 2008/10/01 02:26:45 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2004-06-18  created 
*/

#include "baciROstringSeq.h"
#include "baciAlarm_T.i"
#include "baciAlarmSystemMonitorSeqDisc_T.i"
#include "baciMonitor_T.i"
#include "baciROSeqDiscImpl_T.i"

// mapping IDL strings to C++ is very special case thus we have to exapand tempate list macro by hand
// and we have to specialize new_subscription_Alarm since there is bad alarm implementation for discete type. I hope will be better with new alarm system

//template class Monitor<const char*, CORBA::String_out, ACS::CBstringSeq, POA_ACS::Monitorstring, BACIValue::type_stringSeq>;

template class Monitor<const char*, CORBA::String_out, ACS::CBstringSeq, POA_ACS::Monitorstring, BACIValue::type_stringSeq>;

template<>
ACS::Subscription_ptr ROdiscImpl<ACS::stringSeq*, ACS::CBstringSeq, ACS::stringSeqSeq, ACS::stringSeqSeq_out, ACS::Monitorstring, MonitorstringSeq, ACS::stringSeq, char*, ACE_CString, POA_ACS::ROstringSeq, ACS::Alarmstring, POA_ACS::CBstringSeq, const ACS::stringSeq&>::
new_subscription_Alarm (ACS::Alarmstring *cb,
			const ACS::CBDescIn & desc
			) 
{
    ACE_UNUSED_ARG(cb);
    ACE_UNUSED_ARG(desc);   

    ACS_LOG(LM_RUNTIME_CONTEXT, "ROstringSeq::new_subscription_Alarm",
		  (LM_ERROR, "Not implemented"));
    return ACS::Subscription::_nil();
}

template class ROSeqDiscImpl<ACS::stringSeq*, ACS::CBstringSeq, ACS::stringSeqSeq, ACS::stringSeqSeq_out, ACS::Monitorstring, MonitorstringSeq, ACS::stringSeq, char*, ACE_CString, POA_ACS::ROstringSeq, ACS::Alarmstring, POA_ACS::CBstringSeq, const ACS::stringSeq&>;


/*___oOo___*/
