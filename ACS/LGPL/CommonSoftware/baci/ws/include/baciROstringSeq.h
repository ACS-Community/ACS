#ifndef _baciROstringSeq_H_
#define _baciROstringSeq_H_
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
* "@(#) $Id: baciROstringSeq.h,v 1.5 2008/10/01 02:26:45 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2004-06-17  created
*/

/** 
 * @file 
 * Header file for BACI Read-only String Sequence Property.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciMonitor_T.h>
#include <baciROSeqDiscImpl_T.h>

namespace baci
{
/** @defgroup MonitorstringSeqTemplate MonitorstringSeq Class
 * The MonitorstringSeq class is a templated typedef so there is no actual inline doc generated for it per-se.
 *  @{
 * The MonitorstringSeq class is an implementation of the ACS::MonitorstringSeq IDL interface.
 */
typedef  Monitor<const char*, CORBA::String_out, ACS::CBstringSeq, POA_ACS::Monitorstring, BACIValue::type_stringSeq> MonitorstringSeq;
/** @} */

template<>
ACS::Subscription_ptr ROdiscImpl<ACS::stringSeq*, ACS::CBstringSeq, ACS::stringSeqSeq, ACS::stringSeqSeq_out, ACS::Monitorstring, MonitorstringSeq, ACS::stringSeq, char*, ACE_CString, POA_ACS::ROstringSeq, ACS::Alarmstring, POA_ACS::CBstringSeq, const ACS::stringSeq&>::new_subscription_Alarm (
	ACS::Alarmstring *cb,
	const ACS::CBDescIn & desc
    ) ;

/** @defgroup ROstringSeqTemplate ROstringSeq Class
 * The ROstringSeq class is a templated typedef so there is no actual inline doc generated for it per-se.
 *  @{
 * The ROstringSeq class is an implementation of the ACS::ROstringSeq IDL interface.
 */
typedef  ROSeqDiscImpl<ACS::stringSeq*, ACS::CBstringSeq, ACS::stringSeqSeq, ACS::stringSeqSeq_out, ACS::Monitorstring, MonitorstringSeq, ACS::stringSeq, char*, ACE_CString, POA_ACS::ROstringSeq, ACS::Alarmstring, POA_ACS::CBstringSeq, const ACS::stringSeq&> ROstringSeq;
/** @} */

};//namespace baci

#endif /*!_H*/
