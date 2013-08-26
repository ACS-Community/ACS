#ifndef _baciRWstring_H_
#define _baciRWstring_H_

/*
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2003 
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
*/

/** 
 * @file 
 * Header file for BACI Read-write String Property.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciMonitor_T.h>
#include <baciRWdiscImpl_T.h>


namespace baci {

/** @defgroup MonitorstringImplTemplate MonitorstringImpl Class
 * The MonitorstringImpl class is a templated typedef so there is no actual inline doc generated for it per-se.
 *  @{
 * The MonitorstringImpl class is an implementation of the ACS::Monitorstring IDL interface.
 */
typedef  Monitor<const char*, CORBA::String_out, ACS::CBstring, POA_ACS::Monitorstring, BACIValue::type_string> MonitorstringImpl;
/** @} */

/** @defgroup RWstringTemplate RWstring Class
 * The RWstring class is a templated typedef so there is no actual inline doc generated for it per-se.
 *  @{
 * The RWstring class is an implementation of the ACS::RWstring IDL interface.
 */
typedef  RWdiscImpl<char*, ACS::CBstring, ACS::stringSeq, ACS::stringSeq_out, ACS::Monitor, MonitorstringImpl, ACE_CString, char*, ACE_CString, POA_ACS::RWstring, const char*> RWstring;
/** @} */


 }; 

#endif  /* baciRWstring */

