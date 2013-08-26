#ifndef baciROuLong_H_
#define baciROuLong_H_

/*******************************************************************
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
*
* "@(#) $Id: baciROuLong.h
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram   2003/07/10  created
*/

/** 
 * @file 
 * Header file for BACI Read-only Unsigned Long Long Property.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciMonitor_T.h>
#include <baciROcontImpl_T.h>

namespace baci {

/** @defgroup MonitoruLongTemplate MonitoruLong Class
 * The MonitoruLong class is a templated typedef so there is no actual inline doc generated for it per-se.
 *  @{
 * The MonitoruLong class is an implementation of the ACS::MonitoruLong IDL interface.
 */
typedef  Monitor<ACS_MONITOR(uLong, ACS::uLong)> MonitoruLong;
/** @} */

/** @defgroup ROuLongTemplate ROuLong Class
 * The ROuLong class is a templated typedef so there is no actual inline doc generated for it per-se.
 *  @{
 * The ROuLong class is an implementation of the ACS::ROuLong IDL interface.
 */
typedef  ROcontImpl<ACS_RO_T(uLong, ACS::uLong)> ROuLong;
/** @} */

 }; 

#endif

// ************************************************************************
