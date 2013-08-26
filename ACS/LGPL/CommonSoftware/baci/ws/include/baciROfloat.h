#ifndef baciROfloat_H
#define baciROfloat_H

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
* "@(#) $Id: baciROfloat.h,v 1.3 2006/09/01 02:20:54 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* oat       2003/01/21  added baciMonitor_T (templates)
* bjeram    2002/02/25  added support for DevIO 
* msekoran  2001/03/09  modified
*/

/** 
 * @file 
 * Header file for BACI Read-only Float Property.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciMonitor_T.h>
#include <baciROcontImpl_T.h>

namespace baci {

/** @defgroup MonitorfloatTemplate Monitorfloat Class
 * The Monitorfloat class is a templated typedef so there is no actual inline doc generated for it per-se.
 *  @{
 * The Monitorfloat class is an implementation of the ACS::Monitorfloat IDL interface.
 */
typedef  Monitor<ACS_MONITOR(float, CORBA::Float)> Monitorfloat;
/** @} */

/** @defgroup ROfloatTemplate ROfloat Class
 * The ROfloat class is a templated typedef so there is no actual inline doc generated for it per-se.
 *  @{
 * The ROfloat class is an implementation of the ACS::ROfloat IDL interface.
 */
typedef  ROcontImpl<ACS_RO_T(float, CORBA::Float)> ROfloat;
/** @} */


 }; 

#endif

