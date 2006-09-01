#ifndef baciROdoubleSeq_H
#define baciROdoubleSeq_H

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
* "@(#) $Id: baciROdoubleSeq.h,v 1.96 2006/09/01 02:20:54 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram    2003/02/20 removed everything
* msekoran  2001/02/10  created
*/

/** 
 * @file 
 * Header file for BACI Read-only Double Sequence Property.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciMonitor_T.h>
#include <baciROSeqContImpl_T.h>


namespace baci {

/** @defgroup MonitordoubleSeqTemplate MonitordoubleSeq Class
 * The MonitordoubleSeq class is a templated typedef so there is no actual inline doc generated for it per-se.
 *  @{
 * The MonitordoubleSeq class is an implementation of the ACS::MonitordoubleSeq IDL interface.
 */
typedef  Monitor<ACS_MONITOR_SEQ(double, CORBA::Double)> MonitordoubleSeq;
/** @} */

/** @defgroup ROdoubleSeqTemplate ROdoubleSeq Class
 * The ROdoubleSeq class is a templated typedef so there is no actual inline doc generated for it per-se.
 *  @{
 * The ROdoubleSeq class is an implementation of the ACS::ROdoubleSeq IDL interface.
 */
typedef  ROSeqContImpl<ACS_RO_SEQ_T(double, CORBA::Double)> ROdoubleSeq;
/** @} */

 }; 

#endif  /* baciROdoubleSeq */

