#ifndef baciTime_H
#define baciTime_H

/*******************************************************************
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
* "@(#) $Id: baciTime.h,v 1.97 2006/09/01 02:20:54 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2001/03/12  modified
*/

/** 
 * @file baciTime.h
 * Header file for BACI Time Stamps. This entire header is deprecated!
 * Use acsutilTimeStamp.h instead which includes exactly the same
 * functionality.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciExport.h>
#include <baciTypes.h>

namespace baci {

/**
 * Entire module deprecated. Use acsutilTimeStamp.h instead.
 */

/**
 * Deprecated. Use acsutilTimeStamp.h instead.
 */

#if defined (ACE_LACKS_LONGLONG_T)
  static const CORBA::ULongLong UTCtoUNIXTimeBaseOffset (0xD8539C80, 2);
  // (Lower 32 bits of the offset in hex, Upper 32 bits of the offset in hex)
#else
  static const CORBA::ULongLong UTCtoUNIXTimeBaseOffset = ACE_UINT64_LITERAL(0x2D8539C80);
#endif

/**
 * Deprecated. Use acsutilTimeStamp.h.
 * Type definition for timestamps.
 */
typedef BACITimeStamp TimeStamp;
/**
 * Deprecated. Use acsutilTimeStamp.h.
 * Type defintion for a period of time.
 */
typedef BACITimeInterval TimeInterval;

/**
 * Deprecated. Use acsutilTimeStamp.h instead.
 * Get current time in UTC format
 * @return current time in UTC format
 */
TimeStamp getTimeStamp();

/**
 * Deprecated. Use acsutilTimeStamp.h instead.
 * Get current time in UTC format
 * Should be used only as relative time, this is NOT in UTC format (for performance)
 * @return current time in UTC format
 */
TimeInterval getTime();

/**
 * Deprecated. Use acsutilTimeStamp.h instead.
 * Get current time in ISO8601 format
 * @return current time in ISO8601 format
 */
ACE_CString getStringifiedTimeStamp();

/**
 * Deprecated. Use acsutilTimeStamp.h instead.
 */ 
ACE_Time_Value UTCtoACE_Time_Value(const TimeStamp &time);


/**
 * Deprecated. Use acsutilTimeStamp.h instead.
 * Returns UTC time in the ISO8601 format
 * @return UTC time in the ISO8601 format
 */
ACE_CString getStringifiedUTC(TimeInterval time);

 }; 

#endif  /* baciTime_H */

