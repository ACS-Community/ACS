#ifndef acsutilTime_H
#define acsutilTime_H

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
* "@(#) $Id: acsutilTimeStamp.h,v 1.2 2012/01/20 22:07:43 tstaig Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2001/03/12  modified
*/

/** 
 * @file acsutilTimeStamp.h
 * Header file for rudimentary time helper functions.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acscommonC.h>
#include <ace/SString.h>
#include <ace/OS_NS_time.h>

/**
 * DESCRIPTION: Time stamp representation.
 *
 * Representation of a time stamp coincides with OMG's representation of
 * time, i.e. it represents the number of 100-nanosecond intervals since
 * 15th October 1582 00:00:00. See "CORBAServices: Common Object Services
 * Specification" (ftp://ftp.omg.org/pub/doc/formal/98-12-09.pdf),
 * chapter 14.
 *
 * This method returns the current system time in UTC format.
 *
 * UNIX systems use 1st Jan. 1970 as the Base Time. The CORBA Time
 * Service uses the Universal Time Coordinated (UTC) representation
 * of time from the X/Open DCE Time Service. The UTC time signals
 * broadcast by the WWV radio station of the National Bureau of
 * Standards deliver time that is easier to handle in this
 * representation. UTC time is defined as :
 * 
 *  Time Units : 100 nanosecs
 *  Base Time  : 15th October 1582 00:00:00
 *  Approximate range : AD 30,000
 *
 */

/**
 *  To construct the UTC time from UNIX time we need to add the
 *  difference of days between 15th October 1582 and 1st Jan
 *  1970. This difference is 141427 days or 0x2D8539C80 secs.
 */

#if defined (ACE_LACKS_LONGLONG_T)
  static const CORBA::ULongLong UTCtoUNIXTimeBaseOffset (0xD8539C80, 2);
  // (Lower 32 bits of the offset in hex, Upper 32 bits of the offset in hex)
#else
  static const CORBA::ULongLong UTCtoUNIXTimeBaseOffset = ACE_UINT64_LITERAL(0x2D8539C80);
#endif


/**
 * Get current time in UTC format
 * @return current time in UTC format
 */
ACS::Time getTimeStamp();

/**
 * Get current time in UTC format
 * Should be used only as relative time, this is NOT in UTC format (for performance)
 * @return current time in UTC format
 */
ACS::TimeInterval getTime();

/**
 * Get current time in ISO8601 format
 * @return current time in ISO8601 format
 */
ACE_CString getStringifiedTimeStamp();

inline ACE_Time_Value UTCtoACE_Time_Value(const ACS::Time &time)
{
    ACS::Time seconds = time / static_cast<ACE_UINT32>(10000000);
    ACS::Time microseconds = (time % static_cast<ACE_UINT32>(10000000)) / 10;
    return ACE_Time_Value (ACE_U64_TO_U32 (seconds),
			   ACE_U64_TO_U32 (microseconds));
}

/**
 * Returns UTC time in the ISO8601 format
 * @return UTC time in the ISO8601 format
 */
inline ACE_CString getStringifiedUTC(ACS::TimeInterval time)
{
  ACE_TCHAR str[24];
  ACE_TCHAR ctp[20];

  // convert to UNIX time
  time -= UTCtoUNIXTimeBaseOffset * ACE_static_cast(ACE_UINT32, 10000000);

  ACE_Time_Value tv = UTCtoACE_Time_Value(time);

  time_t ut(tv.sec());
  struct tm *utc_p = ACE_OS::gmtime(&ut);
  ACE_OS::strftime(ctp, sizeof(ctp), "%Y-%m-%dT%H:%M:%S", utc_p);
  
  ACE_OS::sprintf (str, ACE_TEXT ("%s.%03ld"),
		   ctp, tv.usec () / 1000);
  
  return str;
}

#endif 

