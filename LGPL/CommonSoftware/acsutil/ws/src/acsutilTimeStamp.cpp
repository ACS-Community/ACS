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
* "@(#) $Id: acsutilTimeStamp.cpp,v 1.1 2005/12/12 19:12:29 dfugate Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran  17/02/01  created 
*/


#include "acsutilTimeStamp.h"


ACS::Time getTimeStamp()
{
    ACE_Time_Value tv = ACE_OS::gettimeofday();
    return (UTCtoUNIXTimeBaseOffset + static_cast<CORBA::ULongLong>(tv.sec())) *
	static_cast<ACE_UINT32>(10000000) +
	static_cast<CORBA::ULongLong>(tv.usec() * 10);
}

ACS::TimeInterval getTime()
{
    ACE_Time_Value tv = ACE_OS::gettimeofday();
    return (static_cast<CORBA::ULongLong>(tv.sec())) *
	static_cast<ACE_UINT32>(10000000) +
	static_cast<CORBA::ULongLong>(tv.usec() * 10);
}

ACE_CString getStringifiedTimeStamp()
{
    ACE_TCHAR str[24];
    ACE_TCHAR ctp[20];
    ACE_Time_Value tv = ACE_OS::gettimeofday();
    time_t ut(tv.sec());
    struct tm utc_p = {0};
    ACE_OS::gmtime_r(&ut,&utc_p);
    ACE_OS::strftime(ctp, sizeof(ctp), "%Y-%m-%dT%H:%M:%S", &utc_p);
    
    ACE_OS::sprintf (str, ACE_TEXT ("%s.%03ld"),
		     ctp, tv.usec () / 1000);
    
    return str;
}
