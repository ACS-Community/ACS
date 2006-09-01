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
* "@(#) $Id: baciTime.cpp,v 1.94 2006/09/01 02:20:54 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran  17/02/01  created 
*/

/////////////////////////////////////////////////
// BACI Library Milliseconds Time Manipulation
/////////////////////////////////////////////////

#include "baciTime.h"
#include <acsutilTimeStamp.h>

namespace baci {

TimeStamp getTimeStamp()
{
    return ::getTimeStamp();
}

TimeInterval getTime()
{
    return ::getTime();
}

ACE_CString getStringifiedTimeStamp()
{
    return ::getStringifiedTimeStamp();
}

ACE_Time_Value UTCtoACE_Time_Value (const TimeStamp &time)
{
    return ::UTCtoACE_Time_Value(time);
}

ACE_CString getStringifiedUTC(TimeInterval time)
{
  return ::getStringifiedUTC(time);
}

 }; 

