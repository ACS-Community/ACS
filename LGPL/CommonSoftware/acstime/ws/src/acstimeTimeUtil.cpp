/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2011
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: acstimeTimeUtil.cpp,v 1.8 2011/10/28 15:12:04 hsommer Exp $"
*/
//------------------------------------------------------------------------------
#include "acstimeTimeUtil.h"
//------------------------------------------------------------------------------
const short TimeUtil::DaysInMonth[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
//------------------------------------------------------------------------------
acstime::Epoch
TimeUtil::ace2epoch(const ACE_Time_Value &aceTime)
{
    acstime::Epoch retValue; 
    retValue.value = (acstime::ACE_BEGIN + (aceTime.sec() * 10000000LL) + (aceTime.usec() * 10LL));
    return retValue;
}
//------------------------------------------------------------------------------
ACE_Time_Value
TimeUtil::epoch2ace(const acstime::Epoch &epoch)
{
    long long int time = epoch.value - acstime::ACE_BEGIN;  // 100ns units
    long sec = time / 10000000LL;
    
    ACE_Time_Value aceTime;
    aceTime.sec(sec);
    aceTime.usec((time - (sec * 10000000LL)) / 10);
    return aceTime;
}
//------------------------------------------------------------------------------
acstime::Duration
TimeUtil::ace2duration(const ACE_Time_Value& aceTime)
{
    acstime::Duration retValue;
    retValue.value = (aceTime.sec() * 10000000LL + aceTime.usec() * 10LL);
    return retValue;
}
//------------------------------------------------------------------------------
ACE_Time_Value 
TimeUtil::duration2ace(const acstime::Duration &duration)
{
    long sec = duration.value / 10000000LL;
    ACE_Time_Value aceTime;
    aceTime.sec(sec);
    aceTime.usec((duration.value - (sec * 10000000LL)) / 10);
    return aceTime;
}
//------------------------------------------------------------------------------

