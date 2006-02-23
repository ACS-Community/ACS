/* @(#) $Id: acstimeTimeUtil.cpp,v 1.7 2005/02/09 21:39:49 dfugate Exp $
 *
 * Copyright (C) 2001
 * Associated Universities, Inc. Washington DC, USA.
 *
 * Produced for the ALMA project
 *
 * This library is free software; you can redistribute it and/or modify it it 
 * under the terms of the GNU Library General Public License as published by 
 * the Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 *
 * This library is distributed in the hope that it will be useful but WITHOUT 
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
 * License for more details.
 *
 * You should have received a copy of the GNU Library General Public License 
 * along with this library; if not, write to the Free Software Foundation, 
 * Inc., 675 Massachusetts Ave, Cambridge, MA, 02139, USA.
 *
 * Correspondence concerning ALMA should be addressed as follows:
 * Internet email: alma-sw-admin@nrao.edu
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

