#ifndef ACSTIME_TIMEUTIL_H
#define ACSTIME_TIMEUTIL_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) Associated Universities Inc., 2003 
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
* "@(#) $Id: acstimeTimeUtil.h,v 1.9 2005/02/09 21:39:49 dfugate Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david  2003-07-04  created
*/
////////////////////////////////////////////////////////////////////////
#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif
////////////////////////////////////////////////////////////////////////
#include <baci.h>
#include <sstream>
#include "acstimeC.h"
#include "ACSTimeError.h"
////////////////////////////////////////////////////////////////////////
/** @file acstimeTimeUtil.h
 *  Header file TimeUtil class. 
 */

/**
 * @class TimeUtil
 * TimeUtil is a utility class providing static methods to convert 
 * between various time systems.
 *
 * TODO:
 * - nothing
 */
class TimeUtil
{
  public:
    ////////////////////////////////////////////////////////////////////////
    /** Constructor
     */
    TimeUtil(){};
    /** Destructor
     */
    ~TimeUtil(){};
    
    /** days in each month
     */
    static const short DaysInMonth[12];
    
    /**
     * convert ACE_Time_Value to ACS <a href="../../idl/html/structacstime_1_1Epoch.html">Epoch</a>
     * @param value ACE_Time_value.
     * @return value converted to an Epoch
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    static acstime::Epoch 
    ace2epoch(const ACE_Time_Value &value);
    
    /**
     * convert <a href="../../idl/html/structacstime_1_1Epoch.html">Epoch</a> to ACE_Time_Value
     * @param value Epoch.
     * @return value converted to an ACE_Time_Value
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    static ACE_Time_Value 
    epoch2ace(const acstime::Epoch &value);
    
    /**
     * convert ACE_Time_Value to <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a>
     * @param value ACE_Time_value.
     * @return value converted to a Duration
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    static acstime::Duration 
    ace2duration(const ACE_Time_Value &value);
    
    /**
     * convert <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a> to ACE_Time_Value
     * @param value Duration.
     * @return value converted to an ACE_Time_Value
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    static ACE_Time_Value 
    duration2ace(const acstime::Duration &value);
    
  private:
    ////////////////////////////////////////////////////////////////////////
    /** Copy not allowed.
     */
    TimeUtil(const TimeUtil&);

    /** Assignment not allowed.
     */
    void operator= (const TimeUtil&);
};

#endif
