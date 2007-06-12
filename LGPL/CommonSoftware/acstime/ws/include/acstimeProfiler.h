#ifndef ACSTIME_PROFILER_H
#define ACSTIME_PROFILER_H
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
* "@(#) $Id: acstimeProfiler.h,v 1.6 2007/06/12 08:02:23 nbarriga Exp $"
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
#include <acsutilTimeStamp.h>
#include <iostream>
#include <string>
#include "acstimeC.h"
#include "acstimeEpochHelper.h"
////////////////////////////////////////////////////////////////////////
/** @file acstimeProfiler.h
 *  Header file Profiler class. 
 */

/**
 * @class Profiler
 * Profiler is a utility class providing a very simple profiling mechanism.
 *
 * TODO:
 * - calculate meaningful data like standard deviation???
 */
class Profiler
{
  public:
    ////////////////////////////////////////////////////////////////////////
    /** Constructor
     */
    Profiler();
    /** Destructor
     */
    virtual ~Profiler(){};
    
    /**
     * Resets this class's values.
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void reset();

    /**
     * Starts a timing operation.
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void start();

    /**
     * Stops a timing operation. Can only be called after a start invocation.
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    ACS::Time stop();   

    /**
     * Prints out a full description of all times that were saved along with
     * other relevant statistical data.
     * @param msg Message to be printed out.
     * @return value converted to an Epoch
     * @htmlonly
       <br><hr>
       @endhtmlonly
    */
    void fullDescription(const char* msg);

    /**
     * Adds arbitrary data to the full description.
     */
    void addData(const char* key, const char* value);
    
    
  private:
    ////////////////////////////////////////////////////////////////////////
    ///last time start was invoked
    ACS::Time lastStart_m;

    ///total time that has passed between all start/stops
    ACS::Time totalTime;
    ///total number of times start/stop has been invoked
    unsigned long totalNumStarts_m;

    ///the smallest amount of time that has passed between a start/stop
    ACS::Time minDuration;
    ///the largest amount of time that has passed between a start/stop
    ACS::Time maxDuration;

    std::string extraDescrip_m;

    ///Helper object
    EpochHelper *epochHelper_mp;

    ////////////////////////////////////////////////////////////////////////
    /** Copy not allowed.
     */
    Profiler(const Profiler&);

    /** Assignment not allowed.
     */
    void operator= (const Profiler&);
};

#endif




