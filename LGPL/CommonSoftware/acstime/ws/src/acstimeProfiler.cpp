/* @(#) $Id: acstimeProfiler.cpp,v 1.12 2007/06/12 08:02:23 nbarriga Exp $
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
#include "acstimeProfiler.h"
#include <acsutilPorts.h>
#include "acsutilTimeStamp.h"
//------------------------------------------------------------------------------
Profiler::Profiler() :
    epochHelper_mp(0)
{
    if(epochHelper_mp == 0)
	{
	epochHelper_mp = new EpochHelper();
	}

    reset();
}
//------------------------------------------------------------------------------
void
Profiler::reset()
{
    lastStart_m=0;
    totalTime=0;
    totalNumStarts_m=0;
    //pick a ridiculously large value!
    minDuration=0xFFFFFFFFULL;
    maxDuration=0;
    extraDescrip_m = "";
}
//------------------------------------------------------------------------------
void
Profiler::start()
{
    //double-check this to make sure they don't call start twice without a stop!
    if (lastStart_m==0)
	{
	totalNumStarts_m++;
	}
    else
	{
	std::cerr << "Looks like Profiler::start was called twice in a row without invoking Profiler::stop" << std::endl;
	}
    //get the current time.
    lastStart_m=getTimeStamp();
}
//------------------------------------------------------------------------------
ACS::Time
Profiler::stop()
{
    //save the current time
    if (lastStart_m==0)
	{
	std::cerr << "Looks like Profiler::stop was called twice in a row without invoking Profiler::start" << std::endl;
	return 0ULL;
	}
    ACS::Time timeDiff = getTimeStamp() - lastStart_m;

    if (timeDiff>maxDuration)
	{
	maxDuration=timeDiff;
	}

    if (timeDiff<minDuration)
	{
	minDuration=timeDiff;
	}

    //0 last value
    lastStart_m=0;

    //add this to the total time
    totalTime += timeDiff;
    
    return timeDiff;
}
//------------------------------------------------------------------------------
void
Profiler::fullDescription(const char* msg)
{
    //developer forgot to invoke stop!
    if (lastStart_m!=0)
	{
	//just do it for them
	stop();
	}
    
    //if there were no start invocations just bail out
    if (totalNumStarts_m==0)
	{
	std::cerr << "ACS PROFILER: No start invocations - " << msg << std::endl; 
	return;
	}

    long double averageTime = totalTime / totalNumStarts_m;

    std::cout << "#ACS PROFILER# msg=" << msg;
    std::cout << ", avg=" << averageTime / 10000.0; 
    std::cout << ", runs=" << totalNumStarts_m; 
    std::cout << ", mindur=" << minDuration / 10000.0;
    std::cout << ", maxdur=" << maxDuration / 10000.0;
    std::cout << ", cpu=Unknown";
    std::cout << ", mem=Unknown";

    acstime::Epoch currEpoch;
    currEpoch.value = getTimeStamp();
    epochHelper_mp->value(currEpoch);

    std::cout << ", date=" << (epochHelper_mp->toString(acstime::TSArray, "", 0, 0)).c_str();
    std::cout << ", ip=" << ACSPorts::getIP();
    std::cout << ", lang=cpp";
    std::cout << ", units=ms";
    std::cout << extraDescrip_m;
    std::cout << std::endl;
}
//------------------------------------------------------------------------------
void
Profiler::addData(const char* key, const char* value)
{
    extraDescrip_m = extraDescrip_m + ", " + key + "=" + value;
}




