/*******************************************************************************
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
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
 *
 * "@(#) "
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * javarias  Jul 21, 2010  	 created
 */
#include "loggingLogThrottle.h"
#include <ace/ACE.h>
#include <limits.h>

#include <iostream>

using namespace logging;

LogThrottle::LogThrottle(int maxLogPerInterval):
	maxLogPerTimeInterval(maxLogPerInterval), timeIntervalInMillis(1000),
	intervalTimeStartMillis(ACE_OS::gettimeofday().msec()),logCounter(0){
}

void LogThrottle::configureLogging(int maxLogPerTimeInterval){
	this->maxLogPerTimeInterval = maxLogPerTimeInterval;
}

unsigned int LogThrottle::checkPublishLogRecord(){
	if(maxLogPerTimeInterval < 0)
		return UINT_MAX;
	curr_time =  ACE_OS::gettimeofday().msec();
	if(curr_time > intervalTimeStartMillis + timeIntervalInMillis){
		intervalTimeStartMillis = ACE_OS::gettimeofday().msec();
		logCounter = 0;
	}
	return maxLogPerTimeInterval - logCounter;
}

void LogThrottle::updateLogCounter(unsigned int logsSent){
	if(maxLogPerTimeInterval < 0)
		return;
	if(logsSent > maxLogPerTimeInterval - logCounter)
		logsSent = maxLogPerTimeInterval - logCounter;
	logCounter += logsSent;
}
