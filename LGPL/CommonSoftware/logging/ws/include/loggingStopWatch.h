#ifndef _logging_StopWatch_H_
#define _logging_StopWatch_H_
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2014 
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
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2014-05-19  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------

 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "loggingLogTrace.h"

namespace Logging
{
/// the implementation is based on LogTrace. It probably could be merged together.

/**
 * The idea of the StopWatch class is to measure the time a certain operation or block of operations take.
 * In the ctor the timer starts and it stops in dtor, in other words.
 * When a StopWatch object is created the stop watch starts and it stops when the object is deleted - it goes out of scope.
 * At the moment the class is located in the logging module because it uses logging facilities.
 */
class StopWatch
{
public:
	///LogTrace smart pointer
		typedef Loki::SmartPtr<StopWatch,
				       Loki::NoCopy,
				       Loki::DisallowConversion,
				       Loki::RejectNull,
				       Loki::DefaultSPStorage> StopWatchSmartPtr;


	StopWatch(Logger::LoggerSmartPtr logger,
			const std::string &file,
			unsigned long line,
			const  std::string &msg,
			double maxTime=0.0);

	~StopWatch();
protected:
	Logger::LoggerSmartPtr logger_m;

	std::string msg_m;
	//std::string methodName_m;
	std::string fileName_m;
	unsigned long lineNumber_m;

	//if elapsed time exceed maxTime_m there is a warning message
	double maxTime_m;

	ACE_Time_Value startTime_m;
	ACE_Time_Value endTime_m;
};//StopWatch

};

#endif /*!_H*/
