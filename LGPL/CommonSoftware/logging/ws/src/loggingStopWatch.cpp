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



#include "loggingStopWatch.h"
#include <iostream>

using namespace Logging;


StopWatch::StopWatch(Logger::LoggerSmartPtr logger,
		const std::string &file,
		unsigned long line,
		const  std::string &msg,
		//const std::string &method,
		double maxTime) :
					logger_m(logger),
					msg_m(msg),
					//methodName_m(method),
					fileName_m(file),
					lineNumber_m(line),
					maxTime_m(maxTime)
{
	startTime_m = ACE_OS::gettimeofday();

	if (logger_m!=0)
	{
		logger_m->log(Logger::LM_DEBUG,
				std::string("StopWatch starts ..."),
				file,
				line,
				msg_m);
	}
	else
	{
		std::cerr << "SEVERE LOGGING ERROR IN StopWatch/STOP_WATCH - logger/getLogger() is NULL: routine=";
		std::cerr << " file: " << file << " line: " << line << " " << msg_m << std::endl;
	}//if-else
}//StopWatch

StopWatch::~StopWatch()
{
	endTime_m = ACE_OS::gettimeofday();
	double elapsedTime = 0.0;
	elapsedTime = (endTime_m.usec() - startTime_m.usec()) / 1000000;
	elapsedTime = endTime_m.sec() - startTime_m.sec() + elapsedTime;
	char time_str[1024];
	Logging::BaseLog::Priority p;

	if (maxTime_m!=0.0 && elapsedTime>maxTime_m)
	{
		p = Logger::LM_WARNING;
		sprintf(time_str, "StopWatch stops ... execution elapsed time %f sec exceeded max time: %f", elapsedTime, maxTime_m);
	}else
	{
		p = Logger::LM_DEBUG;
		sprintf(time_str, "StopWatch stops ... execution elapsed time %f sec", elapsedTime);
	}

	if (logger_m!=0)
	{
		logger_m->log(p,
				std::string(time_str),
				fileName_m,
				lineNumber_m,
				msg_m);
	}
}//~StopWatch


/*___oOo___*/
