/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2011
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
* "@(#) $Id: bulkDataNTDDSLoggable.cpp,v 1.3 2011/11/10 16:14:56 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/

#include "bulkDataNTDDSLoggable.h"

BulkDataNTDDSLoggable::~BulkDataNTDDSLoggable ()
{
	ACS_TRACE(__FUNCTION__);
	if(logger_mp)
	{
		// we have to call "done" as many times as we call "init" -1, because the done it is called another time in Proxy dtor !!
		for(unsigned int i=1; i<loggerInitCount_m; i++)
			LoggingProxy::done();
		delete logger_mp; // ...  but we have just one proxy object for all DDS reader threads
	}
}//~BulkDataNTDDSLoggable


void BulkDataNTDDSLoggable::initalizeLogging()
{
	// this code is a bit dirty, but wee need to initialize loggerproxy per thread
	//isThreadInit return 0 if it is initialized !!!
	if (LoggingProxy::isInitThread() )
	{
		//TBD here we have to set centralized loggger as well, but we need some support from logging
		if (logger_mp==0) //if we do not have a logger we create one for all DDS threads
			logger_mp = new LoggingProxy(0, 0, 31);
		LoggingProxy::init(logger_mp);
		loggerInitCount_m++; // we initialized Proxy another time
	}
}//initalizeLogging

Logging::Logger::LoggerSmartPtr BulkDataNTDDSLoggable::getLogger ()
{
	initalizeLogging();
	return Logging::Loggable::getLogger();
}//getLogger
