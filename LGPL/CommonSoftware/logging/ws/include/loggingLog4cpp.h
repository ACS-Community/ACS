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
 * "@(#) $Id: loggingLog4cpp.h,v 1.1 2011/02/14 21:15:08 javarias Exp $"
 */
#ifndef LOGGING_LOG4CPP_H_
#define LOGGING_LOG4CPP_H_


#include "loggingACSCategory.h"
#include "loggingACSRemoteAppender.h"
#include <ace/Singleton.h>

namespace logging {

class Logger {
public:
	/**
	 * The proper initialization of the RemoteAppender must be done by the class knowing about
	 * the loggingService (like maciContainer or simpleClient)
	 *
	 * @param remoteAppender the remote appender to be used in the prior logger initializations
	 *
	 * @return the old remote appender reference, it can be NULL
	 */
	ACSRemoteAppender* setRemoteAppender(ACSRemoteAppender* remoteAppender);

	Logger();
	~Logger();

	ACSCategory* getLogger(const std::string& loggerName);
	ACSCategory* getGlobalLogger();
	ACSCategory* getStaticLogger();
private:
	ACSRemoteAppender* remoteAppender;
	ACSCategory* initLogger(const std::string& loggerName);
};

}

#define LOGGER_FACTORY ACE_Singleton<logging::Logger, ACE_Null_Mutex>::instance()

#endif
