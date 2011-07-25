#ifndef LOGGING_ACSREMOTEAPPENDER_H_
#define LOGGING_ACSREMOTEAPPENDER_H_

/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) UNSPECIFIED - FILL IN, 2005
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
* "@(#) $Id: loggingACSRemoteAppender.h,v 1.6 2011/07/25 20:56:33 javarias Exp $"
*
*/


#define LOG4CPP_FIX_ERROR_COLLISION 1
#include <log4cpp/LayoutAppender.hh>

#include <iostream>
#include <deque>
#include <pthread.h>

#include <ace/Synch.h>

#include "loggingLogThrottle.h"
#include "logging_idlC.h"

namespace logging {

class ACSRemoteAppender: public virtual log4cpp::LayoutAppender{
public:
	ACSRemoteAppender(const std::string& name,
			unsigned long cacheSize,
			unsigned int autoFlushTimeoutSec,
			Logging::AcsLogService_ptr centralizedLogger,
			int maxLogsPerSecond);
	virtual ~ACSRemoteAppender();
	void close();

protected:
	void _append(const log4cpp::LoggingEvent& event);

private:
	log4cpp::Priority::Value _threshold;
	log4cpp::Filter* _filter;

};

/**
 *  Thread safe buffer
 */
class RemoteLoggerBuffer {
public :
	RemoteLoggerBuffer(unsigned long cacheSize,
			unsigned int autoFlushTimeoutSec,
			Logging::AcsLogService_ptr centralizedLogger,
			int maxLogsPerSecond );
	void append(Logging::XmlLogRecord& log);
	~RemoteLoggerBuffer();
private:
	void sendLog(Logging::XmlLogRecord& log);
	void sendLog(Logging::XmlLogRecordSeq& logs);
	void flushCache();

	unsigned int _cacheSize;
	unsigned int _flushTimeout;
	Logging::AcsLogService_ptr _logger;
	std::deque<Logging::XmlLogRecord>* _cache;
	ACE_Thread_Mutex _cacheMutex;
	logging::LogThrottle* _logThrottle;
	pthread_t thread;
	//worker entry thread function, it flush the thread at regular intervals or
	//when the cache reaches the max size
	static void* worker(void* arg);
	void svc();
	ACE_SYNCH_MUTEX _workCondThreadMutex;
	ACE_SYNCH_CONDITION _workCond;
	bool _stopThread;

};
}

#endif
