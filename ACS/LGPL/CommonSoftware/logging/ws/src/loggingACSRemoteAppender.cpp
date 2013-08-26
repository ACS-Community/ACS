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
* "@(#) $Id: loggingACSRemoteAppender.cpp,v 1.7 2011/07/25 20:56:33 javarias Exp $"
*/

#include "loggingACSRemoteAppender.h"
#include <log4cpp/Priority.hh>

#include <tao/SystemException.h>

using namespace logging;

static RemoteLoggerBuffer* buffer = NULL;

AcsLogLevels::logLevelValue toIdlLogLevel(log4cpp::Priority::Value prio) {
	switch (prio) {
	case log4cpp::Priority::TRACE:
		return AcsLogLevels::TRACE_VAL;
	case log4cpp::Priority::DELOUSE:
		return AcsLogLevels::DELOUSE_VAL;
	case log4cpp::Priority::DEBUG:
		return AcsLogLevels::DEBUG_VAL;
	case log4cpp::Priority::INFO:
		return AcsLogLevels::INFO_VAL;
	case log4cpp::Priority::NOTICE:
		return AcsLogLevels::NOTICE_VAL;
	case log4cpp::Priority::WARNING:
		return AcsLogLevels::WARNING_VAL;
	case log4cpp::Priority::ERROR:
		return AcsLogLevels::ERROR_VAL;
	case log4cpp::Priority::CRITICAL:
		return AcsLogLevels::CRITICAL_VAL;
	case log4cpp::Priority::ALERT:
		return AcsLogLevels::ALERT_VAL;
	case log4cpp::Priority::EMERGENCY:
		return AcsLogLevels::EMERGENCY_VAL;
	default:
		return (AcsLogLevels::logLevelValue)-1;
	}
}

ACSRemoteAppender::ACSRemoteAppender(const std::string& name,
		unsigned long cacheSize,
		unsigned int autoFlushTimeoutSec,
		Logging::AcsLogService_ptr centralizedLogger,
		int maxLogsPerSecond = -1) :
	log4cpp::LayoutAppender(name),
	_threshold(log4cpp::Priority::NOTSET),
	_filter(NULL) {
	if (buffer == NULL)
		buffer = new RemoteLoggerBuffer(cacheSize, autoFlushTimeoutSec, centralizedLogger, maxLogsPerSecond);

}

ACSRemoteAppender::~ACSRemoteAppender() {
	close();
}


void ACSRemoteAppender::_append(const log4cpp::LoggingEvent& event) {
	std::string message = _getLayout().format(event);
	Logging::XmlLogRecord log;
	log.logLevel = toIdlLogLevel(event.priority);
	log.xml = ::CORBA::string_dup(message.c_str());
	buffer->append(log);
}


void ACSRemoteAppender::close() {
	//_stopThread = true;
	//_workCond.signal();
	//flushCache();
	//flush the cache and stop the flushing thread
}

RemoteLoggerBuffer::RemoteLoggerBuffer(
		unsigned long cacheSize,
		unsigned int autoFlushTimeoutSec,
		Logging::AcsLogService_ptr centralizedLogger,
		int maxLogsPerSecond = -1) :
		_cacheSize(cacheSize),
		_flushTimeout(autoFlushTimeoutSec),
		_logger(Logging::AcsLogService::_duplicate(centralizedLogger)),
		_cache(NULL),
		_logThrottle(NULL),
		_workCond(_workCondThreadMutex),
		_stopThread(false) {
	_cacheMutex.acquire();
	if (_logThrottle == NULL)
		_logThrottle = new LogThrottle(maxLogsPerSecond);
	if (_cache == NULL) {
		_cache = new std::deque<Logging::XmlLogRecord>();

		if (_cacheSize > 0) {
			//This thread must not be a ACE_Thread.
			// Otherwise the thread hook in maci Simple CLient will create a new logging proxy thread
			pthread_create(&thread, NULL, static_cast<ACE_THR_FUNC> (RemoteLoggerBuffer::worker), this);
		}
	}
	_cacheMutex.release();
}

void RemoteLoggerBuffer::sendLog(Logging::XmlLogRecord& log) {
	Logging::XmlLogRecordSeq seq;
	seq.length(1);
	seq[0] = log;
	sendLog(seq);
}

void RemoteLoggerBuffer::sendLog(Logging::XmlLogRecordSeq& logs) {
	if (CORBA::is_nil(_logger)) {
		std::cerr << "Remote logger is not available" << std::endl;
		return;
	}
	try {
		_logger->writeRecords(logs);
	} catch (CORBA::SystemException &ex) {
		std::cerr << "Problem writing to the centralized logger." << std::endl;
	}
}

void RemoteLoggerBuffer::flushCache() {
	Logging::XmlLogRecordSeq logs;
	logs.length(_cacheSize * 2);
	unsigned int count = 0;

	_cacheMutex.acquire();
	while (!_cache->empty() && count < (_cacheSize * 2)) {
		logs[count++] = _cache->front();
		_cache->pop_front();
	}
	_cacheMutex.release();
    if (count == 0)
        return;
	logs.length(count);
	sendLog(logs);
}

void* RemoteLoggerBuffer::worker(void* arg) {
	static_cast<RemoteLoggerBuffer*>(arg)->svc();
	return 0;
}

void RemoteLoggerBuffer::svc() {
	while (!_stopThread) {
		ACE_Time_Value timeout = ACE_OS::gettimeofday() + ACE_Time_Value(
				_flushTimeout, 0);
		_workCondThreadMutex.acquire();
		_workCond.wait(&timeout);
		_workCondThreadMutex.release();
		//if the thread was shut down, the close method should flush the cache
		if (!_stopThread) {
			try {
				flushCache();
			} catch (CORBA::SystemException &ex) {
				std::cerr << "Problem with Remote Logger. Losing these logs" << std::endl;
			}
		}
	}
}

void RemoteLoggerBuffer::append(Logging::XmlLogRecord& log) {
	if (_cacheSize > 0) {
		if (_logThrottle->checkPublishLogRecord() > 0) {
			_logThrottle->updateLogCounter(1);
			//if the log is quite urgent put it directly in the remote log service
			if (log.logLevel == AcsLogLevels::EMERGENCY_VAL)
				sendLog(log);
			else {
				_cacheMutex.acquire();
				_cache->push_back(log);
				_cacheMutex.release();
			}
		}
		if (_cache->size() >= _cacheSize) {
			_workCond.signal();
		}
	}
	else {
		if (_logThrottle->checkPublishLogRecord() > 0) {
			_logThrottle->updateLogCounter(1);
			sendLog(log);
		}
	}
}

RemoteLoggerBuffer::~RemoteLoggerBuffer() {
	_stopThread = true;
	try {
		flushCache();
	} catch (CORBA::SystemException &ex) {
		std::cerr << "Problem with Remote Logger" << std::endl;
	}
	void *res;
	pthread_join(thread, &res);
}


