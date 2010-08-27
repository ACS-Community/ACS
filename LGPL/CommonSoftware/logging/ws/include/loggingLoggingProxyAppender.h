/*
 * loggingLoggingProxyAppender.h
 *
 *  Created on: Jul 30, 2010
 *      Author: javarias
 */

#ifndef LOGGINGLOGGINGPROXYAPPENDER_H_
#define LOGGINGLOGGINGPROXYAPPENDER_H_

#include <log4cpp/LayoutAppender.hh>
#include <logging_idlC.h>

namespace logging {

class LoggingProxyAppender: public log4cpp::LayoutAppender {

protected:
	virtual void _append(const LoggingEvent& event);

public:
	LoggingProxyAppender();
	virtual ~LoggingProxyAppender();
};


class LoggingServiceFeeder {

public:

	LoggingServiceFeeder();

	/// Accumulates the logRecords into a buffer to then flush the accumulated
	/// log records to the remote log service
	void sendLogRecord(Logging::XmlLogRecord logRecord);

	/// Sends the log record immediately to the remote log service
	void sendLogRecordImmediately(Logging::XmlLogRecord logRecord);

	void setCentralizedLogger(Logging::AcsLogService_ptr centralizedLogger);

private:
	Logging::AcsLogService_var m_logger;
	volatile bool shutdown;

	// Mutexes
	ACE_Recursive_Thread_Mutex m_mutex;
	ACE_SYNCH_MUTEX m_doWorkMutex;

	typedef std::deque<Logging::XmlLogRecord> LogDeque;
	LogDeque m_cache;

	void *worker(void *arg);
	void svc();
	void sendCache();
};
};
;

#endif /* LOGGINGLOGGINGPROXYAPPENDER_H_ */
