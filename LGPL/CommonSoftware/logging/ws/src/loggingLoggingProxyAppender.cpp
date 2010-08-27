/*
 * loggingLoggingProxyAppender.cpp
 *
 *  Created on: Jul 30, 2010
 *      Author: javarias
 */

#include "loggingLoggingProxyAppender.h"
#include "loggingACSLoggingEvent.h"


using namespace logging;

void LoggingProxyAppender::_append(const LoggingEvent& event){
	//Convert event to Logging::XmlLogRecord
	Logging::XmlLogRecord logRecord;
	//convert the priority from log4cpp::Priority into AcsLogLevel
	std::string message(_getLayout().format(event));
	logRecord.xml = message.c_str();
	//feeder must be initialized before this point by the container or client
	static LoggingServiceFeeder *feeder;
	feeder->sendLogRecord(logRecord);
}


LoggingServiceFeeder::LoggingServiceFeeder(): shutdown(false){

}

void LoggingServiceFeeder::setCentralizedLogger(Logging::AcsLogService_ptr centralizedLogger){
	ACE_GUARD (ACE_Recursive_Thread_Mutex, ace_mon, m_mutex);
	if(::CORBA::is_nil(m_logger.ptr())){
		m_logger = Logging::AcsLogService::_duplicate(centralizedLogger);
	}

}

void *LoggingServiceFeeder::worker(void *arg){
	static_cast<LoggingServiceFeeder *>(arg)->svc();
	return 0;
}

void LoggingServiceFeeder::svc(){
	sendCache();
	while (!shutdown) {
		m_doWorkMutex.acquire(); //we have to acquire mutex before we are going to wait !!!
		m_doWorkCond.wait(&timeout);
		m_doWorkMutex.release(); // and after we have to release the mutex !!!
		if (!m_shutdown)
			sendCache();
	}
}

void LoggingServiceFeeder::sendCache(){
	Logging::XmlLogRecordSeq reclist;
	reclist.length(m_cache.size());

	for (unsigned int i = 0; i < reclist.length(); i++){
		reclist[i].logLevel = m_cache[0].logLevel;
		reclist[i++].xml = m_cache[0].xml;
		m_cache.pop_front();
	}
	try{
		m_logger->writeRecords(reclist);
	}
	catch(::CORBA::TRANSIENT &){
		//do something with the logs in case of failure
	}
}

void LoggingServiceFeeder::sendLogRecord(Logging::XmlLogRecord logRecord) {
	ACE_GUARD (ACE_Recursive_Thread_Mutex, ace_mon, m_mutex);
	m_cache.push_back(logRecord);

	//TODO: if the deque reach the max allowed it should be flushed
}

void LoggingServiceFeeder::sendLogRecordImmediately(Logging::XmlLogRecord logRecord) {
	Logging::XmlLogRecordSeq reclist;
	reclist.length(1);
	reclist[0] = logRecord;
	try{
		m_logger->writeRecords(reclist);
	}
	catch(::CORBA::TRANSIENT &){
		//do something with the log in case of failure
	}
}
