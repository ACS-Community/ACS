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
* "@(#) $Id: testLogger.cpp,v 1.7 2006/01/05 18:45:10 dfugate Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-03-31  created
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <iostream>
#include <sstream>
#include <unistd.h>

#include "loggingACSLogger.h"
#include "loggingHandler.h"
#include "loggingLogTrace.h"
#include "logging.h"


void logBatchDirectLog(Logging::Logger::LoggerSmartPtr logger)
{
	std::ostringstream oss;
	oss.clear();
	oss.str(std::string());
	oss << "Log batch for logger [" << logger->getName() << "]. USING log() functions.";
	logger->log(Logging::Logger::LM_INFO, oss.str());

	// Test all messages
	//logger->log(Logging::Logger::LM_SHUTDOWN, "Testing LM_SHUTDOWN message: Shutdown messages"); // NOT logging a thing
	logger->log(Logging::Logger::LM_TRACE, "Testing LM_TRACE message: Messages indicating function-calling sequence");
	logger->log(Logging::Logger::LM_DEBUG, "Testing LM_DEBUG message: Messages that contain information normally of use only when debugging a program");
	logger->log(Logging::Logger::LM_INFO, "Testing LM_INFO message: Informational messages");
	logger->log(Logging::Logger::LM_NOTICE, "Testing LM_NOTICE message: Conditions that are not error conditions, but that may require");
	logger->log(Logging::Logger::LM_WARNING, "Testing LM_WARNING message: Warning messages");
	logger->log(Logging::Logger::LM_ERROR, "Testing LM_ERROR message: Error messages");
	logger->log(Logging::Logger::LM_CRITICAL, "Testing LM_CRITICAL message: Critical conditions, such as hard device errors");
	logger->log(Logging::Logger::LM_ALERT, "Testing LM_ALERT message: A condition that should be corrected immediately, such as a corrupted system database");
	logger->log(Logging::Logger::LM_EMERGENCY, "Testing LM_EMERGENCY message: A panic condition.  This is normally broadcast to all users");

} // 10 messages

void logBatchUsingMacrosGeneralLogger()
{
	std::cout << "Log batch for logger [" << "GeneralLogger" << "]. USING ACS MACROS." << std::endl;

	// Test macros
	{
		AUTO_TRACE("TEST_MACROS");
		ACS_LOG(LM_RUNTIME_CONTEXT, __PRETTY_FUNCTION__, (LM_INFO, "LM_RUNTIME_CONTEXT inside TEST_MACROS"));
		ACS_LOG(LM_SOURCE_INFO, __PRETTY_FUNCTION__, (LM_INFO, "LM_SOURCE_INFO inside TEST_MACROS"));
	}

	ACS_LOG(LM_RUNTIME_CONTEXT, __PRETTY_FUNCTION__, (LM_INFO, "LM_RUNTIME_CONTEXT outside TEST_MACROS"));
	ACS_LOG( LM_SOURCE_INFO, __PRETTY_FUNCTION__, (LM_INFO, "LM_SOURCE_INFO outside TEST_MACROS"));

	ACS_TRACE(__PRETTY_FUNCTION__);

    ACS_SHORT_LOG((LM_INFO, "Test ACS_SHORT_LOG with LM_INFO"));

    //ACS_LOG( LM_FULL_INFO, __PRETTY_FUNCTION__, (LM_SHUTDOWN, "Test of LM_SHUTDOWN log")); // NOT logging a thing
    ACS_LOG( LM_FULL_INFO, __PRETTY_FUNCTION__, (LM_TRACE, "Test of LM_TRACE log"));
    //ACS_LOG( LM_FULL_INFO, __PRETTY_FUNCTION__, (LM_DELOUSE, "Test of LM_DELOUSE log")); // Not logginf a thing
    ACS_LOG( LM_FULL_INFO, __PRETTY_FUNCTION__, (LM_DEBUG, "Test of LM_DEBUG log"));
    ACS_LOG( LM_FULL_INFO, __PRETTY_FUNCTION__, (LM_INFO, "Test of LM_INFO log"));
    ACS_LOG( LM_FULL_INFO, __PRETTY_FUNCTION__, (LM_NOTICE, "Test of LM_NOTICE log"));
    ACS_LOG( LM_FULL_INFO, __PRETTY_FUNCTION__, (LM_WARNING, "Test of LM_WARNING log"));
    ACS_LOG( LM_FULL_INFO, __PRETTY_FUNCTION__, (LM_ERROR, "Test of LM_ERROR log"));
    ACS_LOG( LM_FULL_INFO, __PRETTY_FUNCTION__, (LM_CRITICAL, "Test of LM_CRITICAL log"));
    ACS_LOG( LM_FULL_INFO, __PRETTY_FUNCTION__, (LM_ALERT, "Test of LM_ALERT log"));
    ACS_LOG( LM_FULL_INFO, __PRETTY_FUNCTION__, (LM_EMERGENCY, "Test of LM_EMERGENCY log"));

    // Debug messages
    ACS_DEBUG(__PRETTY_FUNCTION__, "Test of ACS_DEBUG macro");
    ACS_DEBUG_PARAM(__PRETTY_FUNCTION__, "Test of ACS_DEBUG_PARAM macro with param: %s", "param1");
} // 17 + 2 messages (entering + exiting)

void logBatchUsingMacrosStaticLogger()
{

	std::cout << "Log batch for logger [" << "StaticLogger" << "]. USING ACS MACROS." << std::endl;

	// Test static logging with audience
	STATIC_LOG_TO_DEVELOPER(LM_INFO, "STATIC_LOG_TO_DEVELOPER");
	STATIC_LOG_TO_OPERATOR(LM_INFO, "STATIC_LOG_TO_OPERATOR");
	STATIC_LOG_TO_SCIENCE(LM_INFO, "STATIC_LOG_TO_SCIENCE");
	STATIC_LOG_TO_SCILOG(LM_INFO, "STATIC_LOG_TO_SCILOG");

	// Static trace
	ACS_STATIC_TRACE(__PRETTY_FUNCTION__);

	// static short log
    ACS_STATIC_SHORT_LOG((LM_INFO, "Test ACS_STATIC_SHORT_LOG with LM_INFO"));

    // Debug messages
    ACS_STATIC_DEBUG(__PRETTY_FUNCTION__, "Test of ACS_STATIC_DEBUG macro");
    ACS_STATIC_DEBUG_PARAM(__PRETTY_FUNCTION__, "Test of ACS_STATIC_DEBUG_PARAM macro with param: %s", "param2");
} // 8 messages

int main(int argc, char *argv[])
{
	std::ostringstream oss;

    ACS_CHECK_LOGGER;

    // First logger
    Logging::Logger::LoggerSmartPtr firstLoggerSmartPtr = getLogger();
    firstLoggerSmartPtr->setName("FirstLogger");

    // Second logger
    Logging::Logger::LoggerSmartPtr secondLoggerSmartPtr = getNamedLogger("SecondLogger");

    // Third logger
    Logging::Logger::LoggerSmartPtr thirdLoggerSmartPtr = getNamedLogger("ThirdLogger");

    // BY DEFAULT STATISTICS ARE NOT ACTIVE: Those messages are not counted
    {
		oss.clear();
		oss.str(std::string());
		oss << "Statistics for logger [" << firstLoggerSmartPtr->getName() << "] are by default disabled";
		firstLoggerSmartPtr->log(Logging::Logger::LM_INFO, oss.str());
		logBatchDirectLog(firstLoggerSmartPtr);
    } // Batch for first logger
    {
		oss.clear();
		oss.str(std::string());
		oss << "Statistics for logger [" << secondLoggerSmartPtr->getName() << "] are by default disabled";
		secondLoggerSmartPtr->log(Logging::Logger::LM_INFO, oss.str());
		logBatchDirectLog(secondLoggerSmartPtr);
    } // Batch for second logger
    {
		oss.clear();
		oss.str(std::string());
		oss << "Statistics for logger [" << thirdLoggerSmartPtr->getName() << "] are by default disabled";
		thirdLoggerSmartPtr->log(Logging::Logger::LM_INFO, oss.str());
		logBatchDirectLog(thirdLoggerSmartPtr);
    } // Batch for third logger

    // Log to general logger using macros
    {
    	logBatchUsingMacrosGeneralLogger();
    } // is the First Logger

    // Activate statistics for first logger
    firstLoggerSmartPtr->log(Logging::Logger::LM_INFO, "Activate statistics for first logger -----------");
    firstLoggerSmartPtr->stats.configureStatistics("testLoggerStats",false, 3, 1);
    // and send a second batch
    {
    	logBatchDirectLog(firstLoggerSmartPtr);
    	logBatchDirectLog(secondLoggerSmartPtr);
    	logBatchDirectLog(thirdLoggerSmartPtr);
    } // only firstLoggerSmartPtr logs will be counted in statistics

    // Log to general logger using macros
    {
    	logBatchUsingMacrosGeneralLogger();
    } // is the First Logger

    // Log to general logger using macros
    {
    	logBatchUsingMacrosStaticLogger();
    } // no stats

    // Activate statistics for second logger
    secondLoggerSmartPtr->log(Logging::Logger::LM_INFO, "Activate statistics for second logger -----------");
    secondLoggerSmartPtr->stats.configureStatistics("testLoggerStats",false, 3, 1);
	// wait 2 seconds to force statistics to be calculated
	unsigned int microseconds = 3100000;
	usleep(microseconds);
	// and send logs to all three loggers
	firstLoggerSmartPtr->log(Logging::Logger::LM_INFO, "FIRST LOGGER STATS ACTIVE");
	secondLoggerSmartPtr->log(Logging::Logger::LM_INFO, "SECOND LOGGER STATS ACTIVE");
	thirdLoggerSmartPtr->log(Logging::Logger::LM_INFO, "THIRD LOGGER STATS INACTIVE");

	// and then send a second batch
	{
    	logBatchDirectLog(firstLoggerSmartPtr);
    	logBatchDirectLog(secondLoggerSmartPtr);
    	logBatchDirectLog(thirdLoggerSmartPtr);
    } // first and second loggers will use them for stats

	// and then send a third batch
	{
		logBatchDirectLog(firstLoggerSmartPtr);
		logBatchDirectLog(secondLoggerSmartPtr);
		logBatchDirectLog(thirdLoggerSmartPtr);
	} // first and second loggers will use them for stats

	// Activate also static logger stats
	firstLoggerSmartPtr->log(Logging::Logger::LM_INFO, "Activate statistics for static logger -----------");
	Logging::Logger::getStaticLogger()->stats.configureStatistics("testLoggerStats",false, 3, 1);
	// and send static batch
	{
    	logBatchUsingMacrosStaticLogger();
	} // stats calculated

    return 0;
}
