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
 * "@(#) $Id: loggingLog4cpp.cpp,v 1.1 2011/02/14 21:15:08 javarias Exp $"
 */

#import "loggingLog4cpp.h"
#import "loggingStdoutlayout.h"
#import "loggingXmlLayout.h"

#import <log4cpp/Appender.hh>
#include <log4cpp/OstreamAppender.hh>

using namespace logging;

Logger::Logger() {
	remoteAppender = NULL;
}

Logger::~Logger() {
	//Clean the Categories
}

ACSRemoteAppender* Logger::setRemoteAppender(ACSRemoteAppender* remoteAppender) {
	ACSRemoteAppender* tmp = Logger::remoteAppender;
	Logger::remoteAppender = remoteAppender;
	return tmp;
}

ACSCategory* Logger::getGlobalLogger() {
	return getLogger("GlobalLogger");
}

ACSCategory* Logger::getStaticLogger() {
	return getLogger("staticLogger");
}

ACSCategory* Logger::getLogger(const std::string& loggerName) {
	ACSCategory* logger = ACSCategory::exist(loggerName);
	if (logger == NULL)
		logger = initLogger(loggerName);
	return logger;
}



ACSCategory* Logger::initLogger(const std::string& loggerName) {
	::log4cpp::Appender* localAppender = new ::log4cpp::OstreamAppender("STDOUT Appender", &::std::cout);
	::log4cpp::Appender* remoteAppender = Logger::remoteAppender;
	localAppender->setLayout(new logging::ACSstdoutLayout());
	if (remoteAppender != NULL)
		remoteAppender->setLayout(new logging::ACSXmlLayout());
	ACSCategory &logger = ACSCategory::getInstance(loggerName);
	logger.addAppender(localAppender);
	if (remoteAppender != NULL)
		logger.addAppender(remoteAppender);
	return ACSCategory::exist(loggerName);
}


