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
 * "@(#) "
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * javarias  May 6, 2010  	 created
 */
#include "loggingXmlLayout.h"
#include "loggingACSLoggingEvent.h"
#include "loggingLoggingProxy.h"

#include <sstream>

using namespace logging;

std::string getPriorityName(log4cpp::Priority::Value level) {
	switch (level) {
		case log4cpp::Priority::TRACE:
			return "Trace";
		case log4cpp::Priority::DELOUSE:
			return "Delouse";
		case log4cpp::Priority::DEBUG:
			return "Debug";
		case log4cpp::Priority::INFO:
			return "Info";
		case log4cpp::Priority::NOTICE:
			return "Notice";
		case log4cpp::Priority::WARNING:
			return "Warning";
		case log4cpp::Priority::ERROR:
			return "Error";
		case log4cpp::Priority::CRITICAL:
			return "Critical";
		case log4cpp::Priority::ALERT:
			return "Alert";
		case log4cpp::Priority::EMERGENCY:
			return "Emergency";
		default:
			return "";
		}
}

ACSXmlLayout::ACSXmlLayout(){
	//allocated previously the size of the string
	m_xml.reserve((size_t)1024);
}

std::string ACSXmlLayout::format(const ::log4cpp::LoggingEvent& event) {
	::std::string retStr = "";
	const ACSLoggingEvent* acsLogEvent =
			static_cast<const ACSLoggingEvent *> (&event);
	ACE_Time_Value time(event.timeStamp.getSeconds(),
			event.timeStamp.getMicroSeconds() * 1000
					+ event.timeStamp.getMilliSeconds());
	ACE_TCHAR timestamp[24];
	//TODO: should this conversion be improved to remove the ACE dependency?
	ACSXmlLayout::formatISO8601inUTC(time, timestamp);

	m_xml.clear();
	m_xml = "<" + getPriorityName(acsLogEvent->priority)
			+ " TimeStamp=\"" + timestamp + "\"";

	//It should be enabled when SOURCE_INFO is enabled
	if(acsLogEvent->fileName.compare("") != 0){
		m_xml += " File=\"" + acsLogEvent->fileName + "\"";
		if(acsLogEvent->line != 0 ){
			m_xml += " Line=\"";
			std::stringstream out;
			out << acsLogEvent->line;
			m_xml += out.str();
			m_xml += "\"";
		}
	}

	// routine (REQUIRED for TRACE, DELOUSE and DEBUG)
	if (acsLogEvent->priority > log4cpp::Priority::INFO) {
		m_xml += " Routine=\"";
		size_t before = m_xml.length();
		m_xml += acsLogEvent->routineName + "\"";
		for (size_t i = before; i < m_xml.length(); i++) {
			/*
			 * We do not want to manipulate the strings (too slow) so we try to fix
			 * some of the problems that can cause a parse error while reading the logs
			 */
			if (m_xml[i] == '<') m_xml[i] = '{';
			else if (m_xml[i] == '>') m_xml[i] = '}';
			else if (m_xml[i] == '&') m_xml[i] = '#';
		}
	}

	m_xml += " Host=\"" + acsLogEvent->hostName + "\"";
	m_xml += " Thread=\"" + acsLogEvent->threadName + "\"";
	m_xml += " Context=\"" + acsLogEvent->contextName + "\"";
	m_xml += " SourceObject=\"" + acsLogEvent->sourceObject + "\"";
	m_xml += " Antenna=\"" + acsLogEvent->antenna + "\"";
	if (acsLogEvent->stackId.length() > 0)
		m_xml += " StackId=\"" + acsLogEvent->stackId + "\"";
	if (acsLogEvent->stackLevel > -1) {
		m_xml += " StackLevel=\"";
		std::stringstream out;
		out << acsLogEvent->stackLevel;
		m_xml += out.str();
		m_xml += "\"";
	}
	//m_xml += " LogId=\"";
	if ( acsLogEvent->uri.length() > 0)
		m_xml += " Uri=\"\"" + acsLogEvent->uri + "\"";
	if(acsLogEvent->priority < log4cpp::Priority::DELOUSE){
		//Convert to a priority number expected by ACS
		unsigned int p = (1000 - (unsigned int)acsLogEvent->priority) / 100;
		if(p > 6) p++; //ACS Old log compatibility
		m_xml += " Priority=\"";
		std::stringstream out;
		out << p;
		m_xml += out.str();
		m_xml += "\"";
	}
	m_xml += ">";
	m_xml += "<![CDATA[" + acsLogEvent->message + "]]>";
	m_xml += "</" + getPriorityName(acsLogEvent->priority) + ">";
	return m_xml;
}

//Recycled method from loggingLoggingProxy
void ACSXmlLayout::formatISO8601inUTC(const ACE_Time_Value &timestamp,
		ACE_TCHAR str[]) {
	ACE_TCHAR ctp[20];
	time_t ut(timestamp.sec());
	struct tm *utc = ACE_OS::gmtime(&ut);
	ACE_OS::strftime(ctp, sizeof(ctp), "%Y-%m-%dT%H:%M:%S", utc);

	ACE_OS::sprintf(str, ACE_TEXT("%s.%03ld"), ctp, timestamp.usec() / 1000);
}

ACSXmlLayout::~ACSXmlLayout(){
}
