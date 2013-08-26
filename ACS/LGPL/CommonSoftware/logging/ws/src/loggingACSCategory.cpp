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
 * "@(#) $Id: loggingACSCategory.cpp,v 1.4 2011/03/25 23:42:00 javarias Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * javarias  May 7, 2010  	 created
 */

#include "loggingACSCategory.h"
#include "loggingACSHierarchyMaintainer.h"

using namespace logging;

ACSCategory::ACSCategory(const std::string& name, log4cpp::Category* parent,
		log4cpp::Priority::Value priority) :
	log4cpp::Category(name, parent, priority) {
}

void ACSCategory::log(logging::ACSLoggingEvent& lr) throw () {
	this->Category::callAppenders(lr);
}

void ACSCategory::log(const std::string& message,
		::log4cpp::Priority::Value priority, const std::string& routine,
		const std::string& file, unsigned int line, const std::string& host,
		const std::string& context, const std::string& audience,
		const std::string& sourceObject, const std::string& array,
		const std::string& antenna, const std::string& stackId,
		const int stackLevel, const std::string& uri) throw () {
	ACSLoggingEvent lr(getName(), message, priority, routine, file, line, host,
			context, audience, sourceObject, array, antenna, stackId,
			stackLevel, uri);
	log(lr);
}

void ACSCategory::log(const std::string& message,
		::log4cpp::Priority::Value priority, const std::string& routine,
		const std::string& file, unsigned int line) throw () {
	ACSLoggingEvent lr(getName(), message, priority, routine, file, line, "",
			"", "", "", "", "", "", 0, "");
	log(lr);
}

void ACSCategory::trace(const std::string& message,
		::log4cpp::Priority::Value priority, const std::string& routine,
		const std::string& file, unsigned int line, const std::string& host,
		const std::string& context, const std::string& audience,
		const std::string& sourceObject, const std::string& array,
		const std::string& antenna, const std::string& stackId,
		const int stackLevel, const std::string& uri) throw () {
	ACSLoggingEvent lr(getName(), message, ::log4cpp::Priority::TRACE, routine,
			file, line, host, context, audience, sourceObject, array, antenna,
			stackId, stackLevel, uri);
	log(lr);
}

void ACSCategory::debug(const std::string& message,
		::log4cpp::Priority::Value priority, const std::string& routine,
		const std::string& file, unsigned int line, const std::string& host,
		const std::string& context, const std::string& audience,
		const std::string& sourceObject, const std::string& array,
		const std::string& antenna, const std::string& stackId,
		const int stackLevel, const std::string& uri) throw () {
	ACSLoggingEvent lr(getName(), message, ::log4cpp::Priority::DEBUG, routine,
			file, line, host, context, audience, sourceObject, array, antenna,
			stackId, stackLevel, uri);
	log(lr);
}

void ACSCategory::delouse(const std::string& message,
		::log4cpp::Priority::Value priority, const std::string& routine,
		const std::string& file, unsigned int line, const std::string& host,
		const std::string& context, const std::string& audience,
		const std::string& sourceObject, const std::string& array,
		const std::string& antenna, const std::string& stackId,
		const int stackLevel, const std::string& uri) throw () {
	ACSLoggingEvent lr(getName(), message, ::log4cpp::Priority::DELOUSE,
			routine, file, line, host, context, audience, sourceObject, array,
			antenna, stackId, stackLevel, uri);
	log(lr);
}

void ACSCategory::info(const std::string& message,
		::log4cpp::Priority::Value priority, const std::string& routine,
		const std::string& file, unsigned int line, const std::string& host,
		const std::string& context, const std::string& audience,
		const std::string& sourceObject, const std::string& array,
		const std::string& antenna, const std::string& stackId,
		const int stackLevel, const std::string& uri) throw () {
	ACSLoggingEvent lr(getName(), message, ::log4cpp::Priority::INFO, routine,
			file, line, host, context, audience, sourceObject, array, antenna,
			stackId, stackLevel, uri);
	log(lr);
}

void ACSCategory::notice(const std::string& message,
		::log4cpp::Priority::Value priority, const std::string& routine,
		const std::string& file, unsigned int line, const std::string& host,
		const std::string& context, const std::string& audience,
		const std::string& sourceObject, const std::string& array,
		const std::string& antenna, const std::string& stackId,
		const int stackLevel, const std::string& uri) throw () {
	ACSLoggingEvent lr(getName(), message, ::log4cpp::Priority::NOTICE,
			routine, file, line, host, context, audience, sourceObject, array,
			antenna, stackId, stackLevel, uri);
	log(lr);
}

void ACSCategory::warn(const std::string& message,
		::log4cpp::Priority::Value priority, const std::string& routine,
		const std::string& file, unsigned int line, const std::string& host,
		const std::string& context, const std::string& audience,
		const std::string& sourceObject, const std::string& array,
		const std::string& antenna, const std::string& stackId,
		const int stackLevel, const std::string& uri) throw () {
	ACSLoggingEvent lr(getName(), message, ::log4cpp::Priority::WARN, routine,
			file, line, host, context, audience, sourceObject, array, antenna,
			stackId, stackLevel, uri);
	log(lr);
}

void ACSCategory::error(const std::string& message,
		::log4cpp::Priority::Value priority, const std::string& routine,
		const std::string& file, unsigned int line, const std::string& host,
		const std::string& context, const std::string& audience,
		const std::string& sourceObject, const std::string& array,
		const std::string& antenna, const std::string& stackId,
		const int stackLevel, const std::string& uri) throw () {
	ACSLoggingEvent lr(getName(), message, ::log4cpp::Priority::ERROR, routine,
			file, line, host, context, audience, sourceObject, array, antenna,
			stackId, stackLevel, uri);
	log(lr);
}

void ACSCategory::crit(const std::string& message,
		::log4cpp::Priority::Value priority, const std::string& routine,
		const std::string& file, unsigned int line, const std::string& host,
		const std::string& context, const std::string& audience,
		const std::string& sourceObject, const std::string& array,
		const std::string& antenna, const std::string& stackId,
		const int stackLevel, const std::string& uri) throw () {
	ACSLoggingEvent lr(getName(), message, ::log4cpp::Priority::CRITICAL,
			routine, file, line, host, context, audience, sourceObject, array,
			antenna, stackId, stackLevel, uri);
	log(lr);
}

void ACSCategory::alert(const std::string& message,
		::log4cpp::Priority::Value priority, const std::string& routine,
		const std::string& file, unsigned int line, const std::string& host,
		const std::string& context, const std::string& audience,
		const std::string& sourceObject, const std::string& array,
		const std::string& antenna, const std::string& stackId,
		const int stackLevel, const std::string& uri) throw () {
	ACSLoggingEvent lr(getName(), message, ::log4cpp::Priority::ALERT, routine,
			file, line, host, context, audience, sourceObject, array, antenna,
			stackId, stackLevel, uri);
	log(lr);
}

void ACSCategory::emerg(const std::string& message,
		::log4cpp::Priority::Value priority, const std::string& routine,
		const std::string& file, unsigned int line, const std::string& host,
		const std::string& context, const std::string& audience,
		const std::string& sourceObject, const std::string& array,
		const std::string& antenna, const std::string& stackId,
		const int stackLevel, const std::string& uri) throw () {
	ACSLoggingEvent lr(getName(), message, ::log4cpp::Priority::EMERG, routine,
			file, line, host, context, audience, sourceObject, array, antenna,
			stackId, stackLevel, uri);
	log(lr);
}

void ACSCategory::setPriority(::log4cpp::Priority::Value priority)
		throw (std::invalid_argument) {
	this->Category::setPriority(priority);
}

ACSCategory* ACSCategory::exist(const std::string& name) {
	return static_cast<ACSCategory*> (ACSHierarchyMaintainer::getDefaultMaintainer().getExistingInstance(name));
}

ACSCategory& ACSCategory::getInstance(const std::string& name) {
	return static_cast<ACSCategory&> (ACSHierarchyMaintainer::getDefaultMaintainer().getInstance(
			name));
}

void ACSCategory::addAppender(log4cpp::Appender* appender)
		throw (std::invalid_argument) {
	this->Category::addAppender(appender);
}

log4cpp::Appender* ACSCategory::getAppender (const std::string &name) const {
	return this->Category::getAppender(name);
}


ACSCategory::~ACSCategory() {

}
