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
 * javarias  May 7, 2010  	 created
 */

#ifndef LOGGINGACSCATEGORY_H_
#define LOGGINGACSCATEGORY_H_

#define LOG4CPP_FIX_ERROR_COLLISION 1
#include <log4cpp/Category.hh>
#include <log4cpp/Appender.hh>
#include "loggingACSLoggingEvent.h"
#include "loggingACSHierarchyMaintainer.h"

namespace logging {
class ACSCategory: private ::log4cpp::Category {
	friend class ACSHierarchyMaintainer;
public:

	ACSCategory(const std::string& name, log4cpp::Category* parent,
			log4cpp::Priority::Value priority = log4cpp::Priority::NOTSET);

	void setPriority(::log4cpp::Priority::Value priority) throw (std::invalid_argument);

	virtual ~ACSCategory();

	void log(ACSLoggingEvent& lr) throw ();
	void log(const std::string& message,
			::log4cpp::Priority::Value priority, const std::string& routine,
			const std::string& file, unsigned int line, const std::string& host,
			const std::string& context, const std::string& audience,
			const std::string& sourceObject, const std::string& array,
			const std::string& antenna, const std::string& stackId,
			const int stackLevel, const std::string& uri) throw ();

	void log (const std::string& message,
			::log4cpp::Priority::Value priority, const std::string& routine,
			const std::string& file, unsigned int line) throw ();

	void trace(const std::string& message, ::log4cpp::Priority::Value priority,
			const std::string& routine, const std::string& file,
			unsigned int line, const std::string& host,
			const std::string& context, const std::string& audience,
			const std::string& sourceObject, const std::string& array,
			const std::string& antenna, const std::string& stackId,
			const int stackLevel, const std::string& uri) throw ();

	inline bool isTraceEnabled() const throw () {
		return isPriorityEnabled(::log4cpp::Priority::TRACE);
	}

	void delouse(const std::string& message, ::log4cpp::Priority::Value priority,
			const std::string& routine, const std::string& file,
			unsigned int line, const std::string& host,
			const std::string& context, const std::string& audience,
			const std::string& sourceObject, const std::string& array,
			const std::string& antenna, const std::string& stackId,
			const int stackLevel, const std::string& uri) throw ();
	inline bool isDelouseEnabled() const throw () {
		return isPriorityEnabled(::log4cpp::Priority::DELOUSE);
	}

	void debug(const std::string& message, ::log4cpp::Priority::Value priority,
			const std::string& routine, const std::string& file,
			unsigned int line, const std::string& host,
			const std::string& context, const std::string& audience,
			const std::string& sourceObject, const std::string& array,
			const std::string& antenna, const std::string& stackId,
			const int stackLevel, const std::string& urie) throw ();
	inline bool isDebugEnabled() const throw () {
		return isPriorityEnabled(::log4cpp::Priority::DEBUG);
	}
	;

	void info(const std::string& message, ::log4cpp::Priority::Value priority,
			const std::string& routine, const std::string& file,
			unsigned int line, const std::string& host,
			const std::string& context, const std::string& audience,
			const std::string& sourceObject, const std::string& array,
			const std::string& antenna, const std::string& stackId,
			const int stackLevel, const std::string& uri) throw ();
	inline bool isInfoEnabled() const throw () {
		return isPriorityEnabled(::log4cpp::Priority::INFO);
	}
	;

	void notice(const std::string& message, ::log4cpp::Priority::Value priority,
			const std::string& routine, const std::string& file,
			unsigned int line, const std::string& host,
			const std::string& context, const std::string& audience,
			const std::string& sourceObject, const std::string& array,
			const std::string& antenna, const std::string& stackId,
			const int stackLevel, const std::string& uri) throw ();
	inline bool isNoticeEnabled() const throw () {
		return isPriorityEnabled(::log4cpp::Priority::NOTICE);
	}
	;

	void warn(const std::string& message, ::log4cpp::Priority::Value priority,
			const std::string& routine, const std::string& file,
			unsigned int line, const std::string& host,
			const std::string& context, const std::string& audience,
			const std::string& sourceObject, const std::string& array,
			const std::string& antenna, const std::string& stackId,
			const int stackLevel, const std::string& uri) throw ();
	inline bool isWarnEnabled() const throw () {
		return isPriorityEnabled(::log4cpp::Priority::WARN);
	}
	;

	void error(const std::string& message, ::log4cpp::Priority::Value priority,
			const std::string& routine, const std::string& file,
			unsigned int line, const std::string& host,
			const std::string& context, const std::string& audience,
			const std::string& sourceObject, const std::string& array,
			const std::string& antenna, const std::string& stackId,
			const int stackLevel, const std::string& uri) throw ();
	inline bool isErrorEnabled() const throw () {
		return isPriorityEnabled(::log4cpp::Priority::ERROR);
	}
	;

	void crit(const std::string& message, ::log4cpp::Priority::Value priority,
			const std::string& routine, const std::string& file,
			unsigned int line, const std::string& host,
			const std::string& context, const std::string& audience,
			const std::string& sourceObject, const std::string& array,
			const std::string& antenna, const std::string& stackId,
			const int stackLevel, const std::string& uri) throw ();
	inline bool isCritEnabled() const throw () {
		return isPriorityEnabled(::log4cpp::Priority::CRIT);
	}
	;

	void alert(const std::string& message, ::log4cpp::Priority::Value priority,
			const std::string& routine, const std::string& file,
			unsigned int line, const std::string& host,
			const std::string& context, const std::string& audience,
			const std::string& sourceObject, const std::string& array,
			const std::string& antenna, const std::string& stackId,
			const int stackLevel, const std::string& uri) throw ();
	inline bool isAlertEnabled() const throw () {
		return isPriorityEnabled(::log4cpp::Priority::ALERT);
	}
	;

	void emerg(const std::string& message, ::log4cpp::Priority::Value priority,
			const std::string& routine, const std::string& file,
			unsigned int line, const std::string& host,
			const std::string& context, const std::string& audience,
			const std::string& sourceObject, const std::string& array,
			const std::string& antenna, const std::string& stackId,
			const int stackLevel, const std::string& uri) throw ();
	inline bool isEmergEnabled() const throw () {
		return isPriorityEnabled(::log4cpp::Priority::EMERG);
	}
	;

	static ACSCategory& getInstance(const std::string& name);

	static ACSCategory* exist(const std::string& name);

	void addAppender(log4cpp::Appender* appender) throw(std::invalid_argument);

	log4cpp::Appender* getAppender (const std::string &name) const;


private:
	ACSCategory(const Category& other);
	ACSCategory& operator=(const Category& other);
};
}
#endif /* LOGGINGACSCATEGORY_H_ */
