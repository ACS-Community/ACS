#ifndef _TaskServices_H
#define _TaskServices_H

/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2005 
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
* who       when       what
* --------  --------   ----------------------------------------------
* sharring  2005-07-14 created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

// loggingMACROS.h in turn includes loggingLogger.h (among others)
// NOTE: the headers included from loggingMACROS.h are ACS/ACE independent
#include <loggingMACROS.h>

namespace ACS 
{
	const char* const TASK_LOGGER_NAME = "TaskLogger";

	/**
	 * The TaskServices class is the base class for all task services,
	 * i.e. services that are needed/used by a task. ACS tasks (e.g.
	 * tasks which extend parameterTask) can use this class as-is by 
	 * linking with the parameterTask library, which in turn links with the 
	 * standard ACS logging library, liblogging. This introduces dependencies
	 * on ACE/TAO which may be undesirable in some cases. In cases where 
	 * dependencies on ACE/TAO are undesirable, decoupling can be achieved in
	 * the following way(s):
	 *
	 * 1) Add the following to your code which initializes things (top of main() for example):
	 * #include <loggingGenericLogger.h>
	 * 
	 * if (Logging::Logger::getGlobalLogger() == 0)
	 * { 
	 *   Logging::Logger::setGlobalLogger(new Logging::GenericLogger(Logging::BaseLog::GLOBAL_LOGGER_NAME));
	 * }
	 * 
	 * Another option is to set the global Logger to be a subclass of Logger 
	 * that you have written.
	 * 
	 * 2) link with the baselogging library instead of the logging library 
	 *    (or instead of the parameterTask library which links with the logging library)
	 * 
	 * In so doing, you would then be completely decoupled from ACE/TAO for 
	 * the purposes of logging.
	 *
	 * So, to summarize:
	 *
	 *		-> To use this class in an ACS-aware manner, you need only to link with the
	 *		   parameterTask library (or the logging library) and use the standard ACS_XYZ logging
	 *                 macros.
	 *
	 *		-> To use this class in an ACS-decoupled manner, you must avoid linking with parameterTask
	 *		   (and/or the logging library), instead linking with the baselogging library and setting your
	 *                 own global logger using the Logging::Logger::setGlobalLogger static method. Additionally,
	 *                 you will currently be limited to using macros defined in loggingMACROS.h.
	 */
	class TaskServices
	{
		public:

			/**
			 * Constructor, which creates the logger.
			 */
			TaskServices() 
			{
				m_logger = getNamedLogger(TASK_LOGGER_NAME);
			}

			/**
			 * Destructor, which must be virtual.
			 */
			virtual ~TaskServices() {}

			/**
			 * The method to acquire a logger for the task. 
			 */
			virtual Logging::Logger::LoggerSmartPtr getLogger() { return m_logger; }; 

		private:
			Logging::Logger::LoggerSmartPtr m_logger; 
	};
}; 
#endif /*!_H*/
