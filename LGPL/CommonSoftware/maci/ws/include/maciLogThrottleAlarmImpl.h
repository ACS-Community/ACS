#ifndef MACITHROTTLEALARM_H
#define MACITHROTTLEALARM_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2012 
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
* "@(#) $Id: maciLogThrottleAlarmImpl.h,v 1.1 2012/02/21 10:48:27 acaproni Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2012-02-20  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <string>

#include <acsContainerServices.h>
#include <loggingThrottleAlarmInterface.h>

namespace maci {

	/**
	 * The implementation of the LogThrottleAlarm.
	 *
	 * This object is sent to the LoggingProxy that will use it
	 * for raising and clearing alarms.
	 *
	 * Note thatthis object is own by the Container but used by the
	 * LoggingProxy so we need to build and hold a reference in the container.
	 * And we need to remove the pointer from the LoggingProxy before destring
	 * this object in the destructor of the Container.
	 */
	class LogThrottleAlarmImpl: public LogThrottleAlarm {

	public:
		/**
		 * Constructor
		 *
		 * @param The not NULL pointer to the ContainerServices
		 * @param The name of the container used as fault member
		 */
		LogThrottleAlarmImpl(const ContainerServices* containerSvcs, std::string name);

		/**
		 * Destructor
		 */
		virtual ~LogThrottleAlarmImpl() {}

		/**
		 * Raise an alarm when the alarm throttle inhibit the logging
		 *
		 * @see LogThrottleAlarm
		 */
		virtual void raiseLogThrottleAlarm();

		/**
		 * Clear the alarm when the alarm throttle allowed the logging
		 *
		 * see LogThrottleAlarm
		 */
		virtual void clearLogThrottleAlarm();

	private:
		/**
		 * Container services
		 */
		const ContainerServices* m_containerSvcs;
	};
};
#endif /*!MACITHROTTLEALARM_H*/
