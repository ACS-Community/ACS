#ifndef LOGGINGTHROTTLEALARM_H
#define LOGGINGTHROTTLEALARM_H
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
* "@(#) $Id: loggingThrottleAlarmInterface.h,v 1.3 2012/02/20 17:53:46 acaproni Exp $"
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
/**
 * The logging module is built much before alarms by the ACS/Makefile.
 * <P>
 * In order to send alarms when the log throttle reaches it maximum allowed number
 * of logs per second, we define here an interface whose implementation must be
 * provided later in maci.
 */
class LogThrottleAlarm {

protected:
	/**
	 * The Fault Family of the log throttle alarm
	 */
	const std::string m_faultFamily;

	/**
	 * The Fault Member of the log throttle alarm
	 */
	const std::string m_faultMember;

	/**
	 * The Fault Code of the log throttle alarm
	 */
	const int m_faultCode;

public:
	/**
	 * Constructor
	 *
	 * @param faultFamily The fault family of the alarm to raise/clear
	 * @param faultMember The fault member of the alarm to raise/clear
	 * @param faultCode   The fault code of the alarm to raise/clear
	 */

	LogThrottleAlarm(std::string faultFamily, std::string faultMember, int faultCode) :
		m_faultFamily(faultFamily), m_faultMember(faultMember), m_faultCode(faultCode) {}

	/**
	 * Virtual destructor
	 */
	virtual ~LogThrottleAlarm() {}

	/**
	 * Raise an alarm when the alarm throttle inhibit the logging
	 */
	virtual void raiseLogThrottleAlarm()=0;

	/**
	 * Clear the alarm when the alarm throttle allowed the logging
	 */
	virtual void clearLogThrottleAlarm()=0;
};

#endif /*!LOGGINGTHROTTLEALARM_H*/
