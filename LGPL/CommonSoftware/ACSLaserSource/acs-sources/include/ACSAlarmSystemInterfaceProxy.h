#ifndef ACS_ALARM_SYSTEM_INTERFACE_PROXY_H
#define ACS_ALARM_SYSTEM_INTERFACE_PROXY_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2006 
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
* "@(#) $Id$"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2006-07-12  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <logging.h>
#import <AlarmSystemInterface.h>
#import <FaultState.h>

using namespace Logging;

/**
 * Implementation of a source that log messages instead of sending msg
 * to the AlarmSrevice
 */
class ACSAlarmSystemInterfaceProxy: public laserSource::AlarmSystemInterface {
	private:
		/**
		 * The logger
		 */
		LoggingProxy* m_logger;
	
	virtual void close() {
	}
	
	public:
	
		ACSAlarmSystemInterfaceProxy(string name);
		
		virtual ~ACSAlarmSystemInterfaceProxy() {}
		/**
	 	 * Push a fault state.
	 	 * @param state the fault state change to push.
	 	 */
		virtual void push(laserSource::FaultState & state);
	
		/**
	 	 * Push a collection of fault states.
	 	 * @param states
	 	 */
		virtual void push(vector<laserSource::FaultState> & states);
	
		/**
	 	 * Push the set of active fault states.
	 	 * @param activeFaults the active fault states.
	 	 */
		virtual void pushActiveList(vector<laserSource::FaultState> & activeFaults);
};

#endif /*!ACS_ALARM_SYSTEM_INTERFACE_PROXY_H*/
