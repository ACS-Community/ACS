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
#include "AlarmSystemInterface.h"
#include "FaultState.h"

/**
 * Implementation of a source that log messages instead of sending msg
 * to the AlarmSrevice
 */
class ACSAlarmSystemInterfaceProxy: public acsalarm::AlarmSystemInterface {
	private:
		virtual void close() {}
	
	public:
	
		ACSAlarmSystemInterfaceProxy(std::string name);
		
		virtual ~ACSAlarmSystemInterfaceProxy();
		/**
	 	 * Push a fault state.
	 	 * @param state the fault state change to push.
	 	 */
		virtual void push(acsalarm::FaultState & state);
	
		/**
	 	 * Push a collection of fault states.
	 	 * @param states
	 	 */
		virtual void push(std::vector<acsalarm::FaultState> & states);
	
		/**
	 	 * Push the set of active fault states.
	 	 * @param activeFaults the active fault states.
	 	 */
		virtual void pushActiveList(std::vector<acsalarm::FaultState> & activeFaults);

	protected:
		virtual bool publishMessage(acsalarm::ASIMessage){ return true; /* NOOP: nothing needed here */ }
};

#endif 

