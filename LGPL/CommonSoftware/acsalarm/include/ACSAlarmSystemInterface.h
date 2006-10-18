#ifndef ACS_ALARM_SYSTEM_INTERFACE_H
#define ACS_ALARM_SYSTEM_INTERFACE_H
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
* acaproni  2006-08-16  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */
 
#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "ACSFaultState.h"

#include <vector>
#include <string>

using std::string;
using std::vector;

namespace acsalarm
{
	/**
 	 * Alarm System interface for C++ alarm generators 
 	 * @author sharring
 	 * Based on cern's java implementation
 	 */
	class ACSAlarmSystemInterface
	{
		public:

			ACSAlarmSystemInterface() {};
			virtual ~ACSAlarmSystemInterface() {};

			/**
		 	 * Set the source name.
		 	 * @param newSourceName the source name.
		 	 */	
			virtual void setSourceName(string newSourceName) { sourceName = newSourceName; }

			/**
		 	 * Get the source name.
		 	 * @return the source name.
		 	 */
			virtual string getSourceName() { return sourceName; }

			/**
		 	 * Close and deallocate resources.
		 	 */
			virtual void close() = 0;

			/**
		 	 * Push a fault state.
		 	 * @param state the fault state change to push.
		 	 * @throws ASIException if the fault state can not be pushed.
		 	 */
			virtual void push(ACSFaultState & state) = 0; //raises ASIException = 0;

			/**
		 	 * Push a collection of fault states.
		 	 * @param states
		 	 * @throws ASIException if the fault state collection can not be pushed.
		 	 */
			virtual void push(vector<ACSFaultState> & states) = 0; // raises ASIException = 0;

			/**
		 	 * Push the set of active fault states.
		 	 * @param activeFaults the active fault states.
		 	 * @throws ASIException if the fault state active list can not be pushed.
		 	 */
			virtual void pushActiveList(vector<ACSFaultState> & activeFaults) = 0; // raises ASIException = 0;

		protected:

			string sourceName;
	};
};
#endif
