#ifndef ALARM_SYSTEM_INTERFACE_H
#define ALARM_SYSTEM_INTERFACE_H
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
* "@(#) $Id: AlarmSystemInterface.h,v 1.4 2009/10/08 10:21:01 acaproni Exp $"
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

#include "FaultState.h"
#include "ASIMessage.h"
#include "ASIConfiguration.h"
#include <vector>
#include <string>
#include "ace/Task.h"

namespace acsalarm
{
	/**
 	 * Alarm System interface for C++ alarm generators 
 	 * @author sharring
 	 * Based on cern's java implementation
 	 */
	class AlarmSystemInterface
	{
		public:

			AlarmSystemInterface() {};
			virtual ~AlarmSystemInterface() {};

			/**
		 	 * Set the source name.
		 	 * @param newSourceName the source name.
		 	 */	
			virtual void setSourceName(std::string newSourceName) { sourceName = newSourceName; }

			/**
		 	 * Get the source name.
		 	 * @return the source name.
		 	 */
			virtual std::string getSourceName() { return sourceName; }

			/**
		 	 * Set the host name.
		 	 * @param newHostName the source name.
		 	 */	
			virtual void setHostName(std::string newHostName) { hostName = newHostName; }

			/**
		 	 * Get the host name.
		 	 * @return the host name.
		 	 */
			virtual std::string getHostName() { return hostName; }

			/**
		 	 * Push a fault state.
		 	 * @param state the fault state change to push.
		 	 * @throws ASIException if the fault state can not be pushed.
		 	 */
			virtual void push(FaultState & state); //raises ASIException = 0;

			/**
		 	 * Push a collection of fault states.
		 	 * @param states
		 	 * @throws ASIException if the fault state collection can not be pushed.
		 	 */
			virtual void push(std::vector<FaultState> & states); // raises ASIException = 0;

			/**
		 	 * Push the set of active fault states.
		 	 * @param activeFaults the active fault states.
		 	 * @throws ASIException if the fault state active list can not be pushed.
		 	 */
			virtual void pushActiveList(std::vector<FaultState> & activeFaults); // raises ASIException = 0;

			/**
		 	 * Cleanup. Must be implemented by concrete sub classes; may be called by destructor or explicitly 
			 * by client; it's up to you how you wish the cleanup process to be initiated.
		 	 */
			virtual void close() = 0;

		protected:
			/** Sends the message to the alarm server; concrete classes must override this and use the communication mechanism of their choice.
			 *  For an example of a concrete class, see ACSLaser/laser-source-cpp/CERNAlarmSystemInterfaceProxy. 
			 */
			virtual bool publishMessage(ASIMessage msg) = 0;

			std::string sourceName;
			std::string hostName;
			acsalarm::ASIConfiguration configuration;

			// Synch the public methods because one source can be used
			// by different threads
			ACE_Recursive_Thread_Mutex m_mutex;

		private:
			/**
			 * Private method to push a collection of fault states, containing the
			 * logic which is common to both the push() and pushActiveList() methods.
			 *
 			 * @param states
 			 * @param backup whether we are sending 'backup' alarms or not. backup alarms
 			 *        are alarms in the active list that are sent on startup, when the source
 			 *        starts and periodically according to the expected backup frequency.
 			 *
 			 * @throws ASIException if the fault state collection can not be pushed.
 			 */
			void commonPush(std::vector<FaultState> & states, bool backup);
	};
};
#endif

