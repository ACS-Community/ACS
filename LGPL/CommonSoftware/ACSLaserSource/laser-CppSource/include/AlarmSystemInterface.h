#ifndef ALARM_SYSTEM_INTERFACE_H
#define ALARM_SYSTEM_INTERFACE_H

#include <iostream>
#include <vector>
#include "FaultState.h"

using std::string;
using std::vector;

namespace laserSource
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
			virtual void push(FaultState & state) = 0; //raises ASIException = 0;

			/**
		 	 * Push a collection of fault states.
		 	 * @param states
		 	 * @throws ASIException if the fault state collection can not be pushed.
		 	 */
			virtual void push(vector<FaultState> & states) = 0; // raises ASIException = 0;

			/**
		 	 * Push the set of active fault states.
		 	 * @param activeFaults the active fault states.
		 	 * @throws ASIException if the fault state active list can not be pushed.
		 	 */
			virtual void pushActiveList(vector<FaultState> & activeFaults) = 0; // raises ASIException = 0;

		protected:

			string sourceName;
	};
};

#endif
