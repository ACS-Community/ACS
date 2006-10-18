#ifndef ALARM_SYSTEM_INTERFACE_PROXY_H
#define ALARM_SYSTEM_INTERFACE_PROXY_H

#include "ASIMessage.h"
#include "ASIConfiguration.h"
#include "AlarmPublisher.h"
#include "ACSAlarmSystemInterface.h"
#include "ACSFaultState.h"

using laserAlarmPublisher::AlarmPublisher;

namespace laserSource
{
	class CERNAlarmSystemInterfaceProxy : public ACSAlarmSystemInterface
	{
		public:
			CERNAlarmSystemInterfaceProxy();
			CERNAlarmSystemInterfaceProxy(string theSourceName);
			virtual ~CERNAlarmSystemInterfaceProxy();
			void close();
			
			/**
		 	* Push a fault state.
		 	* @param state the fault state change to push.
		 	* @throws ASIException if the fault state can not be pushed.
		 	*/
			void push(ACSFaultState & state); //raises ASIException = 0;

			/**
		 	* Push a collection of fault states.
		 	* @param states
		 	* @throws ASIException if the fault state collection can not be pushed.
		 	*/
			void push(vector<ACSFaultState> & states); // raises ASIException = 0;

			/**
		 	* Push the set of active fault states.
		 	* @param activeFaults the active fault states.
		 	* @throws ASIException if the fault state active list can not be pushed.
		 	*/
			void pushActiveList(vector<ACSFaultState> & activeFaults); // raises ASIException = 0;
	
		private:
			// sends the message to the alarm server using directly instantiated class
			bool publishMessage(ASIMessage msg);

			// initialization logic used by the constructors
			void init();

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
			void commonPush(vector<ACSFaultState> & states, bool backup);

			string hostname;
			ASIConfiguration configuration;
			AlarmPublisher *laserPublisher;
	};
};
#endif
