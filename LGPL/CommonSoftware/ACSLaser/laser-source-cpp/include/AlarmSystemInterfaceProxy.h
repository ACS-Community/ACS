#ifndef ALARM_SYSTEM_INTERFACE_PROXY_H
#define ALARM_SYSTEM_INTERFACE_PROXY_H

#include "AlarmSystemInterface.h"
#include "ASIMessage.h"
#include "ASIConfiguration.h"
#include "AlarmPublisher.h"
#include <orbsvcs/CosNotifyChannelAdminS.h>
#include <orbsvcs/CosNotifyCommC.h>
#include <orbsvcs/CosNamingC.h>

using laserAlarmPublisher::AlarmPublisher;

namespace laserSource
{
	class AlarmSystemInterfaceProxy : public AlarmSystemInterface
	{
		private:
			// The naming service
			CosNaming::NamingContext_ptr m_naming_p;
			
		public:
			AlarmSystemInterfaceProxy(CosNaming::NamingContext_ptr naming_p);
			AlarmSystemInterfaceProxy(string theSourceName,CosNaming::NamingContext_ptr naming_p);
			virtual ~AlarmSystemInterfaceProxy();
			void close();
			
			/**
		 	* Push a fault state.
		 	* @param state the fault state change to push.
		 	* @throws ASIException if the fault state can not be pushed.
		 	*/
			void push(FaultState & state); //raises ASIException = 0;

			/**
		 	* Push a collection of fault states.
		 	* @param states
		 	* @throws ASIException if the fault state collection can not be pushed.
		 	*/
			void push(vector<FaultState> & states); // raises ASIException = 0;

			/**
		 	* Push the set of active fault states.
		 	* @param activeFaults the active fault states.
		 	* @throws ASIException if the fault state active list can not be pushed.
		 	*/
			void pushActiveList(vector<FaultState> & activeFaults); // raises ASIException = 0;
	
		private:
			// sends the message to the alarm server
			bool publishMessageDLL(ASIMessage msg);

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
			void commonPush(vector<FaultState> & states, bool backup);

			string hostname;
			ASIConfiguration configuration;
			AlarmPublisher *laserPublisher;
	};
};
#endif
