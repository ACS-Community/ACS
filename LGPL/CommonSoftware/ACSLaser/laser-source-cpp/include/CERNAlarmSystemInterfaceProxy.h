#ifndef ALARM_SYSTEM_INTERFACE_PROXY_H
#define ALARM_SYSTEM_INTERFACE_PROXY_H

#include "AlarmSystemInterface.h"
#include "AcsAlarmPublisher.h"

using acsalarm::AlarmSystemInterface;
using laserSource::AcsAlarmPublisher;

namespace laserSource
{
	class CERNAlarmSystemInterfaceProxy : public AlarmSystemInterface
	{
		public:
			CERNAlarmSystemInterfaceProxy();
			CERNAlarmSystemInterfaceProxy(string theSourceName);
			virtual ~CERNAlarmSystemInterfaceProxy();
			virtual void close();

		protected:
			bool CERNAlarmSystemInterfaceProxy::publishMessage(ASIMessage msg);
	
		private:
			// initialization logic used by the constructors
			void init();
			AcsAlarmPublisher * laserPublisher;
	};
};
#endif

