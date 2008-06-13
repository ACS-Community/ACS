#ifndef ALARM_SYSTEM_INTERFACE_PROXY_H
#define ALARM_SYSTEM_INTERFACE_PROXY_H

#include "AlarmSystemInterface.h"
#include "AcsAlarmPublisher.h"

namespace laserSource
{
	class CERNAlarmSystemInterfaceProxy : public acsalarm::AlarmSystemInterface
	{
		public:
			CERNAlarmSystemInterfaceProxy();
			CERNAlarmSystemInterfaceProxy(std::string theSourceName);
			virtual ~CERNAlarmSystemInterfaceProxy();
			virtual void close();

		protected:
			bool publishMessage(acsalarm::ASIMessage msg);
	
		private:
			// initialization logic used by the constructors
			void init();

			// pointer to our publisher object
			laserSource::AcsAlarmPublisher * laserPublisher;

			// smart ptr to the logger
			Logging::Logger::LoggerSmartPtr myLoggerSmartPtr;
	};
};
#endif

