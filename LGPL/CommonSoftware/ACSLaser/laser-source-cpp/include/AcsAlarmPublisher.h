#ifndef ACS_ALARM_PUBLISHER_H
#define ACS_ALARM_PUBLISHER_H

#include "AlarmSupplier.h"
#include "ASIMessage.h"

namespace laserSource
{
	/*
	 * Concrete alarm publisher which uses ACS Notification Channel
	 * for communication mechanism to send alarms to the laser alarm server.
	 * The Notification-Service-related functionality is encapsulated in
	 * a separate class (which is used by this class), AlarmSupplier.
	 */
	class AcsAlarmPublisher
	{
		public:
			AcsAlarmPublisher(std::string topicName);
			virtual ~AcsAlarmPublisher();

			/*
 			 * Method to publish an alarm to the laser alarm server.
			 * @param msg the ASIMessage to publish.
			 * @throw In case of error publishing the message
			 */
			virtual void publishAlarm(acsalarm::ASIMessage msg);

		private:
			AlarmSupplier * alarmSupplier;

			/**
			 * Get the NameService from the manager
			 *
			 * @throw ACSErrTypeCORBA::CORBAReferenceNilExImpl
			 * @throw ACSErrTypeCORBA::NarrowFailedExImpl
			 */
			CosNaming::NamingContext_var getNamingService();

			/**
			 * The reference to the naming service is shared between different instances
			 * otherwise it is retrieved from the manager for each published alarm
			 *
			 * This variable must not be used directly. The naming service should be
			 * accessed by calling getNamingService()
			 */
			static CosNaming::NamingContext_var naming_v;
	};
};

#endif
