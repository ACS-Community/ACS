#ifndef ACS_ALARM_PUBLISHER_H
#define ACS_ALARM_PUBLISHER_H

#include "AbstractAlarmPublisher.h"
#include "AlarmSupplier.h"

//using laserSource::AbstractAlarmPublisher;

namespace laserSource
{
	/*
	 * Concrete alarm publisher which uses ACS Notification Channel 
	 * for communication mechanism to send alarms to the laser alarm server.
	 * The Notification-Service-related functionality is encapsulated in
	 * a separate class (which is used by this class), AlarmSupplier. 
	 */
	class AcsAlarmPublisher //: public AbstractAlarmPublisher
	{
		public:
			AcsAlarmPublisher(string topicName);
			virtual ~AcsAlarmPublisher();

			/*
 			 * Method to publish an alarm to the laser alarm server.
			 * @param msg the ASIMessage to publish.
			 */
			virtual bool publishAlarm(ASIMessage msg);
	
		private:
			AlarmSupplier * getAlarmSupplier() { return alarmSupplier; }
			void setAlarmSupplier(AlarmSupplier * supplier) { alarmSupplier = supplier; }

			AlarmSupplier * alarmSupplier;
	};
};

#endif
