#ifndef ACS_ALARM_PUBLISHER_H
#define ACS_ALARM_PUBLISHER_H

#include "AlarmPublisher.h"
#include "AlarmSupplier.h"
#include <ace/Mutex.h>

namespace laserAlarmPublisher
{
	/*
	 * Concrete alarm publisher which uses ACS Notification Channel 
	 * for communication mechanism to send alarms to the laser alarm server.
	 * The Notification-Service-related functionality is encapsulated in
	 * a separate class (which is used by this class), AlarmSupplier. 
	 */
	class AcsAlarmPublisher: public AlarmPublisher
	{
		public:
			/*
 			 * Returns the singleton instance, creating it if necessary.
 			 */
			static AlarmPublisher* getInstance(string topicName);

			/*
 			 * Method to publish an alarm to the laser alarm server.
			 * @param msg the ASIMessage to publish.
			 */
			virtual bool publishAlarm(ASIMessage msg);
			virtual ~AcsAlarmPublisher();
	
		private:
			AlarmSupplier * getAlarmSupplier() { return alarmSupplier; }
			void setAlarmSupplier(AlarmSupplier * supplier) { alarmSupplier = supplier; }

			static AlarmPublisher * singletonInstance;
			static AlarmSupplier * alarmSupplier;
			static ACE_Mutex * singletonMutex;
			static ACE_Mutex * alarmSupplierMutex;
	
			// private constructor for singleton enforcement
			// TODO: also disable copy constructor, etc. to enforce singleton
			AcsAlarmPublisher(string topicName);
	};
};

#endif
