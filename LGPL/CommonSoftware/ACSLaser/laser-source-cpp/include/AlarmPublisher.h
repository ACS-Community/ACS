#ifndef ALARM_PUBLISHER_H
#define ALARM_PUBLISHER_H

#include <string>
#include "ASIMessage.h"

using laserSource::ASIMessage;

namespace laserAlarmPublisher
{
	/*
	 * Abstract base class for sending alarms to the laser alarm server.
	 * Concrete implementations, e.g. AcsAlarmPublisher, will extend this class.
	 */
	 class AlarmPublisher
	{
		public:
			/*
			 * Method to publish an alarm to the laser alarm server.
			 * @param msg the ASIMessage to publish.
			 */
			virtual bool publishAlarm(ASIMessage msg) = 0;
			virtual ~AlarmPublisher();
	};
};

#endif
