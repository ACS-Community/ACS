#ifndef ACS_ALARM_PUBLISHER_H
#define ACS_ALARM_PUBLISHER_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) National Research Council of Canada, 2005
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
*/
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
