#ifndef ACS_ALARMSOURCEIMPL_H
#define ACS_ALARMSOURCEIMPL_H
/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2011
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 *    MA 02111-1307  USA
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <map>
#include <set>
#include <memory>

#include "AlarmSource.h"
#include "ACSAlarmSystemInterfaceFactory.h"
#include "AlarmToQueue.h"
#include "AlarmsMap.h"

#include <acsThread.h>

#include "maciS.h"

#include "ace/Task.h"

namespace acsalarm
{

	/**
	 * An implementation of AlarmSource.
	 * <P>
	 * This class uses the ACSAlarmInterfaceFactory to publish alarms.
	 * It assumes that the ACSAlarmInterfaceFactory is already initialized before using
	 * this class. In the same way, it assumes that  ACSAlarmInterfaceFactory is shutdown
	 * after terminated using objects from this class.
	 * At the present, this is usually done by ACS for example for the components and the clients.
	 * <P>
	 * It is possible to send alarms with several overloaded methods like raise(...), clear(...) and
	 * set(...). All the overloaded methods delegate to set(...).
	 * internalAlarmSend(..) does the real sending of alarms to the alarm server.
	 *
	 * @see AlarmSource
	 * @author acaproni
	 */
	class AlarmSourceImpl: public AlarmSource, ACS::Thread {
	private:
		// true if the sending of alarms has been disabled
		bool m_disabled;

		// Signal if the object is locally queuing alarms instead of
		// sending them immediately to the alarm server
		bool m_queuing;

		// Synchronize access to shared vars
		ACE_Recursive_Thread_Mutex m_mutex;

		/**
		 * The map of queued alarms
		 * <P>
		 * The key is the ID of the alarm
		 */
		std::map<std::string, AlarmToQueue*> m_queue;

		/**
		 * The alarms that have been activated and not yet terminated.
		 * <P>
		 * This is used by the terminateAllAlarms()
		 */
		std::set<std::string> m_activatedAlarms;

		/**
		 * The map to avoid repeating alarms if their states did
		 * not change.
		 *
		 * The map requires to be updated from time to time
		 * (@see AlarmsMap documentation). The updating is explicitly requested
		 * by the thread loop.
		 */
		AlarmsMap m_alarms;

		/**
		 * The updating of the AlarmMap m_alarms is not done
		 * in each loop of the thread but every 10 iterations
		 * i.e. once per second.
		 */
		int m_updateMap;

		/**
		 * The source to send alarms to the AS
		 */
		auto_ptr<acsalarm::AlarmSystemInterface> m_alarmSource_ap;

	public:

		/**
		 * Constructor
		 *
		 * @param manager The ACS manager
		 */
		AlarmSourceImpl();

		/**
		 * Destructor
		 */
		virtual ~AlarmSourceImpl();

		/**
		 * @see AlarmSource
		 */
		void raiseAlarm(
				std::string faultFamily,
				std::string faultMember,
				int faultCode);

		/**
		 * @see AlarmSource
		 */
		void raiseAlarm(
				std::string faultFamily,
				std::string faultMember,
				int faultCode,
				Properties properties);

		/**
		 * @see AlarmSource
		 */
		void clearAlarm(
				std::string faultFamily,
				std::string faultMember,
				int faultCode);

		/**
		 * @see AlarmSource
		 */
		void setAlarm(
				std::string faultFamily,
				std::string faultMember,
				int faultCode,
				Properties alarmProps,
				bool active);

		/**
		 * @see AlarmSource
		 */
		void setAlarm(
				std::string faultFamily,
				std::string faultMember,
				int faultCode,
				bool active);

		/**
		 * @see AlarmSource
		 */
		void terminateAllAlarms();

		/**
		 * @see AlarmSource
		 */
		void queueAlarms(ACS::TimeInterval time);

		/**
		 * @see AlarmSource
		 */
		void queueAlarms();

		/**
		 * @see AlarmSource
		 */
		void flushAlarms();

		/**
		 * @see AlarmSource
		 */
		void disableAlarms();

		/**
		 * @see AlarmSource
		 */
		void enableAlarms();

		/**
		 * @see ACS::Thread
		 */
		void runLoop();

		/**
		 * @see AlarmSource
		 */
		void start();

		/**
		 * @see AlarmSource
		 */
		void tearDown();
	private:
		/**
		 * Build the ID of an alarm from the triplet
		 *
		 * @param faultFamily The Fault Family
		 * @param faultMember The Fault Member
		 * @param faultCode The Fault Code
		 */
		std::string buildAlarmID(std::string faultFamily, std::string faultMember, int faultCode);

		/**
		 * Send an alarm to the alarm server.
		 *
		 * @param faultFamily The FaultFamily
		 * @param faultMember The FaultMember
		 * @param faultCode The FaultCode
		 * @param alarmProps The user properties.
		 *                   It can be <code>null</code> or empty.
		 * @param active if <code>true</code> the alarm is activated
		 * 				 otherwise it is terminated.
		 */
		void internalAlarmSender(
				std::string faultFamily,
				std::string faultMember,
				int faultCode,
				Properties alarmProps,
				bool active);
	};

}

#endif // ACS_ALARMSOURCEIMPL_H
