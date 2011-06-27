#ifndef ALARMSMAP_H
#define ALARMSMAP_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2011 
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
* "@(#) $Id: AlarmsMap.h,v 1.3 2011/06/27 20:26:35 javarias Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2011-06-19  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <map>
#include "ace/Task.h"

#include "acscommonC.h"

#include <acsThread.h>

namespace acsalarm {

/**
 * The information stored in the map for each alarm
 */
struct AlarmInfo  {
	// The time the alarm event has been submitted for the last time
	time_t acsTime_m;

	// The state of the last submission
	// It is true if ACTIVE, false if TERMINATE
	bool active_m;

	/**
	 * Constructor
	 *
	 * @param isActive true if the state of the alarm is active
	 */
	AlarmInfo(const bool isActive);

	/**
	 * Copy constructor
	 */
	AlarmInfo(const AlarmInfo& ai);
};

/**
 * AlarmsMap is a collection of alarms to avoid resending an alarm if its state
 * did not change.
 * <P>
 * It stores alarm IDs and the time of their submission and offers
 * methods to tell if a submitted alarm has to be sent to the alarm
 * server or not.
 * <BR>
 * A alarm have to be sent to the AS if
 *   - it has never been sent before
 *   - its state changed
 *   - the last time the alarm has been sent is older then KEEP_ALARMS_TIME
 * <P>
 * Alarms older then a given number of seconds are deleted from the queue
 * so that they will be resend anyhow.
 * <P>
 *  Life cycle: start() has to be called before using objects of this class;
 *  shutdown() has to be called when terminated.
 *  After shutdown is called, the class does not accept any new alarm
 *  and always returns false in clear(...) and raise(...)
 */
class AlarmsMap: public ACS::Thread {
public:

	/**
	 * Constructor
	 */
	AlarmsMap();

	/**
	 * Add an active alarm in the map and check if it has to be sent to the
	 * alarm server by returning true.
	 *
	 * @param alarmID The ID of the alarm to raise
	 * @return true if the alarm was already present in the map (i.e.
	 * 				it must not be sent to the alarm server); false otherwise
	 * 				false is also returned if the object has been shut down
	 */
	bool raise(std::string alarmID);

	/**
	 * Lyfe cycle: this method has to be called before
	 * using objects from this class.
	 * <P>
	 * This method initialize the internal data structures
	 */
	void start();

	/**
	 * Lyfe cycle: this method has to be called after
	 * using objects from this class.
	 * <P>
	 * This method free all the allocated resources
	 */
	void shutdown();

	/**
	 * Add a terminate alarm in the map and check if it has to be sent to the
	 * alarm server by returning true.
	 *
	 * @param alarmID The ID of the alarm to raise
	 * @return true if the alarm was already present in the map (i.e.
	 * 				it must not be sent to the alarm server); false otherwise;
	 * 				false is also returned if the object has been shut down
	 */
	bool clear(std::string alarmID);

	/**
	 * The loop that removes the alarm older then KEEP_ALARMS_TIME
	 * from the map
	 */
	virtual void runLoop();

	/**
	 * @return the number of items in the map
	 */
	int size() { return alarmsMap.size(); }

	/**
	 * Copy of all the elements in the map in the passed vector.
	 *
	 * @param alarms the vector to store all the alarms in the map
	 */
	void getAllAlarms(std::vector<AlarmInfo> alarms);

	/**
	 * Clear the map
	 */
	void clear() { alarmsMap.clear(); }

private:
	// Alarms are stored in the que for the KEEP_ALARMS_TIME seconds
	// and then they are removed from the queue
	static const ACS::Time KEEP_ALARMS_TIME=30;

	// The map of the alarms
	//
	// The key is the alarm ID
	// The object is a pointer to an AlarmInfo (a pointer
	// because std::map does not support elements without
	// a default constructor while AlarmInfo needs at least the
	// state of the alarm
	std::map<std::string, AlarmInfo*> alarmsMap;

	// Synchronize access to the map
	ACE_Recursive_Thread_Mutex m_mutex;

	// true if the object has been shut down
	bool m_closed;

	/**
	 * Store the alarm in the map with the passed ID and
	 * activation state.
	 *
	 * @param alarmID The ID of the alarm
	 * @param state The state (true means ACTIVE)
	 * @return true If the alarm must be send to the alarm service
	 */
	bool alarmSet(std::string alarmID, bool state);
};
};


#endif /*!ALARMSMAP_H*/
