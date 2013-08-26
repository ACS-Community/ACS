#ifndef ALARMSOURCETHREAD_H
#define ALARMSOURCETHREAD_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2012 
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
* "@(#) $Id: AlarmSourceThread.h,v 1.1 2012/04/26 12:35:42 acaproni Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2012-04-24  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <set>
#include "ace/Task.h"

#include <acsThread.h>

#include "AlarmSource.h"

namespace acsalarm {

	/**
	 * AlarmSource and AlarmsMap require to be updated in a loop, this was initially
	 * done by one thread for each object that means 2 more threads for each component.
	 *
	 * Together with other strategies, we decided to have only one thread for updating
	 * all the AlarmSources running in a container.
	 * Each AlarmSource object that wants to be updated, must register itself for updating and
	 * deregister before being deleted.
	 * <P>
	 * The AlarmSourceThread does not take ownership of the AlarmSource pointers
	 * passed to the register and deregister methods. This means that the memory management
	 * of such objects is responsibility of the caller. In particular the AlarmSource object
	 * a pointer refers to must be instantiated before being passed to registerForUpdating(..)
	 * and deleted only after being deregistered with unregisterFromUpdating(...).
	 * The pointers passed to registerForUpdating(..) and to unregisterFromUpdating(..) can't be NULL.
	 * <P>
	 * The fail-fast approach is used for failures in runLoop() (@see AlarmSourceThread::runLoop())
	 * <P>
	 * The thread runs in a loop regardless of how many AlarmSoource objects it updates.
	 * This means that if there are many objects the thread does not update them with a precise
	 * time interval and the AlarmSource must take that in account i.e. rely on the actual
	 * time instead of, for example, counting the iterations.
	 */
	class AlarmSourceThread: public ACS::Thread {
	private:

		// Synchronize access to shared vars
		ACE_Recursive_Thread_Mutex m_mutex;

		std::set<AlarmSource*> m_alarmSources;

	public:

		/**
		 * Constructor
		 */
		AlarmSourceThread();

		/**
		 * Destructor
		 */
		virtual ~AlarmSourceThread();

		/**
		 * The thread to update the AlrmSource objects at regular intervals.
		 * <P>
		 * runLoop iterates through all the registered AlarmSource's and invoke AlarmSource::update().
		 * The AlarmSource::update() method is called in a try/catch block to detect problems in the
		 * AlarmSource's. In this case we log a message and throw the exception to a higher level (fail-fast).
		 * If such an error happens then we are loosing an important functionality (the ability to set/clear alarms)
		 * and we want to know it immediately and fix.
		 *
		 * @see ACS::Thread
		 */
		void runLoop();

		/**
		 * Register the AlarmSource for updating
		 *
		 * @param src The not NULL AlarmSource to be updated
		 * @return true if the element has been effectively registered or false otherwise
		 */
		bool registerForUpdating(AlarmSource* src);

		/**
		 * Unregister the AlarmSource from the updating thread.
		 *
		 * @param src The not NULL AlarmSource to be updated
		 * @return true if the element has been effectively unregistered or false otherwise
		 */
		bool unregisterFromUpdating(AlarmSource* src);
	};
}

#endif /*!ALARMSOURCETHREAD_H*/
