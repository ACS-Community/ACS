/*
 * ConsumerTimer.h
 *
 *  Created on: Sep 29, 2014
 *      Author: almamgr
 */

#ifndef CONSUMERTIMER_H_
#define CONSUMERTIMER_H_
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2014
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
*
* who       when      what
* --------  --------  ----------------------------------------------
* almadev  2014-08-28  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#include <Event_Handler.h>

class Consumer;

class ConsumerTimer : public ACE_Event_Handler {
public:
	ConsumerTimer(Consumer &consumer);
	virtual ~ConsumerTimer();

	virtual int handle_timeout(const ACE_Time_Value &current_time,
			const void *act);

protected:
	Consumer *m_consumer;
	uint64_t m_lastNumEventsReceived;
};

#endif /* CONSUMERTIMER_H_ */
