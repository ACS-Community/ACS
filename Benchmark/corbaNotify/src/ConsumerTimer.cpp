/*
 * ConsumerTimer.cpp
 *
 *  Created on: Sep 29, 2014
 *      Author: almamgr
 */

#include "ConsumerTimer.h"
#include "pDataConsumer.h"

ConsumerTimer::ConsumerTimer(Consumer &consumer)
 : m_consumer(&consumer), m_lastNumEventsReceived(0)
{
}

ConsumerTimer::~ConsumerTimer()
{
}

int ConsumerTimer::handle_timeout(const ACE_Time_Value &current_time,
		const void *act)
{
	uint64_t numEventsReceived = m_consumer->getNumEventsReceived();

	ACE_DEBUG((LM_NOTICE, "%T Number of events received: %Q [%Q]\n",
			(numEventsReceived - m_lastNumEventsReceived), numEventsReceived ));

	m_lastNumEventsReceived = numEventsReceived;
	return 0;
}
