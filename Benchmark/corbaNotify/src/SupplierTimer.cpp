/*
 * SupplierTimer.cpp
 *
 *  Created on: Sep 29, 2014
 *      Author: almamgr
 */

#include "SupplierTimer.h"
#include "pDataSupplier.h"

SupplierTimer::SupplierTimer(DataSupplier &supplier)
 : m_supplier(&supplier), m_lastNumEventsSent(0)
{
}

SupplierTimer::~SupplierTimer()
{
}

int SupplierTimer::handle_timeout(const ACE_Time_Value &current_time,
		const void *act)
{
	uint64_t numEventsSent = m_supplier->getNumEventsSent();
	ACE_DEBUG((LM_NOTICE, "%T Number of events sent: %Q [%Q]\n",
			(numEventsSent - m_lastNumEventsSent), numEventsSent));
	m_lastNumEventsSent = numEventsSent;
	return 0;
}
