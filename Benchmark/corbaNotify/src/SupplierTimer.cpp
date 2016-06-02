/*
 * SupplierTimer.cpp
 *
 *  Created on: Sep 29, 2014
 *      Author: almamgr
 */

#include "SupplierTimer.h"
#include "pDataSupplier.h"

SupplierTimer::SupplierTimer(DataSupplier &supplier)
 : m_supplier(&supplier), m_lastNumEventsSent(0), m_lastNumEventsSentOk(0), m_lastNumEventsSentErrTimeout(0), 
	m_lastNumEventsSentErrTransient(0), m_lastNumEventsSentErrObjNotExist(0), m_lastNumEventsSentErrCommFailure(0), 
	m_lastNumEventsSentErrUnknown(0)
{
}

SupplierTimer::~SupplierTimer()
{
}

int SupplierTimer::handle_timeout(const ACE_Time_Value &current_time,
		const void *act)
{
	uint64_t numEventsSent = m_supplier->getNumEventsSent();
	uint64_t numEventsSentOk = m_supplier->getNumEventsSentOk();
	uint64_t numEventsSentErrTimeout = m_supplier->getNumEventsSentErrTimeout();
	uint64_t numEventsSentErrTransient = m_supplier->getNumEventsSentErrTransient();
	uint64_t numEventsSentErrObjNotExist = m_supplier->getNumEventsSentErrObjNotExist();
	uint64_t numEventsSentErrCommFailure = m_supplier->getNumEventsSentErrCommFailure();
	uint64_t numEventsSentErrUnknown = m_supplier->getNumEventsSentErrUnknown();

	ACE_DEBUG((LM_NOTICE, "%T Number of events sent: %Q [%Q], OK=(%Q [%Q]), TIMEOUT=(%Q [%Q]), TRANSIENT=(%Q [%Q]), OBJECT_NOT_EXIST=(%Q [%Q]), COMM_FAILURE=(%Q [%Q]), UNKNOWN_ERR=(%Q [%Q])\n",
			(numEventsSent - m_lastNumEventsSent), numEventsSent,
			(numEventsSentOk - m_lastNumEventsSentOk), numEventsSentOk,
			(numEventsSentErrTimeout - m_lastNumEventsSentErrTimeout), numEventsSentErrTimeout,
			(numEventsSentErrTransient - m_lastNumEventsSentErrTransient), numEventsSentErrTransient,
			(numEventsSentErrObjNotExist - m_lastNumEventsSentErrObjNotExist), numEventsSentErrObjNotExist,
			(numEventsSentErrCommFailure - m_lastNumEventsSentErrCommFailure), numEventsSentErrCommFailure,
			(numEventsSentErrUnknown - m_lastNumEventsSentErrUnknown), numEventsSentErrUnknown
	));

	m_lastNumEventsSent = numEventsSent;
	m_lastNumEventsSentOk = numEventsSentOk;
	m_lastNumEventsSentErrTimeout = numEventsSentErrTimeout;
	m_lastNumEventsSentErrTransient = numEventsSentErrTransient;
	m_lastNumEventsSentErrObjNotExist = numEventsSentErrObjNotExist;
	m_lastNumEventsSentErrCommFailure = numEventsSentErrCommFailure;
	m_lastNumEventsSentErrUnknown = numEventsSentErrUnknown;
	return 0;
}
