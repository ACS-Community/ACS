/*
 * SupplierTimer.h
 *
 *  Created on: Sep 29, 2014
 *      Author: almamgr
 */

#ifndef SUPPLIERTIMER_H_
#define SUPPLIERTIMER_H_

#include <Event_Handler.h>
#include <stdint.h>

class DataSupplier;

class SupplierTimer : public ACE_Event_Handler {
public:
	SupplierTimer(DataSupplier &supplier);
	virtual ~SupplierTimer();

	virtual int handle_timeout(const ACE_Time_Value &current_time,
			const void *act);

private:
	DataSupplier *m_supplier;
	uint64_t m_lastNumEventsSent;
};

#endif /* SUPPLIERTIMER_H_ */
