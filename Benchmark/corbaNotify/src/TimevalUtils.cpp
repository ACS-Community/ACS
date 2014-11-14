/*
 * TimevalUtils.cpp
 *
 *  Created on: Oct 2, 2014
 *      Author: almamgr
 */

#include "TimevalUtils.h"
#include <stddef.h>

void TimevalUtils::set_timeval(timeval &t,int64_t sec,int64_t msec)
{
	t.tv_sec = sec;
	t.tv_usec = msec;
}

void TimevalUtils::get_current_timeval(timeval &t)
{
	gettimeofday(&t, NULL);
}

uint64_t TimevalUtils::timeval_2_ms(const timeval &t)
{
	uint64_t ms;
	ms = t.tv_sec * SEC_2_MSEC + (t.tv_usec / SEC_2_MSEC);
	return ms;
}

int64_t TimevalUtils::diff_timeval(const timeval &end,const timeval &start)
{
	int64_t msec;
	msec = (end.tv_sec - start.tv_sec) * SEC_2_MSEC;
	msec += (end.tv_usec - start.tv_usec) / SEC_2_MSEC;
	return msec;
}

void TimevalUtils::ms_2_timeval(uint64_t t,timeval &tv)
{
	tv.tv_sec = t / SEC_2_MSEC;
	tv.tv_usec = (t - tv.tv_sec * SEC_2_MSEC) * SEC_2_MSEC;
}

