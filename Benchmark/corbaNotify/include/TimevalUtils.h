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
#ifndef TIMEVALUTILS_H_
#define TIMEVALUTILS_H_

#include <stdint.h>
#include <sys/time.h>

class TimevalUtils {
public:
	static const int32_t SEC_2_MSEC = 1e+3;

	TimevalUtils();
	virtual ~TimevalUtils();

	static void set_timeval(timeval &t,int64_t sec,int64_t msec);
	static void get_current_timeval(timeval &t);
	static uint64_t timeval_2_ms(const timeval &t);
	static int64_t diff_timeval(const timeval &end,const timeval &start);
	static void ms_2_timeval(uint64_t t,timeval &tv);

};

#endif /* TIMEVALUTILS_H_ */
