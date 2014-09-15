#ifndef TIMESPECUTILS_H
#define TIMESPECUTILS_H
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

#include <stdint.h>
#include <string>

bool operator<(const timespec &t1, const timespec &t2);

class TimespecUtils
{
public:
	static const uint64_t SEC_2_NSEC	= 1e+9;
	static const uint64_t SEC_2_100NSEC 	= 1e+7;
	static const double NSEC_2_100NSEC 	= 1e-2;
	static const double NSEC_2_SEC 		= 1e-9;
	static const double NSEC100_2_SEC	= 1e-7;

	static void set_timespec(timespec &t,int64_t sec,int64_t nsec);
	static void double_2_timespec(double value,timespec &t);
	static std::string timespec_2_str(const timespec &t);
	static timespec diff_timespec(const timespec &end, const timespec &start);
	static uint64_t timespec_2_100ns(const timespec &t);
	static void ns100_2_timespec(uint64_t t,timespec &ts);
	static void get_current_timespec(timespec &t);
};

#endif // TIMESPECUTILS
