#include "TimespecUtils.h"
#include <sstream>
#include <iostream>

void TimespecUtils::set_timespec(timespec &t,int64_t sec,int64_t nsec)
{
	t.tv_sec = sec;
	t.tv_nsec = nsec;
}

void TimespecUtils::double_2_timespec(double value,timespec &t)
{
	t.tv_sec = (time_t)value;
	t.tv_nsec = (value - (time_t)value) * SEC_2_NSEC;
}

std::string TimespecUtils::timespec_2_str(const timespec &t)
{
	double d = t.tv_nsec * NSEC_2_SEC;
	std::ostringstream ossDec;
	std::ostringstream oss;
	oss << t.tv_sec;
	ossDec << d;
	std::string sdec = ossDec.str();
	size_t pos = sdec.find_first_of(".");
	if(pos != std::string::npos && pos < (sdec.size() - 1))
	{
		oss << "." << sdec.substr(pos + 1);
	}
	return oss.str();
}

timespec TimespecUtils::diff_timespec(const timespec &end, const timespec &start)
{
	timespec temp;
	if ((end.tv_nsec - start.tv_nsec) < 0) 
	{
		temp.tv_sec = end.tv_sec - start.tv_sec - 1;
		temp.tv_nsec = SEC_2_NSEC + end.tv_nsec - start.tv_nsec;
	} else {
		temp.tv_sec = end.tv_sec - start.tv_sec;
		temp.tv_nsec = end.tv_nsec - start.tv_nsec;
	}
	return temp;
}

uint64_t TimespecUtils::timespec_2_100ns(const timespec &t)
{
	uint64_t ns;
	uint64_t sec = t.tv_sec;
	uint64_t nsec = t.tv_nsec;
	ns = (sec * SEC_2_100NSEC);
	ns += nsec * NSEC_2_100NSEC;
	return ns;
}

void TimespecUtils::ns100_2_timespec(uint64_t t,timespec &ts)
{
	uint64_t sec = t * NSEC100_2_SEC;
	ts.tv_sec = sec;
	ts.tv_nsec = (t - sec * SEC_2_100NSEC) * 100;
	//std::cout << "ns100_2_timespec: t=" << t << ", sec=" << sec << ", nsec=" << (t - sec * SEC_2_100NSEC) << std::endl;
}

bool operator<(const timespec &t1, const timespec &t2)
{
//	double ns1 = t1.tv_nsec * TimespecUtils::NSEC_2_SEC;
//	double ns2 = t2.tv_nsec * TimespecUtils::NSEC_2_SEC;

//	std::cout << "op< " << TimespecUtils::timespec_2_str(t1) <<  " < " << TimespecUtils::timespec_2_str(t2) 
//		<< "  ->  nsec1=" << t1.tv_nsec << ", nsec2=" << t2.tv_nsec << std::endl;

	if(t1.tv_sec < t2.tv_sec)
	{
		return true;
	} else if(t1.tv_sec > t2.tv_sec) {
		return false;
	//} else if(ns1 < ns2) {
	} else if(t1.tv_nsec < t2.tv_nsec) {
		//std::cout << "   nsec1=" << t1.tv_nsec << " < " << " nsec2=" << t2.tv_nsec << std::endl;
		return true;
	}
	return false;
}

void TimespecUtils::get_current_timespec(timespec &t)
{
	//clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &t);
	clock_gettime(CLOCK_REALTIME, &t);
}
