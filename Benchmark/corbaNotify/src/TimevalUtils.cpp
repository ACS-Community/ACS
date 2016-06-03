/*
 * TimevalUtils.cpp
 *
 *  Created on: Oct 2, 2014
 *      Author: almamgr
 */

#include "TimevalUtils.h"
#include <stddef.h>


const std::string TimevalUtils::STR_TIMEOUT_ORB="orb";
const std::string TimevalUtils::STR_TIMEOUT_THREAD="thread";
const std::string TimevalUtils::STR_TIMEOUT_PROXY="proxy";

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

void TimevalUtils::fillTimeout(TimeoutMS &timeout,const std::string &config)
{
    
    std::string substr = config;
    std::string keyValue;
    std::string key;
    std::string value;
    uint32_t iValue;
    std::size_t pos = substr.find_first_of(",");
    if(pos == std::string::npos)
    {
        pos = substr.size();
    }
    std::size_t pos2;
    while(false == substr.empty())
    {
        keyValue = substr.substr(0, pos);
        pos2 = keyValue.find_first_of(":");
        if(pos2 != std::string::npos)
        {
            key = keyValue.substr(0,pos2);
            value = keyValue.substr(pos2+1);
            if(value.find_first_not_of("0123456789") == std::string::npos)
            {
                iValue = atoi(value.c_str());
                if(key == STR_TIMEOUT_ORB)
                {
                    timeout.orb = iValue;
                } else if(key == STR_TIMEOUT_THREAD) {
                    timeout.thread = iValue;
                } else if(key == STR_TIMEOUT_PROXY) {
                    timeout.proxy = iValue;
                }    
            }    
        }
        if(substr.size() > pos + 1)
        {
            substr = substr.substr(pos + 1);
        } else {
            substr = std::string();
        }
        pos = substr.find_first_of(",");
        if(pos == std::string::npos)
        {
            pos = substr.size();
        }
    }
}
