/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2011
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
* "@(#) $Id: acstimeDurationHelper.cpp,v 1.17 2011/10/28 15:12:04 hsommer Exp $"
*/
//------------------------------------------------------------------------------
#include "acstimeDurationHelper.h"
#include <iomanip>

using namespace std;

//------------------------------------------------------------------------------
using namespace ACSTimeError;

DurationHelper::DurationHelper(const acstime::Duration &inValue) :
    TimeUtil()
{
    this->value(inValue);
}
//------------------------------------------------------------------------------
DurationHelper::DurationHelper() :
    TimeUtil()
{
    this->reset();
}
//------------------------------------------------------------------------------
DurationHelper::DurationHelper(long double seconds) :
    TimeUtil()
{
    this->value(seconds);
}
//------------------------------------------------------------------------------
DurationHelper::DurationHelper(const ACS::TimeInterval &inValue) :
    TimeUtil()
{
    this->value(inValue);
}
//------------------------------------------------------------------------------
acstime::Duration 
DurationHelper::value()
{    
    return value_m;
}
//------------------------------------------------------------------------------
void 
DurationHelper::value(const acstime::Duration &value)
{   
    value_m = value;
    m_toAttributes();
}
//------------------------------------------------------------------------------
void 
DurationHelper::value(const ACS::TimeInterval &val)
{   
    acstime::Duration tDuration;
    tDuration.value = val;
    value(tDuration);
}
//------------------------------------------------------------------------------
void 
DurationHelper::value(long double seconds)
{   
    value(static_cast<ACS::TimeInterval>(seconds * 1e7));
}
//------------------------------------------------------------------------------
CORBA::Boolean 
DurationHelper::positive()
{
    return positive_m;
}
//------------------------------------------------------------------------------
void 
DurationHelper::positive(const CORBA::Boolean &value)   
{
    positive_m = value;
    m_toValue();
}
//------------------------------------------------------------------------------

CORBA::Long 
DurationHelper::day()    
{
    return day_m;
}
//------------------------------------------------------------------------------
void 
DurationHelper::day(const CORBA::Long &value)
{
    day_m = value;
    m_toValue();
}
//------------------------------------------------------------------------------
CORBA::Long
DurationHelper::hour()    
{   
    return hour_m;
}
//------------------------------------------------------------------------------
void 
DurationHelper::hour(const CORBA::Long &value)
{
    hour_m = value;
    m_toValue();
}
//------------------------------------------------------------------------------
CORBA::Long
DurationHelper::minute()    
{
    return minute_m;
}
//------------------------------------------------------------------------------
void 
DurationHelper::minute(const CORBA::Long &value)
{
    minute_m = value;
    m_toValue();
}
//------------------------------------------------------------------------------
CORBA::Long
DurationHelper::second()   
{
    return second_m;
}
//------------------------------------------------------------------------------
void 
DurationHelper::second(const CORBA::Long &value)
{
    second_m = value;
    m_toValue();
}
//------------------------------------------------------------------------------
CORBA::ULong 
DurationHelper::microSecond()
{    
    return microSecond_m;
}
//------------------------------------------------------------------------------
void 
DurationHelper::microSecond(const CORBA::ULong &value)
{
    microSecond_m = value;
    m_toValue();
}
//------------------------------------------------------------------------------
CORBA::Boolean 
DurationHelper::normalize()    
{
    return normalize_m;
}
//------------------------------------------------------------------------------
void 
DurationHelper::normalize(const CORBA::Boolean &value)
{
    normalize_m = value;
}
//------------------------------------------------------------------------------
void 
DurationHelper::reset()    
{
    value_m.value = 0LL;
    positive_m = true;
    day_m = 0;
    hour_m = 0;
    minute_m = 0;
    second_m = 0;
    microSecond_m = 0;
    normalize_m = false;
}
//------------------------------------------------------------------------------
acstime::TimeComparison 
DurationHelper::compare(const acstime::Duration &comparee)
{
    if (comparee.value == value_m.value)
        {
	return acstime::TCEqualTo;
        }
    if (comparee.value > value_m.value)
        {
	return acstime::TCGreaterThan;
        }
    return acstime::TCLessThan;
}
//------------------------------------------------------------------------------
CORBA::Boolean 
DurationHelper::operator==(const acstime::Duration &duration) const
{
    return (value_m.value == duration.value);
}
//------------------------------------------------------------------------------
CORBA::Boolean
DurationHelper::operator<=(const acstime::Duration &duration) const
{
    return (value_m.value <= duration.value);
}
//------------------------------------------------------------------------------
CORBA::Boolean 
DurationHelper::operator<(const acstime::Duration &duration) const
{
    return (value_m.value < duration.value);
}
//------------------------------------------------------------------------------
CORBA::Boolean 
DurationHelper::operator>=(const acstime::Duration &duration) const
{
    return (value_m.value >= duration.value);
}
//------------------------------------------------------------------------------
CORBA::Boolean 
DurationHelper::operator>(const acstime::Duration &duration) const
{
    return (value_m.value > duration.value);
}
//------------------------------------------------------------------------------
void 
DurationHelper::add(const acstime::Duration &additive)
{
   long long sum = value_m.value + additive.value;
   if ((value_m.value > 0 && additive.value > 0 && sum < 0)
    || (value_m.value < 0 && additive.value < 0 && sum > 0))
       {
       throw (OverflowOrUnderflowExImpl(__FILE__, __LINE__, "DurationHelper::add"));
       }
    value_m.value = sum;
    m_toAttributes();
}
//------------------------------------------------------------------------------
DurationHelper& 
DurationHelper::operator+=(const acstime::Duration &additive)
{
    this->add(additive);
    return *this;
}
//------------------------------------------------------------------------------
void 
DurationHelper::subtract(const acstime::Duration &subtrahend)
{
    long long remainder = value_m.value - subtrahend.value;
    if ((value_m.value > 0 && subtrahend.value < 0 && remainder < 0)
	|| (value_m.value < 0 && subtrahend.value > 0 && remainder > 0))
	{
	throw (OverflowOrUnderflowExImpl(__FILE__, __LINE__, "DurationHelper::subtract"));
	}
    value_m.value = remainder;
    m_toAttributes();
}
//------------------------------------------------------------------------------
DurationHelper& 
DurationHelper::operator-=(const acstime::Duration &subtrahend)
{
    this->subtract(subtrahend);
    return *this;
}
//------------------------------------------------------------------------------
void 
DurationHelper::modulo(const acstime::Duration &divisor)
{
    value_m.value %= divisor.value;
    m_toAttributes();
}
//------------------------------------------------------------------------------
DurationHelper& 
DurationHelper::operator%=(const acstime::Duration &divisor)
{
    this->modulo(divisor);
    return *this;
}
//------------------------------------------------------------------------------
void 
DurationHelper::multiply(const CORBA::ULong &multiplier)
{
    long long product = value_m.value * multiplier;
    if ((value_m.value > 0 && product < 0)
	|| (value_m.value < 0 && product > 0))
	{
	throw (OverflowOrUnderflowExImpl(__FILE__, __LINE__, "DurationHelper::multiply"));
	}
    value_m.value = product;
    m_toAttributes();
}
//------------------------------------------------------------------------------
DurationHelper& 
DurationHelper::operator*=(const CORBA::ULong &multiplier)
{
    this->multiply(multiplier);
    return *this;
}
//------------------------------------------------------------------------------
void 
DurationHelper::divide(const CORBA::ULong &divisor)
{   
    value_m.value /= divisor;
    m_toAttributes();
}
//------------------------------------------------------------------------------
DurationHelper& 
DurationHelper::operator/=(const CORBA::ULong &divisor)
{
    this->divide(divisor);
    return *this;
}
//------------------------------------------------------------------------------
void 
DurationHelper::m_toValue()
{
    // check values for proper range, if out-of-range 
    // normalize values if flag is set, otherwise cause exception
    if (microSecond_m >= 10000000)
        {
	if (normalize_m != 0)
	    {
	    second_m += microSecond_m / 10000000;
	    microSecond_m %= 10000000;
	    }
	else
            {  // micro-second value is too big
	    throw (OverflowOrUnderflowExImpl(__FILE__, __LINE__, "DurationHelper::m_toValue"));
            }
	}

    if (second_m >= 60)
        {
	if (normalize_m != 0)
	    {
	    minute_m += second_m / 60;
	    second_m %= 60;
	    }
	else
            {  // second value is too big
	    throw (OverflowOrUnderflowExImpl(__FILE__, __LINE__, "DurationHelper::m_toValue"));
            }
	}

    if (minute_m >= 60)
        {
	if (normalize_m != 0)
	    {
	    hour_m += minute_m / 60;
	    minute_m %= 60;
	    }
	else
            {  // minute value is too big
	    throw (OverflowOrUnderflowExImpl(__FILE__, __LINE__, "DurationHelper::m_toValue"));
            }
	}

    if (hour_m >= 24)
        {
	if (normalize_m != 0)
	    {
	    day_m += hour_m / 24;
	    hour_m %= 24;
	    }
	else
            {  // hour value is too big
	    throw (OverflowOrUnderflowExImpl(__FILE__, __LINE__, "DurationHelper::m_toValue"));
            }
	}

    // convert values to duration
    value_m.value = ((((day_m * 24
		  + hour_m) * 60LL
		  + minute_m) * 60
		  + second_m) * 1000000
                  + microSecond_m) * 10;

    if (positive_m == 0)
        {
        value_m.value = -value_m.value;
        }
}

//------------------------------------------------------------------------------
void 
DurationHelper::m_toAttributes()
{
    long long t = value_m.value / 10;    // microseconds
    if (value_m.value >= 0)
        {
        positive_m = true;
        }
    else
        {
	positive_m = false;
	t = -t;
	}
    microSecond_m = t % 1000000;
    t /= 1000000;
    second_m = t % 60;
    t /= 60;
    minute_m = t % 60;
    t /= 60;
    hour_m = t % 24;
    day_m = t / 24;
}
//------------------------------------------------------------------------------
std::string
DurationHelper::toString(const char *format)
{
    std::ostringstream ostr;
    
    if (*format != 0)
        {  // I can't do very much yet
        ostr << std::ends;
        throw (ArgErrorExImpl(__FILE__, __LINE__, "DurationHelper::toString"));
	return ostr.str();
	}

    if (positive_m == 0)
	{
        ostr << "-";
	}
    if (day_m != 0)
	{
        ostr << day_m << " ";
	}
    ostr << setw(2) << setfill('0') << hour_m << ":"
         << setw(2) << setfill('0') << minute_m << ":"
         << setw(2) << setfill('0') << second_m;

    if (microSecond_m != 0)
        {
        ostr << ".";
        m_microSec(ostr);
        }

    return ostr.str(); 
}

//------------------------------------------------------------------------------
long double
DurationHelper::toSeconds()
{
    //just divide by 10000000 (100ns) 
    return static_cast<long double>(value_m.value / 1e7);
}
//------------------------------------------------------------------------------
void 
DurationHelper::m_microSec(std::ostringstream& ostr)
{
    char buf[] = "000000";
    long microSec = microSecond_m;
    for (int i = 5; i >= 0; i--)
        {
	buf[i] = microSec % 10 + '0';
	microSec /= 10;
	if (microSec == 0)
	    {
	    break;
	    }
	}
    ostr << buf;
}
//------------------------------------------------------------------------------
void 
DurationHelper::fromString(const char *duration)
{
    // Right now I only accept e.g. "10675199 02:48:05.477580",
    //                              "-10675199 02:48:05.477580",
    //                              "-02:48:05.477580"
    // In the future I may accept more variety of input formats
    //
    // N.B. Do NOT change normalize_m switch, e.g. by calling reset()

    std::istringstream istr(duration);
    char chr;

    while ((chr = istr.get()) == ' ')
	{
	;    // skip any leading white space
	}
    istr.putback(chr);

    positive_m = true;
    if (istr.peek() == '+')
        {
        istr.get();
        }
    else if (istr.peek() == '-')
        {
        istr.get();
        positive_m = false;
        }

    istr >> day_m;    // first number can be either day or hour
    if (istr==0)
        {
        throw (ArgErrorExImpl(__FILE__, __LINE__, "DurationHelper::fromString"));
	}
    if (istr.peek() == ':')
        {    // here first number is hour
        hour_m = day_m;
        day_m = 0;
        istr.get();
        }
    else
        {    // here first number is day
        istr >> hour_m;
        if ((istr==0) || istr.get() != ':')
            {
            throw (ArgErrorExImpl(__FILE__, __LINE__, "DurationHelper::fromString"));
	    }
        }

    istr >> minute_m;
    if ( (istr==0) || istr.get() != ':')
        {
        throw (ArgErrorExImpl(__FILE__, __LINE__, "DurationHelper::fromString"));
	}

    istr >> second_m;
    if (istr==0)
        {
        throw (ArgErrorExImpl(__FILE__, __LINE__, "DurationHelper::fromString"));
	}

#if 1
    // can't use >> with usec as it can contain implicit trailing zeroes
    // istr.gcount() would make this simplier, but it always returned zero
    microSecond_m = 0;
    if (istr.get() == '.')
        {
	int i;
	for (i = 0; i < 6; i++)
	    {
	    char c = istr.get();
	    if ((istr==0) || (isdigit(c)==0))
		{
	        break;
		}
	    microSecond_m = microSecond_m * 10 + c - '0';
	    }
	while (i < 6)
	    {
	    microSecond_m *= 10;
	    i++;
	    }
	}
#else
    if (istr.get() == '.')
        {
        istr >> microSecond_m;
	if (istr==0)
            {
            throw (ArgErrorExImpl(__FILE__, __LINE__, "DurationHelper::fromString"));
            }
	}
    else
        {
        microSecond_m = 0;
	}
#endif
    m_toValue();
}








