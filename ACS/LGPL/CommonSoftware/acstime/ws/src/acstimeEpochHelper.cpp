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
* "@(#) $Id: acstimeEpochHelper.cpp,v 1.23 2011/10/28 15:12:04 hsommer Exp $"
*/
#include "acstimeEpochHelper.h"
#include <iomanip>

using namespace std;

//------------------------------------------------------------------------------
using namespace ACSTimeError;

EpochHelper::EpochHelper(const acstime::Epoch &value) :
    TimeUtil()
{
    value_m = value;
    m_toAttributes();
}
//------------------------------------------------------------------------------
EpochHelper::EpochHelper() :
    TimeUtil()
{
    this->reset();
}
//-----------------------------------------------------------------------------
EpochHelper::EpochHelper(long double inValue) :
    TimeUtil()
{
    value(inValue);
}
//------------------------------------------------------------------------------
EpochHelper::EpochHelper(const ACS::Time &inValue) :
    TimeUtil()
{
    value(inValue);
}
//------------------------------------------------------------------------------
acstime::Epoch 
EpochHelper::value()
{
    return value_m;
}
//------------------------------------------------------------------------------
void 
EpochHelper::value(const acstime::Epoch &value)
{
    value_m = value;
    m_toAttributes();
}
//------------------------------------------------------------------------------
void 
EpochHelper::value(const ACS::Time &value)
{
    acstime::Epoch tEpoch;
    tEpoch.value = value;
    value_m = tEpoch;
    m_toAttributes();
}
//------------------------------------------------------------------------------
void 
EpochHelper::value(long double MJDSeconds)
{
    //convert value from seconds to 100 nanoseconds
    long double myVal = MJDSeconds * 1e7;
    
    //add the EpochOrigin in. should always be positive value
    myVal = myVal - acstime::EpochOriginInMJD * 8.64e11;
    
    if(myVal<0.0)
	{
	throw (OverflowOrUnderflowExImpl(__FILE__, __LINE__, "EpochHelper::EpochHelper"));
	}
    else
	{
	value_m.value = static_cast<ACS::Time>(myVal);
	m_toAttributes();
	}
}
//------------------------------------------------------------------------------
CORBA::ULong 
EpochHelper::year()
{
    return year_m;
}
//------------------------------------------------------------------------------
void 
EpochHelper::year(const CORBA::ULong &value)
{
    year_m = value;
    m_toValue(false);
}
//------------------------------------------------------------------------------
CORBA::Long
EpochHelper::month()
{
    return month_m;
}
//------------------------------------------------------------------------------
void 
EpochHelper::month(const CORBA::Long &value)
{
    month_m = value;
    m_toValue(false);
}
//------------------------------------------------------------------------------
CORBA::Long
EpochHelper::day()
{
    return day_m;
}
//------------------------------------------------------------------------------
void 
EpochHelper::day(const CORBA::Long &value)
{
    day_m = value;
    m_toValue(false);
}
//------------------------------------------------------------------------------
CORBA::Long
EpochHelper::dayOfYear()
{
    return day_mOfYear;
}
//------------------------------------------------------------------------------
void 
EpochHelper::dayOfYear(const CORBA::Long &value)
{
    day_mOfYear = value;
    m_toValue(true);
}
//------------------------------------------------------------------------------
CORBA::ULong
EpochHelper::dayOfWeek()
{
    return dayOfWeek_m;
}
//------------------------------------------------------------------------------
CORBA::Long
EpochHelper::hour()
{
    return hour_m;
}
//------------------------------------------------------------------------------
void 
EpochHelper::hour(const CORBA::Long &value)
{
    hour_m = value;
    m_toValue(false);
}
//------------------------------------------------------------------------------
CORBA::Long
EpochHelper::minute()
{
    return minute_m;
}
//------------------------------------------------------------------------------
void 
EpochHelper::minute(const CORBA::Long &value)
{
    minute_m = value;
    m_toValue(false);
}
//------------------------------------------------------------------------------
CORBA::Long
EpochHelper::second()
{
    return second_m;
}
//------------------------------------------------------------------------------
void 
EpochHelper::second(const CORBA::Long &value)
{
    second_m = value;
    m_toValue(false);
}
//------------------------------------------------------------------------------
CORBA::Long 
EpochHelper::microSecond()
{
    return microSecond_m;
}
//------------------------------------------------------------------------------
void 
EpochHelper::microSecond(const CORBA::Long &value)
{
    microSecond_m = value;
    m_toValue(false);
}
//------------------------------------------------------------------------------
CORBA::Boolean 
EpochHelper::normalize()
{
    return normalize_m;
}
//------------------------------------------------------------------------------
void 
EpochHelper::normalize(const CORBA::Boolean &value)
{
    normalize_m = value;
}
//------------------------------------------------------------------------------
void 
EpochHelper::reset()
{
    // zero attributes
    value_m.value = 0ULL;
    year_m = 0;
    month_m = 0;
    day_m = 0;
    day_mOfYear = 0;
    dayOfWeek_m = 0;
    hour_m = 0;
    minute_m = 0;
    second_m = 0;
    microSecond_m = 0;
    normalize_m = false;
}
//------------------------------------------------------------------------------
acstime::TimeComparison 
EpochHelper::compare(const acstime::Epoch &comparee)
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
EpochHelper::operator==(const acstime::Epoch &epoch) const
{
    return (value_m.value == epoch.value);
}
//------------------------------------------------------------------------------
CORBA::Boolean
EpochHelper::operator<=(const acstime::Epoch &epoch) const
{
    return (value_m.value <= epoch.value);
}
//------------------------------------------------------------------------------
CORBA::Boolean 
EpochHelper::operator<(const acstime::Epoch &epoch) const
{
    return (value_m.value < epoch.value);
}
//------------------------------------------------------------------------------
CORBA::Boolean 
EpochHelper::operator>=(const acstime::Epoch &epoch) const
{
    return (value_m.value >= epoch.value);
}
//------------------------------------------------------------------------------
CORBA::Boolean 
EpochHelper::operator>(const acstime::Epoch &epoch) const
{
    return (value_m.value > epoch.value);
}
//------------------------------------------------------------------------------
void 
EpochHelper::add(const acstime::Duration &additive)
{
    unsigned long long sum = value_m.value + additive.value;
    if ((additive.value > 0 && sum < value_m.value)
	|| (additive.value < 0 && sum > value_m.value))
	{
	throw (OverflowOrUnderflowExImpl(__FILE__, __LINE__, "EpochHelper::add"));
	}
    value_m.value = sum;
    m_toAttributes();
}
//------------------------------------------------------------------------------
EpochHelper& 
EpochHelper::operator+=(const acstime::Duration &additive)
{
    this->add(additive);
    return *this;
}
//------------------------------------------------------------------------------
void 
EpochHelper::subtract(const acstime::Duration &subtrahend)
{
    unsigned long long remainder = value_m.value - subtrahend.value;
    if ((subtrahend.value < 0 && remainder < value_m.value)
	|| (subtrahend.value > 0 && remainder > value_m.value))
	{
	throw (OverflowOrUnderflowExImpl(__FILE__, __LINE__, "EpochHelper::subtract"));
	}
    value_m.value = remainder;
    m_toAttributes();
}
//------------------------------------------------------------------------------
EpochHelper& 
EpochHelper::operator-=(const acstime::Duration &subtrahend)
{
    this->subtract(subtrahend);
    return *this;
}
//------------------------------------------------------------------------------
acstime::Duration
EpochHelper::difference(const acstime::Epoch &subtrahend)
{
    acstime::Duration s;
    s.value = static_cast<long long>(value_m.value) - static_cast<long long>(subtrahend.value);
    return s;
}
//------------------------------------------------------------------------------
void 
EpochHelper::modulo(const acstime::Epoch &divisor)
{
    value_m.value %= divisor.value;
    m_toAttributes();
}
//------------------------------------------------------------------------------
EpochHelper& 
EpochHelper::operator%=(const acstime::Epoch &divisor)
{
    this->modulo(divisor);
    return *this;
}
//------------------------------------------------------------------------------
CORBA::Double 
EpochHelper::toUTCdate(CORBA::Long array2TAI, CORBA::Long TAI2UTC)
{   
    // algoritm taken from ticsAntMount::antMountForeGrdLoop.cpp
    return (value_m.value - array2TAI - TAI2UTC * 1e7) / 8.64e11 + acstime::EpochOriginInMJD;
}
//------------------------------------------------------------------------------
long double 
EpochHelper::toMJDseconds()
{   
    return (value_m.value / static_cast<long double>(10000000.0)) + acstime::EpochOriginInMJD*8.64e4;
}
//------------------------------------------------------------------------------
CORBA::Double
EpochHelper::toJulianYear(CORBA::Long array2TAI, CORBA::Long TAI2UTC)
{
    // algoritm taken from ticsAntMount::antMountForeGrdLoop.cpp
    return (value_m.value - array2TAI - TAI2UTC * 1e7) / 8.64e11 / 365.25 + acstime::EpochOriginInYears;
}
//------------------------------------------------------------------------------
// use attributes to set value
void 
EpochHelper::m_toValue(CORBA::Boolean dayOfYearSet)
{
    // ignore if don't have either year, month, & day, or year & day-of-year
    if (year_m == 0
	|| ((month_m == 0 || day_m == 0) && day_mOfYear == 0))
        {
        return;
        }
    
    // check values for proper range, if out-of-range 
    // normalize values if flag is set, otherwise cause exception
    if ((microSecond_m < 0) || (microSecond_m >= 1000000))
        {
	if (normalize_m == true)
	    {
	    int diff =  microSecond_m / 1000000;
	    if (microSecond_m < 0)
		{
		diff--;
		}
	    second_m += diff;
	    microSecond_m -= (diff * 1000000);
	    }
	else
            {  // micro-second value is too small or too big
	    throw (OverflowOrUnderflowExImpl(__FILE__, __LINE__, "EpochHelper::m_toValue"));
            }
	}
    
    if ((second_m < 0) || (second_m >= 60))
        {
	if (normalize_m == true)
	    {
	    int diff =  second_m / 60;
	    if (second_m < 0)
		{
		diff--;
		}
	    minute_m += diff;
	    second_m -= diff * 60;
	    }
	else
            {  // second value is too small or too big
	    throw (OverflowOrUnderflowExImpl(__FILE__, __LINE__, "EpochHelper::m_toValue"));
            }
	}

    if ((minute_m < 0) || (minute_m >= 60))
        {
	if (normalize_m == true)
	    {
	    int diff = minute_m / 60;
	    if (minute_m < 0)
		{
		diff--;
		}
	    hour_m += diff;
	    minute_m -= diff * 60;
	    }
	else
            {  // minute value is too small or too big
	    throw (OverflowOrUnderflowExImpl(__FILE__, __LINE__, "EpochHelper::m_toValue"));
            }
	}

    if ((hour_m < 0) || (hour_m >= 24))
        {
	if (normalize_m == true)
	    {
	    int diff = hour_m / 24;
	    if (hour_m < 0)
		{
		diff--;
		}
	    if ((day_mOfYear == 0) || ( dayOfYearSet == false))
	        {
		day_m += diff;
		}
	    else
		{
	        day_mOfYear += diff;
		}
	    hour_m -= diff * 24;
	    }
	else
            {  // hour value is too small or too big
	    throw (OverflowOrUnderflowExImpl(__FILE__, __LINE__, "EpochHelper::m_toValue"));
            }
	}

    long leap = m_calcLeap();
    unsigned long long dayAccum = 0;    // days kept in int32 is sufficient
    if ((day_mOfYear == 0) || ( dayOfYearSet == false))
        {
	// here use month,day values (instead of day-of-year)
	if ((month_m < 1) || (month_m > 12))
	    {
	    if (normalize_m == true)
	        {
		int diff = month_m / 12;
		if (month_m < 0)
		    {
		    diff--;
		    }
		year_m += diff;
		month_m -= diff * 12;
		}
	    else
                {  // month value is too small or too big
		throw (OverflowOrUnderflowExImpl(__FILE__, __LINE__, "EpochHelper::m_toValue"));
                }
	    }

	if (normalize_m == true)
	    { 
	    while (day_m < 1)
	        {
		if (--month_m < 1)
		    {
		    month_m = 12;
		    year_m--;
		    leap = m_calcLeap();
		    }
		day_m += DaysInMonth[month_m - 1];
		if (month_m == 2)
                    {
		    day_m += leap;
                    }
		}
	    while ((month_m != 2 && day_m > DaysInMonth[month_m - 1])
		   || (month_m == 2 && day_m > DaysInMonth[1] + leap))
	        {
		day_m -= DaysInMonth[month_m - 1];
		if (month_m == 2)
                    {
		    day_m -= leap;
                    }
		if (++month_m > 12)
		    {
		    month_m = 1;
		    year_m++;
		    leap = m_calcLeap();
		    }
		}
	    }
	else if (month_m < 1
		 || (month_m != 2 && day_m > DaysInMonth[month_m - 1])
		 || (month_m == 2 && day_m > DaysInMonth[1] + leap))
            {  // day value is too small or too big
	    throw (OverflowOrUnderflowExImpl(__FILE__, __LINE__, "EpochHelper::m_toValue"));
            }

	// calc number of days to start of current month
	for (int i = 0; i < month_m - 1; i++)
	    {
	    dayAccum += DaysInMonth[i];
	    }
	if (month_m > 2)
            {
	    dayAccum += leap;
            }

	// add days in current month
	dayAccum += day_m - 1;
	}
    else
        {    
	// here use day-of-year value (instead of month,day values)
	if (normalize_m == true)
	    { 
	    while (day_mOfYear < 1)
	        {
		day_mOfYear += (365 + leap);
		year_m--;
		leap = m_calcLeap();
		}
	    while (day_mOfYear > 365 + leap)
	        {
		day_mOfYear -= (365 + leap);
		year_m++;
		leap = m_calcLeap();
		}
	    }
	else if (day_mOfYear > 365 + leap)
            {  // day of year is too big for year
	    throw (OverflowOrUnderflowExImpl(__FILE__, __LINE__, "EpochHelper::m_toValue"));
            }

	// calc number of days in current year
	dayAccum += day_mOfYear - 1;
	}

    if (year_m > 60038)
        {  // year is too big
	throw (OverflowOrUnderflowExImpl(__FILE__, __LINE__, "EpochHelper::m_toValue"));
        }

    // add number of days from Gregorian calendar start to start of current year
    int yearMinusOne = year_m - 1;
    dayAccum += yearMinusOne * 365
              + yearMinusOne / 4 
              - yearMinusOne / 100 
              + yearMinusOne / 400;

    if (dayAccum < acstime::STARTDATE)
        {  // date is before 1582-10-15
	throw (OverflowOrUnderflowExImpl(__FILE__, __LINE__, "EpochHelper::m_toValue"));
        }

    // subtract days at starting date (1582-10-15)
    dayAccum -= acstime::STARTDATE;

    // check for greater than days allowed in representation
    // N.B. it's still possible to overflow representation if you're clever
    if (dayAccum > 21350398)
        {  // date is too big
	throw (OverflowOrUnderflowExImpl(__FILE__, __LINE__, "EpochHelper::m_toValue"));
        }

    // add contribution from hour, minute, second, and micro-second
    value_m.value = ((((dayAccum * 24 + hour_m) * 60
		                + minute_m) * 60 
		                + second_m) * 1000000
                                + microSecond_m) * 10;

    // epoch value is now set so calc all other values from it
    m_toAttributes();
}
//------------------------------------------------------------------------------
// return 1 if year is a leap year, otherwise = 0
long 
EpochHelper::m_calcLeap()
{
    int leap = 0;
    if (year_m % 4 == 0)
        {
        leap = 1;
        }
    if (year_m % 100 == 0)
        {
        leap = 0;
        }
    if (year_m % 400 == 0)
        {
        leap = 1;
        }
    return leap;
}
//------------------------------------------------------------------------------
// use value to set attributes
void 
EpochHelper::m_toAttributes()
{
    int imon;
    
    // set hour, minute, second, and micro-second values
    long long t = value_m.value / 10;
    microSecond_m = t % 1000000;
    t /= 1000000;
    second_m = t % 60;
    t /= 60;
    minute_m = t % 60;
    t /= 60;
    hour_m = t % 24;
    t /= 24;		// t is now in days

    // add starting date (1582-10-15)
    t += acstime::STARTDATE;

    // calc number of 400 Gregorian years
    int icenFour = t / 146097;	// 146097 = days in 400 years
    t -= icenFour * 146097;	// remaining days in current 400 years

    // days of week repeats every 400 years
    dayOfWeek_m = (t + 1) % 7 + 1;  // set day of week value

    // calc number of centuries since last 400 Gregorian years
    int icen = t / 36524;	// 36524 = days in century
    if (icen == 4)
        {
        icen = 3;
        }
    t -= (icen * 36524);	// remaining days in current century

    // calc number of quadrenia (four years) since last century
    int iyrFour = t / 1461;	// 1461 = days in quadrenia
    t -= iyrFour * 1461;		// remaining days in current quadrenia

    // calc number of years since last quadrenia
    int iyr = t / 365;		// 365 = days in year
    if (iyr == 4)
        {
        iyr = 3;
        }
    int iday = t - iyr * 365;	// remaining days in current year
    day_mOfYear = iday + 1;	// set day of year value

    // calc month and day of month
    for (imon = 0; iday >= 0; imon++)
        {
	iday = iday - DaysInMonth[imon] - ((iyr == 3 && imon == 1) ? 1 : 0);
        }
    imon--;		// restore imon, iday to last loop value
    iday = iday + DaysInMonth[imon] + ((iyr == 3 && imon == 1) ? 1 : 0);

    // set year, month, and day values
    year_m = icenFour * 400 + icen * 100 + iyrFour * 4 + iyr + 1;
    month_m = imon + 1;
    day_m = iday + 1;
}
//------------------------------------------------------------------------------
std::string 
EpochHelper::toString(acstime::TimeSystem ts, const char* format, const CORBA::Long array2TAI, const CORBA::Long TAI2UTC)
{
    std::ostringstream ostr;
    
    if ((ts == acstime::TSTAI) || (ts == acstime::TSUTC))
        {  // here TAI or UTC
	bool origNormalize = normalize_m;
	normalize_m = true;
        microSecond_m += array2TAI;
	if (ts == acstime::TSUTC)
	    {
	    microSecond_m += static_cast<unsigned long>(TAI2UTC * 1e7);
	    }
	m_toValue(false);
	normalize_m = origNormalize;
	}

    else if (ts != acstime::TSArray)
        {  // here illegal
        ostr << ends;
        throw (ArgErrorExImpl(__FILE__, __LINE__, "EpochHelper::toString"));
	return ostr.str();
	}

    if (*format == '\0')
        {
	// ISO-8601 date and time
	m_iso8601full(ostr);
	}

    else while (*format != '\0')
        {
	if (*format == '%')
	    {
	    format++;

	    // microsecond format ('q') maybe proceded by width digit
	    if (isdigit(*format) != 0)
	        {
		int digits = *format - '0';
		if (digits < 1 || digits > 7)
		    {
		    ostr << ends;
		    throw (ArgErrorExImpl(__FILE__, __LINE__, "EpochHelper::toString"));
		    return ostr.str();
		    }
		format++;
		if (*format != 'q')
		    {
		    ostr << ends;
		    throw (ArgErrorExImpl(__FILE__, __LINE__, "EpochHelper::toString"));
		    return ostr.str();
		    }
		m_microSec(ostr,digits);
		}

	    else
		{
		switch (*format)
		    {  
		    case 'x':
			// Unix format
			m_almostUnix(ostr);
			break;

		    case 'G':
			// ISO-8601 date and time
			m_iso8601full(ostr);
			break;

		    case 'g':
			// ISO-8601 date
			m_iso8601date(ostr);
			break;

		    case 'Y':
			// year, e.g. "1995"
			ostr << year_m;
			break;

		    case 'y':
			// last two digits of year, e.g. "95"
			ostr << setw(2) << setfill('0') << year_m % 100;
			break;

		    case 'm':
			// month (01..12)
			ostr << setw(2) << setfill('0') << month_m;
			break;

		    case 'h':
			// abbreviated month name (Jan..Dec)
			m_abbrevMonth(ostr);
			break;

		    case 'j':
			// day of year (001..366)
			ostr << setw(3) << setfill('0') << day_mOfYear;
			break;

		    case 'd':
			// day of month (01..31)
			ostr << setw(2) << setfill('0') << day_m;
			break;

		    case 'e':
			// day of month, blank padded ( 1..31)
			ostr << setw(2) << setfill(' ') << day_m;
			break;

		    case 'w':
			// day of week (0..6); 0 represents Sunday
			ostr << dayOfWeek_m - 1;
			break;

		    case 'a':
			// abbreviated weekday name (Sun..Sat)
			m_abbrevWeekday(ostr);
			break;

		    case 'H':
			// hour (00..23)
			ostr << setw(2) << setfill('0') << hour_m;
			break;

		    case 'M':
			// minute (00..59)
			ostr << setw(2) << setfill('0') << minute_m;
			break;

		    case 'S':
			// second (00..59)
			ostr << setw(2) << setfill('0') << second_m;
			break;
			
		    case 'q':
			// microsecond (000000..999999)
			m_microSec(ostr,6);
			break;

		    default:
			// illegal format
			ostr << ends;
			throw (ArgErrorExImpl(__FILE__, __LINE__, "EpochHelper::toString"));
			return ostr.str();
		    }
		}
	    }

	else
	    {
	    // if char in NOT '%' just put it in output
	    ostr << *format;
	    }

	// bump pointer to next char
	format++;
	}

    return ostr.str(); 
}

//------------------------------------------------------------------------------
void 
EpochHelper::m_iso8601date(std::ostringstream& ostr)
{
    ostr << setw(4) << setfill(' ') << year_m << "-"
	 << setw(2) << setfill('0') << month_m << "-"
	 << setw(2) << setfill('0') << day_m;
}
//------------------------------------------------------------------------------
void 
EpochHelper::m_iso8601full(std::ostringstream& ostr)
{
    m_iso8601date(ostr);
    ostr << "T"
	 << setw(2) << setfill('0') << hour_m << ":"
	 << setw(2) << setfill('0') << minute_m << ":"
	 << setw(2) << setfill('0') << second_m;
}
//------------------------------------------------------------------------------
void 
EpochHelper::m_almostUnix(std::ostringstream& ostr)
{
    m_abbrevWeekday(ostr);
    ostr << ", " << day_m << " ";
    m_abbrevMonth(ostr);
    ostr << " " << year_m << " "
	 << setw(2) << setfill('0') << hour_m << ":"
	 << setw(2) << setfill('0') << minute_m << ":"
	 << setw(2) << setfill('0') << second_m << ".";
    m_microSec(ostr,6);
    ostr << " " << day_mOfYear;
}
//------------------------------------------------------------------------------
void 
EpochHelper::m_abbrevMonth(std::ostringstream& ostr)
{
    static char* months[] = { "Jan","Feb","Mar","Apr","May","Jun",
			      "Jul","Aug","Sep","Oct","Nov","Dec"  };
    ostr << months[month_m - 1];
}
//------------------------------------------------------------------------------
void 
EpochHelper::m_abbrevWeekday(std::ostringstream& ostr)
{
    static char* days[] = { "Sun","Mon","Tue","Wed","Thu","Fri","Sat" };
    ostr << days[dayOfWeek_m - 1];
}
//------------------------------------------------------------------------------
void 
EpochHelper::m_microSec(std::ostringstream& ostr, int digits)
{
    char buf[] = "0000000";
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
    buf[digits] = '\0';
    ostr << buf;
}
//------------------------------------------------------------------------------
void 
EpochHelper::fromString(acstime::TimeSystem ts, const char* epoch)
{
    // Currently only time system is ISO-8601
    // In the future, should accept a variety of input formats
    //
    // N.B. Do NOT change normalize_m switch, e.g. by calling reset()
    
    if (ts != acstime::TSArray)
        {
        throw (ArgErrorExImpl(__FILE__, __LINE__, "EpochHelper::fromString"));
	}

    std::istringstream istr(epoch);

    day_mOfYear = dayOfWeek_m = 0;

    istr >> year_m;
    if ( (istr==0) || istr.get() != '-')
        {
        throw (ArgErrorExImpl(__FILE__, __LINE__, "EpochHelper::fromString"));
	}

    istr >> month_m;
    if ( (istr==0) || istr.get() != '-')
        {
        throw (ArgErrorExImpl(__FILE__, __LINE__, "EpochHelper::fromString"));
	}

    istr >> day_m;
    if ( (istr==0))
        {
        throw (ArgErrorExImpl(__FILE__, __LINE__, "EpochHelper::fromString"));
	}

    // for now, accept either ' ' or 'T' between date and time
    char c = istr.get();
    if (c != ' ' && c != 'T')
        {
        throw (ArgErrorExImpl(__FILE__, __LINE__, "EpochHelper::fromString"));
	}

    istr >> hour_m;
    if ( (istr==0) || istr.get() != ':')
        {
        throw (ArgErrorExImpl(__FILE__, __LINE__, "EpochHelper::fromString"));
	}

    istr >> minute_m;
    if ( (istr==0) || istr.get() != ':')
        {
        throw (ArgErrorExImpl(__FILE__, __LINE__, "EpochHelper::fromString"));
	}

    istr >> second_m;
    if ( (istr==0))
        {
        throw (ArgErrorExImpl(__FILE__, __LINE__, "EpochHelper::fromString"));
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
	    if ( (istr==0) || ! isdigit(c))
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
    // original- usec must not be present or contain exactly six digits
    if (istr.get() == '.')
        {
        istr >> microSecond_m;
	if (istr==0)
	    {
            throw (ArgErrorExImpl(__FILE__, __LINE__, "EpochHelper::fromString"));
            }
    else
        {
        microSecond_m = 0;
	}
#endif

    m_toValue(false);
}
//------------------------------------------------------------------------------
DurationHelper* operator%(const EpochHelper &epoch, const ACS::TimeInterval &divisor)
{
    ACS::TimeInterval retTI;
    retTI = epoch.value_m.value % divisor;
    DurationHelper* retValue_p = new DurationHelper(retTI);
    return retValue_p;
}
//------------------------------------------------------------------------------
DurationHelper* operator%(const ACS::TimeInterval &divisor, const EpochHelper &epoch)
{
    ACS::TimeInterval retTI;
    retTI = divisor % epoch.value_m.value;
    DurationHelper* retValue_p = new DurationHelper(retTI);
    return retValue_p;
}












