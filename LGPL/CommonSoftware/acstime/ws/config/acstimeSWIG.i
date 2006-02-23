%module acstimeSWIG
%{
#include "acstimeC.h"
#include "ACSTimeError.h"
#include "acstimeTimeUtil.h"
#include "acstimeDurationHelper.h"
#include "acstimeEpochHelper.h"
%}

//////////////////////////////
namespace ACS
{
    typedef long long TimeInterval;
    typedef unsigned long long Time;
};
//////////////////////////////
namespace acstime
{
    enum TimeComparison
    {
	TCEqualTo,       // values are equal
	TCLessThan,      // this is less than comparee
	TCGreaterThan,   // this is greater than comparee
	TCIndeterminate  // comparison cannot be done
    };

    enum TimeSystem
    {
        TSArray,   // array time
	TSTAI,     // International Atomic Time (TAI)
	TSUTC      // Universal Coordinate Time (UTC)
    };

    struct Duration
    {
	ACS::TimeInterval value;
    };
    
    struct Epoch
    {
	ACS::Time value;
    };
};
//////////////////////////////
class TimeUtil
{
  public:
    TimeUtil(){};
    virtual ~TimeUtil(){};
  private:
    TimeUtil(const TimeUtil&);
    void operator= (const TimeUtil&);
};
//////////////////////////////
class DurationHelper : TimeUtil
{
  public:    
    DurationHelper(const acstime::Duration &duration);
    DurationHelper();
    
    virtual ~DurationHelper(){};
    acstime::Duration value();
    void value(const acstime::Duration &duration);
    bool positive();
    void positive(const bool&);
    long int day();
    void day(const long int&);
    long int hour();    
    void hour(const long int&);
    long int minute();
    void minute(const long int&);
    long int second();
    void second(const long int&);
    unsigned long long microSecond();
    void microSecond(const unsigned long long&);
    bool normalize();
    void normalize(const bool&);
    void reset();
    
    acstime::TimeComparison compare(const acstime::Duration &duration);

    void add(const acstime::Duration &duration) 
	throw(OverflowOrUnderflowExImpl);
    
    DurationHelper& operator+=(const acstime::Duration &duration);

    void subtract(const acstime::Duration &duration) 
	throw(OverflowOrUnderflowExImpl);
    
    DurationHelper& operator-=(const acstime::Duration &duration);

    void modulo(const acstime::Duration &duration);

    DurationHelper& operator%=(const acstime::Duration &duration);

    void multiply(const unsigned long long &multiplier) 
	throw(OverflowOrUnderflowExImpl);

    DurationHelper& operator*=(const unsigned long long &multiplier);
    
    void divide(const unsigned long long &divider);

    DurationHelper& operator/=(const unsigned long long &divider);
    
    char* toString(const char *format)
	throw(ArgErrorExImpl);
    
    void fromString(const char *duration)
	throw(ArgErrorExImpl);

  private:
    
    DurationHelper(const DurationHelper&);
    void operator= (const DurationHelper&);
};
//////////////////////////////////////
class EpochHelper : TimeUtil
{
  public:
    EpochHelper(const acstime::Epoch &epoch); 
    EpochHelper();
    virtual ~EpochHelper(){};
    
    acstime::Epoch value();
    void value(const acstime::Epoch &epoch);
    unsigned long int year();
    void year(const unsigned long int&);
    long int month();
    void month(const long int&);
    long int day();
    void day(const long int&);
    long int dayOfYear();
    void dayOfYear(const long int&);
    unsigned long int dayOfWeek();
    long int hour();
    void hour(const long int&);
    long int minute();
    void minute(const long int&);
    long int second();
    void second(const long int&);
    long int microSecond();
    void microSecond(const long int&);
    bool normalize();
    void normalize(const bool&);
    void reset();
    
    acstime::TimeComparison compare(const acstime::Epoch &epoch);
    
    void add(const acstime::Duration&)
	throw(OverflowOrUnderflowExImpl);
    
    void subtract(const acstime::Duration&)
	throw(OverflowOrUnderflowExImpl);    
    
    acstime::Duration difference(const acstime::Epoch &subtrahend);
    double toUTCdate(long int array2TAI, long int TAI2UTC);
    double toJulianYear(long int array2TAI, long int TAI2UTC);

    char* toString(acstime::TimeSystem, const char*, const long int array2TAI, const long int TAI2UTC)
	throw(ArgErrorExImpl);
    void fromString(acstime::TimeSystem, const char*)
	throw(ArgErrorExImpl);

  private:
    EpochHelper(const EpochHelper&);  
    void operator= (const EpochHelper&);
};
