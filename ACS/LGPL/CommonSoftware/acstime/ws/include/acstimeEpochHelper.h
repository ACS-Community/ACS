#ifndef ACSTIME_EPOCH_HELPER_H
#define ACSTIME_EPOCH_HELPER_H
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
* "@(#) $Id: acstimeEpochHelper.h,v 1.19 2011/10/28 15:12:04 hsommer Exp $"
*/
/* ----------------------------------------------------------------*/
#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif
/* ----------------------------------------------------------------*/
#include "acstimeTimeUtil.h"
#include "acstimeDurationHelper.h"
/* ----------------------------------------------------------------*/
/** @file acstimeEpochHelper.h
 *  Header file for EpochHelper 
 */

/** @class EpochHelper
 *  EpochHelper is derived from TimeUtil and provides the developer with 
 *  an easy means of manipulating ACS <a href="../../idl/html/structacstime_1_1Epoch.html">Epochs</a> 
 *  (unique instance in time defined as a structure in acstime.idl).
 *  
 *  TODO:
 *  - doxygen comments for private methods.
 */
class EpochHelper : TimeUtil
{
  public:
    /* ----------------------------------------------------------------*/
    /**
     * Constructor
     * @param epoch Epoch this helper class is based on. 
     */
    EpochHelper(const acstime::Epoch &epoch);
    
    /** Standard constructor
     */
    EpochHelper();

    /**
     * Constructor
     * @param MJDSeconds Modified Julian Date in seconds this helper class is based on. 
     */
    EpochHelper(long double MJDSeconds);

    /**
     * Constructor
     * @param value Epoch in time this helper class is based on. 
     */
    EpochHelper(const ACS::Time &value);

    /** Destructor - nothing to delete!
     */
    virtual ~EpochHelper(){};
    /* ----------------------------------------------------------------*/
    /**
     * Returns the current value of the <a href="../../idl/html/structacstime_1_1Epoch.html">Epoch</a>.
     * @return Epoch
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    acstime::Epoch value();

    /**
     * Sets the current value of the <a href="../../idl/html/structacstime_1_1Epoch.html">Epoch</a>.
     * @param Epoch
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void value(const acstime::Epoch &epoch);

    /**
     * Sets the current value of the <a href="../../idl/html/structacstime_1_1Epoch.html">Epoch</a>.
     * @param Epoch
     * @htmlonly
       <br><hr>
       @endhtmlonly
    */
    void value(const ACS::Time &epoch);

    /**
     * Sets the current value of the <a href="../../idl/html/structacstime_1_1Epoch.html">Epoch</a>.
     * @param Epoch
     * @htmlonly
       <br><hr>
       @endhtmlonly
    */
    void value(long double MJDSeconds);
    /* ----------------------------------------------------------------*/
    /**
     * Returns the current value of the year.
     * @return year
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    CORBA::ULong year();

    /**
     * Sets the current value of the year.
     * @param year
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void year(const CORBA::ULong&);
    /* ----------------------------------------------------------------*/
    /**
     * Returns the current value of the month.
     * @return month
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    CORBA::Long month();

    /**
     * Sets the current value of the month.
     * @param month
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void month(const CORBA::Long&);
    /* ----------------------------------------------------------------*/
    /**
     * Returns the current value of the day.
     * @return day
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    CORBA::Long day();

    /**
     * Sets the current value of the day.
     * @param day
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void day(const CORBA::Long&);
    /* ----------------------------------------------------------------*/
    /**
     * Returns the current value of the day of year.
     * @return dayofyear
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    CORBA::Long dayOfYear();

    /**
     * Sets the current value of the day of year.
     * @param dayofyear
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void dayOfYear(const CORBA::Long&);
    /* ----------------------------------------------------------------*/
    /**
     * Returns the current value of the day of week.
     * @return dayofweek
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    CORBA::ULong dayOfWeek();
    /* ----------------------------------------------------------------*/
    /**
     * Returns the current value of the hour.
     * @return hour
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    CORBA::Long hour();

    /**
     * Sets the current value of the hour.
     * @param hour
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void hour(const CORBA::Long&);
    /* ----------------------------------------------------------------*/
    /**
     * Returns the current value of the minute.
     * @return minute
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    CORBA::Long minute();

    /**
     * Sets the current value of the minute.
     * @param minute
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void minute(const CORBA::Long&);
    /* ----------------------------------------------------------------*/
    /**
     * Returns the current value of the second.
     * @return second
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    CORBA::Long second();

    /**
     * Sets the current value of the second.
     * @param second
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void second(const CORBA::Long&);
    /* ----------------------------------------------------------------*/
    /**
     * Returns the current value of the microsecond.
     * @return microsecond
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    CORBA::Long microSecond();

    /**
     * Sets the current value of the microsecond.
     * @param microsecond
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void microSecond(const CORBA::Long&);
    /* ----------------------------------------------------------------*/
    /**
     * Get normalize or out-of-range flag.
     *
     * When this flag is set FALSE the class causes an exception when 
     * any out-of-range value is set.  @b normalize is set to FALSE 
     * initially and after a reset().
     *
     * When this flag is set TRUE the class accepts out-of-range 
     * values for month, day, dayOfYear, hour, minute, second, or 
     * microSecond.  An out-of-range value causes all attributes to be 
     * normalized.
     *
     * For example, with normalize set true and setting the month,day to 
     * 3,32 will result in month,day 4,1.  Setting hour,minute to 2,-62 
     * will result in hour,minute 1,2.
     *
     * This can be used as a convenient way of adding to or subtracting 
     * from an Epoch.
     *
     * @return bool
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    CORBA::Boolean normalize();

    /**
     * Set normalize or out-of-range flag.
     *
     * When this flag is set FALSE the class causes an exception when 
     * any out-of-range value is set.  @b normalize is set to FALSE 
     * initially and after a reset().
     *
     * When this flag is set TRUE the class accepts out-of-range 
     * values for month, day, dayOfYear, hour, minute, second, or 
     * microSecond.  An out-of-range value causes all attributes to be 
     * normalized.
     *
     * For example, with normalize set true and setting the month,day to 
     * 3,32 will result in month,day 4,1.  Setting hour,minute to 2,-62 
     * will result in hour,minute 1,2.
     *
     * This can be used as a convenient way of adding to or subtracting 
     * from an Epoch.
     *
     * @param bool
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void normalize(const CORBA::Boolean&);
    /* ----------------------------------------------------------------*/
    /**
     * Sets all numeral attributes to zero, and normalize to FALSE.
     * This facilities reuse of the object.
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void reset();
    /* ----------------------------------------------------------------*/
    /**
     * Compares this <a href="../../idl/html/structacstime_1_1Epoch.html">Epoch</a> 
     * with the given <a href="../../idl/html/structacstime_1_1Epoch.html">Epoch</a> 
     * and returns the relation.
     *
     * DWF-should this be replaced completely by operators? ? ?
     *
     * @param epoch    Epoch to be compared
     * @return The TimeComparison enumeration.
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    acstime::TimeComparison 
    compare(const acstime::Epoch &epoch);
    //These are the equivalent of the compare method()
    CORBA::Boolean operator==(const acstime::Epoch &epoch) const;
    CORBA::Boolean operator<=(const acstime::Epoch &epoch) const;
    CORBA::Boolean operator<(const acstime::Epoch &epoch) const;
    CORBA::Boolean operator>=(const acstime::Epoch &epoch) const;
    CORBA::Boolean operator>(const acstime::Epoch &epoch) const;

    /**
     * Adds the given <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a> 
     * to this <a href="../../idl/html/structacstime_1_1Epoch.html">Epoch</a>.
     * @param duration Duration to be added
     * @throw ACSTimeError::OverflowOrUnderflowExImpl
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void add(const acstime::Duration&);
    
    /**
     * Adds the given <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a> 
     * to this <a href="../../idl/html/structacstime_1_1Epoch.html">Epoch</a>.
     * @param duration Duration to be added
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    EpochHelper& operator+=(const acstime::Duration &duration);

    /**
     * Subtracts the given <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a> 
     * from this <a href="../../idl/html/structacstime_1_1Epoch.html">Epoch</a>.
     * @param duration    Duration to be subtracted
     * @throw ACSTimeError::OverflowOrUnderflowExImpl
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void subtract(const acstime::Duration&);
    
    /**
     * Subtracts the given <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a> 
     * from this <a href="../../idl/html/structacstime_1_1Epoch.html">Epoch</a>.
     * @param duration    Duration to be subtracted
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    EpochHelper& operator-=(const acstime::Duration &duration);

    /**
     * Subtracts the given <a href="../../idl/html/structacstime_1_1Epoch.html">Epoch</a> 
     * from this <a href="../../idl/html/structacstime_1_1Epoch.html">Epoch</a> 
     * and returns a <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a>.
     * result.
     *
     * Notice the returned duration will be positive if the subtrahend is 
     * earlier than this, and negative if the subtrahend is later than this.
     *
     * @param subtrahend  this gets subtracted from this Epoch
     * @return  Duration between the two Epoch's;
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    acstime::Duration difference(const acstime::Epoch &subtrahend);

    /**
     * Modulos this <a href="../../idl/html/structacstime_1_1Epoch.html">Epoch</a> 
     * by the given <a href="../../idl/html/structacstime_1_1Epoch.html">Epoch</a>.
     * @param epoch     modulo epoch by this Epoch
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void modulo(const acstime::Epoch &epoch);

    /**
     * Modulos this <a href="../../idl/html/structacstime_1_1Epoch.html">Epoch</a> 
     * by the given <a href="../../idl/html/structacstime_1_1Epoch.html">Epoch</a>.
     * @param epoch     modulo epoch by this Epoch
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    EpochHelper& operator%=(const acstime::Epoch &epoch);
    
    /**
     * Returns this <a href="../../idl/html/structacstime_1_1Epoch.html">Epoch</a>'s 
     * value as the equivalent UTC date and fractional day.
     * 
     * @note This method is especially designed to produce a result for 
     *       use with the SLALIB library routines slaMappa and slaAoppat.
     * @param array2TAI Should be the array2TAI property in the Clock Component
     * @param TAI2UTC Should be the TAI2UTC property in the Clock Component
     * @return  UTC date with fractional day as double
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    CORBA::Double toUTCdate(CORBA::Long array2TAI, CORBA::Long TAI2UTC);

    /**
     * Returns this <a href="../../idl/html/structacstime_1_1Epoch.html">Epoch</a>'s 
     * value as the equivalent modified Julian date in the form of fractional seconds.
     * 
     * @note This method was requested by CORR but there was no request to take the
     *       values of the array2TAI and TAI2UTC properties into account. Hence,
     *       this method does not require the two additional long parameters common
     *       to other methods of this class.
     * @return  The underlying acstime::Epoch of this helper class converted to 
     *          Modified Julian Date in seconds (precision up to microseconds? ? ?).
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    long double toMJDseconds();
    
    /**
     * Returns this <a href="../../idl/html/structacstime_1_1Epoch.html">Epoch</a>'s 
     * value as the equivalent Julian year.
     * 
     * @note This method is especially designed to produce a result for 
     *       use with the SLALIB library routine slaPm.
     * @param array2TAI Should be the array2TAI property in the Clock Component
     * @param TAI2UTC Should be the TAI2UTC property in the Clock Component
     * @return  Julian year as double
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    CORBA::Double toJulianYear(CORBA::Long array2TAI, CORBA::Long TAI2UTC);
    /* ----------------------------------------------------------------*/
    /**
     * Returns this <a href="../../idl/html/structacstime_1_1Epoch.html">Epoch</a>'s value as a String.
     * @param TimeSystem Time system to be used.
     * @param format     format for the output string
     * @param array2TAI Should be the array2TAI property in the Clock Component
     * @param TAI2UTC Should be the TAI2UTC property in the Clock Component
     * @return  object's value as equivalent string
     * @throw ACSTimeError::ArgErrorExImpl
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    std::string toString(acstime::TimeSystem, const char*, const CORBA::Long array2TAI, const CORBA::Long TAI2UTC);

    /**
     * Sets this <a href="../../idl/html/structacstime_1_1Epoch.html">Epoch</a>'s 
     * value from the given string.
     * @param TimeSystem Time system to be used.
     * @param epoch      use this string to set object's value
     * @throw ACSTimeError::ArgErrorExImpl
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void fromString(acstime::TimeSystem, const char*);

    friend DurationHelper* operator% (const EpochHelper&, const ACS::TimeInterval &);
    friend DurationHelper* operator% (const ACS::TimeInterval &, const EpochHelper&);

  private:
    /* ----------------------------------------------------------------*/
    /**
    * @throw ACSTimeError::OverflowOrUnderflowExImpl
    */
    void m_toValue(CORBA::Boolean booVal);
    
    void m_toAttributes();
    
    long m_calcLeap();
    
    void m_almostUnix(std::ostringstream&);
    void m_iso8601date(std::ostringstream&);
    void m_iso8601full(std::ostringstream&);
    void m_abbrevMonth(std::ostringstream&);
    void m_abbrevWeekday(std::ostringstream&);
    void m_microSec(std::ostringstream&, int);
    /* ----------------------------------------------------------------*/
    //The real variables affected by calls to public methods.
    acstime::Epoch value_m;
    CORBA::ULong year_m;
    CORBA::Long month_m;
    CORBA::Long day_m;
    CORBA::Long day_mOfYear;
    CORBA::ULong dayOfWeek_m;
    CORBA::Long hour_m;
    CORBA::Long minute_m;
    CORBA::Long second_m;
    CORBA::Long microSecond_m;
    CORBA::Boolean normalize_m;
    
    /** copy not allowed
     */
    EpochHelper(const EpochHelper&);
    
    /** assignment not allowed
     */
    void operator= (const EpochHelper&);
};

/**
 * Implemented this modulo operand to fulfill the requirements on an SPR
 * from Correlator. Caller is responsible for deleting the DurationHelper
 * allocated by this operand.
 * @param EpochHelper 
 * @param TimeInterval
 * @return Result of the modulo operand (EpochHelper.value().value % TimeInterval)
 */
DurationHelper* operator% (const EpochHelper&, const ACS::TimeInterval &);

/**
 * Implemented this modulo operand to fulfill the requirements on an SPR
 * from Correlator. Caller is responsible for deleting the DurationHelper
 * allocated by this operand.
 * @param TimeInterval
 * @param EpochHelper 
 * @return Result of the modulo operand (TimeInterval % EpochHelper.value().value)
 */
DurationHelper* operator% (const ACS::TimeInterval &, const EpochHelper&);

#endif










