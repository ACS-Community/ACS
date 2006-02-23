/* @(#) $Id: acstimeDurationHelper.h,v 1.14 2005/08/29 08:35:50 vwang Exp $
 *
 * Copyright (C) 2001
 * Associated Universities, Inc. Washington DC, USA.
 *
 * Produced for the ALMA project
 *
 * This library is free software; you can redistribute it and/or modify it it 
 * under the terms of the GNU Library General Public License as published by 
 * the Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 *
 * This library is distributed in the hope that it will be useful but WITHOUT 
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
 * License for more details.
 *
 * You should have received a copy of the GNU Library General Public License 
 * along with this library; if not, write to the Free Software Foundation, 
 * Inc., 675 Massachusetts Ave, Cambridge, MA, 02139, USA.
 *
 * Correspondence concerning ALMA should be addressed as follows:
 * Internet email: alma-sw-admin@nrao.edu
 */
/* ----------------------------------------------------------------*/
#ifndef ACSTIME_DURATION_HELPER_H
#define ACSTIME_DURATION_HELPER_H
/* ----------------------------------------------------------------*/
#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif
/* ----------------------------------------------------------------*/
#include "acstimeTimeUtil.h"
using namespace ACSTimeError;
/* ----------------------------------------------------------------*/
/** @file acstimeDurationHelper.h
 *  Header file for DurationHelper 
 */

/** @class DurationHelper
 *  DurationHelper is derived from TimeUtil and provides the developer with 
 *  an easy means of manipulating <a href="../../idl/html/structacstime_1_1Duration.html">Durations</a>
 *  (difference in time between 
 *  two <a href="../../idl/html/structacstime_1_1Epoch.html">epochs</a>).
 *  
 *  TODO:
 *  - doxygen comments for private methods.  These were taken directly from
 *    the Control subsystem without any modifications.
 */
class DurationHelper : TimeUtil
{
  public:
    /* ----------------------------------------------------------------*/
    /**
     * Constructor
     * @param duration Duration this helper class will utilize. 
     */
    DurationHelper(const acstime::Duration &duration);

    /** Standard constructor
     */
    DurationHelper();

    /**
     * Constructor
     * @param MJDSeconds Modified Julian Date in seconds this helper class is based on. 
     */
    DurationHelper(long double seconds);

    /**
     * Constructor
     * @param duration Time interval this helper class will utilize. 
     */
    DurationHelper(const ACS::TimeInterval &duration);
    
    /** Destructor - nothing to delete!
     */
    virtual ~DurationHelper(){};
    /* ----------------------------------------------------------------*/
    /**
     * Returns the current value of the <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a>.
     * @return Duration
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    acstime::Duration value();

    /**
     * Sets the current value of the <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a>.
     * @param Duration
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void value(const acstime::Duration &duration);

    /**
     * Sets the current value of the <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a>.
     * @param Duration
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void value(const ACS::TimeInterval &duration);

    /**
     * Sets the current value of the <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a>.
     * @param Duration
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void value(long double seconds);
    /* ----------------------------------------------------------------*/
    /**
     * Returns <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a>'s sign, TRUE => positive, FALSE => negative.
     * @b positive is set TRUE initially and after a reset().
     * @return bool
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    CORBA::Boolean positive();
    
    /**
     * Sets <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a>'s sign, TRUE => positive, FALSE => negative.
     * @b positive is set TRUE initially and after a reset()..
     * @param bool
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void positive(const CORBA::Boolean&);
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
    CORBA::ULong microSecond();

    /**
     * Sets the current value of the microsecond.
     * @param microsecond
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void microSecond(const CORBA::ULong&);
    /* ----------------------------------------------------------------*/
    /**
     * Returns the Normalize or out-of-range flag.
     *
     * When this flag is set FALSE the interface causes an exception 
     * when any out-of-range attribute value is set.  @b normalize is set 
     * to FALSE initially and after a reset().
     *
     * When this flag is set TRUE the interface accepts out-of-range 
     * values for hour, minute, second, or microSecond.  An out-of-range 
     * value causes all attributes to be normalized.
     *
     * @return bool
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    CORBA::Boolean normalize();

    /**
     * Sets the Normalize or out-of-range flag.
     *
     * When this flag is set FALSE the interface causes an exception 
     * when any out-of-range attribute value is set.  @b normalize is set 
     * to FALSE initially and after a reset().
     *
     * When this flag is set TRUE the interface accepts out-of-range 
     * values for hour, minute, second, or microSecond.  An out-of-range 
     * value causes all attributes to be normalized.
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
     * Compares this <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a> 
     * with the given <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a>
     * and returns the relation.
     *
     * DWF-should this be replaced completely by operators? ? ?
     * @param duration    Duration to be compared
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    acstime::TimeComparison compare(const acstime::Duration &duration);
    //These are the equivalent of the compare method()
    CORBA::Boolean operator==(const acstime::Duration &duration) const;
    CORBA::Boolean operator<=(const acstime::Duration &duration) const;
    CORBA::Boolean operator<(const acstime::Duration &duration) const;
    CORBA::Boolean operator>=(const acstime::Duration &duration) const;
    CORBA::Boolean operator>(const acstime::Duration &duration) const;

    /**
     * Adds the given <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a> 
     * to this <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a>.
     * @param duration    Duration to be added
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void add(const acstime::Duration &duration) 
	throw(OverflowOrUnderflowExImpl);
    
    /**
     * Adds the given <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a> 
     * to this <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a>.
     * @param duration    Duration to be added
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    DurationHelper& operator+=(const acstime::Duration &duration);

    /**
     * Subtracts the given <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a>
     * from this <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a>.
     * @param duration    Duration to be subtracted
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void subtract(const acstime::Duration &duration) 
	throw(OverflowOrUnderflowExImpl);
    
    /**
     * Subtracts the given <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a>
     * from this <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a>.
     * @param duration    Duration to be subtracted
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    DurationHelper& operator-=(const acstime::Duration &duration);

    /**
     * Modulos this <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a> 
     * by the given <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a>.
     * @param duration     modulo duration by this Duration
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void modulo(const acstime::Duration &duration);

    /**
     * Modulos this <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a> 
     * by the given <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a>.
     * @param duration     modulo duration by this Duration
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    DurationHelper& operator%=(const acstime::Duration &duration);

    /**
     * Multiplies this <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a> 
     * by the integer value.
     * @param multiplier  multiply duration by this value
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void multiply(const CORBA::ULong &multiplier) 
	throw(OverflowOrUnderflowExImpl);

    /**
     * Multiplies this <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a>
     * by the integer value.
     * @param multiplier  multiply duration by this value
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    DurationHelper& operator*=(const CORBA::ULong &multiplier);
    
    /**
     * Divides this <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a> 
     * by the given integer value.
     * @param divider     divide duration by this value
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void divide(const CORBA::ULong &divider);

    /**
     * Divides this <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a> 
     * by the given integer value.
     * @param divider     divide duration by this value
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    DurationHelper& operator/=(const CORBA::ULong &divider);
    /* ----------------------------------------------------------------*/
    /**
     * Returns this <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a>'s 
     * value as a String.
     * @param format      format for the output string
     * @return  object's value as equivalent string
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    std::string toString(const char *format)
	throw(ArgErrorExImpl);
    
    /**
     * Sets this <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a>'s 
     * value from the given String.
     * @param duration    use this duration to set object's value
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void fromString(const char *duration)
	throw(ArgErrorExImpl);

    /**
     * Returns this <a href="../../idl/html/structacstime_1_1Duration.html">Duration</a>'s 
     * value as the equivalent value in seconds (fractional).
     * 
     * @note This method was requested by CORR but there was no request to take the
     *       values of the array2TAI and TAI2UTC properties into account.
     * @return  The underlying acstime::Duration of this helper class converted to 
     *          seconds (precision up to microseconds? ? ?).
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    long double toSeconds();


  private:
    /* ----------------------------------------------------------------*/
    void m_toValue()
	throw(OverflowOrUnderflowExImpl);
    
    void m_toAttributes();
      
    void m_microSec(std::ostringstream&);
    /* ----------------------------------------------------------------*/
    //The real variables affected by calls to public methods.
    acstime::Duration value_m;
    CORBA::Boolean positive_m;
    CORBA::Long day_m;
    CORBA::Long hour_m;
    CORBA::Long minute_m;
    CORBA::Long second_m;
    CORBA::ULong microSecond_m;
    CORBA::Boolean normalize_m;
    /* ----------------------------------------------------------------*/
    /** copy not allowed
     */
    DurationHelper(const DurationHelper&);

    /** assignment not allowed
     */
    void operator= (const DurationHelper&);
};

#endif








