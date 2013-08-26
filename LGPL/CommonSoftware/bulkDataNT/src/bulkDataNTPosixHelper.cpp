/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) AUI - Associated Universities Inc., 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************
 * 
 * "@(#) $Id: bulkDataNTPosixHelper.cpp,v 1.1 2013/02/11 18:37:33 rbourtem Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * ramestic  2010/10/05  created
 */

//
// System stuff
//
#define __STDC_FORMAT_MACROS
#include <inttypes.h>
#include <sstream>
#include <math.h>

//
// ACS stuff
//
#include <logging.h>
#include <acsutilTimeStamp.h>

//
// CORR stuff
//
// TODO Replace exceptions
//#include <CorrEx.h>

//
// Local stuff
//
#include "bulkDataNTPosixHelper.h"

using namespace std;

//----------------------------------------------------------------------------------
bool AcsBulkdata::Pthread::CondVar::wait(pthread_cond_t &cond, pthread_mutex_t &mux, const ACS::TimeInterval to)
{
    int rc;
            
    //
    // convert timeout parameter to an absolute time
    //
    timespec abstime = UTCtoACE_Time_Value(getTimeStamp() + to - UTCtoUNIXTimeBaseOffset * ACE_static_cast(ACE_UINT32, 10000000));

    rc = pthread_cond_timedwait(&cond, &mux, &abstime);

    if ( rc != 0 ) 
    {
        if ( rc == ETIMEDOUT )
        {
            return false;
        }
        else
        {
        	// TODO IMPORTANT!!
            //CORR_EX_THROW_EX("error on waiting for condition variable (err/to=%d/%" PRId64 ")", rc, to);
        }
    }

    return true;
}

//----------------------------------------------------------------------------------
void AcsBulkdata::Pthread::Mutex::init(pthread_mutex_t &m, const bool recursive)
{
    int rc;
    pthread_mutexattr_t attrs;

    //
    // create a default mutex attribute
    //
    if ( (rc = pthread_mutexattr_init(&attrs)) )
    {
    	// TODO IMPORTANT!!
        ///CORR_EX_THROW_EX("failied to initialize mutex attributes (err=%d)", rc);
    }

    //
    // if requested then change mutex type attribute to recursive
    //
    if ( recursive )
    {
        if ( (rc = pthread_mutexattr_settype(&attrs, PTHREAD_MUTEX_RECURSIVE)) )
        {
        	// TODO IMPORTANT!!
            ///CORR_EX_THROW_EX("failed to set recursive type (err=%d)", rc);
        }
    }

    //
    // initialize the recursive mutex
    //
    if ( (rc = pthread_mutex_init(&m, &attrs)) )
    {
    	// TODO IMPORTANT!!
        ///CORR_EX_THROW_EX("failed to initialize recursive mutex");
    }
}

//----------------------------------------------------------------------------------
bool AcsBulkdata::Pthread::Mutex::lock(const char *desc, pthread_mutex_t &m, const ACS::TimeInterval &to, bool logTimeout)
{
    int stat;

    //
    // timeout as an absolute timestamp
    //
    timespec ts = UTCtoACE_Time_Value(getTimeStamp() + to - UTCtoUNIXTimeBaseOffset * ACE_static_cast(ACE_UINT32, 10000000));


    //
    // lock on the mutex
    //
    if ( (stat = pthread_mutex_timedlock(&m, &ts)) )
    {
        if ( stat == ETIMEDOUT )
        {
            if ( logTimeout )
            {
                timespec now;
                    
                clock_gettime(CLOCK_REALTIME, &now);
                    
                ACE_Time_Value delta = ACE_Time_Value(now) - ACE_Time_Value(ts);
                    
                ACS_SHORT_LOG((LM_ERROR, "%s: timeout on pthread mutex (to=%" PRId64 "acs delta=%" PRIdMAX "s/%" PRIdMAX "us)", desc, to, (intmax_t)delta.sec(), (intmax_t)delta.usec()));
            }
        }
        else
        {
            ACS_SHORT_LOG((LM_ERROR, "%s: error locking pthread mutex (err=%d)", desc, stat));
        }

        return false;
    }

    return true;
}

//----------------------------------------------------------------------------------
bool AcsBulkdata::Pthread::Mutex::unlock(const char *desc, pthread_mutex_t &m)
{
    int stat;

    if ( (stat = pthread_mutex_unlock(&m)) )
    {
        if ( desc != NULL )
        {
            ACS_SHORT_LOG((LM_ERROR, "%s: error unlocking pthread mutex (err=%d)", desc, stat));
        }
        else
        {
            ACS_SHORT_LOG((LM_ERROR, "error unlocking pthread mutex (err=%d)", stat));
        }

        return false;
    }

    return true;
}

//----------------------------------------------------------------------------------
bool AcsBulkdata::Pthread::Mutex::unlock(pthread_mutex_t &m)
{
    return AcsBulkdata::Pthread::Mutex::unlock(NULL, m);
}

//----------------------------------------------------------------------------------
bool AcsBulkdata::Pthread::Semaphore::lock(const char *desc, sem_t &s, const ACS::TimeInterval &to, bool logTimeout)
{
    //
    // timeout as an absolute timestamp
    //
    timespec ts = UTCtoACE_Time_Value(getTimeStamp() + to - UTCtoUNIXTimeBaseOffset * ACE_static_cast(ACE_UINT32, 10000000));

    //
    // take the semaphore
    //
    if ( sem_timedwait(&s, &ts) )
    {
        if ( errno == ETIMEDOUT )
        {
            if ( logTimeout )
            {
                timespec now;
                    
                clock_gettime(CLOCK_REALTIME, &now);
                    
                ACE_Time_Value delta = ACE_Time_Value(now) - ACE_Time_Value(ts);
                
                ACS_SHORT_LOG((LM_ERROR, "%s: timeout on posix semaphore (to=%" PRId64 "acs delta=%" PRIdMAX "s/%" PRIdMAX "us)", desc, to, (intmax_t)delta.sec(), (intmax_t)delta.usec()));
            }
        }
        else
        {
            ACS_SHORT_LOG((LM_ERROR, "%s: error waiting on posix semaphore (err=%d)", desc, errno));
        }

        return false;
    }

    return true;
}
    
//----------------------------------------------------------------------------------
bool AcsBulkdata::Pthread::Semaphore::post(const char *desc, sem_t &s)
{
    if ( sem_post(&s) )
    {
        ACS_SHORT_LOG((LM_ERROR, "%s: error posting posix semaphore (err=%d)", desc, errno));

        return false;
    }

    return true;
}

/*___oOo___*/
