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
 * "@(#) $Id: bulkDataNTThreadSyncGuard.cpp,v 1.1 2013/02/11 18:37:33 rbourtem Exp $"
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

//
// ACE stuff
//
#include <System_Time.h>

//
// ACS stuff
//

//
// CORR stuff
//

//
// Local stuff
//
#include "bulkDataNTPosixHelper.h"
#include "bulkDataNTThreadSyncGuard.h"

using namespace std;

//----------------------------------------------------------------------------------
AcsBulkdata::ThreadSyncGuard::ThreadSyncGuard():
    m_label(""),
    m_mutex_p(NULL),
    m_acquired(false)
{
}

//----------------------------------------------------------------------------------
AcsBulkdata::ThreadSyncGuard::ThreadSyncGuard(const char *label, pthread_mutex_t *m):
    m_label(label),
    m_mutex_p(m),
    m_acquired(false)
{
}

//----------------------------------------------------------------------------------
AcsBulkdata::ThreadSyncGuard::ThreadSyncGuard(const char *label,
                                       pthread_mutex_t *m,
                                       const ACS::TimeInterval &to):
    m_label(label),
    m_mutex_p(m),
    m_acquired(false)
{
    //
    // try to acquire the access mutex
    //
    acquire(to);
}

//----------------------------------------------------------------------------------
AcsBulkdata::ThreadSyncGuard::~ThreadSyncGuard()
{
    //
    // release the mutex only if it was acquired
    //
    if ( m_acquired )
    {
        pthread_mutex_unlock(m_mutex_p);
    }
}

//----------------------------------------------------------------------------------
void AcsBulkdata::ThreadSyncGuard::acquire(const ACS::TimeInterval &to)
{
    ACS::Time t0 = getTimeStamp();

    // 
    // already locked then complain
    //
    if ( m_acquired )
    {
        BDNT_EX_THROW_EX("mutex already locked (label=%s)", m_label.c_str());
    }

    //
    // if a mutex was never given then complain
    //
    if ( m_mutex_p == NULL )
    {
        BDNT_EX_THROW_EX("invalid acquire usage, mutex pointer is NULL");
    }
 
    //
    // Try to acquire the access mutex
    //
    if ( !AcsBulkdata::Pthread::Mutex::lock(m_label.c_str(), *m_mutex_p, to, true) )
    {
        BDNT_EX_THROW_EX("failed to acquire mutex (t0/label=%s/%s)", getStringifiedUTC(t0).c_str(), m_label.c_str());
    }
    
    m_acquired = true;
}

/*___oOo___*/
