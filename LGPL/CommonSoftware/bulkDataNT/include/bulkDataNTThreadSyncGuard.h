#ifndef BULK_DATA_NT_THREAD_SYNC_GUARD_H
#define BULK_DATA_NT_THREAD_SYNC_GUARD_H
/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) AUI - Associated Universities Inc., 2013
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
 * "@(#) $Id: bulkDataNTThreadSyncGuard.h,v 1.1 2013/02/11 18:37:33 rbourtem Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * rbourtem  2013-02-13  created from CorrThreadSyncGuard.h file
 */

//
// System stuff
//

//
// ACS stuff
//
#include <acstimeTimeUtil.h>

//
// CORR stuff
//
#include <bulkDataNTGenEx.h>

namespace AcsBulkdata
{
    /** An automatic unlocking guard that allows a timed wait.
     */
    class ThreadSyncGuard
    {
    public:
        /** An empty constructor to force a healthy and no-op
         ** construction of our ACS base.
         */
        ThreadSyncGuard();

        /** Instanciates but it does not try to lock.
         */
        ThreadSyncGuard(const char *label, pthread_mutex_t *m);

        /** Instanciates and tries to lock.
         */
        ThreadSyncGuard(const char *label, pthread_mutex_t *m, const ACS::TimeInterval &to);

        /** Destructor unlocks the mutex if needed.
         */
        ~ThreadSyncGuard();

        /** Acquire with timeout.
         */
        void acquire(const ACS::TimeInterval &to);

    private:
        /** Copying from a second guard is not part of the semantic,
         ** the mutex state is not be transfer from one object to a
         ** second.
         */
        ThreadSyncGuard(const ThreadSyncGuard &);
        ThreadSyncGuard & operator=(const ThreadSyncGuard &);

        /** A short description to add meaningful content to error logs.
         */
        std::string m_label;

        /** Mutex provided by the user.
         */
        pthread_mutex_t *m_mutex_p;

        /** Whether the mutex is currently locked or not.
         */
        bool m_acquired;
    };
};

#endif

/*___oOo___*/
