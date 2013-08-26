#ifndef bulkDataNTPosixHelper_H
#define bulkDataNTPosixHelper_H
/*
 * @(#) $Id: bulkDataNTPosixHelper.h,v 1.1 2013/02/11 18:37:33 rbourtem Exp $
 *
 * Copyright (C) 2013
 * Associated Universities, Inc. Washington DC, USA.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Library General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
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
 * Correspondence concerning ALMA Software should be addressed as follows:
 * Internet email: alma-sw-admin@nrao.edu
 */

//
// System stuff
//
#include <pthread.h>
#include <semaphore.h>

//
// ACS stuff
//
#include <acstime.h>


namespace AcsBulkdata
{

namespace Pthread
{
/** Helper functions to wait on a conditional variable.
 */
namespace CondVar
{
bool wait(pthread_cond_t &cond, pthread_mutex_t &mux, const ACS::TimeInterval to);
};

/** Mutex functions.
 */
namespace Mutex
{
/** Initialize given mutex as recursive or not recursive.
 */
void init(pthread_mutex_t &m, const bool recursive);

/** Helper class to block on a pthread mutex for a given time.
 ** @param desc description of the mutex to add context to eventual
 ** error logs.
 ** @param m pointer to pthread mutex.
 ** @param to timeout interval in ACS units.
 */
bool lock(const char *desc, pthread_mutex_t &m, const ACS::TimeInterval &to, bool logTimeout = false);

/** Helper class to unlock a pthread mutex.
 ** @param desc description of the mutex to add context to eventual
 ** error logs.
 ** @param m pointer to pthread mutex.
 */
bool unlock(const char *desc, pthread_mutex_t &m);

/** Helper class to unlock a pthread mutex.
 ** @param m pointer to pthread mutex.
 */
bool unlock(pthread_mutex_t &m);
};

/** Semaphore functions.
 */
namespace Semaphore
{
/** Helper class to wait with timeout on a posix semaphore.
 */
bool lock(const char *desc, sem_t &s, const ACS::TimeInterval &to, bool logTimeout = false);

/** Helper function to give a posix semaphore. If this function
 ** fails (return false) means that something major has broken
 ** down.
 */
bool post(const char *desc, sem_t &s);
};
};
};

#endif

/*___oOo___*/
