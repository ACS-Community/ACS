#ifndef loggingCacheLogger_H
#define loggingCacheLogger_H

/*******************************************************************************
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *
 * "@(#) $Id: loggingCacheLogger.h,v 1.36 2009/01/26 06:45:26 cparedes Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * msekoran  2001-12-17  created.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsutil.h>
#include <loggingExport.h>

#include <ace/Log_Record.h>
#include <ace/Log_Msg_Backend.h>

/**
 * Implements an ACE_Log_Msg_Backend that logs cache.
 * (In case of centralized logger could not be contancted.)
 */

class logging_EXPORT CacheLogger : public ACE_Log_Msg_Backend
{
  public:
    
    /// Destructor
    virtual ~CacheLogger (void) {};
    
    /// Open a new connection
    virtual int open (const ACE_TCHAR * key = 0) = 0;
    
    /// No-op for UDP.
    virtual int reset (void) = 0;
    
    /// No-op for UDP.
    virtual int close (void) = 0;
    
    /// ACE_Log_Record with msg_data and priority set.
    /// Logs the record's msg_data() and maps ACE priority to syslog priority.
    /// ACE_Log_Record.msg_data() copies message (performacne concern).
    virtual ssize_t log (ACE_Log_Record &log_record) = 0;
    
    /// Send log.
    virtual ssize_t log (int priority, const ACE_TCHAR * msg) = 0;
    
    /// Returns identification string, e.g. "Local syslog".
    virtual const ACE_TCHAR * getIdentification() = 0;
    
    /// Returns the destination of the logger, e.g. "dina.ijs.si".
    virtual const ACE_TCHAR * getDestination() = 0;
    
};

#endif /* loggingCacheLogger_H */

