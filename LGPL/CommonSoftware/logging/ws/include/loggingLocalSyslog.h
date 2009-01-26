#ifndef loggingLocalSyslog_H
#define loggingLocalSyslog_H

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
* "@(#) $Id: loggingLocalSyslog.h,v 1.35 2009/01/26 06:45:26 cparedes Exp $"
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
#include <loggingCacheLogger.h>

#include <ace/Log_Record.h>
#include <ace/Log_Msg_Backend.h>

#if !defined(MAKE_VXWORKS) && !defined(ACS_HAS_WIN32)
#define ACS_HAS_LOCAL_SYSLOG_CALLS
#endif


#ifdef ACS_HAS_LOCAL_SYSLOG_CALLS

#include <syslog.h>

/**
 * Implements an ACE_Log_Msg_Backend that logs to a local syslog.
 */

class logging_EXPORT LocalSyslogLogger : public CacheLogger
{
public:
    /// Constructor
    /// facility has to be already shifted left by 3 bits.
    LocalSyslogLogger (int facility = LOG_USER);
    
    /// Destructor
    virtual ~LocalSyslogLogger (void);
    
    /// Open a new connection
    virtual int open (const ACE_TCHAR * ident = 0);

    /// No-op for UDP.
    virtual int reset (void);

    /// No-op for UDP.
    virtual int close (void);
    
    /// ACE_Log_Record with msg_data and priority set.
    /// Logs the record's msg_data() and maps ACE priority to syslog priority.
    /// ACE_Log_Record.msg_data() copies message (performacne concern).
    virtual ssize_t log (ACE_Log_Record &log_record);

    /// Send log.
    virtual ssize_t log (int priority, const ACE_TCHAR * msg);
    
    /// Returns identification string, e.g. "Local syslog".
    virtual const ACE_TCHAR * getIdentification();

    /// Returns the destination of the logger, e.g. "syslog".
    virtual const ACE_TCHAR * getDestination();

  private:

    /// syslog facility.
    int m_facility;

};

#endif       /* ACS_HAS_LOCAL_SYSLOG_CALLS */

#endif /* loggingLocalSyslog_H */

// ************************************************************************
//
// REVISION HISTORY:
//
// $Log: loggingLocalSyslog.h,v $
// Revision 1.35  2009/01/26 06:45:26  cparedes
// Changing from int to ssize_t
//
// Revision 1.34  2003/03/14 10:24:37  rgeorgie
// LGPL
//
// Revision 1.33  2002/09/23 12:43:05  vltsccm
// msekoran: loggingXMLParser fixed, memory leak removed and tat test added.
//
// Revision 1.32  2002/04/10 14:41:39  vltsccm
// logging1.32
//
// Revision 1.31  2002/03/27 16:44:23  vltsccm
// logging1.31
//
// Revision 1.30  2002/02/13 12:55:31  vltsccm
// logging1.30
//
// Revision 1.29  2002/02/08 13:40:54  vltsccm
// logging1.29
//
// Revision 1.28  2002/02/05 17:51:55  vltsccm
// logging1.28
//
// Revision 1.27  2002/02/04 08:26:28  vltsccm
// logging1.27
//
// Revision 1.26  2002/01/18 09:42:58  vltsccm
// logging1.26
//
// Revision 1.25  2002/01/16 10:41:29  vltsccm
// logging1.25
//
// Revision 1.24  2002/01/15 12:42:20  vltsccm
// logging1.24
//
// Revision 1.23  2002/01/14 21:10:49  vltsccm
// logging1.23
//
// Revision 1.22  2001/12/27 19:04:09  vltsccm
// logging1.22
//
// Revision 1.21  2001/12/24 13:31:12  vltsccm
// logging1.21
//
// Revision 1.20  2001/12/24 13:31:12  vltsccm
// logging1.20
//
// Revision 1.19  2001/12/24 13:31:12  vltsccm
// logging1.19
//
// Revision 1.18  2001/12/24 13:31:11  vltsccm
// logging1.18
//
// Revision 1.17  2001/12/24 13:31:11  vltsccm
// logging1.17
//
// Revision 1.16  2001/12/24 13:31:11  vltsccm
// logging1.16
//
// Revision 1.15  2001/12/24 13:31:10  vltsccm
// logging1.15
//
// Revision 1.14  2001/12/24 13:31:10  vltsccm
// logging1.14
//
// Revision 1.13  2001/12/24 13:31:10  vltsccm
// logging1.13
//
// Revision 1.12  2001/12/24 13:31:09  vltsccm
// logging1.12
//
// Revision 1.11  2001/12/24 13:31:09  vltsccm
// logging1.11
//
// Revision 1.10  2001/12/24 13:31:09  vltsccm
// logging1.10
//
// Revision 1.9  2001/12/24 13:31:08  vltsccm
// logging1.9
//
// Revision 1.8  2001/12/24 13:31:08  vltsccm
// logging1.8
//
// Revision 1.7  2001/12/24 13:31:08  vltsccm
// logging1.7
//
// Revision 1.6  2001/12/24 13:31:07  vltsccm
// logging1.6
//
// Revision 1.5  2001/12/24 13:31:07  vltsccm
// logging1.5
//
// Revision 1.4  2001/12/24 13:31:07  vltsccm
// logging1.4
//
// Revision 1.3  2001/12/24 13:31:06  vltsccm
// logging1.3
//
// Revision 1.2  2001/12/24 13:31:06  vltsccm
// logging1.2
//
// Revision 1.1  2001/12/24 13:31:06  vltsccm
// logging1.1
//
// Revision 1.0  2001/12/24 13:31:06  vltsccm
// logging1.0
//
//
// ************************************************************************
