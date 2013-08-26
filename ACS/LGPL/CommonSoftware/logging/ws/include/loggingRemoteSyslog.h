#ifndef loggingRemoteSyslog_H
#define loggingRemoteSyslog_H

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
* "@(#) $Id: loggingRemoteSyslog.h,v 1.37 2012/02/29 12:50:09 tstaig Exp $"
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

#include <ace/SString.h>

#ifndef ACS_HAS_LOCAL_SYSLOG_CALLS

/*
syslog priorities, facilities
-----------------------------
*/

#define	LOG_EMERG	0	// system is unusable 
#define	LOG_ALERT	1	// action must be taken immediately 
#define	LOG_CRIT	2	// critical conditions 
#define	LOG_ERR		3	// error conditions 
#define	LOG_WARNING	4	// warning conditions 
#define	LOG_NOTICE	5	// normal but significant condition 
#define	LOG_INFO	6	// informational 
#define	LOG_DEBUG	7	// debug-level messages

#define	LOG_USER	(1<<3)	// random user-level messages

#define	LOG_LOCAL0	(16<<3)
#define	LOG_LOCAL1	(17<<3)
#define	LOG_LOCAL2	(18<<3)
#define	LOG_LOCAL3	(19<<3)
#define	LOG_LOCAL4	(20<<3)
#define	LOG_LOCAL5	(21<<3)
#define	LOG_LOCAL6	(22<<3)
#define	LOG_LOCAL7	(23<<3)

#else

#include <syslog.h>

#endif

#include <ace/SOCK_Dgram.h>
#include <ace/INET_Addr.h>

/**
 * Implements an ACE_Log_Msg_Backend that logs to a remote syslog 
 * using User Datagram Protocal (UDP).
 */
class logging_EXPORT RemoteSyslogLogger : public CacheLogger
{
public:

    /// Structure used for facility name to int value mapping.
    typedef struct {
	const ACE_TCHAR * name;
	int value;
    } facilityPair;
    
    /// Facility name to int value mapping table.
    static facilityPair m_facilityNames[];

    /// Constructor
    /// facility has to be already shifted left by 3 bits.
    RemoteSyslogLogger (int facility = LOG_USER);
    
    /// Destructor
    virtual ~RemoteSyslogLogger (void);
    
    /// Open a new connection
    virtual int open (const ACE_TCHAR * host);

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
     
    /// Returns identification string, e.g. "Remote syslog".
    virtual const ACE_TCHAR * getIdentification();

    /// Returns the destination of the logger, e.g. "dina.ijs.si".
    virtual const ACE_TCHAR * getDestination();

  private:

    /// Remote address.
    ACE_INET_Addr m_remoteAddress;

    /// UDP socket object.
    ACE_SOCK_Dgram m_socket;

    /// syslog facility.
    int m_facility;

    /// Remote address.
    ACE_CString m_address;

};

#endif /* loggingRemoteSyslog_H */

// ************************************************************************
//
// REVISION HISTORY:
//
// $Log: loggingRemoteSyslog.h,v $
// Revision 1.37  2012/02/29 12:50:09  tstaig
// Changes were made to remove warning "'visibility' attribute ignored on non-class
// types" messages.
//
// Revision 1.36  2012/01/20 22:07:52  tstaig
// Backport from branches ACS-9_0_0-windows-B and ACS-9_1_0-windows-B to support
// ACS on Windows under Cygwin. This commit corresponds to the folowing
// CommonSoftware modules:
// jacsutil acsEclipseUtils xmljbind xmlpybind acserridl acsidlcommon acsutil
// acsutilpy acsstartup loggingidl logging acserr acserrTypes acsQoS
// Along with adding dependencies for some libraries in acsdaemon and acstime
// modules so they would be built correctly.
//
// Revision 1.35  2009/01/26 06:45:26  cparedes
// Changing from int to ssize_t
//
// Revision 1.34  2003/03/14 10:24:37  rgeorgie
// LGPL
//
// Revision 1.33  2002/09/23 12:43:05  vltsccm
// msekoran: loggingXMLParser fixed, memory leak removed and tat test added.
//
// Revision 1.32  2002/04/10 14:41:38  vltsccm
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
// Revision 1.28  2002/02/05 17:51:54  vltsccm
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
// Revision 1.24  2002/01/15 12:42:19  vltsccm
// logging1.24
//
// Revision 1.23  2002/01/14 21:10:49  vltsccm
// logging1.23
//
// Revision 1.22  2001/12/27 19:04:09  vltsccm
// logging1.22
//
// Revision 1.21  2001/12/24 13:31:05  vltsccm
// logging1.21
//
// Revision 1.20  2001/12/24 13:31:05  vltsccm
// logging1.20
//
// Revision 1.19  2001/12/24 13:31:05  vltsccm
// logging1.19
//
// Revision 1.18  2001/12/24 13:31:04  vltsccm
// logging1.18
//
// Revision 1.17  2001/12/24 13:31:04  vltsccm
// logging1.17
//
// Revision 1.16  2001/12/24 13:31:04  vltsccm
// logging1.16
//
// Revision 1.15  2001/12/24 13:31:03  vltsccm
// logging1.15
//
// Revision 1.14  2001/12/24 13:31:03  vltsccm
// logging1.14
//
// Revision 1.13  2001/12/24 13:31:03  vltsccm
// logging1.13
//
// Revision 1.12  2001/12/24 13:31:02  vltsccm
// logging1.12
//
// Revision 1.11  2001/12/24 13:31:02  vltsccm
// logging1.11
//
// Revision 1.10  2001/12/24 13:31:02  vltsccm
// logging1.10
//
// Revision 1.9  2001/12/24 13:31:01  vltsccm
// logging1.9
//
// Revision 1.8  2001/12/24 13:31:01  vltsccm
// logging1.8
//
// Revision 1.7  2001/12/24 13:31:01  vltsccm
// logging1.7
//
// Revision 1.6  2001/12/24 13:31:00  vltsccm
// logging1.6
//
// Revision 1.5  2001/12/24 13:31:00  vltsccm
// logging1.5
//
// Revision 1.4  2001/12/24 13:31:00  vltsccm
// logging1.4
//
// Revision 1.3  2001/12/24 13:30:59  vltsccm
// logging1.3
//
// Revision 1.2  2001/12/24 13:30:59  vltsccm
// logging1.2
//
// Revision 1.1  2001/12/24 13:30:59  vltsccm
// logging1.1
//
// Revision 1.0  2001/12/24 13:30:59  vltsccm
// logging1.0
//
//
// ************************************************************************
