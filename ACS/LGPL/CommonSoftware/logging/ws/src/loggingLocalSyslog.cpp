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
* "@(#) $Id: loggingLocalSyslog.cpp,v 1.35 2009/01/26 06:45:26 cparedes Exp $"
*
* who       when        what
* --------  ---------   ----------------------------------------------
* msekoran  2001-12-17  created.
*/

#include <vltPort.h>
//#include <logging.h>
#include <loggingLocalSyslog.h>

#ifdef ACS_HAS_LOCAL_SYSLOG_CALLS

LocalSyslogLogger::LocalSyslogLogger (int facility) :
    m_facility(facility)
{
}

LocalSyslogLogger::~LocalSyslogLogger (void)
{
}

int
LocalSyslogLogger::open (const ACE_TCHAR * ident)
{
    if (ident)
	openlog(ident, 0, m_facility);
    else
	//openlog(LoggingProxy::ProcessName(), 0, m_facility);
	openlog("unknown", 0, m_facility);
    return 0;
}

int
LocalSyslogLogger::reset (void)
{
    return 0;
}

int
LocalSyslogLogger::close (void)
{
    closelog();
    return 0;
}

ssize_t
LocalSyslogLogger::log (ACE_Log_Record &log_record)
{
    return log(log_record.priority(), log_record.msg_data());
}

ssize_t
LocalSyslogLogger::log (int pri, const ACE_TCHAR * msg)
{
    if (!msg)
	return 2;

   int priority = LOG_ERR;  // default

   // convert to LM_ code
   pri = 1 << (pri-1);

   switch (pri)
    {
	case LM_TRACE:
	case LM_DEBUG:
	   priority = LOG_DEBUG;
	   break;
        case LM_INFO:
	   priority = LOG_INFO;
	   break;
        case LM_NOTICE:
	   priority = LOG_NOTICE;
	   break;
        case LM_WARNING:
	   priority = LOG_WARNING;
	   break;
        case LM_ERROR:
	   priority = LOG_ERR;
	   break;
        case LM_CRITICAL:
	   priority = LOG_CRIT;
	   break;
        case LM_ALERT:
	   priority = LOG_ALERT;
	   break;
        case LM_EMERGENCY:
	   priority = LOG_EMERG;
	   break;

	case LM_STARTUP:
	case LM_SHUTDOWN:
	default:
	  // take default
	  break;
      }

    // set facility
    priority = priority + m_facility;
  
    syslog(priority, msg);

    return 0;
}

const ACE_TCHAR *
LocalSyslogLogger::getIdentification()
{
    return "Local syslog";
}

const ACE_TCHAR *
LocalSyslogLogger::getDestination()
{
    return "syslog";
}

#endif    /* ACS_HAS_LOCAL_SYSLOG_CALLS */

// ************************************************************************
//
// REVISION HISTORY:
//
// $Log: loggingLocalSyslog.cpp,v $
// Revision 1.35  2009/01/26 06:45:26  cparedes
// Changing from int to ssize_t
//
// Revision 1.34  2003/03/14 10:24:49  rgeorgie
// LGPL
//
// Revision 1.33  2002/09/23 12:45:10  vltsccm
// msekoran: loggingXMLParser fixed, memory leak removed and tat test added.
//
// Revision 1.32  2002/04/10 14:41:41  vltsccm
// logging1.32
//
// Revision 1.31  2002/03/27 16:44:26  vltsccm
// logging1.31
//
// Revision 1.30  2002/02/13 12:55:34  vltsccm
// logging1.30
//
// Revision 1.29  2002/02/08 13:40:57  vltsccm
// logging1.29
//
// Revision 1.28  2002/02/05 17:51:57  vltsccm
// logging1.28
//
// Revision 1.27  2002/02/04 08:26:31  vltsccm
// logging1.27
//
// Revision 1.26  2002/01/18 09:43:01  vltsccm
// logging1.26
//
// Revision 1.25  2002/01/16 10:41:31  vltsccm
// logging1.25
//
// Revision 1.24  2002/01/15 12:42:23  vltsccm
// logging1.24
//
// Revision 1.23  2002/01/14 21:10:53  vltsccm
// logging1.23
//
// Revision 1.22  2001/12/27 19:04:12  vltsccm
// logging1.22
//
// Revision 1.21  2001/12/24 13:31:42  vltsccm
// logging1.21
//
// Revision 1.20  2001/12/24 13:31:41  vltsccm
// logging1.20
//
// Revision 1.19  2001/12/24 13:31:41  vltsccm
// logging1.19
//
// Revision 1.18  2001/12/24 13:31:41  vltsccm
// logging1.18
//
// Revision 1.17  2001/12/24 13:31:41  vltsccm
// logging1.17
//
// Revision 1.16  2001/12/24 13:31:40  vltsccm
// logging1.16
//
// Revision 1.15  2001/12/24 13:31:40  vltsccm
// logging1.15
//
// Revision 1.14  2001/12/24 13:31:40  vltsccm
// logging1.14
//
// Revision 1.13  2001/12/24 13:31:39  vltsccm
// logging1.13
//
// Revision 1.12  2001/12/24 13:31:39  vltsccm
// logging1.12
//
// Revision 1.11  2001/12/24 13:31:39  vltsccm
// logging1.11
//
// Revision 1.10  2001/12/24 13:31:38  vltsccm
// logging1.10
//
// Revision 1.9  2001/12/24 13:31:38  vltsccm
// logging1.9
//
// Revision 1.8  2001/12/24 13:31:38  vltsccm
// logging1.8
//
// Revision 1.7  2001/12/24 13:31:37  vltsccm
// logging1.7
//
// Revision 1.6  2001/12/24 13:31:37  vltsccm
// logging1.6
//
// Revision 1.5  2001/12/24 13:31:37  vltsccm
// logging1.5
//
// Revision 1.4  2001/12/24 13:31:36  vltsccm
// logging1.4
//
// Revision 1.3  2001/12/24 13:31:36  vltsccm
// logging1.3
//
// Revision 1.2  2001/12/24 13:31:36  vltsccm
// logging1.2
//
// Revision 1.1  2001/12/24 13:31:35  vltsccm
// logging1.1
//
// Revision 1.0  2001/12/24 13:31:35  vltsccm
// logging1.0
//
//
// ************************************************************************
