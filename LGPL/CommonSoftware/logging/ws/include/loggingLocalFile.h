#ifndef loggingLocalFile_H
#define loggingLocalFile_H

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
* "@(#) $Id: loggingLocalFile.h,v 1.35 2009/01/26 06:45:26 cparedes Exp $"
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

/**
 * Implements an ACE_Log_Msg_Backend that logs to a file.
 */
class logging_EXPORT LocalFileLogger : public CacheLogger
{
public:
    /// Constructor
    LocalFileLogger ();
    
    /// Destructor
    virtual ~LocalFileLogger (void);
    
    /// Open a new connection
    virtual int open (const ACE_TCHAR * filename = 0);

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
   
    /// Returns identification string, e.g. "Local file".
    virtual const ACE_TCHAR * getIdentification();

    /// Returns the destination of the logger, e.g. "/tmp/local_cache.dat".
    virtual const ACE_TCHAR * getDestination();

  private:

    /// File handle.
    FILE * m_file;

    /// Filename.
    ACE_TCHAR * m_fileName;

    /// New line length.
    int m_newLineLen;

};

#endif /* loggingLocalFile_H */

// ************************************************************************
//
// REVISION HISTORY:
//
// $Log: loggingLocalFile.h,v $
// Revision 1.35  2009/01/26 06:45:26  cparedes
// Changing from int to ssize_t
//
// Revision 1.34  2003/03/14 10:24:37  rgeorgie
// LGPL
//
// Revision 1.33  2002/09/23 12:43:06  vltsccm
// msekoran: loggingXMLParser fixed, memory leak removed and tat test added.
//
// Revision 1.32  2002/04/10 14:41:39  vltsccm
// logging1.32
//
// Revision 1.31  2002/03/27 16:44:24  vltsccm
// logging1.31
//
// Revision 1.30  2002/02/13 12:55:32  vltsccm
// logging1.30
//
// Revision 1.29  2002/02/08 13:40:54  vltsccm
// logging1.29
//
// Revision 1.28  2002/02/05 17:51:55  vltsccm
// logging1.28
//
// Revision 1.27  2002/02/04 08:26:29  vltsccm
// logging1.27
//
// Revision 1.26  2002/01/18 09:42:59  vltsccm
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
// Revision 1.21  2001/12/24 13:31:19  vltsccm
// logging1.21
//
// Revision 1.20  2001/12/24 13:31:19  vltsccm
// logging1.20
//
// Revision 1.19  2001/12/24 13:31:19  vltsccm
// logging1.19
//
// Revision 1.18  2001/12/24 13:31:18  vltsccm
// logging1.18
//
// Revision 1.17  2001/12/24 13:31:18  vltsccm
// logging1.17
//
// Revision 1.16  2001/12/24 13:31:18  vltsccm
// logging1.16
//
// Revision 1.15  2001/12/24 13:31:17  vltsccm
// logging1.15
//
// Revision 1.14  2001/12/24 13:31:17  vltsccm
// logging1.14
//
// Revision 1.13  2001/12/24 13:31:17  vltsccm
// logging1.13
//
// Revision 1.12  2001/12/24 13:31:16  vltsccm
// logging1.12
//
// Revision 1.11  2001/12/24 13:31:16  vltsccm
// logging1.11
//
// Revision 1.10  2001/12/24 13:31:16  vltsccm
// logging1.10
//
// Revision 1.9  2001/12/24 13:31:15  vltsccm
// logging1.9
//
// Revision 1.8  2001/12/24 13:31:15  vltsccm
// logging1.8
//
// Revision 1.7  2001/12/24 13:31:15  vltsccm
// logging1.7
//
// Revision 1.6  2001/12/24 13:31:14  vltsccm
// logging1.6
//
// Revision 1.5  2001/12/24 13:31:14  vltsccm
// logging1.5
//
// Revision 1.4  2001/12/24 13:31:14  vltsccm
// logging1.4
//
// Revision 1.3  2001/12/24 13:31:13  vltsccm
// logging1.3
//
// Revision 1.2  2001/12/24 13:31:13  vltsccm
// logging1.2
//
// Revision 1.1  2001/12/24 13:31:13  vltsccm
// logging1.1
//
// Revision 1.0  2001/12/24 13:31:13  vltsccm
// logging1.0
//
//
// ************************************************************************
