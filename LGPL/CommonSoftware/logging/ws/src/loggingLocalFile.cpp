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
* "@(#) $Id: loggingLocalFile.cpp,v 1.35 2009/01/26 06:45:26 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* almamgr 2002-02-05 m_filename is allocated with strdup: free and not delete[].
* msekoran  2001-12-17  created.
*/

#include <vltPort.h>
#include <logging.h>
#include <loggingLocalFile.h>

LocalFileLogger::LocalFileLogger () : m_file(0), m_fileName(0)
{
    m_newLineLen = ACE_OS::strlen("\n");
}

LocalFileLogger::~LocalFileLogger (void)
{
    close();
}

int
LocalFileLogger::open (const ACE_TCHAR * filename)
{
    if (m_file || !filename)
	return 2;

    // add pid to syslog ident (process name)
    // watch if LoggingProxy::ProcessName() is unitialized and is 0
    ACE_CString processName = "unknown";
    if (LoggingProxy::ProcessName())
	processName = LoggingProxy::ProcessName();

    // only process name without any e.g. "../bin/"
    int pos = processName.rfind(ACE_DIRECTORY_SEPARATOR_CHAR);
    if (pos!=ACE_CString::npos)
	processName = processName.substr(pos+1);
    
    ACE_TCHAR file[250];
    ACE_OS::sprintf(file, "%s_%s_%lu", filename, processName.c_str(), (unsigned long)ACE_OS::getpid());

    m_file = ACE_OS::fopen (file, "a");  
    if (!m_file)
	return 1;

    m_fileName = ACE_OS::strdup(file);
    return 0;
}

int
LocalFileLogger::reset (void)
{
    return 0;
}

int
LocalFileLogger::close (void)
{
    if (m_file)
    {
	ACE_OS::fclose(m_file);
	m_file = 0;
    }

    if (m_fileName)
    {
#ifndef ACS_HAS_WIN32
#ifndef MAKE_VXWORKS
	chmod(m_fileName, 0664);
#endif
#endif
	free(m_fileName);
	m_fileName = 0;
    }

    return 0;
}

ssize_t
LocalFileLogger::log (ACE_Log_Record &log_record)
{
    return log(log_record.priority(), log_record.msg_data());
}

ssize_t
LocalFileLogger::log (int priority, const ACE_TCHAR * msg)
{
    ACE_UNUSED_ARG(priority);

    if (!m_file || !msg)
	return 2;

    int len = ACE_OS::strlen(msg)+m_newLineLen;     
    if (ACE_OS::fprintf(m_file, "%s\n", msg)!=len)
	return 1;
    else
	return 0;
}

const ACE_TCHAR *
LocalFileLogger::getIdentification()
{
    return "Local file";
}

const ACE_TCHAR *
LocalFileLogger::getDestination()
{
    return m_fileName;
}

// ************************************************************************
//
// REVISION HISTORY:
//
// $Log: loggingLocalFile.cpp,v $
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
// Revision 1.31  2002/03/27 16:44:27  vltsccm
// logging1.31
//
// Revision 1.30  2002/02/13 12:55:35  vltsccm
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
// Revision 1.21  2001/12/24 13:31:49  vltsccm
// logging1.21
//
// Revision 1.20  2001/12/24 13:31:48  vltsccm
// logging1.20
//
// Revision 1.19  2001/12/24 13:31:48  vltsccm
// logging1.19
//
// Revision 1.18  2001/12/24 13:31:48  vltsccm
// logging1.18
//
// Revision 1.17  2001/12/24 13:31:48  vltsccm
// logging1.17
//
// Revision 1.16  2001/12/24 13:31:47  vltsccm
// logging1.16
//
// Revision 1.15  2001/12/24 13:31:47  vltsccm
// logging1.15
//
// Revision 1.14  2001/12/24 13:31:47  vltsccm
// logging1.14
//
// Revision 1.13  2001/12/24 13:31:46  vltsccm
// logging1.13
//
// Revision 1.12  2001/12/24 13:31:46  vltsccm
// logging1.12
//
// Revision 1.11  2001/12/24 13:31:46  vltsccm
// logging1.11
//
// Revision 1.10  2001/12/24 13:31:45  vltsccm
// logging1.10
//
// Revision 1.9  2001/12/24 13:31:45  vltsccm
// logging1.9
//
// Revision 1.8  2001/12/24 13:31:45  vltsccm
// logging1.8
//
// Revision 1.7  2001/12/24 13:31:44  vltsccm
// logging1.7
//
// Revision 1.6  2001/12/24 13:31:44  vltsccm
// logging1.6
//
// Revision 1.5  2001/12/24 13:31:44  vltsccm
// logging1.5
//
// Revision 1.4  2001/12/24 13:31:43  vltsccm
// logging1.4
//
// Revision 1.3  2001/12/24 13:31:43  vltsccm
// logging1.3
//
// Revision 1.2  2001/12/24 13:31:43  vltsccm
// logging1.2
//
// Revision 1.1  2001/12/24 13:31:42  vltsccm
// logging1.1
//
// Revision 1.0  2001/12/24 13:31:42  vltsccm
// logging1.0
//
//
// ************************************************************************
