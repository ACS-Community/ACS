#ifndef logging_H
#define logging_H

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
* "@(#) $Id: logging.h,v 1.49 2006/01/05 22:59:08 dfugate Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram    2003-03-06  added #include <ace/Log_Record.h> (needed by ACE/TAO x.3)
* bjeram    2002-04-10  added setStdio
* msekoran  2001-12-17  added CL failure detection, logging to syslog, file, ...
* bjeram    2001-09-13  added logXML(...) method
* bjeram    2001-08     added ACS_LOG_TIME macro
* msekoran  2001-07-12  renamed m_data and m_flags variables in LoggingTSSStorage class
* msekoran  2001-06-08  Implementation according new specifications
* almamgr   2000-12-03  Removed static and changed to char* from filename and oldfilename
* almamgr   2000-12-03  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "loggingGetLogger.h"
#include "loggingACEMACROS.h"

#endif /*!logging_H*/
