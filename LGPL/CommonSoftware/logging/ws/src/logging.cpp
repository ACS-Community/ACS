/*******************************************************************************
*     ALMA - Atacama Large Millimiter Array
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
*
* "@(#) $Id: logging.cpp,v 1.55 2005/08/08 22:52:06 dfugate Exp $"
*
* who       when        what
* --------  ---------   ----------------------------------------------
* acaproni  2004-11-25  Into log(), replaced < and > with { and } of the msg to avoid parsing errors
* mschilli  2004-01-05  added stdout-flush after the printf's in method log()
* bjeram    2002-03-06  changed ACE_CString AAAA(x, y) to   ACE_CString AAAA((const char*)x, y); needed because by ACE x.3
* msekoran  2002-03-18  Using getTempFileName() helper method.
* msekoran  2002-02-13  Unregistred as ACE logging callback in destructor
* bjeram    2002-01-17  Added stdout flush in flush & converting  output of ACE_OS::getpid()
*                       to unsigned long to work good on Linux and Sun
* msekoran  2001-12-28  Logs with Archive XML entry type do not got to the STDOUT
* msekoran  2001-12-17  Added CL failure detection, logging to syslog, file, ...
* bjeram    2001-11-12  added initialized flag and isInit()
* bjeram    2001-07-12  ACS_LOG_STDIO -> ACS_LOG_STDOUT
* msekoran  2001-07-12  Output to a file if CL is not available
* msekoran  2001-06-08  Implementation according new specifications
* almamgr   2000-12-03  Removed and changed to char* from filename and oldfilename
* almamgr   2000-10-19  Looging of INFO on stdout more compact than XML output
* almamgr   2000-10-19  created 
*/
//---------------------------------------------------------------------------------------
#include "logging.h"
//---------------------------------------------------------------------------------------
ACE_RCSID(logging, logging, "$Id: logging.cpp,v 1.55 2005/08/08 22:52:06 dfugate Exp $");
//---------------------------------------------------------------------------------------

