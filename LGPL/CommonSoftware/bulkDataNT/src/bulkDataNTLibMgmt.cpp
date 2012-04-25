/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2011
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
*
* "@(#) $Id: bulkDataNTLibMgmt.cpp,v 1.1 2012/04/25 12:28:10 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/

#include "bulkDataNTDDSLoggable.h"

/// function that will be called when bulkDataNT shared library is unloaded

void __attribute__ ((destructor)) bulkDataNTunload(void);

void  bulkDataNTunload(void)
{
	if(BulkDataNTDDSLoggable::logger_mp){
		BulkDataNTDDSLoggable::logger_mp->flush();
		delete BulkDataNTDDSLoggable::logger_mp; // ...  but we have just one proxy object for all DDS reader threads
		BulkDataNTDDSLoggable::logger_mp = 0;
	}//if
}//bulkDataNT_unload

