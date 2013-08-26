/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration),
*    All rights reserved
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
* "@(#) $Id: baci.cpp,v 1.106 2006/09/01 02:20:54 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate 2005-01-17 refactored due to C++ coding standard violations (e.g., this file needed to be split into separate files for each class implementation)
* almamgr 2003-05-23 removed #ifdef MAKE_VXWORKS #include <ccs.h> #include <err.h> #endif
* msekoran 2003-05-07 monitor with zero trigger time bug fixed
* msekoran 2002-03-25 sync. monitors fixed
* bgustafs 2002-02-26 changed case of longlong for monitor interval
* gchiozzi 2002-02-01 Removed ccsInit and ccsExit from TheadWorker() functions.
* gchiozzi 2001-09-21 Renamed database attributes from Archive* to archive_*
* msekoran  17/02/01  created 
*/

#include "baci.h"


ACE_RCSID(baci, baci, "$Id: baci.cpp,v 1.106 2006/09/01 02:20:54 cparedes Exp $");

namespace baci {

// make core baci indepented (avoid changing this file)
#include "baciCallbackDispatcher.cpp"

 }; 

/*___oOo___*/


