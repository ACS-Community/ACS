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
* "@(#) $Id: baciROlong.cpp,v 1.100 2009/09/15 08:52:12 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* rcirami 2003-03-13 removed all the code
* oat      2003/01/21 added templates for Monitors
* bgustafs 2001-07-12 removed warnings for unused arguments
* msekoran  2001/03/10  modified 
*/

#include "baciROlong.h"
#include "baciAlarm_T.i"
#include "baciAlarmSystemMonitorCont_T.i"
#include "baciROcontImpl_T.i"
#include "baciMonitor_T.i"

template class baci::Monitor<ACS_MONITOR(long, CORBA::Long)>;
template class baci::ROcommonImpl<ACS_RO_T(long, CORBA::Long)>;
template class baci::ROcontImpl<ACS_RO_T(long, CORBA::Long)>;
/*___oOo___*/

