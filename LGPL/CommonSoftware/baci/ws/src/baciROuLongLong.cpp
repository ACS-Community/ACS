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
* who       when      what
* --------  --------  ----------------------------------------------
* created  2003/07/10  created
*/


#include "baciROuLongLong.h"
#include "baciAlarm_T.i"
#include "baciAlarmSystemMonitorCont_T.i"
#include "baciROcontImpl_T.i"
#include "baciMonitor_T.i"

template class baci::Monitor<ACS_MONITOR(uLongLong, ACS::uLongLong)>;
template class baci::ROcommonImpl<ACS_RO_T(uLongLong, ACS::uLongLong)>;
template class baci::ROcontImpl<ACS_RO_T(uLongLong, ACS::uLongLong)>;
/*___oOo___*/

