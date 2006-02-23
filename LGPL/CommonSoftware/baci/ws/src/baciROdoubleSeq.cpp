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
* "@(#) $Id: baciROdoubleSeq.cpp,v 1.94 2004/06/23 09:51:54 bjeram Exp $"
*
* who       when        what
* --------  ---------   ----------------------------------------------
* bjeram    2003/02/20  removed everything
* msekoran  2002/02/10  created
*/


#include "baciROdoubleSeq.h"
#include "baciAlarm_T.i"
#include "baciROSeqContImpl_T.i"
#include "baciMonitor_T.i"

template class  Monitor<ACS_MONITOR_SEQ(double, CORBA::Double)>;
template class ROSeqContImpl<ACS_RO_SEQ_T(double, CORBA::Double)>;

/*___oOo___*/
