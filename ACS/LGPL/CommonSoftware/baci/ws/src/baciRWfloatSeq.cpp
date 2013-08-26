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
* E.S.O. - ACS project
*
* "@(#) $Id: baciRWfloatSeq.cpp,v 1.3 2008/07/25 07:29:52 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram    2003/02/18  replaced with templates impl
* bjeram    2002/12/20  replaced floatSeqValue with Value<ACS::floatSeq> or just Value
* msekoran  2002/01/03  created
*/

#include "baciRWfloatSeq.h"
#include "baciRWSeqContImpl_T.i"

template class baci::RWSeqContImpl<ACS_RW_SEQ_T(float, CORBA::Float)>;

/*___oOo___*/

