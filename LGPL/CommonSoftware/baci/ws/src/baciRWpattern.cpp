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
* "@(#) $Id: baciRWpattern.cpp,v 1.106 2006/09/01 02:20:54 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram    2003/02/22  template impl. used
* msekoran  2001/12/28  modified 
*/

#include "baciRWpattern.h"
#include "baciRWdiscImpl_T.i"


namespace baci {

template class RWdiscImpl<ACS_RW_T(pattern, ACS::pattern)>;

RWpatternImpl::RWpatternImpl(const ACE_CString& name, BACIComponent *component_p, DevIO<ACS::pattern> *devIO, bool flagdeldevIO) :
    RWdiscImpl<ACS_RW_T(pattern, ACS::pattern)>(name, component_p, devIO, flagdeldevIO, false),
    PpatternImpl(name, this->getProperty())
{
    initialization_m = 0;
    ACS_DEBUG("baci::RWpatternImpl::RWpatternImpl", "Successfully created.");
}

RWpatternImpl::~RWpatternImpl()
{
    ACS_TRACE("baci::RWpatternImpl::~RWpatternImpl");
}

 }; 

/*___oOo___*/


