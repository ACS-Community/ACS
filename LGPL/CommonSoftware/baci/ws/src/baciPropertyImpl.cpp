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
* "@(#) $Id: baciPropertyImpl.cpp,v 1.11 2008/10/01 02:26:45 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram 2003-02-05*created 
*/

#include "baciDB.h"
#include "baciPropertyImpl.h"

namespace baci {

PropertyImpl::PropertyImpl(const ACE_CString& name, BACIComponent* component_p) :
    CharacteristicModelImpl(name, component_p->getCharacteristicModel()),
    component_mp(component_p)
{
    ACS_TRACE("baci::PropertyImpl::PropertyImpl");
}//PropertyImpl

PropertyImpl::~PropertyImpl()
{ 
    ACS_TRACE("PropertyImpl::~PropertyImpl");
}//~PropertyImpl

char *
PropertyImpl::name ()
{
  return CORBA::string_dup(name_m.c_str());           
}

char *
PropertyImpl::characteristic_component_name ()
{
  
  return CORBA::string_dup (component_mp->getName());
}

 }; 












