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
* "@(#) $Id: baciTypelessPropertyImpl.cpp,v 1.13 2008/10/01 02:26:45 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram 2003-02-05*created 
*/

#include "baciDB.h"
#include "baciTypelessPropertyImpl.h"
#include <string.h>

namespace baci {

TypelessPropertyImpl::TypelessPropertyImpl(const ACE_CString& name, BACIComponent* component_p) :
    PropertyImpl(name, component_p)
{
 if (!readCharacteristics()) 
    {
		ACS_LOG(LM_RUNTIME_CONTEXT, "baci::TypelessPropertyImpl::TypelessPropertyImpl",
			(LM_ERROR, "Failed to read static data for '%s'", name.c_str()));
		return;
    }

}//TypelessPropertyImpl

TypelessPropertyImpl::~TypelessPropertyImpl()
{ 

}//~TypelessPropertyImpl

bool TypelessPropertyImpl::readCharacteristics()
{
    
  DAONode* dao = getDAONode();
  if (!dao)
      return false;
  
  try
      {
      CORBA::String_var str;
      
      str = dao->get_string("description");
      description_m = str.in();
      
      str = dao->get_string("format");
      format_m = str.in();
      
      str = dao->get_string("units");
      units_m = str.in();

      resolution_m = dao->get_long("resolution");
       
       str = dao->get_string("initialize_devio");
       if(strcmp(str.in(), "false") == 0 || strcmp(str.in(), "0") == 0 ) initializeDevIO_m = false;
       else initializeDevIO_m = true;

      return true;
      }
  catch (ACSErr::ACSbaseExImpl& ex)
      {
      ex.log();
      return false;
      }
  catch (...)
      {
      return false;
      }

}

char * 
TypelessPropertyImpl::description ()
{
  
  return CORBA::string_dup (desc_mription.c_str());
}

char * 
TypelessPropertyImpl::format ()
{
  
  return CORBA::string_dup (format_m.c_str());
}

 CORBA::Boolean
 TypelessPropertyImpl::initialize_devio ()
 {
  return initializeDevIO_m;


char * 
TypelessPropertyImpl::units ()
{
  
  return CORBA::string_dup (units_m.c_str());
}

ACS::pattern 
TypelessPropertyImpl::resolution ()
{
  
  return resolution_m;
}

 }; 
















