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
* "@(#) $Id: baciPpatternImpl.cpp,v 1.18 2008/10/01 02:26:45 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram  2003/02/20    created
*/


#include "baciPpatternImpl.h"


namespace baci {

/////////////////////////////////////////////////
// Ppattern
/////////////////////////////////////////////////

PpatternImpl::PpatternImpl(const ACE_CString& name, BACIProperty *property_p /*, BACIComponent* component_p, DevIO<ACS::pattern>* devIO*/)
{

  ACS_TRACE("baci::PpatternImpl::PpatternImpl");
  
  // initialize
  whenSet_m.length(0);
  whenCleared_m.length(0);
  bitDescription_m.length(0);
  // read static data
  if (readCharacteristics(property_p->getCharacteristicModel())==false) 
    {
		ACS_LOG(LM_RUNTIME_CONTEXT, "baci::ROpatternImpl::ROpatternImpl",
			(LM_ERROR, "Failed to read static data for '%s'", name.c_str()));
		return;
    }
  
  
  ACS_DEBUG("baci::PpatternImpl::PpatternImpl", "Successfully created.");
}
 
PpatternImpl::~PpatternImpl()
{
  ACS_TRACE("baci::PpatternImpl::~PpatternImpl");
}

bool PpatternImpl::readCharacteristics(CharacteristicModelImpl *model)
{
    
  cdb::DAONode* dao = model->getDAONode();
  if (!dao)
      return false;
  
  try
      {
      
      CDB::stringSeq_var descs = dao->get_string_seq("bitDescription");
      CORBA::ULong len = descs->length();
      bitDescription_m.length(len);
      for (CORBA::ULong i = 0; i < len; i++)
          bitDescription_m[i] = CORBA::string_dup(descs[i].in());

      CDB::longSeq_var ls = dao->get_long_seq("whenSet");
      len = ls->length();
      whenSet_m.length(len);
      for (CORBA::ULong i = 0; i < len; i++)
          whenSet_m[i] = static_cast<ACS::Condition>(ls[i]);

      CDB::longSeq_var lc = dao->get_long_seq("whenCleared");
      len = lc->length();
      whenCleared_m.length(len);
      for (CORBA::ULong i = 0; i < len; i++)
          whenCleared_m[i] = static_cast<ACS::Condition>(lc[i]);

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

/* ---------------------- [ CORBA interface ] ---------------------- */

ACS::stringSeq *
PpatternImpl::bitDescription ()
{
  

  // create and initialize the return parameter
  ACS::stringSeq_var result = new ACS::stringSeq(bitDescription_m);
  // to return, take the sequence away from the _var
  return result._retn();
}

ACS::ConditionSeq *
PpatternImpl::whenSet ()
{
  

  // create and initialize the return parameter
  ACS::ConditionSeq_var result = new ACS::ConditionSeq(whenSet_m);
  // to return, take the sequence away from the _var
  return result._retn();
}


ACS::ConditionSeq *
PpatternImpl::whenCleared ()
{
  

  // create and initialize the return parameter
  ACS::ConditionSeq_var result = new ACS::ConditionSeq(whenCleared_m);
  // to return, take the sequence away from the _var
  return result._retn();
}

 }; 

/*___oOo___*/













