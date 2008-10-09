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
* "@(#) $Id: baciCharacteristicModelImpl.cpp,v 1.17 2008/10/09 06:18:16 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram 2003-01-24 created 
*/

#include "baciDB.h"
#include "baciCharacteristicModelImpl.h"
#include "baciCDBPropertySet.h"
#include <cdbErrType.h>

using namespace std;

namespace baci {

PortableServer::POA_ptr CharacteristicModelImpl::offShootPOA = 0;

CharacteristicModelImpl::CharacteristicModelImpl(const ACE_CString& model_name, maci::ContainerServices* containerServices) :
    model_name_m(model_name), m_daoNode_p(0)
{

    ACS_TRACE("baci::CharacteristicModelImpl::CharacteristicModelImpl");

    // create DAO node
    try
	{
	string prefixedName("alma/");
	prefixedName.append(model_name.c_str());
	
	CDB::DAL_var dal = containerServices->getCDB();
	//PortableServer::POA_var offShootPOA = containerServices->createOffShootPOA();
	if (!offShootPOA)
	    {
	    PortableServer::POA_var op = containerServices->createOffShootPOA();
	    offShootPOA = op._retn();
	    }
	m_daoNode_p = new cdb::DAONode(prefixedName.c_str(), dal.in(), offShootPOA/*.in()*/);
	}

    /**
     * @todo This exception handling has be added to allow (for the time being)
     *       having Characteristic Components with no CDB.
     *       But they should actually be only ACSComponents
     *       This deprecated code will be removed later on
     *       The original code had return here, but then registerCharacteristicModel()
     *       was not called.
     *       Also, erro handling should be changed to build a real error stack.
     */ 
    catch (cdbErrType::CDBRecordDoesNotExistExImpl& ex)
	{
	ex.log();
	ACS_LOG(LM_RUNTIME_CONTEXT, "baci::CharacteristicModelImpl::CharacteristicModelImpl",
		(LM_ERROR, "ATTENTION CDB record for Characteristic Model '%s' "
                           "does not exist. Continue assuming there are actually no "
                           "characteristics. This should then be just a simple ACSComponent", 
		 model_name.c_str()));
	}
    catch (ACSErr::ACSbaseExImpl& ex)
	{
	ex.log();
	ACS_LOG(LM_RUNTIME_CONTEXT, "baci::CharacteristicModelImpl::CharacteristicModelImpl",
		(LM_ERROR, "Failed to create characteristic model for '%s'", model_name.c_str()));
	return;
	}
    catch (...)
	{
	ACS_LOG(LM_RUNTIME_CONTEXT, "baci::CharacteristicModelImpl::CharacteristicModelImpl",
		(LM_ERROR, "Failed to create characteristic model for '%s'", model_name.c_str()));
	return;
	}

   if (CDBPropertySet::getInstance()!=0)
       CDBPropertySet::getInstance()->registerCharacteristicModel(model_name_m.c_str(), this);   

    if (readCharacteristics()==false) 
	{
	ACS_LOG(LM_RUNTIME_CONTEXT, "baci::CharacteristicModelImpl::CharacteristicModelImpl",
		(LM_ERROR, "Failed to read static data for '%s'", model_name_m.c_str()));
	return;
	}

}//CharacteristicModelImpl

CharacteristicModelImpl::CharacteristicModelImpl(const ACE_CString& child_name, CharacteristicModelImpl* parentModel)
//   : model_name_m(parentModel->model_name_m+"/"+child_name.c_str()), m_daoNode_p(0)
    : model_name_m(child_name.c_str()), m_daoNode_p(0)
{
    ACS_TRACE("baci::CharacteristicModelImpl::CharacteristicModelImpl");

    // extract child name
    // TODO GCH here we should rather use getDAONode and handle the exception
    if (!parentModel || !parentModel->m_daoNode_p)
	{
	ACS_LOG(LM_RUNTIME_CONTEXT, "baci::CharacteristicModelImpl::CharacteristicModelImpl",
		(LM_ERROR, "Failed to create child characteristic model for '%s' - parent model is NULL.", model_name_m.c_str()));
	return;
	}

    // extract child name
    if (parentModel->model_name_m.length() >= child_name.length() ||
	ACE_OS::strncmp(child_name.c_str(), parentModel->model_name_m.c_str(),
			parentModel->model_name_m.length()) != 0)
	{
	ACS_LOG(LM_RUNTIME_CONTEXT, "baci::CharacteristicModelImpl::CharacteristicModelImpl",
		(LM_ERROR, "Failed to create child characteristic model for '%s' - child model name does not starts with parent name.", model_name_m.c_str()));
	return;
	}

    const char * onlyChildName = (child_name.c_str()+parentModel->model_name_m.length()+1);

    // create DAO node
    try
	{
	m_daoNode_p = parentModel->getDAONode()->createChild(onlyChildName);
	}
    /**
     * TODO This exception handling has be added to allow (for the time being)
     *      having Characteristic Components with no CDB.
     *      But they should actually be only ACSComponents
     *      This deprecated code will be removed later on
     *      The original code had return here, but then registerCharacteristicModel()
     *      was not called.
     *      Also, erro handling should be changed to build a real error stack.
     */ 
    catch (cdbErrType::CDBRecordDoesNotExistExImpl& ex)
	{
	ex.log();
	ACS_LOG(LM_RUNTIME_CONTEXT, "baci::CharacteristicModelImpl::CharacteristicModelImpl",
		(LM_ERROR, "ATTENTION CDB record for Characteristic Model '%s' "
                           "does not exist. Continue assuming there are actually no "
                           "characteristics. This should then be just a simple ACSComponent", 
		 onlyChildName));
	}
    catch (ACSErr::ACSbaseExImpl& ex)
	{
	ex.log();
	ACS_LOG(LM_RUNTIME_CONTEXT, "baci::CharacteristicModelImpl::CharacteristicModelImpl",
		(LM_ERROR, "Failed to create child characteristic model for '%s'", model_name_m.c_str()));
	return;
	}
    catch (...)
	{
	ACS_LOG(LM_RUNTIME_CONTEXT, "baci::CharacteristicModelImpl::CharacteristicModelImpl",
		(LM_ERROR, "Failed to create child characteristic model for '%s'", model_name_m.c_str()));
	return;
	}

    if (CDBPropertySet::getInstance()!=0)
       CDBPropertySet::getInstance()->registerCharacteristicModel(model_name_m.c_str(), this);   

   if (readCharacteristics()==false) 
	{
	ACS_LOG(LM_RUNTIME_CONTEXT, "baci::CharacteristicModelImpl::CharacteristicModelImpl",
		(LM_ERROR, "Failed to read static data for '%s'", model_name_m.c_str()));
	return;
	}

}

CharacteristicModelImpl::~CharacteristicModelImpl()
{ 
   if (CDBPropertySet::getInstance()!=0)
       CDBPropertySet::getInstance()->deregisterCharacteristicModel(model_name_m.c_str());   

    if (m_daoNode_p)
	delete m_daoNode_p;
}//~CharacteristicModelImpl



cdb::DAONode* CharacteristicModelImpl::getDAONode() const 
{
    if(m_daoNode_p == NULL)
	{
	cdbErrType::CDBRecordDoesNotExistExImpl ex = 
	    cdbErrType::CDBRecordDoesNotExistExImpl(__FILE__,
						    __LINE__,
						    "CharacteristicModelImpl::getDAONode");
	ex.setCurl(model_name_m.c_str());
	
	throw ex;
	}
    return m_daoNode_p;
}

CosPropertyService::PropertySet_ptr
CharacteristicModelImpl::get_all_characteristics (
	    )
{
    if (CDBPropertySet::getInstance()==0)
	throw CORBA::NO_RESOURCES();

    return CDBPropertySet::getInstance()->getPropertySet(model_name_m.c_str());
}

CORBA::Any *
CharacteristicModelImpl::get_characteristic_by_name (const char * name)
{
    /*
     * TODO GCH
     * Here we should have ACS exception trace and not just
     * an exception catched and a new one thrown
     */
    try
	{
	CORBA::String_var strVal = getDAONode()->get_string(name);

	CORBA::Any * value_p = new CORBA::Any();
	(*value_p) <<= strVal.in();
	
	return value_p;
	}
    catch (cdbErrType::CDBFieldDoesNotExistEx& fde)
	{
	ACS::NoSuchCharacteristic nsc;
	nsc.characteristic_name = CORBA::string_dup(name);
	nsc.component_name = CORBA::string_dup(model_name_m.c_str());
	throw nsc;
	}
    catch (cdbErrType::CDBRecordDoesNotExistEx& rde)
	{
	ACS::NoSuchCharacteristic nsc;
	nsc.characteristic_name = CORBA::string_dup(name);
	nsc.component_name = CORBA::string_dup(model_name_m.c_str());
	throw nsc;
	}
    catch (CORBA::SystemException& se)
	{
	throw;
	}
    catch (...)
	{
	throw CORBA::UNKNOWN();
	}

}

ACS::stringSeq *
CharacteristicModelImpl::find_characteristic (const char * reg_exp)
{
    cdb::DAONode* node;

    /*
     * TODO GCH
     * This works but is not nice.
     * We should rather extend the signature of the method with
     * new exceptions and we should send back an exception
     * if there is no characteristic to search for.
     */ 
    try
	{
	node = getDAONode();
	}
    catch (cdbErrType::CDBRecordDoesNotExistExImpl& rde)
	{
	ACS::stringSeq_var strSeq = new ACS::stringSeq(0);
	return strSeq._retn();
	}

    CDB::stringSeq_var allSeq = node->get_string_seq("");
	
    CORBA::ULong maxSize = allSeq->length();
    
    // filter
    ACS::stringSeq_var strSeq = new ACS::stringSeq(maxSize);
    strSeq->length(maxSize);
    
    CORBA::ULong n = 0UL;
    for (CORBA::ULong i = 0; i < maxSize; i++) 
	{
	if (Wildcard::wildcardfit(reg_exp, allSeq[i].in())==1)
	    {
	    strSeq[n++] = CORBA::string_dup(allSeq[i].in());
	    }
	}
    strSeq->length(n);
    return strSeq._retn();
}

bool CharacteristicModelImpl::readCharacteristics()
{
    return true;
}

 }; 
