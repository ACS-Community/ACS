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
* "@(#) $Id: baciCDBPropertySet.cpp,v 1.100 2008/10/01 02:26:45 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2001/02/21  created 
* msekoran  2001/05/13  implementation using new specs
*/

#include <vltPort.h>
#include "baciCDBPropertySet.h"

#include "baciDB.h"
#include "logging.h"

#include <orbsvcs/Property/CosPropertyService_i.h>
#include <cdbErrType.h>

 using namespace baci;
 using namespace cdb;
 using namespace cdbErrType;

ACE_RCSID(baci, baciCDBPropertySet, "$Id: baciCDBPropertySet.cpp,v 1.100 2008/10/01 02:26:45 cparedes Exp $");

CDBPropertySet * CDBPropertySet::instance_mp = 0;

// Singleton constructor
CDBPropertySet *
CDBPropertySet::createInstance (CORBA::ORB_ptr orb,
				PortableServer::POAManager_ptr poa_manager,
				PortableServer::POA_ptr root_poa)
{
  if (instance_mp==0)
      {
      instance_mp = new CDBPropertySet(orb, poa_manager, root_poa);
      }
  return instance_mp;
}

// Implementation skeleton constructor
CDBPropertySet::CDBPropertySet (CORBA::ORB_ptr orb,
				PortableServer::POAManager_ptr poa_manager,
				PortableServer::POA_ptr root_poa) :
  poa_m(PortableServer::POA::_nil()),
  poaCurrent_m(PortableServer::Current::_nil())
{
  ACS_TRACE("baci::CDBPropertySet::CDBPropertySet");

  CORBA::PolicyList policies (5);
  policies.length (5);

  // ID Assignment Policy
  policies[0] =
    root_poa->create_id_assignment_policy (PortableServer::USER_ID);
  
  // Lifespan Policy
  policies[1] =
    root_poa->create_lifespan_policy (PortableServer::PERSISTENT);
  
  // Request Processing Policy
  policies[2] =
    root_poa->create_request_processing_policy (PortableServer::USE_DEFAULT_SERVANT);

  // Servant Retention Policy
  policies[3] =
    root_poa->create_servant_retention_policy (PortableServer::RETAIN);

  // Id Uniqueness Policy
  policies[4] =
    root_poa->create_id_uniqueness_policy (PortableServer::MULTIPLE_ID);

  ACE_CString name = "DefaultServantPOA";
  poa_m = root_poa->create_POA (name.c_str (),
				poa_manager,
				policies);
  
  for (CORBA::ULong i = 0UL; i < policies.length (); ++i)
    {
      CORBA::Policy_ptr policy_p = policies[i];
      policy_p->destroy();
    }

  // Get the POA Current object reference
  CORBA::Object_var obj =
			orb->resolve_initial_references ("POACurrent");
  
  // Narrow the object reference to a POA Current reference
  this->poaCurrent_m =
      PortableServer::Current::_narrow (obj.in());
  
  // Set default servant
  poa_m->set_servant (this);
  
  instance_mp = this;
}
  
// Implementation skeleton destructor
CDBPropertySet::~CDBPropertySet (void)
{
  ACS_TRACE("baci::CDBPropertySet::~CDBPropertySet");
  instance_mp = 0;
}
 
void
CDBPropertySet::registerCharacteristicModel(const char * modelName, CharacteristicModelImpl* model)
{
    modelMap[modelName] = model;
}

void
CDBPropertySet::deregisterCharacteristicModel(const char * modelName)
{
    CharacteristicModelImplMap::iterator iter = modelMap.find(modelName);
    if (iter != modelMap.end())
	modelMap.erase(iter);
}

const char * 
CDBPropertySet::getRepositoryId ()
{
  return _interface_repository_id();
}

char * 
CDBPropertySet::getObjectId()
{
  ACS_TRACE("baci::CDBPropertySet::getObjectId");
  PortableServer::ObjectId_var id = poaCurrent_m->get_object_id();
  return PortableServer::ObjectId_to_string(id.in());
}

CosPropertyService::PropertySet *
CDBPropertySet::getPropertySet(const char * propertyName)
{
  ACS_TRACE("baci::CDBPropertySet::getPropertySet");

  // check if characteristic model is registered
  CharacteristicModelImplMap::iterator iter = modelMap.find(propertyName);
  if (iter == modelMap.end())
      throw CORBA::NO_RESOURCES();

  // create id
  PortableServer::ObjectId_var id = 
    PortableServer::string_to_ObjectId(propertyName);

  // create reference with that id
  CORBA::Object_var obj = poa_m->create_reference_with_id(id.in(), getRepositoryId());
  ACE_CHECK_RETURN (CosPropertyService::PropertySet::_nil ());

  CosPropertyService::PropertySet_var set = CosPropertyService::PropertySet::_nil ();
  if (CORBA::is_nil(obj.in())==false)
    {
      set = CosPropertyService::PropertySet::_narrow (obj.in ());
      ACE_CHECK_RETURN (CosPropertyService::PropertySet::_nil ());
    }
  else
      {
      return CosPropertyService::PropertySet::_nil ();
      }

  return set._retn ();
}

 
void
CDBPropertySet::define_property ( const char * property_name,
				 const CORBA::Any & property_value)
{
    ACE_UNUSED_ARG(property_value);
    ACS_TRACE("baci::CDBPropertySet::define_property");

    if (is_property_defined(property_name))
	throw CosPropertyService::ConflictingProperty();

    throw CosPropertyService::ReadOnlyProperty();
}
  
void
CDBPropertySet::define_properties (const CosPropertyService::Properties & nproperties)
{
  ACS_TRACE("baci::CDBPropertySet::define_properties");
  
  // Get the length.
  CORBA::ULong sequenceLength = nproperties.length ();
  
  // Define multiple exceptions object.
  CosPropertyService::MultipleExceptions multiEx;
  
  for (CORBA::ULong pi = 0UL; pi < sequenceLength; pi++)
      {
      try
        {
          // Define this property.
          this->define_property (nproperties [pi].property_name.in(),
                                 nproperties [pi].property_value);
        }
      catch (CosPropertyService::InvalidPropertyName ex)
        {
          CORBA::ULong len = multiEx.exceptions.length ();
          multiEx.exceptions.length (len + 1);
          multiEx.exceptions[len].reason =
            CosPropertyService::invalid_property_name;
          multiEx.exceptions[len].failing_property_name =
            nproperties[pi].property_name;
        }
      catch (CosPropertyService::ConflictingProperty ex)
        {
          CORBA::ULong len = multiEx.exceptions.length ();
          multiEx.exceptions.length (len + 1);
          multiEx.exceptions[len].reason =
            CosPropertyService::conflicting_property;
          multiEx.exceptions[len].failing_property_name =
            nproperties[pi].property_name;
        }
      catch (CosPropertyService::ReadOnlyProperty ex)
        {
          CORBA::ULong len = multiEx.exceptions.length ();
          multiEx.exceptions.length (len + 1);
          multiEx.exceptions[len].reason =
            CosPropertyService::read_only_property;
          multiEx.exceptions[len].failing_property_name =
            nproperties[pi].property_name;
        }
      catch (CosPropertyService::UnsupportedTypeCode ex)
        {
          CORBA::ULong len = multiEx.exceptions.length ();
          multiEx.exceptions.length (len + 1);
          multiEx.exceptions[len].reason =
            CosPropertyService::unsupported_type_code;
          multiEx.exceptions[len].failing_property_name =
            nproperties[pi].property_name;
        }
      catch (CosPropertyService::UnsupportedProperty ex)
	  {
	    CORBA::ULong len = multiEx.exceptions.length ();
	    multiEx.exceptions.length (len + 1);
	    multiEx.exceptions[len].reason =
	    CosPropertyService::unsupported_property;
	    multiEx.exceptions[len].failing_property_name =
		nproperties[pi].property_name;
	  }
      catch(...)
	  {
	  throw;
	  }
    }

  // Raise the multi exception if needed.
  if (multiEx.exceptions.length () > 0)
      {
      throw CosPropertyService::MultipleExceptions (multiEx);
      }
}
  
CORBA::ULong
CDBPropertySet::get_number_of_properties ()
{
    ACS_TRACE("baci::CDBPropertySet::get_number_of_properties");
    
  // check if characteristic model is registered
  CORBA::String_var objId = getObjectId();
  CharacteristicModelImplMap::iterator iter = modelMap.find(objId.in());
  if (iter == modelMap.end())
      throw CORBA::NO_RESOURCES();

  DAONode * node = iter->second->getDAONode();
  if (!node)
      throw CORBA::NO_RESOURCES();

  try
      {
      CDB::stringSeq_var properties = node->get_string_seq("");
      return properties->length();
      }
  catch (...)
      {
      throw CORBA::NO_RESOURCES();
      }

}
  
void
CDBPropertySet::get_all_property_names (CORBA::ULong how_many,
					CosPropertyService::PropertyNames_out property_names,
					CosPropertyService::PropertyNamesIterator_out rest)
{
  ACS_TRACE("baci::CDBPropertySet::get_all_property_names");

  // Allocating storage is a must.
  ACE_NEW (property_names,
           CosPropertyService::PropertyNames);

  // check if characteristic model is registered
  CORBA::String_var objId = getObjectId();
  CharacteristicModelImplMap::iterator iter = modelMap.find(objId.in());
  if (iter == modelMap.end())
      throw CORBA::NO_RESOURCES();

   DAONode * node = iter->second->getDAONode();
  if (!node)
      throw CORBA::NO_RESOURCES();

  CDB::stringSeq_var properties;
  try
      {
      properties = node->get_string_seq("");
      }
  catch (...)
      {
      throw CORBA::NO_RESOURCES();
      }
  
  // Validate the length.
  CORBA::ULong numOfProperties = properties->length();

  if (numOfProperties == 0)
      {
      // Nothing to do.
      return;
      }
  
  // Set the length of the property_names appropriately.
  CORBA::ULong sequenceLength = 0UL;

  if (how_many > 0)
      {
      if (how_many >= numOfProperties)
	  {
	  sequenceLength = numOfProperties;
	  }
      else
	  {
        sequenceLength = how_many;
	  }
      property_names->length (sequenceLength);
    }

  // Iterate thru names and put them in the property_names.

  CORBA::ULong ni = 0UL;
  for (; ni < sequenceLength;
       ni++)
      {
      property_names [ni] = CORBA::string_dup (properties[ni].in());
      }
  
  // If there are some more properties, put them in the
  // iterator. How?? Make a new PropertySet and use that to create
  // propertyNames Iterator.

  if (numOfProperties > how_many)
    {
      TAO_PropertySet *propertySet_p=0;

      ACE_NEW (propertySet_p, TAO_PropertySet);

      CORBA::Any anyval;
      anyval.type(CORBA::_tc_void);
      for (CORBA::ULong i = how_many;
           i < numOfProperties;
           i++, ni++)
	  {
	  propertySet_p->define_property(properties[ni].in(), anyval);
	  }

      // Make the NamesIterator out of this TAO_PropertySet.
      TAO_PropertyNamesIterator *namesIterator_p=0;
      ACE_NEW (namesIterator_p, TAO_PropertyNamesIterator (*propertySet_p));

      // Init the out parameter.

      // Get the Interface ptr.
      CosPropertyService::PropertyNamesIterator_ptr iterator_p =
        namesIterator_p->_this ();
      

      // POA stuff todo here, since we have <destroy> method in the
      // <NamesIterator> interface.
      // Give ownership of this servant to the POA.
      namesIterator_p->_remove_ref ();
      

      // Init the out parameter.
      rest = iterator_p;
    }
}
  
CORBA::Any *
CDBPropertySet::get_property_value (const char * property_name)
{
  ACS_TRACE("baci::CDBPropertySet::get_property_value");

  // Check the name's validity.
  if (!property_name)
      throw CosPropertyService::InvalidPropertyName();

  // check if characteristic model is registered
  CORBA::String_var objId = getObjectId();
  CharacteristicModelImplMap::iterator iter = modelMap.find(objId.in());
  if (iter == modelMap.end())
      throw CORBA::NO_RESOURCES();

  DAONode * node = iter->second->getDAONode();
  if (!node)
      throw CORBA::NO_RESOURCES();

  try
      {
      CORBA::String_var strVal = node->get_string(property_name);

      CORBA::Any * value_p = new CORBA::Any();
      (*value_p) <<= strVal.in();
      
      return value_p;
      }
  catch (cdbErrType::CDBFieldDoesNotExistEx& fde)
      {
      throw CosPropertyService::PropertyNotFound();
      }

}
  
CORBA::Boolean
CDBPropertySet::get_properties (const CosPropertyService::PropertyNames & property_names,
				CosPropertyService::Properties_out nproperties)
{
  ACS_TRACE("baci::CDBPropertySet::get_properties");

  // Allocate memory for the out parameter.
  ACE_NEW_RETURN (nproperties,
                  CosPropertyService::Properties,
                  0);

  // Validate the length.
  CORBA::ULong n = property_names.length ();
  if (n == 0)
      {
      return 0;
      }

  // Set the length for the out parameter.
  nproperties->length (n);

  // Get values for all the names.

  CORBA::Any_ptr any_p = 0;
  CORBA::Boolean retVal = true;

  for (CORBA::ULong i = 0UL; i < n; i++)
    {
      any_p = get_property_value(property_names[i]);
      ACE_CHECK_RETURN (0);

      if (any_p != 0)
        {
          // Property is found.
          nproperties [i].property_name = property_names [i];
          nproperties [i].property_value = *any_p;
        }
      else
        {
          // Invalid name. Ret value is False.
          retVal = false;

          // Assign void type to this name in the out parameter.
          nproperties [i].property_name = property_names [i];

          // Make an any value with tk_void type.
	  CORBA::Any voidany;
	  voidany.type(CORBA::_tc_void);
          nproperties [i].property_value = voidany; 
//           CORBA::Any(CORBA::_tc_void);
        }
    }
  return retVal;
}
  
void
CDBPropertySet::get_all_properties (CORBA::ULong how_many,
				    CosPropertyService::Properties_out nproperties,
				    CosPropertyService::PropertiesIterator_out rest)
{
  ACS_TRACE("baci::CDBPropertySet::get_all_properties");

  // Allocate memory for the out parameter.
  ACE_NEW (nproperties,
           CosPropertyService::Properties);

  // check if characteristic model is registered
  CORBA::String_var objId = getObjectId();
  CharacteristicModelImplMap::iterator iter = modelMap.find(objId.in());
  if (iter == modelMap.end())
      throw CORBA::NO_RESOURCES();

   DAONode * node = iter->second->getDAONode();
  if (!node)
      throw CORBA::NO_RESOURCES();

  CDB::stringSeq_var properties;
  try
      {
      properties = node->get_string_seq("");
      }
  catch (...)
      {
      throw CORBA::NO_RESOURCES();
      }
  
  // Validate the length.
  CORBA::ULong numOfProperties = properties->length();

  if (numOfProperties == 0)
      {
      // Nothing to do.
      return;
      }

  // Set the length for the nproperties if how_many > 0.
  CORBA::ULong sequenceLength = 0UL;

  if (how_many > 0)
    {
      if (how_many >= numOfProperties)
	  {
	  sequenceLength = numOfProperties;
	  }
      else
	  {
	  sequenceLength = how_many;
	  }
      nproperties->length(sequenceLength);
    }

  // Prepare an iterator and iterate through the PropertySet. Retrive
  // the values.

  CORBA::Any anyval;
  CORBA::ULong ni = 0UL;
  for (; ni < sequenceLength;
       ni++)
      {
      nproperties[ni].property_name = CORBA::string_dup (properties[ni].in());

      try
	  {
	  CORBA::String_var strVal = node->get_string(properties[ni].in());
	  anyval <<= strVal.in();
	  nproperties[ni].property_value = anyval;
	  }
      catch (...)
	  {
	  CORBA::Any voidany;
	  voidany.type(CORBA::_tc_void);
	  nproperties[ni].property_value = voidany; //CORBA::Any(CORBA::_tc_void);
	  }
      }

  // If there are more properties, put them in the <PropertiesIterator>.
  // Make a new <TAO_PropertySet> and use that to create an Properties
  // iterator.  put that in a iterator and assign that to the out
  // paramerter.

  if (numOfProperties > how_many)
      {
      TAO_PropertySet *propSet_p=0;
      
      ACE_NEW (propSet_p, TAO_PropertySet);
      
      for (CORBA::ULong i = how_many;
           i < numOfProperties;
           i++, ni++)
	  {

	  try
	      {
	      CORBA::String_var strVal = node->get_string(properties[ni].in());
	      anyval <<= strVal.in();
	      propSet_p->define_property(properties[ni].in(), anyval);
	      }
	  catch (...)
	      {
	      CORBA::Any voidany;
	      voidany.type(CORBA::_tc_void);
	      propSet_p->define_property(properties[ni].in(), voidany /*CORBA::Any(CORBA::_tc_void)*/);
	      }
	  
	  }

      // Make the iterator out of the new TAO_Propset.
      TAO_PropertiesIterator *iterator_p = 0;
      ACE_NEW (iterator_p,
               TAO_PropertiesIterator (*propSet_p));

      // Init the out parameter.

      // Get the interface ptr.
      CosPropertyService::PropertiesIterator_ptr iterator2_p =
        iterator_p->_this ();
      

      // POA stuff todo here, since we have <destroy> method in the
      // <NamesIterator> interface.
      // Give ownership of this servant to the POA.
      iterator_p->_remove_ref ();
      

      // Init the out parameter.
      rest = iterator2_p;
    }
}
  
void
CDBPropertySet::delete_property (const char * property_name)
{
  ACS_TRACE("baci::CDBPropertySet::delete_property");

  if (!is_property_defined(property_name))
      throw CosPropertyService::PropertyNotFound();
  
  throw CosPropertyService::FixedProperty();
}
  
void
CDBPropertySet::delete_properties (const CosPropertyService::PropertyNames & property_names)
{
  ACS_TRACE("baci::CDBPropertySet::delete_properties");

  // Get the length.
  CORBA::ULong sequenceLength = property_names.length ();

  // Declare multiple exceptions' object.
  CosPropertyService::MultipleExceptions *multiEx_p = 0;
  ACE_NEW (multiEx_p,
           CosPropertyService::MultipleExceptions);

  for (CORBA::ULong pi = 0UL; pi < sequenceLength; pi++)
    {
      try
        {
          // Delete this property.
          this->delete_property (property_names[pi]); 
        }
      catch (CosPropertyService::InvalidPropertyName ex)
	  {
          // Put this exception in the multiple exception.
	    CORBA::ULong len = multiEx_p->exceptions.length ();
	    multiEx_p->exceptions.length (len + 1);
	    multiEx_p->exceptions[len].reason =
		CosPropertyService::invalid_property_name;
	    multiEx_p->exceptions[len].failing_property_name =
		property_names[pi];
	  }
      catch (CosPropertyService::PropertyNotFound ex)
	  {
	  // Put this exception in the multiple exception.
	    CORBA::ULong len = multiEx_p->exceptions.length ();
	    multiEx_p->exceptions.length (len + 1);
	    multiEx_p->exceptions[len].reason =
		CosPropertyService::property_not_found;
	    multiEx_p->exceptions[len].failing_property_name =
		property_names[pi];
	  }
      catch (CosPropertyService::FixedProperty ex)
	  {
          // Put this exception in the multiple exception.
	    CORBA::ULong len = multiEx_p->exceptions.length ();
	    multiEx_p->exceptions.length (len + 1);
	    multiEx_p->exceptions[len].reason =
		CosPropertyService::fixed_property;
	    multiEx_p->exceptions[len].failing_property_name =
		property_names[pi];
	  }
      catch (...)
	  {
          // We cant afford to get this. Throw this.
          throw;
	  }
    }

  // Raise the multiple exceptions if there are any.
  if (multiEx_p->exceptions.length () > 0)
      {
      throw CosPropertyService::MultipleExceptions (*multiEx_p);
      }

}
  
CORBA::Boolean
CDBPropertySet::delete_all_properties ()
{
  ACS_TRACE("baci::CDBPropertySet::delete_all_properties");
  return false;
}
  
CORBA::Boolean
CDBPropertySet::is_property_defined (const char * property_name)
{
    ACS_TRACE("baci::CDBPropertySet::is_property_defined");
    
  // Check the name's validity.
  if (!property_name)
      throw CosPropertyService::InvalidPropertyName();

  // check if characteristic model is registered
  CORBA::String_var objId = getObjectId();
  CharacteristicModelImplMap::iterator iter = modelMap.find(objId.in());
  if (iter == modelMap.end())
      throw CORBA::NO_RESOURCES();

  DAONode * node = iter->second->getDAONode();
  if (!node)
      throw CORBA::NO_RESOURCES();

  try
      {
      CORBA::String_var strVal = node->get_string(property_name);
      return true;
      }
  catch (CDBFieldDoesNotExistEx &fde)
      {
      return false;
      }
}
