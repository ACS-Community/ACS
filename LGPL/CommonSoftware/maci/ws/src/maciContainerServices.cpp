/*******************************************************************************
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
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
 *
 * "@(#) $Id: maciContainerServices.cpp,v 1.20 2006/06/12 14:06:36 msekoran Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * rcirami   27/11/03  created 
 */

#include <maciContainerServices.h>
#include <iostream>

using namespace maci;
using namespace acsErrTypeContainerServices;

//
// ContainerServices Constructor
//
MACIContainerServices::MACIContainerServices(
    const maci::Handle componentHandle, 
    ACE_CString& name,
    PortableServer::POA_ptr poa) :
  ContainerServices(name,poa), m_manager(0), m_componentHandle(componentHandle)
{
  ACS_TRACE("maci::MACIContainerServices::MACIContainerServices");
  m_containerImpl = maci::ContainerImpl::getContainer();
  m_manager = maci::Manager::_duplicate(m_containerImpl->getManager());
  m_offShootPOA = PortableServer::POA::_nil();
  componentStateManager_mp = new MACIComponentStateManager(name);
  m_usedComponents.clear(); // Redundant 
}

//
// MACIContainerServices Destructor
//
MACIContainerServices::~MACIContainerServices()
{
  ACS_TRACE("maci::MACIContainerServices::~MACIContainerServices");
  delete componentStateManager_mp;
  m_usedComponents.clear();
}

ACE_CString_Vector
MACIContainerServices::findComponents(const char *nameWildcard, const char *typeWildcard)
{
  ACE_CString_Vector names;

  if(nameWildcard == NULL)
    {
      nameWildcard = "*";
    }
  
  if(typeWildcard == NULL)
    {
      typeWildcard = "*";
    }
  
  maci::HandleSeq seq;
  maci::ComponentInfoSeq_var devs = m_manager->get_component_info(m_componentHandle,seq,nameWildcard,typeWildcard,false);

  CORBA::ULong len = devs->length (); 

  for (CORBA::ULong i=0; i < len; i++) 
    {
      names.push_back(devs[i].name.in());
    }

  return names;
}

CORBA::Object*  MACIContainerServices::getCORBAComponent(const char* name)
{   
    // code is reused, so I tried not make another version of it
    const char * domain = 0;
    bool activate = true;
    
    /**
     * Check if <name> is null
     */
    if(!name)
    {
        ACS_SHORT_LOG((LM_DEBUG, "Name parameter is null."));
        return CORBA::Object::_nil();
    }
    
    /**
     * First creates the CURL and query the Manager for the component
     */
    ACE_CString curl = "curl://";
    if (domain)
    {
        curl += domain;
    }

    curl += ACE_CString("/");

    curl += name;

    ACS_SHORT_LOG((LM_DEBUG, "Getting component: '%s'.",  curl.c_str()));
    
    try
    {
        CORBA::ULong status;
        CORBA::Object_var obj = m_manager->get_component(m_componentHandle, curl.c_str(), activate, status);
   
        if (CORBA::is_nil(obj.in()) || status!=maci::Manager::COMPONENT_ACTIVATED)
        {
            ACS_SHORT_LOG((LM_DEBUG, "Failed to create '%s', status: %d.",  curl.c_str(), status));
            return CORBA::Object::_nil();
        }
 
        m_usedComponents.push_back(name);
        return CORBA::Object::_narrow(obj.in());
    }
    catch( CORBA::Exception &ex )
    {
        ACE_PRINT_EXCEPTION(ex,
             "maci::MACIContainerServices::getComponent");
        return CORBA::Object::_nil();
    }
    catch(...)
    {
        return CORBA::Object::_nil();
    }
    return CORBA::Object::_nil();
}

CORBA::Object* 
MACIContainerServices::getCORBADynamicComponent(maci::ComponentSpec compSpec, bool markAsDefault)
{
   //The IDL ComponentInfo structure returned by the get_dynamic_component method
   //contains tons of information about the newly created component and the most important
   //field is "reference" (i.e., the unnarrowed dynamic component).
   
   // Activate the dynamic component
   ComponentInfo_var cInfo;
   try 
   {
    cInfo  = m_manager->get_dynamic_component(m_componentHandle,    //Must pass the client's handle
                                              compSpec,    //Pass the component specifications
                                              markAsDefault); 
    CORBA::Object_var obj = cInfo->reference;
     if (CORBA::is_nil(obj.in())) 
     {
        ACS_SHORT_LOG((LM_DEBUG, "Failed getting the dynamic component."));
        return CORBA::Object::_nil();
     }
     m_usedComponents.push_back(cInfo->name.in());
     return CORBA::Object::_narrow(obj.in());
     
   } catch (...) 
   {
    ACS_SHORT_LOG((LM_DEBUG, "Failed getting the dynamic component."));
    return CORBA::Object::_nil();
   }
}

CORBA::Object* 
MACIContainerServices::getCORBACollocatedComponent(maci::ComponentSpec compSpec, bool markAsDefault, const char* targetComponent)
{
   //The IDL ComponentInfo structure returned by the get_collocated_component method
   //contains tons of information about the newly created component and the most important
   //field is "reference" (i.e., the unnarrowed collocated component).
   
   // Activate the dynamic component
   ComponentInfo_var cInfo;
   try 
   {
    cInfo  = m_manager->get_collocated_component(m_componentHandle,    //Must pass the client's handle
                                              compSpec,    //Pass the component specifications
                                              markAsDefault,
					      targetComponent); 
    CORBA::Object_var obj = cInfo->reference;
     if (CORBA::is_nil(obj.in())) 
     {
        ACS_SHORT_LOG((LM_DEBUG, "Failed getting the collocated component."));
        return CORBA::Object::_nil();
     }
     m_usedComponents.push_back(cInfo->name.in());
     return CORBA::Object::_narrow(obj.in());
     
   } catch (...) 
   {
    ACS_SHORT_LOG((LM_DEBUG, "Failed getting the collocated component."));
    return CORBA::Object::_nil();
   }
}


CORBA::Object* 
MACIContainerServices::getCORBADefaultComponent(const char* idlType)
{
   ComponentInfo_var cInfo;
   try
    {
      cInfo  = m_manager->get_default_component(m_componentHandle,idlType);
      CORBA::Object_var obj = cInfo->reference;
      if (CORBA::is_nil(obj.in())) 
      {
        ACS_SHORT_LOG((LM_DEBUG, "Failed getting the default component."));
        return CORBA::Object::_nil();
      }
      m_usedComponents.push_back(cInfo->name.in());
    return CORBA::Object::_narrow(obj.in());
    }
    catch (...) 
    {
      ACS_SHORT_LOG((LM_DEBUG, "Failed getting the default component of type %s",idlType));
      return CORBA::Object::_nil();
    }
}


ComponentInfo MACIContainerServices::getComponentDescriptor(const char* componentName)
throw (acsErrTypeContainerServices::GettingCompInfoExImpl)
{
	maci::HandleSeq seq;
	ComponentInfoSeq_var compInfoSeq = 
        m_manager->get_component_info(m_componentHandle,seq,componentName,"*",false);
	
	if (compInfoSeq!=NULL && compInfoSeq->length()==1) 
	{
		return (*compInfoSeq)[0];
	}
	else
	{
		ACS_SHORT_LOG((LM_WARNING,"Failed retrieving the component info for %s",componentName));
		acsErrTypeContainerServices::GettingCompInfoExImpl ex(__FILE__,__LINE__,"MACIContainerServices::deactivateOffShoot");
		throw ex;
	}
}

void 
MACIContainerServices::releaseComponent(const char *name)
{
	// Check if the component is used
	std::vector<std::string>::iterator pos = findUsedComponent(name);
	if (pos==m_usedComponents.end()) 
	{
		ACS_SHORT_LOG((LM_ERROR,"Error releasing %s: component not used",name));
		return;
	}
  // TODO exception hadning missing here (or should be done by caller)
  m_manager->release_component(m_componentHandle, name);
  // Remove the component from the list of the used components
  m_usedComponents.erase(pos);
}

void MACIContainerServices::releaseAllComponents()
{
	/* Implementation note:
	 * at the beginning the m_usedComponents was a list
	 * but I had an error whenever the releaseComponent deleted
	 * the entry in the list. Maybe this was due to the fact that 
	 * the entry was still in use somewhere in the tree of calls
	 * With the vector this doesn't happen. Probably for the different
	 * implementation of this container.
	 * I wonder if it is safe...
	 */
	 
	// Scans the list to release all the components
	// The item is removed in the releaseComponent method
	// so here I'm using a collateral effect (bad!)
	while (!m_usedComponents.empty())
	{
		this->releaseComponent(m_usedComponents[0].c_str());
	}
}

CDB::DAL_ptr
MACIContainerServices::getCDB()
{
  CDB::DAL_var dalObj = CDB::DAL::_nil();
  CORBA::ULong status;
  CORBA::Object_var obj = m_manager->get_component(m_componentHandle, "CDB", false, status);
  
  if (!CORBA::is_nil(obj.in()))
    {
      dalObj = CDB::DAL::_narrow(obj.in());
      if (CORBA::is_nil(dalObj.in())) 
	{
	  ACS_SHORT_LOG((LM_INFO, "MACIContainerServices::getCDB() - Failed to narrow DAL"));
	  return 0;
	}
    }
  
  return dalObj._retn();
}

ACS::OffShoot_ptr
MACIContainerServices::activateOffShoot(PortableServer::Servant cbServant)
{
  if (!dynamic_cast<POA_ACS::OffShoot_ptr> (cbServant))
    {
      return ACS::OffShoot ::_nil();
    }

  if (m_offShootPOA.ptr() == PortableServer::POA::_nil())
    { 
    	// It is normal the first time we execute this method
    	m_offShootPOA=createOffShootPOA();
    	if (m_offShootPOA.ptr() == PortableServer::POA::_nil())
    	{
    		// Something went wrong creating the POA
      		return ACS::OffShoot ::_nil();
    	}
    }
  
  // activate the CORBA object (SYSTEM_ID -> activate_object)
  PortableServer::ObjectId_var oid;
  oid = m_offShootPOA->activate_object(cbServant);
  
  // create an object reference
  CORBA::Object_var obj;
  obj = m_offShootPOA->id_to_reference(oid.in());
  ACS::OffShoot_var shoot = ACS::OffShoot::_narrow(obj.in());
  
  return shoot._retn();
}

PortableServer::POA_var MACIContainerServices::createOffShootPOA()
{

	// Check if the POA was already created
	if (m_offShootPOA.ptr() != PortableServer::POA::_nil()) {
		return m_offShootPOA;
	}
	
  // get the container POA
  PortableServer::POA_var containerPOA = m_containerImpl->getContainerPOA();

  // get the POA Manager
  PortableServer::POAManager_var poaManager = m_containerImpl->getPOAManager();

  //
  // Prepare policies OffShoot POA will be using.
  //
  PortableServer::IdAssignmentPolicy_var offshoot_system_id_policy =
    containerPOA->create_id_assignment_policy(PortableServer::SYSTEM_ID);
  
  
  PortableServer::LifespanPolicy_var offshoot_transient_policy =
    containerPOA->create_lifespan_policy(PortableServer::TRANSIENT);
  
  
  PortableServer::RequestProcessingPolicy_var offshoot_use_active_object_map_only_policy =
    containerPOA->create_request_processing_policy (PortableServer::USE_ACTIVE_OBJECT_MAP_ONLY);
  
  
  PortableServer::ServantRetentionPolicy_var offshoot_servant_retention_policy  =
    containerPOA->create_servant_retention_policy (PortableServer::RETAIN);
  
  CORBA::PolicyList policiesOffShoot;
  policiesOffShoot.length(4);
  
  policiesOffShoot[0] = PortableServer::LifespanPolicy::_duplicate(offshoot_transient_policy.in());
  policiesOffShoot[1] = PortableServer::IdAssignmentPolicy::_duplicate(offshoot_system_id_policy.in());
  policiesOffShoot[2] = PortableServer::ServantRetentionPolicy::_duplicate(offshoot_servant_retention_policy.in());
  policiesOffShoot[3] = PortableServer::RequestProcessingPolicy::_duplicate(offshoot_use_active_object_map_only_policy.in());
  
  
  m_offShootPOA = containerPOA->create_POA("OffShootPOA",poaManager.in(),policiesOffShoot);
  return m_offShootPOA;
}

void MACIContainerServices::deactivateOffShoot(PortableServer::Servant cbServant)
throw (
	acsErrTypeContainerServices::OffShootDeactivationExImpl,
	acsErrTypeContainerServices::OffShootPOAExImpl)
{
	if (!dynamic_cast<POA_ACS::OffShoot_ptr> (cbServant))
    {
    	// TODO: throw the exception
    	acsErrTypeContainerServices::OffShootDeactivationExImpl ex(__FILE__,__LINE__,"MACIContainerServices::deactivateOffShoot");
		throw ex;
    }

	if (m_offShootPOA.ptr() == PortableServer::POA::_nil())
	{
		acsErrTypeContainerServices::OffShootPOAExImpl ex(__FILE__,__LINE__,"MACIContainerServices::deactivateOffShoot");
		throw ex;
	}
	
	// Deactivate the servant
	PortableServer::ObjectId* id = m_offShootPOA->servant_to_id(cbServant);
	
	m_offShootPOA->deactivate_object(*id);
}

ComponentStateManager* 
MACIContainerServices::getComponentStateManager()
{
  return componentStateManager_mp;
}

std::vector<std::string>::iterator MACIContainerServices::findUsedComponent(std::string name)
{
	std::vector<std::string>::iterator pos;
	for (pos=m_usedComponents.begin(); pos!=m_usedComponents.end(); pos++)
	{
		std::string temp = *pos;
		if (temp==name) 
		{
			return pos;
		}
	}
	return pos;
}

