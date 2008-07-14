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
* "@(#) $Id: baciCORBA.cpp,v 1.98 2008/07/14 12:33:47 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* gchiozzi 2001-12-20 Added getPOAManager() and getPOARoot() methods.
* msekoran  2001/02/17  created
* msekoran  2001/05/21  fixed destroy bug
* msekoran  2001/08/26  added InitCORBA, DoneCORBA for BACI modular tests
*/

#include <vltPort.h>
#include "logging.h"
#include "baciCORBA.h"

BACI_CORBA * BACI_CORBA::instance_mp = 0;

BACI_CORBA::BACI_CORBA(CORBA::ORB_ptr orb_m,
		       PortableServer::POAManager_ptr poaManager_m,
		       PortableServer::POA_ptr poaRoot_m,
		       PortableServer::POA_ptr poaPersistent_m,
		       PortableServer::POA_ptr poaTransient_m)
{
  this->orb_m = CORBA::ORB::_duplicate(orb_m);
  this->poaManager_m = PortableServer::POAManager::_duplicate(poaManager_m);
  this->poaRoot_m = PortableServer::POA::_duplicate(poaRoot_m);
  this->poaPersistent_m = PortableServer::POA::_duplicate(poaPersistent_m);
  this->poaTransient_m = PortableServer::POA::_duplicate(poaTransient_m);
}

BACI_CORBA::~BACI_CORBA()
{
}


BACI_CORBA *
BACI_CORBA::getInstance()
{
  return instance_mp;
}

BACI_CORBA *
BACI_CORBA::createInstance(CORBA::ORB_ptr orb_m,
			   PortableServer::POAManager_ptr poaManager_m,
			   PortableServer::POA_ptr poaRoot_m,
			   PortableServer::POA_ptr poaPersistent_m,
			   PortableServer::POA_ptr poaTransient_m)
{
  if (instance_mp==0)
      {
      instance_mp = new BACI_CORBA(orb_m, poaManager_m, poaRoot_m, poaPersistent_m, poaTransient_m);
      }
  return instance_mp;
}

void
BACI_CORBA::destroyInstance()
{
  if (instance_mp!=0)
    {
      delete instance_mp;
      instance_mp = 0;
    }
}

CORBA::ORB_ptr
BACI_CORBA::getORB()
{
    if (instance_mp!=0)
	{
	return instance_mp->orb_m.ptr();
	}
    else
	{
	return CORBA::ORB::_nil();
	}
}

PortableServer::POAManager_ptr
BACI_CORBA::getPOAManager()
{
    if (instance_mp!=0)
	{
	return instance_mp->poaManager_m.ptr();
	}
    else
	{
	return PortableServer::POAManager::_nil();
	}
}

PortableServer::POA_ptr
BACI_CORBA::getPOARoot()
{
    if (instance_mp!=0)
	{
	return instance_mp->poaRoot_m.ptr();
	}
    else
	{
	return PortableServer::POA::_nil();
	}
}

PortableServer::POA_ptr
BACI_CORBA::getPOA()
{
    if (instance_mp!=0)
	{
	return instance_mp->poaPersistent_m.ptr();
	}
    else
	{
	return PortableServer::POA::_nil();
	}
}

CORBA::Object_ptr
BACI_CORBA::ActivateCORBAObject(PortableServer::Servant srvnt, const char * name)
{

  if (instance_mp==0 ||
      CORBA::is_nil(instance_mp->poaPersistent_m.ptr()) )
      {
      return CORBA::Object::_nil();
      }

  try
    {
      PortableServer::ObjectId_var id =
	PortableServer::string_to_ObjectId(name);
      instance_mp->poaPersistent_m->activate_object_with_id(id.in(), srvnt);
      CORBA::Object_var obj = instance_mp->poaPersistent_m->servant_to_reference(srvnt);
      return obj._retn();
    }
  catch(...)
    {
    ACS_LOG(LM_RUNTIME_CONTEXT, "BACI_CORBA::ActivateCORBAObject",
	    (LM_ERROR, "Failed to activate CORBA object"));
    }

  return CORBA::Object::_nil();
}

bool
BACI_CORBA::DestroyCORBAObject(CORBA::Object_ptr obj)
{

  if (instance_mp==0 ||
		  CORBA::is_nil(instance_mp->poaPersistent_m.ptr()))
      {
      return false;
      }

  try
    {
      PortableServer::ObjectId_var id =
	instance_mp->poaPersistent_m->reference_to_id(obj);
      instance_mp->poaPersistent_m->deactivate_object(id.in());
      return true;
    }
  catch(...)
    {
      ACS_LOG(LM_RUNTIME_CONTEXT, "BACI_CORBA::DestroyCORBAObject",
	      (LM_ERROR, "Failed to deactivate CORBA object"));
    }

  return false;
}

bool
BACI_CORBA::DestroyCORBAObject(PortableServer::Servant srvnt)
{
    if (instance_mp==0 ||
    		CORBA::is_nil(instance_mp->poaPersistent_m.ptr()))
	{
	return false;
	}

    try
	{
	PortableServer::ObjectId_var id =
	    instance_mp->poaPersistent_m->servant_to_id(srvnt);
	instance_mp->poaPersistent_m->deactivate_object(id.in());
	return true;
	}
    catch(...)
	{
	ACS_LOG(LM_RUNTIME_CONTEXT, "BACI_CORBA::DestroyCORBAObject (servant)",
		(LM_ERROR, "Failed to deactivate CORBA object"));
	}

    return false;
}

bool
BACI_CORBA::DestroyTransientCORBAObject(CORBA::Object_ptr obj)
{
    if (instance_mp==0 ||
    		CORBA::is_nil(instance_mp->poaTransient_m.ptr()))
	{
	return false;
	}

    try
	{
	PortableServer::ObjectId_var id =
	    instance_mp->poaTransient_m->reference_to_id(obj);
	instance_mp->poaTransient_m->deactivate_object(id.in());
	return true;
	}
    catch(...)
	{
	ACS_LOG(LM_RUNTIME_CONTEXT, "BACI_CORBA::DestroyTransientCORBAObject",
		(LM_ERROR, "Failed to deactivate CORBA object"));
	}
  return false;
}

bool
BACI_CORBA::DestroyTransientCORBAObject(PortableServer::Servant srvnt)
{
    if (instance_mp==0 ||
    		CORBA::is_nil(instance_mp->poaTransient_m.ptr()))
	{
	return false;
	}

    try
	{
	PortableServer::ObjectId_var id =
	    instance_mp->poaTransient_m->servant_to_id(srvnt);

	instance_mp->poaTransient_m->deactivate_object(id.in());
	return true;
	}
    catch(...)
	{
	ACS_LOG(LM_RUNTIME_CONTEXT, "BACI_CORBA::DestroyTransientCORBAObject (servant)",
		(LM_ERROR, "Failed to deactivate CORBA object"));
	}

    return false;
}

bool BACI_CORBA::InitCORBA(int argc, char* argv[])
{
  try
      {
      // Initialize the ORB.
      CORBA::ORB_var orb_m = CORBA::ORB_init(argc, argv, "TAO");

      if(CORBA::is_nil(orb_m.ptr()))
	{
	return false;
	}

      //
      // Initialize POAs.
      //

      // Get the Root POA.
      CORBA::Object_var objRootPOA =
	  orb_m->resolve_initial_references("RootPOA");

      PortableServer::POA_var poaRoot_m = PortableServer::POA::_narrow(objRootPOA.in());

      if(CORBA::is_nil(poaRoot_m.ptr()))
	  {
	  return false;
	  }

      // Get the manager of the root POA to apply to the child POAs.
      PortableServer::POAManager_var poaManager =
	  poaRoot_m->the_POAManager();

      //
      // Prepare policies our POAs will be using.
      //

      // @@ Unavoidable memory leak here if the POA has been improperly set
      //    up, eg. by a -ORBEndpoint command-line option that does not
      //    correspond to the local host.
      PortableServer::IdAssignmentPolicy_var userIdPolicy =
	  poaRoot_m->create_id_assignment_policy(PortableServer::USER_ID);
      PortableServer::LifespanPolicy_var persistentPolicy =
	  poaRoot_m->create_lifespan_policy(PortableServer::PERSISTENT);

      CORBA::PolicyList policies;

      //Create the transiet poa which has no policies
      PortableServer::POA_var poaTransient_m = poaRoot_m->create_POA("TransientPOA",
								      poaManager.in(),
								      policies);

      policies.length(2);

      policies[0] = PortableServer::LifespanPolicy::_duplicate(persistentPolicy.in());
      policies[1] = PortableServer::IdAssignmentPolicy::_duplicate(userIdPolicy.in());

      PortableServer::POA_var poaPersistent_m = poaRoot_m->create_POA("PersistentPOA",
								      poaManager.in(),
								      policies);

      // We're done using the policies.
      userIdPolicy->destroy();
      persistentPolicy->destroy();

      // POA Manager can start processing incoming requests.
      poaManager->activate();

      // create BACI_CORBA
      BACI_CORBA::createInstance(orb_m.ptr(), poaManager.ptr(),
				 poaRoot_m.ptr(), poaPersistent_m.ptr(), poaTransient_m.ptr());

      }
  catch(...)
      {
      ACS_LOG(0, "baci::BACI_CORBA::InitCORBA", (LM_ERROR, "Unexpected CORBA exception"));
      return false;
      }

  ACS_DEBUG("baci::BACI_CORBA::InitCORBA", "CORBA initialized successfully");
  return true;
}

bool BACI_CORBA::DoneCORBA()
{

  if (instance_mp==0)
      {
      return false;
      }

  try
      {
      // copy of ptrs
      PortableServer::POA_var poaRoot = PortableServer::POA::_duplicate(BACI_CORBA::getInstance()->getPOA());
//      PortableServer::POAManager_var poaManager = PortableServer::POAManager::_duplicate(BACI_CORBA::getInstance()->getPOAManager());
      CORBA::ORB_var orb = CORBA::ORB::_duplicate(BACI_CORBA::getInstance()->getORB());
/*
      if(poaManager.ptr()!=PortableServer::POAManager::_nil())
	{
	  poaManager->deactivate(1, 1);
	}

      ACS_DEBUG("baci::BACI_CORBA::InitCORBA", "POA deactivated (with ehterealization).");
*/
      if(poaRoot.ptr()!=PortableServer::POA::_nil())
	  {
	  poaRoot->destroy(1, 1);
	  }

      if(orb.ptr()!=CORBA::ORB::_nil())
	  {
	  orb->destroy();
	  }

      // delete instance
      BACI_CORBA::destroyInstance();
      }
  catch(...)
    {
      ACS_LOG(0, "baci:BACI_CORBA::DoneCORBA", (LM_ERROR, "Unexpected exception occured"));
      return false;
    }

  return true;
}








