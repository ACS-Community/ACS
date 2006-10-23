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
* "@(#) $Id: baciTestContainerServices.h,v 1.4 2006/10/23 21:23:37 gchiozzi Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
*/
 
#include <vltPort.h>
#include <acsutil.h> 
#include <acsutilPorts.h>

#include <acsContainerServices.h>

//--------------------------------------
class TestContainerServices : public virtual maci::ContainerServices
{
  public:
    CORBA::ORB_var m_orb;

    TestContainerServices(ACE_CString& compName, PortableServer::POA_ptr poa, CORBA::ORB_ptr orb) :
	ContainerServices(compName, poa), m_orb(CORBA::ORB::_duplicate(orb))
	{;}
    
  protected:
    virtual CORBA::Object* getCORBAComponent(const char* name)
	    throw (maciErrType::CannotGetComponentExImpl)
	{
	    return (CORBA::Object*)0;
	}

    virtual CORBA::Object* getCORBADynamicComponent(maci::ComponentSpec compSpec, bool markAsDefault)
	    throw(maciErrType::IncompleteComponentSpecExImpl, 
		  maciErrType::InvalidComponentSpecExImpl, 
		  maciErrType::ComponentSpecIncompatibleWithActiveComponentExImpl, 
		  maciErrType::CannotGetComponentExImpl)
	{
	    return (CORBA::Object*)0;
	}

    virtual CORBA::Object* getCORBADefaultComponent(const char* idlType)
	    throw (maciErrType::NoDefaultComponentExImpl, 
		   maciErrType::CannotGetComponentExImpl)
	{
	    return (CORBA::Object*)0;
	}

    virtual CORBA::Object* getCORBACollocatedComponent(maci::ComponentSpec, bool, const char*)
	    throw(maciErrType::IncompleteComponentSpecExImpl, 
		  maciErrType::InvalidComponentSpecExImpl, 
		  maciErrType::ComponentSpecIncompatibleWithActiveComponentExImpl, 
		  maciErrType::CannotGetComponentExImpl)
	{
	    return (CORBA::Object*)0;
	}

        CORBA::Object* getCORBAComponentNonSticky(const char*)
	   throw (maciErrType::CannotGetComponentExImpl)
	{
	    return (CORBA::Object*)NULL;
	}
        


  public:
    virtual maci::ComponentInfo getComponentDescriptor(const char* componentName)
	throw (acsErrTypeContainerServices::GettingCompInfoExImpl)
	{
	    maci::ComponentInfo retVal;
	    return retVal;
	}

    virtual ACE_CString_Vector findComponents(const char *nameWilcard, const char *typeWildcard)
	{
	    ACE_CString_Vector retVal;
	    return retVal;
	}

    virtual void releaseComponent(const char *name)
	{;}

    virtual void releaseAllComponents()
	{;}

    virtual CDB::DAL_ptr getCDB() throw (acsErrTypeContainerServices::CanNotGetCDBExImpl) 
	{

	    ACE_TCHAR corbalocRef[230];
	    ACE_TCHAR * envRef = ACE_OS::getenv ("DAL_REFERENCE");

	    if (envRef && *envRef)
		{
		ACS_LOG(0, "TestContainerServices::getCDB",
			(LM_INFO, "CDB obtained via environment: '%s'", envRef));
		strcpy(corbalocRef, envRef);
		}
	else
	    {
	    // corbaloc::<hostname>:<port>/CDB
	    const char* hostname = 0;
	    hostname = ACSPorts::getIP();
	    if (hostname==0)
		return (CDB::DAL *)0;
	    
	  
	    ACE_OS::sprintf(corbalocRef, "corbaloc::%s:%s/CDB", hostname, ACSPorts::getCDBPort().c_str());

	    ACS_LOG(0, "TestContainerServices::getCDB",
		    (LM_INFO, "CDB reference generated using localhost address: '%s'", corbalocRef));
	    }//if-than

	CDB::DAL_var dalObj = CDB::DAL::_nil();
	CORBA::Object_var obj = m_orb->string_to_object(corbalocRef);
  
	if (!CORBA::is_nil(obj.in()))
	    {
	    dalObj = CDB::DAL::_narrow(obj.in());
	    if (CORBA::is_nil(dalObj.in())) 
		{
		ACS_SHORT_LOG((LM_INFO, "TestContainerServices::getCDB() - Failed to narrow CDB"));
		return (CDB::DAL *)0;
		}
	    }
	
	return dalObj._retn();
	}

    virtual PortableServer::POA_var getOffShootPOA()
	{
	    PortableServer::POA_var retVal;
	    return retVal;
	}

    virtual void deactivateOffShoot(PortableServer::Servant cbServant)throw (
           acsErrTypeContainerServices::OffShootDeactivationExImpl,
           acsErrTypeContainerServices::OffShootPOAExImpl)
	{;}

    virtual PortableServer::POA_var createOffShootPOA()
	{
	    PortableServer::POA_var retVal;
	    return retVal;
	}

    virtual ACS::OffShoot_ptr activateOffShoot(PortableServer::Servant cbServant)
	{
	    return (ACS::OffShoot *)0;
	}
 
    virtual ComponentStateManager* getComponentStateManager()
    {
      return (ComponentStateManager*)NULL;
    }
};
