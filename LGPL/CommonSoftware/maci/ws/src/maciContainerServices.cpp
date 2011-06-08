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
 * "@(#) $Id: maciContainerServices.cpp,v 1.39 2011/06/08 23:21:43 javarias Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * rcirami   27/11/03  created
 */

#include <maciContainerServices.h>
#include <iostream>
#include <ACSErrTypeCORBA.h>

using namespace maci;
using namespace acsErrTypeContainerServices;
using namespace maciErrType;

//
// ContainerServices Constructor
//
MACIContainerServices::MACIContainerServices(
    const maci::Handle componentHandle,
    ACE_CString& name,
    ACE_CString& type,
    PortableServer::POA_ptr poa) :
  ContainerServices(name,poa), m_manager(0), m_componentHandle(componentHandle)
{
  ACS_TRACE("maci::MACIContainerServices::MACIContainerServices");
  m_containerImpl = maci::ContainerImpl::getContainer();
  m_manager = maci::Manager::_duplicate(m_containerImpl->getManager());
  m_offShootPOA = PortableServer::POA::_nil();
  componentStateManager_mp = new MACIComponentStateManager(name);
  m_poa = PortableServer::POA::_duplicate(m_containerImpl->getContainerPOA().in());
  m_componentType = ACE_CString(type);
}

MACIContainerServices::MACIContainerServices(
	const maci::Handle componentHandle,
	ACE_CString& name,
	PortableServer::POA_ptr poa,
	Manager_ptr manager) :
	ContainerServices(name, poa), m_manager(0), 
	m_componentHandle(componentHandle)
{
	ACS_TRACE("maci::MACIContainerServices::MACIContainerServices");
	m_manager = maci::Manager::_duplicate(manager);
	m_offShootPOA = PortableServer::POA::_nil();
	componentStateManager_mp = new MACIComponentStateManager(name);
}

//
// MACIContainerServices Destructor
//
MACIContainerServices::~MACIContainerServices()
{
  ACS_TRACE("maci::MACIContainerServices::~MACIContainerServices");
  delete componentStateManager_mp;
  m_usedComponents.unbind_all();
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

    ACE_TRACE("MACIContainerServices::getCORBAComponent");

    if(!name)          // Check if <name> is null
	{
	ACSErrTypeCommon::NullPointerExImpl nullEx(__FILE__, __LINE__,
						   "MACIContainerServices::getCORBAComponent");
	nullEx.setVariable("(parameter) name");
	maciErrType::CannotGetComponentExImpl lex(__FILE__, __LINE__,
						  "MACIContainerServices::getCORBAComponent");
	lex.setCURL("NULL");
	throw lex;
    }//if

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
	CORBA::Object_var obj =
	    m_manager->get_component(m_componentHandle, curl.c_str(), true);

	m_usedComponents.bind(name, m_componentHandle);
	return CORBA::Object::_narrow(obj.in());
	}
    catch (maciErrType::NoPermissionEx &ex)
	{
	CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
				     "MACIContainerServices::getCORBAComponent");
	lex.setCURL(curl);
	throw lex;
	}
    catch (maciErrType::ComponentNotAlreadyActivatedEx &ex)
	{
	CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
				     "MACIContainerServices::getCORBAComponent");
	lex.setCURL(curl);
	throw lex;
	}
    catch (maciErrType::CannotGetComponentEx &ex)
	{
	CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
						  "MACIContainerServices::getCORBAComponent");
	lex.setCURL(curl);
	throw lex;
	}
    catch( maciErrType::ComponentConfigurationNotFoundEx &ex)
	{
	CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
						  "MACIContainerServices::getCORBAComponent");
	lex.setCURL(curl);
	throw lex;
	}
    catch( CORBA::SystemException &ex )
	{
	ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__,
							    "MACIContainerServices::getCORBAComponent");
	corbaProblemEx.setMinor(ex.minor());
	corbaProblemEx.setCompletionStatus(ex.completed());
	corbaProblemEx.setInfo(ex._info().c_str());

	CannotGetComponentExImpl lex(corbaProblemEx, __FILE__, __LINE__,
				     "MACIContainerServices::getCORBAComponent");
	lex.setCURL(curl);
	throw lex;
	}
    catch (...)
	{
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
							"MACIContainerServices::getCORBAComponent");
	CannotGetComponentExImpl lex(uex, __FILE__, __LINE__,
				     "MACIContainerServices::getCORBAComponent");
	lex.setCURL(curl);
	throw lex;
	}//try-catch
}//getCORBAComponent

CORBA::Object*  MACIContainerServices::getCORBAComponentNonSticky(const char* name)
{
    ACE_TRACE("MACIContainerServices::getCORBAComponentNonSticky");

    if(!name)          // Check if <name> is null
	{
	ACSErrTypeCommon::NullPointerExImpl nullEx(__FILE__, __LINE__,
						   "MACIContainerServices::getCORBAComponentNonSticky");
	nullEx.setVariable("(parameter) name");
	maciErrType::CannotGetComponentExImpl lex(__FILE__, __LINE__,
						  "MACIContainerServices::getCORBAComponentNonSticky");
	lex.setCURL("NULL");
	throw lex;
    }//if

    ACS_SHORT_LOG((LM_DEBUG, "Getting component non sticky: '%s'.",  name));

    try
	{
	CORBA::Object_var obj =
	    m_manager->get_component_non_sticky(m_componentHandle, name);
    // @todo not really sure if we have to add non sticky component
//	m_usedComponents.bind(name, m_componentHandle);
	return CORBA::Object::_narrow(obj.in());
	}
    catch (maciErrType::CannotGetComponentEx &ex)
	{
	CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
						  "MACIContainerServices::getCORBAComponentNonSticky");
	lex.setCURL(name);
	throw lex;
	}
    catch (maciErrType::ComponentNotAlreadyActivatedEx &ex)
	{
	CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
				     "MACIContainerServices::getCORBAComponentNonSticky");
	lex.setCURL(name);
	throw lex;
	}
    catch( maciErrType::NoPermissionEx &ex)
	{
	CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
				     "MACIContainerServices::getCORBAComponentNonSticky");
	lex.setCURL(name);
	throw lex;
	}
    catch( CORBA::SystemException &ex )
	{
	ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__,
							    "MACIContainerServices::getCORBAComponentNonSticky");
	corbaProblemEx.setMinor(ex.minor());
	corbaProblemEx.setCompletionStatus(ex.completed());
	corbaProblemEx.setInfo(ex._info().c_str());

	CannotGetComponentExImpl lex(corbaProblemEx, __FILE__, __LINE__,
				     "MACIContainerServices::getCORBAComponentNonSticky");
	lex.setCURL(name);
	throw lex;
	}
    catch (...)
	{
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
							"MACIContainerServices::getCORBAComponentNonSticky");
	CannotGetComponentExImpl lex(uex, __FILE__, __LINE__,
				     "MACIContainerServices::getCORBAComponentNonSticky");
	lex.setCURL(name);
	throw lex;
	}//try-catch
}//getCORBAComponentNonSticky

CORBA::Object*
MACIContainerServices::getCORBADynamicComponent(maci::ComponentSpec compSpec, bool markAsDefault)
{
   //The IDL ComponentInfo structure returned by the get_dynamic_component method
   //contains tons of information about the newly created component and the most important
   //field is "reference" (i.e., the unnarrowed dynamic component).

   // Activate the dynamic component
   ComponentInfo_var cInfo;
   ACS_TRACE("MACIContainerServices::getCORBADynamicComponent");
   try
       {
       cInfo  = m_manager->get_dynamic_component(m_componentHandle,//Must pass the client's handle
						 compSpec, //Pass the component specifications
						 markAsDefault);
       CORBA::Object_var obj = cInfo->reference;
       if (CORBA::is_nil(obj.in()))
	   {
	   ACSErrTypeCORBA::CORBAReferenceNilExImpl ex(__FILE__, __LINE__,
						       "MACIContainerServices::getCORBADynamicComponent");
	   ex.setVariable("cInfo->reference");
	   throw ex; // it will be caught down
	   }//if
       m_usedComponents.bind(cInfo->name.in(), m_componentHandle);
       return CORBA::Object::_narrow(obj.in());
       }
   catch (maciErrType::NoPermissionEx &ex)
       {
       NoPermissionExImpl lex(ex, __FILE__, __LINE__,
			      "MACIContainerServices::getCORBADynamicComponent");
       throw lex;
       }
   catch (maciErrType::IncompleteComponentSpecEx &ex)
       {
       IncompleteComponentSpecExImpl lex(ex, __FILE__, __LINE__,
					 "MACIContainerServices::getCORBADynamicComponent");
       lex.setCURL(compSpec.component_name.in());
       throw lex;
       }
   catch (maciErrType::InvalidComponentSpecEx &ex)
       {
       InvalidComponentSpecExImpl lex(ex, __FILE__, __LINE__,
				      "MACIContainerServices::getCORBADynamicComponent");
       throw lex;
       }
   catch (maciErrType::ComponentSpecIncompatibleWithActiveComponentEx &ex)
       {
       ComponentSpecIncompatibleWithActiveComponentExImpl lex(ex, __FILE__, __LINE__,
							       "MACIContainerServices::getCORBADynamicComponent");
       lex.setCURL(compSpec.component_name.in());
       throw lex;
       }
   catch (maciErrType::CannotGetComponentEx &ex)
       {
       CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
				    "MACIContainerServices::getCORBADynamicComponent");
       lex.setCURL(compSpec.component_name.in());
       throw lex;
       }
   catch(ACSErr::ACSbaseExImpl &ex)
       {
       CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
				     "MACIContainerServices::getCORBADynamicComponent");
       lex.setCURL(compSpec.component_name.in());
       throw lex;
       }
   catch( CORBA::SystemException &ex )
       {
       ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__,
							    "MACIContainerServices::getCORBADynamicComponent");
       corbaProblemEx.setMinor(ex.minor());
       corbaProblemEx.setCompletionStatus(ex.completed());
       corbaProblemEx.setInfo(ex._info().c_str());

       CannotGetComponentExImpl lex(corbaProblemEx, __FILE__, __LINE__,
				     "MACIContainerServices::getCORBADynamicComponent");
       lex.setCURL(compSpec.component_name.in());
       throw lex;
       }
   catch (...)
       {
       ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
							"MACIContainerServices::getCORBADynamicComponent");
       CannotGetComponentExImpl lex(uex, __FILE__, __LINE__,
				    "MACIContainerServices::getCORBADynamicComponent");
       lex.setCURL(compSpec.component_name.in());
       throw lex;
       }//try-catch
}//getCORBADynamicComponent

CORBA::Object*
MACIContainerServices::getCORBACollocatedComponent(maci::ComponentSpec compSpec, bool markAsDefault, const char* targetComponent)
{
   //The IDL ComponentInfo structure returned by the get_collocated_component method
   //contains tons of information about the newly created component and the most important
   //field is "reference" (i.e., the unnarrowed collocated component).

   // Activate the dynamic component
   ComponentInfo_var cInfo;
   ACS_TRACE("MACIContainerServices::getCORBACollocatedComponent");

   try
       {
       cInfo = m_manager->get_collocated_component(m_componentHandle, //Must pass the client's handle
						   compSpec,//Pass the component specifications
						   markAsDefault,
						   targetComponent);
       CORBA::Object_var obj = cInfo->reference;
       if (CORBA::is_nil(obj.in()))
	   {
	   ACSErrTypeCORBA::CORBAReferenceNilExImpl ex(
	       __FILE__, __LINE__,
	       "MACIContainerServices::getCORBACollocatedComponent");
	   ex.setVariable("cInfo->reference");
	   throw ex;
	   }//if
       m_usedComponents.bind(cInfo->name.in(), m_componentHandle);
       return CORBA::Object::_narrow(obj.in());
       }
   catch (maciErrType::NoPermissionEx &_ex)
       {
       maciErrType::NoPermissionExImpl ex(__FILE__, __LINE__,
		 "MACIContainerServices::getCORBACollocatedComponent");
       throw ex;
       }
   catch (maciErrType::IncompleteComponentSpecEx &ex)
       {
       IncompleteComponentSpecExImpl lex(ex, __FILE__, __LINE__,
					 "MACIContainerServices::getCORBACollocatedComponent");
       lex.setCURL(compSpec.component_name.in());
       throw lex;
       }
   catch (maciErrType::InvalidComponentSpecEx &ex)
       {
       InvalidComponentSpecExImpl lex(ex, __FILE__, __LINE__,
				      "MACIContainerServices::getCORBACollocatedComponent");
       throw lex;
       }
   catch (maciErrType::ComponentSpecIncompatibleWithActiveComponentEx &ex)
       {
       ComponentSpecIncompatibleWithActiveComponentExImpl lex(ex, __FILE__, __LINE__,
							      "MACIContainerServices::getCORBACollocatedComponent");
       lex.setCURL(compSpec.component_name.in());
       throw lex;
       }
   catch (maciErrType::CannotGetComponentEx &ex)
       {
       CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
				    "MACIContainerServices::getCORBACollocatedComponent");
       lex.setCURL(compSpec.component_name.in());
       throw lex;
       }
   catch(ACSErr::ACSbaseExImpl &ex)
       {
       CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
				    "MACIContainerServices::getCORBACollocatedComponent");
       lex.setCURL(compSpec.component_name.in());
       throw lex;
       }
   catch( CORBA::SystemException &ex )
       {
       ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__,
							    "MACIContainerServices::getCORBACollocatedComponent");
       corbaProblemEx.setMinor(ex.minor());
       corbaProblemEx.setCompletionStatus(ex.completed());
       corbaProblemEx.setInfo(ex._info().c_str());

       CannotGetComponentExImpl lex(corbaProblemEx, __FILE__, __LINE__,
				     "MACIContainerServices::getCORBACollocatedComponent");
       lex.setCURL(compSpec.component_name.in());
       throw lex;
       }
   catch (...)
       {
       ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
						       "MACIContainerServices::getCORBACollocatedComponent");
       CannotGetComponentExImpl lex(uex, __FILE__, __LINE__,
				    "MACIContainerServices::getCORBACollocatedComponent");
	lex.setCURL(compSpec.component_name.in());
	throw lex;
       }//try-catch
}//getCORBACollocatedComponent

CORBA::Object*
MACIContainerServices::getCORBADefaultComponent(const char* idlType)
{
   ComponentInfo_var cInfo;
   ACS_TRACE("MACIContainerServices::getCORBADefaultComponent");
   try
    {
    cInfo  = m_manager->get_default_component(m_componentHandle, idlType);
    CORBA::Object_var obj = cInfo->reference;
    if (CORBA::is_nil(obj.in()))
	{
	ACSErrTypeCORBA::CORBAReferenceNilExImpl ex(
	    __FILE__, __LINE__,
	    "MACIContainerServices::getCORBADefaultComponent");
	ex.setVariable("cInfo->reference");
	throw ex;
	}//if
    m_usedComponents.bind(cInfo->name.in(), m_componentHandle);
    return CORBA::Object::_narrow(obj.in());
    }
   catch (maciErrType::NoPermissionEx &ex)
       {
       throw NoPermissionExImpl (ex, __FILE__, __LINE__,
				 "MACIContainerServices::getCORBADefaultComponent");
       }
   catch (maciErrType::NoDefaultComponentEx &ex)
       {
       throw NoDefaultComponentExImpl (ex, __FILE__, __LINE__,
				       "MACIContainerServices::getCORBADefaultComponent");
       }
   catch (maciErrType::CannotGetComponentEx &ex)
       {
       CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
				    "MACIContainerServices::getCORBADefaultComponent");
       lex.setCURL("IDL type:"+ACE_CString(idlType));
       throw lex;
       }
   catch(ACSErr::ACSbaseExImpl &ex)
       {
       CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
				    "MACIContainerServices::getCORBADefaultComponent");
       lex.setCURL("IDL type:"+ACE_CString(idlType));
       throw lex;
       }
   catch( CORBA::SystemException &ex )
       {
       ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__,
							    "MACIContainerServices::getCORBADefaultComponent");
       corbaProblemEx.setMinor(ex.minor());
       corbaProblemEx.setCompletionStatus(ex.completed());
       corbaProblemEx.setInfo(ex._info().c_str());

       CannotGetComponentExImpl lex(corbaProblemEx, __FILE__, __LINE__,
				     "MACIContainerServices::getCORBADefaultComponent");
       lex.setCURL("IDL type:"+ACE_CString(idlType));
       throw lex;
       }
   catch (...)
       {
       ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
						       "MACIContainerServices::getCORBADefaultComponent");
       CannotGetComponentExImpl lex(uex, __FILE__, __LINE__,
				    "MACIContainerServices::getCORBADefaultComponent");
       lex.setCURL("IDL type:"+ACE_CString(idlType));
       throw lex;
       }//try-catch
}//MACIContainerServices::getCORBADefaultComponent


ComponentInfo MACIContainerServices::getComponentDescriptor(const char* componentName)
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
		acsErrTypeContainerServices::GettingCompInfoExImpl
		    ex(__FILE__,__LINE__,"MACIContainerServices::getComponentDescriptor");
		ex.setCURL(componentName);
		throw ex;
	}
}

	//m_manager->force_release_component(m_componentHandle, name);

void
MACIContainerServices::releaseComponent(const char *name)
{
	int found = m_usedComponents.unbind(name);
    try
	{

    // Check if the component is used and unbind
    	if (found == -1)
    	{
    		maciErrType::ComponentNotInUseExImpl ex1(__FILE__, __LINE__,
    				"MACIContainerServices::releaseComponent");
    		ex1.setCURL(name);

    		maciErrType::CannotReleaseComponentExImpl ex(ex1, __FILE__, __LINE__,
    				"MACIContainerServices::releaseComponent");
    		ex.setCURL(name);

    		throw ex;
    	}//if


	m_manager->release_component(m_componentHandle, name);
	}
    catch (maciErrType::NoPermissionEx &_ex)
	{
	maciErrType::CannotReleaseComponentExImpl ex(_ex, __FILE__, __LINE__,
						     "MACIContainerServices::releaseComponent");
	ex.setCURL(name);
	throw ex;
	}
    catch (ACSErr::ACSbaseExImpl &_ex)
	{
	// failed to release, bind back
	if (found != -1) m_usedComponents.bind(name, m_componentHandle);
	maciErrType::CannotReleaseComponentExImpl ex(_ex, __FILE__, __LINE__,
						     "MACIContainerServices::releaseComponent");
	ex.setCURL(name);
	throw ex;
	}
    catch( CORBA::SystemException &ex )
	{
	// failed to release, bind back
	m_usedComponents.bind(name, m_componentHandle);
	ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__,
							  "MACIContainerServices::releaseComponent");
	corbaProblemEx.setMinor(ex.minor());
	corbaProblemEx.setCompletionStatus(ex.completed());
	corbaProblemEx.setInfo(ex._info().c_str());
      	maciErrType::CannotReleaseComponentExImpl ex(corbaProblemEx,  __FILE__, __LINE__,
						     "MACIContainerServices::releaseComponent");
	ex.setCURL(name);
	throw ex;
	}
    catch (...)
	{
	// failed to release, bind back
	if (found != -1) m_usedComponents.bind(name, m_componentHandle);
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
							"MACIContainerServices::releaseComponent");
	maciErrType::CannotReleaseComponentExImpl ex(uex,  __FILE__, __LINE__,
						     "MACIContainerServices::releaseComponent");
	ex.setCURL(name);
	throw ex;
	}//try-catch
}//releaseComponent

void MACIContainerServices::releaseAllComponents()
{
	ACE_CString_Vector componentsToRelease;
	{
	  ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_usedComponents.mutex());

	  // get reverse deactivation order of components
	  for (COMPONENT_HASH_MAP::CONST_ITERATOR iter(m_usedComponents); !iter.done(); iter.advance())
	    componentsToRelease.push_back(iter->key());
	}

	unsigned int len = componentsToRelease.size();
	for (unsigned int i = 0; i < len; i++)
		this->releaseComponent(componentsToRelease[i].c_str());
}


void MACIContainerServices::fireComponentsAvailable (ACE_CString_Vector& compNames) {
	if (!withCompListener || compListener == NULL) {
	    return;
	}

    if (compListener->includeForeignComponents()) {
		ContainerServices::fireComponentsAvailable(compNames);
    }
    else if (m_usedComponents.current_size() > 0) {
        ACE_CString_Vector interesting;
        for (int i=0; i < (int)compNames.size(); i++) {
           ACE_CString cn = compNames[i];
            if (m_usedComponents.find(cn) != -1) {
                interesting.push_back(cn);
            }
        }
		if (interesting.size() > 0)
			ContainerServices::fireComponentsAvailable(interesting);
    }
}

void MACIContainerServices::fireComponentsUnavailable(ACE_CString_Vector& compNames){
	if (!withCompListener || compListener == NULL) {
	    return;
	}

    if (compListener->includeForeignComponents()) {
		ContainerServices::fireComponentsUnavailable(compNames);
    }
    else if (m_usedComponents.current_size() > 0) {
        ACE_CString_Vector interesting;
        for (int i=0; i < (int)compNames.size(); i++) {
           ACE_CString cn = compNames[i];
            if (m_usedComponents.find(cn) != -1) {
                interesting.push_back(cn);
            }
        }
		if (interesting.size() > 0)
			ContainerServices::fireComponentsUnavailable(interesting);
    }
 }


CDB::DAL_ptr
MACIContainerServices::getCDB()
{
  CDB::DAL_var dalObj = CDB::DAL::_nil();
  ACS_TRACE("MACIContainerServices::getCDB");

  try
      {
      //bje: for some reason this should be done using get_component ....
      // ... and not get_service/getService
      CORBA::Object_var obj = m_manager->get_component(m_componentHandle, "CDB", false);
      dalObj = CDB::DAL::_narrow(obj.in());
      return dalObj._retn();   //bje: I do not know if this is OK, ...
      // ... since getCDB is local and not CORBA call so differen MM
      }
  catch (maciErrType::NoPermissionEx &ex)
      {
      throw CanNotGetCDBExImpl (ex, __FILE__, __LINE__,
				"MACIContainerServices::getCDB");
      }
  catch (maciErrType::CannotGetComponentEx &ex)
      {
      throw CanNotGetCDBExImpl (ex, __FILE__, __LINE__,
				"MACIContainerServices::getCDB");
      }
  catch( maciErrType::ComponentConfigurationNotFoundEx &ex)
      {
      throw CanNotGetCDBExImpl (ex, __FILE__, __LINE__,
				"MACIContainerServices::getCDB");
      }
  catch( CORBA::SystemException &ex )
      {
      ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__,
							  "MACIContainerServices::getCDB");
      corbaProblemEx.setMinor(ex.minor());
      corbaProblemEx.setCompletionStatus(ex.completed());
      corbaProblemEx.setInfo(ex._info().c_str());

      throw CanNotGetCDBExImpl (corbaProblemEx, __FILE__, __LINE__,
				"MACIContainerServices::getCDB");
      }
  catch (...)
      {
      ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
						      "MACIContainerServices::getCDB");
      throw CanNotGetCDBExImpl (uex, __FILE__, __LINE__,
				"MACIContainerServices::getCDB");
      }//try-catch
}//getCDB

    ACS::OffShoot_ptr
MACIContainerServices::activateOffShoot(PortableServer::Servant cbServant)
{
    ACS::OffShoot_ptr inOffShoot = ACS::OffShoot::_narrow(cbServant->_get_component());
    if (CORBA::is_nil(inOffShoot))
    {
        return ACS::OffShoot ::_nil();
    }
    //CORBA::release(inOffShoot);
    if (CORBA::is_nil(m_offShootPOA.ptr()))
    {
        // It is normal the first time we execute this method
        m_offShootPOA=createOffShootPOA();
        if (CORBA::is_nil(m_offShootPOA.ptr()))
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
	if (!CORBA::is_nil(m_offShootPOA.ptr())) {
		return m_offShootPOA;
	}

  // get the container POA
  PortableServer::POA_var containerPOA = m_poa;

  try{
	  m_offShootPOA = containerPOA->find_POA("OffShootPOA", false);
	  return m_offShootPOA;
  }catch(CORBA::Exception &ex){
	  //if does not exist we just continue and create a new one
  }

  // get the POA Manager
  PortableServer::POAManager_var poaManager = m_poa->the_POAManager();

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

//Don't throw exceptions the user doesn't want to know about de-activation
void MACIContainerServices::deactivateOffShoot(PortableServer::Servant cbServant)
{
    ACS::OffShoot_ptr offShoot = ACS::OffShoot::_narrow(cbServant->_get_component());
	if (CORBA::is_nil(offShoot))
    {
    	// The OffShoot was already deactivated
        return;
    }

	if (CORBA::is_nil(m_offShootPOA.ptr()))
	{
		acsErrTypeContainerServices::OffShootPOAExImpl ex(__FILE__,__LINE__,"MACIContainerServices::deactivateOffShoot");
		throw ex;
	}
	//CORBA::release(offShoot);
	// Deactivate the servant
	PortableServer::ObjectId* id = m_offShootPOA->servant_to_id(cbServant);

	m_offShootPOA->deactivate_object(*id);
}

ComponentStateManager*
MACIContainerServices::getComponentStateManager()
{
  return componentStateManager_mp;
}
