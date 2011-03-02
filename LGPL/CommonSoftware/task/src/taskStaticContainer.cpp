/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2004
*
*This library is free software; you can redistribute it and/or
*modify it under the terms of the GNU Lesser General Public
*License as published by the Free Software Foundation; either
*version 2.1 of the License, or (at your option) any later version.
*
*This library is distributed in the hope that it will be useful,
*but WITHOUT ANY WARRANTY; without even the implied warranty of
*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*Lesser General Public License for more details.
*
*You should have received a copy of the GNU Lesser General Public
*License along with this library; if not, write to the Free Software
*Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: taskStaticContainer.cpp,v 1.17 2011/03/02 17:23:42 rtobar Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2004-09-27  created
*/

#include <acsContainerServices.h>
#include "taskStaticContainer.h"
#include "taskComponentS.h"
#include <acscomponentImpl.h>
#include <maciContainerServices.h>
#include "taskStaticContainerServices.h"

#ifdef _TASK_LIB
extern "C" PortableServer::Servant ConstructComponent(  maci::Handle h,
							const char * name,
							const char * type,
							maci::ContainerServices * containerServices
    );

#endif //_TASK_LIB

StaticContainer::StaticContainer():
    m_logger(0), services_m(true)
{
}

void StaticContainer::initCORBA(int &argc, char **argv)
{
    ACE_TRACE("StaticContainer::initCORBA");

    try
	{
	orb_m = CORBA::ORB_init(argc, argv);
	if(CORBA::is_nil(orb_m.ptr()))
	    return; //TBD: EH


	CORBA::Object_var objRootPOA =
	    orb_m->resolve_initial_references("RootPOA");
	poaRoot_m = PortableServer::POA::_narrow(objRootPOA.in());
	if (CORBA::is_nil(poaRoot_m.ptr()))
	    return; //TBD: EH
	poaManager_m = poaRoot_m->the_POAManager();

	PortableServer::IdAssignmentPolicy_var user_id_policy =
	    poaRoot_m->create_id_assignment_policy(PortableServer::USER_ID);

	CORBA::PolicyList policies;
	policies.length(1);
	policies[0] = PortableServer::IdAssignmentPolicy::_duplicate(user_id_policy.in());

	componentPOA_m = poaRoot_m->create_POA("ContainerPOA",
					 poaManager_m.in(),
					 policies);
	if (CORBA::is_nil(componentPOA_m.ptr()))
	    return;  //TBD: EH

	user_id_policy->destroy();

	poaManager_m->activate();
	}
    catch(...)
	{
	printf("exception in initCORBA\n");
	}
}

/*******************************************************************************************/

void StaticContainer::doneCORBA()
{
    ACE_TRACE("StaticContainer::doneCORBA");
    try
	{

	if(poaRoot_m.ptr() != PortableServer::POA::_nil())
	    {
	    poaRoot_m->destroy(1, 1);
	    }

	if(orb_m.ptr() != CORBA::ORB::_nil())
	    {
	    orb_m->destroy();
	    }
	}
    catch(CORBA::Exception &ex)
	{
	ex._tao_print_exception ("occured in StaticContainer::doneCORBA");
	}
    catch(...)
	{
	ACS_LOG(LM_RUNTIME_CONTEXT, "StaticContainer::doneCORBA",
		(LM_ERROR, "Unexpected exception"));
	}
}

/*******************************************************************************************/

void StaticContainer::init(int argc, char **argv, const char *containerName)
{
    if ( containerName!=0 && ACE_OS::strlen(containerName)!=0 )
	{
	containerName_m = containerName;
	}
    else
	{
	ACE_Time_Value tv = ACE_OS::gettimeofday();
	char timeBuf[25];
	sprintf(timeBuf, "%ld", tv.sec());

	containerName_m += "Container-";
	containerName_m += timeBuf;
	}//if-else
//TBD: container name from cmd line

   containerArgv.add(argv[0]);
   containerArgv.add(containerName_m.c_str()); //first comes container name

    for(int i=1; i<argc; i++)
	{
	if (ACE_OS::strcmp(argv[i], "-nosvcs") == 0)
	    {
	    services_m = false;
	    }
	else
	    {
	    containerArgv.add(argv[i]);
	    ACE_OS::printf("argv[%d]=%s\n", i, argv[i]);
	    }///if

	}//for

    try
	{
	if ((services_m == true) && container_m.init(containerArgv.argc(), containerArgv.argv())==true )
	    {
		container_m.connect(); //error handling
		services_m = true;
		componentPOA_m = container_m.getContainerPOA();
	        orb_m = container_m.getContainerORB();
	    }
	else
	    {
	    services_m = false;
	    m_logger = new LoggingProxy(0, 0, 31);
	    LoggingProxy::init (m_logger);
	    ACSError::init();
	    initCORBA(argc, argv);
	    }//if-else
	}
    catch(...)
	{
	ACS_LOG(LM_RUNTIME_CONTEXT, "StaicContainer::init", (LM_ERROR, "unknown exception"));
	}
}

/***************************************************************************************/

void StaticContainer::done()
{
    ACE_TRACE("StaticContainer::done");
if ( services_m == true)
    {
/*	    ACS_DEBUG_PARAM("main", "deactivating the component %s", componentName.c_str());
	    servant->_remove_ref();
	    container.deactivateCORBAObject(obj);
*/
    container_m.shutdown(CONTAINER_EXIT << 8);
    container_m.done();
    }
else
    {
/*	    ACS_DEBUG_PARAM("main", "deleting the component %s", argv[1]);
*/
    doneCORBA();
    m_logger->flush();
    LoggingProxy::done();
    delete m_logger;
    ACS_TRACE("szxsadasdasd");
    ACSError::done();
    }
}
/*****************************************************************************/

CORBA::Object_ptr StaticContainer::createComponentWithName(const char *name)
{
    const char *libname;
    if ( ACE_OS::strlen(name) == 0 )
	{
#ifndef _TASK_LIB
	ACS_LOG(LM_RUNTIME_CONTEXT, "StaticComponenet::createComponentWithName",
		    (LM_ERROR, "component could not be created just with name (name of library needed)"));
	return CORBA::Object::_nil();
#else
	ACE_CString compName = "Component-";
	ACE_Time_Value tv = ACE_OS::gettimeofday();
	char timeBuf[25];
	sprintf(timeBuf, "%ld", tv.sec());
	compName+=timeBuf;

	libname = 0; // here libray name can be  NULL since the library is linked by linker and nota loaded on demand

	return createComponent(compName.c_str(), libname);
#endif
	}
    else
	{
	// reteive libname from CDB
	libname = "SDFSDS";
	return createComponent(name, libname);
	}
}
/*****************************************************************************/

CORBA::Object_ptr StaticContainer::createComponent(const char *libname)
{
    ACE_CString compName = "Component-";
    ACE_Time_Value tv = ACE_OS::gettimeofday();
    char timeBuf[25];
    sprintf(timeBuf, "%ld", tv.sec());
    compName+=timeBuf;

    return createComponent(compName.c_str(), libname);
}

/*****************************************************************************/

CORBA::Object_ptr StaticContainer::createComponent(const char* compName, const char *libname)
    {
     CORBA::Object_var obj = CORBA::Object::_nil();
     ACE_CString cn = "Component-";

     ACE_TRACE("StaticContainer::createComponent");
     if ( ACE_OS::strlen(compName)!=0 )
	 {
#ifndef _TASK_LIB
	 if (ACE_OS::strlen(libname)==0)
	     {
	     if (services_m == true )
		 {
		 // try to read libname  from CDB
		 }
	     else
		 {
		 ACS_LOG(LM_RUNTIME_CONTEXT, "StaticComponenet::createComponent",
		    (LM_ERROR, "component could not be created w/o providing library name"));
		 return obj._retn();
		 }
	     }
#endif
	 }
     else
	 {
#ifndef _TASK_LIB
	 if (ACE_OS::strlen(libname) == 0 )
	     {
	     ACS_LOG(LM_RUNTIME_CONTEXT, "StaticComponenet::createComponent",
		     (LM_ERROR, "component could not be created w/o providing library name"));
	     return obj._retn();
	     }
#endif
	 // create unique component name
	 ACE_Time_Value tv = ACE_OS::gettimeofday();
	 char timeBuf[25];
	 sprintf(timeBuf, "%ld", tv.sec());
	 cn+=timeBuf;
	 compName = cn.c_str();
	 }

#ifndef _TASK_LIB
    if (services_m==true) // if we have services (i.e. also the manager) we can create and activate the component by using concept of dynamic component
     {
     ACS_DEBUG("StaticContainer::createComponent", "Activating component");

     maci::ComponentSpec_var compSpec = new maci::ComponentSpec();
     compSpec->component_name = CORBA::string_dup(compName);
     compSpec->component_type = CORBA::string_dup("IDL:alma/ACS/Task:1.0"); //TBD:: IFR ?
     compSpec->component_code = CORBA::string_dup(libname);
     compSpec->container_name = CORBA::string_dup(containerName_m.c_str());
/// @todo  get_dynamic_component can throw an exception which should be caught!
     maci::ComponentInfo_var compInfo  =
	 container_m.getManager()->get_dynamic_component(container_m.getHandle(),
							 compSpec.in(),
							 false);
     // at this point we have done everything so we can return
     return compInfo->reference._retn();
     }//if

    // otherwise we have to load the library and find ConstructComponent function in the library
    ACS_DEBUG_PARAM("StaticContainer::createComponent", "loading library: %s", libname);
    ACE_CString strDLLPath(ACE_OS::getenv ("LD_LIBRARY_PATH"));
    dllmgr_m.setSearchPath(strDLLPath.c_str());

    int libHandle = 0;

    libHandle = dllmgr_m.load(libname);

    if (libHandle == 0)
	{
	printf("error loading the library\n");
	return 0; // -1;
	}

    ACS_DEBUG_PARAM("StaticContainer::createComponent", "Library: %s has been loaded", libname);

    maci::ConstructComponentFunc ConstructComponent =
	(maci::ConstructComponentFunc)(dllmgr_m.getSymbol(libHandle, "ConstructComponent"));
    if (ConstructComponent == 0)
	{
	printf("error finding the constructor for the component\n");
	return 0;// -1;
	}
#endif //!_TASK_LIB

    ACS_DEBUG_PARAM("StaticContainer::createComponent", "Creating component: %s", compName);
    maci::ContainerServices* acsCS = 0;
    ACE_CString cmpName(compName);
    ACE_CString cmpType("IDL:alma/ACS/Task:1.0");

    if (services_m == true)
	{
	acsCS =   new maci::MACIContainerServices(0/*handel*/, cmpName, cmpType, container_m.getContainerPOA().in());
	if (acsCS==0)
	    {
	    ACS_LOG(LM_RUNTIME_CONTEXT, "StaticContainer::createComponent",
		    (LM_ERROR,  "Error creating the ContainerServices"));
	    return 0;
	    }
	}
    else
	{

	acsCS =   new StaticContainerServices(0/*handel*/, cmpName, cmpType, container_m.getContainerPOA().in(), orb_m.in());
	if (acsCS==0)
	    {
	    ACS_LOG(LM_RUNTIME_CONTEXT, "StaticContainer::createComponent",
		    (LM_ERROR,  "Error creating the ContainerServices"));
	    return 0;
	    }
	}

    PortableServer::Servant servant = ConstructComponent(0/*handel*/, compName, "ANY", acsCS);
    if (servant == 0)
	{
	printf("error constructing the component\n");
	return 0;// -1;
	}

       // life cycle
    ACS_DEBUG("StaticContainer::createComponent", "Component Life Cycle");
    acscomponent::ACSComponentImpl *acsComponent =
	dynamic_cast<acscomponent::ACSComponentImpl*>(servant);

    acsComponent->getContainerServices()->getComponentStateManager()->setState(ACS::COMPSTATE_INITIALIZING);
    acsComponent->__initialize();
    acsComponent->getContainerServices()->getComponentStateManager()->setState(ACS::COMPSTATE_INITIALIZED);
    acsComponent->getContainerServices()->getComponentStateManager()->setState(ACS::COMPSTATE_OPERATIONAL);
    acsComponent->__execute();

    ACS_DEBUG("StaticContainer::createComponent", "Activating the component");

    try
	{
	if (services_m==true )
	    {
	    obj = container_m.activateCORBAObject(servant, componentName_m.c_str());
	    }
	else
	    {

	    PortableServer::ObjectId_var id =
		PortableServer::string_to_ObjectId(componentName_m.c_str());

	    componentPOA_m->activate_object_with_id(id.in(), servant);

	    obj = componentPOA_m->servant_to_reference(servant);

	    servant->_remove_ref();
	    }
	}
    catch(CORBA::Exception &ex)
	{
	 ex._tao_print_exception ("occured");
	 return CORBA::Object::_nil();
	}
    return obj._retn();
}//createComponent

/**************************************************************************************/

void StaticContainer::destroyComponent(CORBA::Object_ptr obj)
{
    ACS::ACSComponent_var acscomp;
    try
	{
	acscomp = ACS::ACSComponent::_narrow(obj);
	if ( CORBA::is_nil(acscomp.ptr()) )
	    {
	    ACS_LOG(LM_RUNTIME_CONTEXT, "StaticContainer::destroyComponent",
			    (LM_ERROR,  "component narrowed to nil!!"));
	    }
	}
    catch(...)
	{
	}
#ifndef _TASK_LIB
    if ( services_m == true )
	{
	ACE_CString compNam = acscomp->name();
	ACS_DEBUG_PARAM("StaticContainer::destroyComponent", "releasing the component %s via the manager", compNam.c_str());
	container_m.getManager()->release_component(container_m.getHandle(), acscomp->name());
	return;
	}
#endif  // !_TASK_LIB

    PortableServer::Servant servant;
    servant = componentPOA_m->reference_to_servant(obj);

 // life cycle
    ACS_DEBUG("StaticContainer::destroyComponent", "Component Life Cycle");
    acscomponent::ACSComponentImpl *acsComponent =
	dynamic_cast<acscomponent::ACSComponentImpl*>(servant);
    acsComponent->getContainerServices()->getComponentStateManager()->setState(ACS::COMPSTATE_DESTROYING);
    acsComponent->__cleanUp();
    acsComponent->getContainerServices()->getComponentStateManager()->setState(ACS::COMPSTATE_DEFUNCT);
// ContainerServices is deleted in the destructor of ComponentImpl

    if ( services_m == true)
	{
	ACS_DEBUG_PARAM("StaticContainer::destroyComponent", "deactivating the component %s", acscomp->name());
	container_m.deactivateCORBAObject(obj);
	}
    else
	{
	ACS_DEBUG_PARAM("StaticContainer::destroyComponent", "deleting the component %s", acscomp->name());
	servant->_remove_ref();
	}//if-else
}//destroyComponenet

/*___oOo___*/

