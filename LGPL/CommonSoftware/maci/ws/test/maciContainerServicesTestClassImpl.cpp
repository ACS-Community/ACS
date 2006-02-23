/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: maciContainerServicesTestClassImpl.cpp,v 1.10 2005/04/18 17:14:48 acaproni Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* acaproni  2005-02-28  created 
*/

/************************************************************************
*   NAME
*   
* 
*   SYNOPSIS
*
*   
*   PARENT CLASS
*
* 
*   DESCRIPTION
*
*
*   PUBLIC METHODS
*
*
*   PUBLIC DATA MEMBERS
*
*
*   PROTECTED METHODS
*
*
*   PROTECTED DATA MEMBERS
*
*
*   PRIVATE METHODS
*
*
*   PRIVATE DATA MEMBERS
*
*
*   FILES
*
*   ENVIRONMENT
*
*   COMMANDS
*
*   RETURN VALUES
*
*   CAUTIONS 
*
*   EXAMPLES
*
*   SEE ALSO
*
*   BUGS   
* 
*------------------------------------------------------------------------
*/

#define _POSIX_SOURCE 1
#include "vltPort.h"

static char *rcsId="@(#) $Id: maciContainerServicesTestClassImpl.cpp,v 1.10 2005/04/18 17:14:48 acaproni Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "maciContainerServicesTestClassImpl.h"
#include <acsContainerServices.h>
#include <maciContainerServices.h>

using namespace maci;

MaciContainerServicesTestClassImpl::MaciContainerServicesTestClassImpl(
    const ACE_CString& name,
    maci::ContainerServices* containerServices):
    acscomponent::ACSComponentImpl(name,containerServices)
{
}

MaciContainerServicesTestClassImpl::~MaciContainerServicesTestClassImpl()
{
}

void MaciContainerServicesTestClassImpl::initialize()
	throw (acsErrTypeLifeCycle::LifeCycleExImpl)
{
}

void MaciContainerServicesTestClassImpl::execute()
	throw (acsErrTypeLifeCycle::LifeCycleExImpl)
{
}

void MaciContainerServicesTestClassImpl::cleanUp()
{
}


void MaciContainerServicesTestClassImpl::aboutToAbort()
{
}

void MaciContainerServicesTestClassImpl::dynamicComponentTest() 
	throw (CORBA::SystemException)
{
	 
	 // Prepare the ComponentSpec struct
	 ComponentSpec cSpec;
	 cSpec.component_name="*";		
	 cSpec.component_code="*";	
	 cSpec.container_name="*";
	 cSpec.component_type=IDLTYPE;
	 
	 // Get the default component for the given IDL interface
	 MACI_TEST::DynamicTestClass_var comp = 
	 	getContainerServices()->getDynamicComponent<MACI_TEST::DynamicTestClass>(cSpec,false);
	 if (CORBA::is_nil(comp.in())) {
	 	ACS_SHORT_LOG((LM_ERROR,"dynamicComponentTest: Error getting type %s",cSpec.component_type.in()));
	 	return;
	 }
	 // Execute a method on the remote component
	 comp->whoami();
	 
	 // Release the component
	 getContainerServices()->releaseComponent(comp->name());
}

void MaciContainerServicesTestClassImpl::defaultComponentTest() 
	throw (CORBA::SystemException)
{
	 
	 // Get the default component for the given IDL interface
	 MACI_TEST::DynamicTestClass_var comp = 
	 	getContainerServices()->getDefaultComponent<MACI_TEST::DynamicTestClass>(IDLTYPE);
	 if (CORBA::is_nil(comp.in())) {
	 	ACS_SHORT_LOG((LM_ERROR,"defaultComponentTest: Error getting %s",COMPNAME));
	 	return;
	 }
	 // Execute a method on the remote component
	 comp->whoami();
	 
	 // Release the component
	 getContainerServices()->releaseComponent(comp->name());
}
  
void MaciContainerServicesTestClassImpl::componentDescriptorTest() 
	throw (CORBA::SystemException)
{
    ComponentInfo cInfo = getContainerServices()->getComponentDescriptor(COMPNAME);
    ACS_SHORT_LOG((LM_INFO,"ComponentInfo, Name: %s",cInfo.name.in()));
    ACS_SHORT_LOG((LM_INFO,"ComponentInfo, Type: %s",cInfo.type.in()));
    ACS_SHORT_LOG((LM_INFO,"ComponentInfo, Code: %s",cInfo.code.in()));
}

void MaciContainerServicesTestClassImpl::releaseResourcesTest() 
	throw (CORBA::SystemException)
{
	 // Get the COMPNAME component
	 MACI_TEST::DynamicTestClass_var comp = 
	 	getContainerServices()->getComponent<MACI_TEST::DynamicTestClass>(COMPNAME);
	 if (CORBA::is_nil(comp.in())) {
	 	ACS_SHORT_LOG((LM_ERROR,"releaseResourcesTest: Error getting %s",COMPNAME));
	 	return;
	 }
	 // Get the COMPNAME component
	 MACI_TEST::DynamicTestClass_var comp2 = 
	 	getContainerServices()->getComponent<MACI_TEST::DynamicTestClass>(COMPNAME2);
	 if (CORBA::is_nil(comp.in())) {
	 	ACS_SHORT_LOG((LM_ERROR,"releaseResourcesTest: Error getting %s",COMPNAME2));
	 	return;
	 }
	 getContainerServices()->releaseAllComponents();
	 // We need to log a message to be sure that all the components
	 // were release by calling the ContainerServices method and not 
	 // because comp and comp2 habe been destroyed when the block
	 // is terminated
	 ACS_SHORT_LOG((LM_INFO,"Released all the components"));
}

void MaciContainerServicesTestClassImpl::getComponentTest() 
	throw (CORBA::SystemException)
{
	 // Get the COMPNAME component
	 MACI_TEST::DynamicTestClass_var comp = 
	 	getContainerServices()->getComponent<MACI_TEST::DynamicTestClass>(COMPNAME);
	 if (CORBA::is_nil(comp.in())) {
	 	ACS_SHORT_LOG((LM_ERROR,"getComponentTest: Error getting %s",COMPNAME));
	 	return;
	 }
	 // Execute a method on the remote component
	 comp->whoami();
	 
	 // Release the component
	 getContainerServices()->releaseComponent(COMPNAME);
}

//--------------------------------------------------------------
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(MaciContainerServicesTestClassImpl)

/*___oOo___*/

