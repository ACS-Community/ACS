/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: maciContainerServicesTestClassImpl.cpp,v 1.20 2008/10/09 07:05:37 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* acaproni  2005-02-28  created 
*/

#define _POSIX_SOURCE 1
#include "vltPort.h"

static char *rcsId="@(#) $Id: maciContainerServicesTestClassImpl.cpp,v 1.20 2008/10/09 07:05:37 cparedes Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "maciContainerServicesTestClassImpl.h"
#include <acsContainerServices.h>
#include <maciContainerServices.h>
#include "maciBlockingComponentListener.h"

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
{
}

void MaciContainerServicesTestClassImpl::execute()
{
}

void MaciContainerServicesTestClassImpl::cleanUp()
{
}


void MaciContainerServicesTestClassImpl::aboutToAbort()
{
}

void MaciContainerServicesTestClassImpl::dynamicComponentTest() 
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
	 
	 // here we test some error handling
	 try
	 {
		 // we try to narrow to the wrong type (MaciTestClass)!
	 MACI_TEST::MaciTestClass_var comp = 
	 	 	getContainerServices()->getDynamicComponent<MACI_TEST::MaciTestClass>(cSpec,false);
	 }
	 catch(ACSErr::ACSbaseExImpl &ex)
	 {
		 ex.log();
	 }//try-catch
}//dynamicComponentTest

void MaciContainerServicesTestClassImpl::dynamicComponentSmartPtrTest() 
{
	 
	 // Prepare the ComponentSpec struct
	 ComponentSpec cSpec;
	 cSpec.component_name="*";		
	 cSpec.component_code="*";	
	 cSpec.container_name="*";
	 cSpec.component_type=IDLTYPE;
	 
	 // Get the default component for the given IDL interface
	 SmartPtr<MACI_TEST::DynamicTestClass> comp = 
	 	getContainerServices()->getDynamicComponentSmartPtr<MACI_TEST::DynamicTestClass>(cSpec,false);

	 // Execute a method on the remote component
	 comp->whoami();
}

void MaciContainerServicesTestClassImpl::collocatedComponentTest() 
{
	 
	 // Prepare the ComponentSpec struct
	 ComponentSpec cSpec;
	 cSpec.component_name="*";		
	 cSpec.component_code="*";	
	 cSpec.container_name="*";
	 cSpec.component_type=IDLTYPE;
	 
	 // Get the default component for the given IDL interface
	 MACI_TEST::DynamicTestClass_var comp = 
	 	getContainerServices()->getCollocatedComponent<MACI_TEST::DynamicTestClass>(cSpec,false,"MACI_DYN_TEST1");

	 // Execute a method on the remote component
	 comp->whoami();
	 // Release the component
	 getContainerServices()->releaseComponent(comp->name());
	 
	 // here we test some error handling
	 	 try
	 	 {
	 		 // we try to narrow to the wrong type (MaciTestClass)!
	 		MACI_TEST::MaciTestClass_var comp =
	 			 	getContainerServices()->getCollocatedComponent<MACI_TEST::MaciTestClass>(cSpec,false,"MACI_DYN_TEST1");
	 	 }
	 	 catch(ACSErr::ACSbaseExImpl &ex)
	 	 {
	 		 ex.log();
	 	 }//try-catch 
}

void MaciContainerServicesTestClassImpl::collocatedComponentSmartPtrTest() 
{
	 
	 // Prepare the ComponentSpec struct
	 ComponentSpec cSpec;
	 cSpec.component_name="*";		
	 cSpec.component_code="*";	
	 cSpec.container_name="*";
	 cSpec.component_type=IDLTYPE;
	 
	 // Get the default component for the given IDL interface
	 SmartPtr<MACI_TEST::DynamicTestClass> comp = 
	 	getContainerServices()->getCollocatedComponentSmartPtr<MACI_TEST::DynamicTestClass>(cSpec,false,"MACI_DYN_TEST1");

	 // Execute a method on the remote component
	 comp->whoami();
}

void MaciContainerServicesTestClassImpl::defaultComponentTest() 
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
	 
	 // here we test some error handling
	 try
	 {
		 // we try to narrow to the wrong type (MaciTestClass)!
		 MACI_TEST::MaciTestClass_var comp =
			 getContainerServices()->getDefaultComponent<MACI_TEST::MaciTestClass>(IDLTYPE);
	 }
	 catch(ACSErr::ACSbaseExImpl &ex)
	 {
		 ex.log();
	 }//try-catch 
}
  
void MaciContainerServicesTestClassImpl::defaultComponentSmartPtrTest() 
{
	 
	 // Get the default component for the given IDL interface
	 SmartPtr<MACI_TEST::DynamicTestClass> comp = 
	 	getContainerServices()->getDefaultComponentSmartPtr<MACI_TEST::DynamicTestClass>(IDLTYPE);

	 // Execute a method on the remote component
	 comp->whoami();
}
  
void MaciContainerServicesTestClassImpl::componentDescriptorTest() 
{
    ComponentInfo cInfo = getContainerServices()->getComponentDescriptor(COMPNAME);
    ACS_SHORT_LOG((LM_INFO,"ComponentInfo, Name: %s",cInfo.name.in()));
    ACS_SHORT_LOG((LM_INFO,"ComponentInfo, Type: %s",cInfo.type.in()));
    ACS_SHORT_LOG((LM_INFO,"ComponentInfo, Code: %s",cInfo.code.in()));
}

void MaciContainerServicesTestClassImpl::releaseResourcesTest() 
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
	 
	 // here we test some error handling
	 try
	 {
		 // we try to narrow to the wrong type (MaciTestClass)!
		 MACI_TEST::MaciTestClass_var comp =
			 getContainerServices()->getComponent<MACI_TEST::MaciTestClass>(COMPNAME);
	 }
	 catch(ACSErr::ACSbaseExImpl &ex)
	 {
		 ex.log();
	 }//try-catch 
}//getComponentTest

void MaciContainerServicesTestClassImpl::getComponentSmartPtrTest() 
{
	 // Get the COMPNAME component
	 SmartPtr<MACI_TEST::DynamicTestClass> comp = 
	 	getContainerServices()->getComponentSmartPtr<MACI_TEST::DynamicTestClass>(COMPNAME);

	 // Execute a method on the remote component
	 comp->whoami();
}

void MaciContainerServicesTestClassImpl::getComponentNonStickyTest() 
{
    try
	{
	 // 1st we have to activate the COMPNAME component
	 MACI_TEST::DynamicTestClass_var comp = 
	 	getContainerServices()->getComponent<MACI_TEST::DynamicTestClass>(COMPNAME);

	 MACI_TEST::DynamicTestClass_var cns = 
	     getContainerServices()->getComponentNonSticky<MACI_TEST::DynamicTestClass>(COMPNAME);
	 // Execute a method on the remote component
	 cns->whoami();
	 
	 // Release the component
	 getContainerServices()->releaseComponent(COMPNAME);
	}
    catch(maciErrType::CannotGetComponentExImpl &_ex)
	{
	ACS_SHORT_LOG((LM_ERROR,"getComponentNonStickyTest: Error getting %s non sticky",COMPNAME));
	_ex.log();
	}
    
// test w/o activating the component be4
    try
	{
	MACI_TEST::DynamicTestClass_var cns = 
	    getContainerServices()->getComponentNonSticky<MACI_TEST::DynamicTestClass>(COMPNAME);	
	}
    catch(maciErrType::CannotGetComponentExImpl &_ex)
	{
	ACS_SHORT_LOG((LM_ERROR, "getComponentNonStickyTest (%s): Got an exception (right behaviour)", COMPNAME));
	_ex.log();
	}//try-catch
    
    // here we test some error handling
    try
    {
    	// 1st we have to activate the COMPNAME component
    	MACI_TEST::DynamicTestClass_var c1 = 
    		getContainerServices()->getComponent<MACI_TEST::DynamicTestClass>(COMPNAME);

    	// we try to narrow to the wrong type (MaciTestClass)!
    	MACI_TEST::MaciTestClass_var comp =
    		getContainerServices()->getComponentNonSticky<MACI_TEST::MaciTestClass>(COMPNAME);
    }
    catch(ACSErr::ACSbaseExImpl &ex)
    {
    	ex.log();
    }//try-catch 
    // Release the component
    getContainerServices()->releaseComponent(COMPNAME);
}//getComponentNonStickyTest

void MaciContainerServicesTestClassImpl::getComponentNonStickySmartPtrTest() 
{
    try
	{
	 // 1st we have to activate the COMPNAME component
	 SmartPtr<MACI_TEST::DynamicTestClass> comp = 
	 	getContainerServices()->getComponentSmartPtr<MACI_TEST::DynamicTestClass>(COMPNAME);

	 SmartPtr<MACI_TEST::DynamicTestClass> cns = 
	     getContainerServices()->getComponentNonStickySmartPtr<MACI_TEST::DynamicTestClass>(COMPNAME);
	 // Execute a method on the remote component
	 cns->whoami();
	}
    catch(maciErrType::CannotGetComponentExImpl &_ex)
	{
	ACS_SHORT_LOG((LM_ERROR,"getComponentNonStickyTest: Error getting %s non sticky",COMPNAME));
	_ex.log();
	}
// test w/o activating the component be4
    try
	{
	SmartPtr<MACI_TEST::DynamicTestClass> cns = 
	    getContainerServices()->getComponentNonStickySmartPtr<MACI_TEST::DynamicTestClass>(COMPNAME);	
	}
    catch(maciErrType::CannotGetComponentExImpl &_ex)
	{
	ACS_SHORT_LOG((LM_ERROR, "getComponentNonStickyTest (%s): Got an exception (right behaviour)", COMPNAME));
	_ex.log();
	}//try-catch
}

void MaciContainerServicesTestClassImpl::componentListenerTest()
{
    try {
        BlockingComponentListener* blockLizzy = new BlockingComponentListener();
        getContainerServices()->registerComponentListener(blockLizzy);
        MACI_TEST::MaciTestClass* comp1 = getContainerServices()->getComponent<MACI_TEST::MaciTestClass>("MACI01");
        //comp1->activate_internal_component();
        // m_contSrvTesterComp.testForceReleaseComponent(DEFAULT_DUMMYCOMP_INSTANCE, true);
	    comp1->release_internal_component();
        sleep (2);
        // we shouldn't get notified as we're not a client of the component yet
        if(blockLizzy->getAllCompsAvailable().size() != 0) ACS_SHORT_LOG((LM_ERROR, "componentListenerTest getAllCompsAvailable != 0"));
        if(blockLizzy->getAllCompsUnavailable().size() != 0) ACS_SHORT_LOG((LM_ERROR, "componentListenerTest getAllCompsUnavailable != 0"));

        MACI_TEST::MaciTestClass_var obj =
	        getContainerServices()->getComponent<MACI_TEST::MaciTestClass>("MACI_SUB");
       
        // from now on we should be notified

        // component already active, this call will kill it . Should yield one notification
        blockLizzy->clearAndExpect(1);
	    
        system("maciReleaseComponent MACI_SUB");  
    //comp1->release_internal_component();
        sleep (2);
        if(!blockLizzy->awaitNotifications(10))
            ACS_SHORT_LOG((LM_ERROR, "Failed to get expected notification from manager within 10 seconds"));
        if(blockLizzy->getAllCompsAvailable().size() != 0) ACS_SHORT_LOG((LM_ERROR, "componentListenerTest getAllCompsAvailable != 0"));
        if(blockLizzy->getAllCompsUnavailable().size() != 1) ACS_SHORT_LOG((LM_ERROR, "componentListenerTest getAllCompsUnavailable != 1"));
        
        // this call will both activate and kill the component . Should yield two notifications in total

        /* blockLizzy.clearAndExpect(2);
        m_contSrvTesterComp.testForceReleaseComponent(DEFAULT_DUMMYCOMP_INSTANCE, true);
        assertTrue("Failed to get expected notification from manager within 10 seconds",
                blockLizzy.awaitNotifications(10, TimeUnit.SECONDS));
        assertEquals(1, blockLizzy.getAllCompsAvailable().size());
        assertEquals(1, blockLizzy.getAllCompNamesUnavailable().size());
*/
	    getContainerServices()->releaseComponent(comp1->name());
        getContainerServices()->releaseComponent("MACI_SUB");
    } catch (...) {
        ACS_SHORT_LOG((LM_ERROR,"componentListenerTest Exception"));
    }


}

//--------------------------------------------------------------
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(MaciContainerServicesTestClassImpl)

/*___oOo___*/

