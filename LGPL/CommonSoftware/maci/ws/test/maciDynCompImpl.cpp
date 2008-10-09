/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: maciDynCompImpl.cpp,v 1.13 2008/10/09 07:05:37 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* 
* acaproni  2005-02-28  created 
* 
*------------------------------------------------------------------------
*/

#define _POSIX_SOURCE 1
#include "vltPort.h"

static char *rcsId="@(#) $Id: maciDynCompImpl.cpp,v 1.13 2008/10/09 07:05:37 cparedes Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "maciDynCompImpl.h"
#include <iostream>

using namespace maci;

DynamicTestClassImpl::DynamicTestClassImpl(
    const ACE_CString& name,
    maci::ContainerServices* containerServices):
	acscomponent::ACSComponentImpl(name,containerServices),
    launchException_m(0)
{
    ACE_CString fullName="alma/"+name;
    
    
    try 
    {
        CDB::DAL_ptr dal_p = getContainerServices()->getCDB();
        CDB::DAO_ptr dao_p = dal_p->get_DAO_Servant(fullName.c_str());
        launchException_m = dao_p->get_long("Exception");
    } 
    catch (...) 
    {
        // We receive an error from the CDB 
        // maybe a WrongDataType or a FieldDoesNotExist exception
        launchException_m=0;
        ACS_SHORT_LOG((LM_ERROR,"Error reading Exception from the CDB"));
    }
    
    if (launchException_m!=0)
    {
      ACS_SHORT_LOG((LM_WARNING,"%s will throw an exception if %d is executed",name.c_str(),launchException_m));
    }
    
    if (launchException_m==1)
    {
      ACS_SHORT_LOG((LM_INFO,"Throwing an exception in the constructor"));
      // This could be another kind of exception as well
      // I used this one to avoid creating some temporary ACS exception only for 
      // this example
      throw acsErrTypeLifeCycle::LifeCycleExImpl(__FILE__,__LINE__,"ACSComponentImpl::__initialize(): exception in user initialize");
    }
}

DynamicTestClassImpl::~DynamicTestClassImpl()
{
}
  
void DynamicTestClassImpl::initialize()
{
  if (launchException_m==2)
    {
      ACS_SHORT_LOG((LM_INFO,"Throwing an exception in initialize"));
      throw acsErrTypeLifeCycle::LifeCycleExImpl(__FILE__,__LINE__,"DynamicTestClassImpl::initialize() test exception");
    }
}

void DynamicTestClassImpl::execute()
{
  if (launchException_m==3)
    {
      ACS_SHORT_LOG((LM_INFO,"Throwing an exception in execute"));
      throw acsErrTypeLifeCycle::LifeCycleExImpl(__FILE__,__LINE__,"DynamicTestClassImpl::execute() test exception");
    }
}

void DynamicTestClassImpl::cleanUp()
{
  if (launchException_m==4)
    {
      ACS_SHORT_LOG((LM_INFO,"Throwing an exception in cleanUp"));
      throw acsErrTypeLifeCycle::LifeCycleExImpl(__FILE__,__LINE__,"DynamicTestClassImpl::cleanUp() test exception");
    }
}

void DynamicTestClassImpl::aboutToAbort()
{
  if (launchException_m==5)
    {
      ACS_SHORT_LOG((LM_INFO,"Throwing an exception in aboutToAbort"));
      throw acsErrTypeLifeCycle::LifeCycleExImpl(__FILE__,__LINE__,"DynamicTestClassImpl::aboutToAbort() test exception");
    }
}

  /* ----------------------------------------------------------------*/
  /* --------------------- [ CORBA interface ] ----------------------*/
  /* ----------------------------------------------------------------*/

void DynamicTestClassImpl::whoami()
{
	ACS_SHORT_LOG((LM_INFO,"I am %s",getContainerServices()->getName().c_str()));
}

#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(DynamicTestClassImpl)

/*___oOo___*/

