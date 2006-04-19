/*************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: acscomponentImpl.cpp,v 1.35 2006/04/19 19:57:13 bjeram Exp $"
*
* who       when        what
* --------  --------    --------------------------------------------------
* rcirami   2003-08-28  created
*/

#include "acscomponentImpl.h"
#include "loggingLogger.h"
#include "acsErrTypeComponent.h"
#include <string>

using namespace maci; 
using namespace acscomponent;

//
// ACSComponent Constructor
//
ACSComponentImpl::ACSComponentImpl(
    const ACE_CString& name,
    maci::ContainerServices *containerServices) :
  m_name(name),
						m_containerServices_p(containerServices)//,  Save the reference to the ContainerServices
//  logger_m(getNamedLogger(name.c_str()))
{
  ACS_TRACE("acscomponent::ACSComponentImpl::ACSComponentImpl");
  
  // Check if the ContainerServices is NULL
  // Now the ContainerServices is very important and does a lof things
  // and the component
  if (containerServices==NULL)
  {
    acsErrTypeComponent::InvalidContainerServicesExImpl ex(__FILE__,__LINE__,"acscomponent::ACSComponentImpl::ACSComponentImpl");
    throw ex;
  }
}

//
// ACSComponent Destructor
//
ACSComponentImpl::~ACSComponentImpl()
{
  ACS_TRACE("acscomponent::ACSComponentImpl::~ACSComponentImpl");
}

/*********************** IDL interface **********************/

char *
ACSComponentImpl::name ()
  throw (CORBA::SystemException)
{
  return CORBA::string_dup(m_name.c_str());
}

ACS::ComponentStates
ACSComponentImpl::componentState ()
  throw (CORBA::SystemException)
{
  if (m_containerServices_p==NULL)
  {
    return ACS::COMPSTATE_UNKNOWN;
  }
  else if (m_containerServices_p->getComponentStateManager()==NULL)
  {
    return ACS::COMPSTATE_UNKNOWN;
  }
  else return m_containerServices_p->getComponentStateManager()->getCurrentState();
}


/********************** LifeCycle methods ***********************/

void ACSComponentImpl::initialize() throw (ACSErr::ACSbaseExImpl)
{
  ACS_TRACE("acscomponent::ACSComponentImpl::initialize");
}

void ACSComponentImpl::execute() throw (ACSErr::ACSbaseExImpl)
{
  ACS_TRACE("acscomponent::ACSComponentImpl::execute");
}

void ACSComponentImpl::cleanUp()
{
  ACS_TRACE("acscomponent::ACSComponentImpl::cleanUp");
}

void ACSComponentImpl::aboutToAbort()
{
  ACS_TRACE("acscomponent::ACSComponentImpl::cleanUp");
}

void ACSComponentImpl::__initialize() throw (ACSErr::ACSbaseExImpl)
{   
   ACS_TRACE("acscomponent::ACSComponentImpl::__initialize");
    try
    {
        initialize();
    }
    catch (ACSErr::ACSbaseExImpl &ex)
    {
        throw acsErrTypeLifeCycle::LifeCycleExImpl(ex, __FILE__,__LINE__,"ACSComponentImpl::__initialize");
    }
}

void ACSComponentImpl::__execute() throw (ACSErr::ACSbaseExImpl)
{
  ACS_TRACE("acscomponent::ACSComponentImpl::__execute");
/*
startAllThreads is not there but it might be goot to put there
so that all threads are just created in suspend mode and than startAllThreads would resume them
  if (getContainerServices()->getThreadManager()->startAllThreads() == false)
      {
      throw acsErrTypeLifeCycle::LifeCycleExImpl(ex,__FILE__,__LINE__,"ACSComponentImpl::__execute(): startAllThreads failed");
      }
*/
    try
    {
        execute();
    }
    catch (ACSErr::ACSbaseExImpl &ex)
    {
        throw acsErrTypeLifeCycle::LifeCycleExImpl(ex,__FILE__,__LINE__,"ACSComponentImpl::__execute");
    }
}
    
void ACSComponentImpl::__aboutToAbort()
{
  ACS_TRACE("acscomponent::ACSComponentImpl::__aboutToAbort");
  if (getContainerServices()->getThreadManager()->stopAll() == false)
      {
      throw acsErrTypeLifeCycle::StoppingThreadsFailureExImpl(__FILE__,__LINE__,"ACSComponentImpl::__aboutToAbort");
      }

    try
    {
        aboutToAbort();
    }
    catch (ACSErr::ACSbaseExImpl &ex)
    {
        throw acsErrTypeLifeCycle::LifeCycleExImpl(ex,__FILE__,__LINE__,"ACSComponentImpl::__aboutToAbort");
    }
}
    
void ACSComponentImpl::__cleanUp()
{
  ACS_TRACE("acscomponent::ACSComponentImpl::__cleanUp");
 
  try
      {
      cleanUp();
      }
  catch(ACSErr::ACSbaseExImpl &ex)
      {
      throw acsErrTypeLifeCycle::LifeCycleExImpl(ex, __FILE__,__LINE__,"ACSComponentImpl::__cleanUp");
      }
  catch(...)
      {
      throw acsErrTypeLifeCycle::LifeCycleExImpl( __FILE__,__LINE__,"ACSComponentImpl::__cleanUp");
      }

  // just in case if a user does not stop the threads we stop them here
  if (getContainerServices()->getThreadManager()->stopAll() == false)
      {
      throw acsErrTypeLifeCycle::StoppingThreadsFailureExImpl(__FILE__,__LINE__,"ACSComponentImpl::__cleanUp");
      }
}

/*******************************  protected methods *******************************/


maci::ContainerServices*
ACSComponentImpl::getContainerServices()
{
  return GetImpl(m_containerServices_p);
}

