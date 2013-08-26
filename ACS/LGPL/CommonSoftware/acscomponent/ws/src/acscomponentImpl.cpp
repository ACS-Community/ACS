/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
/*************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: acscomponentImpl.cpp,v 1.41 2013/01/16 16:22:16 acaproni Exp $"
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
    Logging::Loggable(containerServices->getLogger()),
    m_name(name),
    m_containerServices_p(containerServices)
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
{
  return CORBA::string_dup(componentName());
}

ACS::ComponentStates
ACSComponentImpl::componentState ()
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

void ACSComponentImpl::initialize()
{
  ACS_TRACE("acscomponent::ACSComponentImpl::initialize");
}

void ACSComponentImpl::execute()
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

void ACSComponentImpl::__initialize()
{
   ACS_TRACE("acscomponent::ACSComponentImpl::__initialize");
    try
    {
        initialize();
    }
    catch (ACSErr::ACSbaseExImpl &ex)
    {
        throw acsErrTypeLifeCycle::LifeCycleExImpl(ex, __FILE__,__LINE__,"ACSComponentImpl::__initialize");
    }catch(const std::exception &stdex){
    	ACSErrTypeCommon::StdExceptionExImpl ex(__FILE__, __LINE__, __FUNCTION__);
    	ex.setWhat(stdex.what());
    	acsErrTypeLifeCycle::LifeCycleExImpl lex(ex, __FILE__,__LINE__, __FUNCTION__);
    	lex.addData("WARNING:", "component lifecylce method: initialize can throw just ACS based exceptions. Please check your code.");
    	throw lex;

    }catch( CORBA::SystemException &_ex )
    {
    	ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__, __FUNCTION__);
    	corbaProblemEx.setMinor(_ex.minor());
    	corbaProblemEx.setCompletionStatus(_ex.completed());
    	corbaProblemEx.setInfo(_ex._info().c_str());
    	acsErrTypeLifeCycle::LifeCycleExImpl lex(corbaProblemEx, __FILE__,__LINE__, __FUNCTION__);
    	lex.addData("WARNING:", "component lifecylce method: initialize can throw just ACS based exceptions. Please check your code.");
    	throw lex;
    }catch( CORBA::UserException &_ex )
    {
    	ACSErrTypeCommon::CORBAUserExceptionExImpl corbaProblemEx(__FILE__, __LINE__, __FUNCTION__);
    	corbaProblemEx.setInfo(_ex._info());
    	acsErrTypeLifeCycle::LifeCycleExImpl lex(__FILE__,__LINE__, __FUNCTION__);

    	lex.addData("WARNING:", "component lifecylce method: initialize can throw just ACS based exceptions. Please check your code.");
    	throw lex;
    }catch(...){

    	ACSErrTypeCommon::UnknownExImpl ex(__FILE__, __LINE__, __FUNCTION__);
    	acsErrTypeLifeCycle::LifeCycleExImpl lex(ex, __FILE__,__LINE__, __FUNCTION__);
    	lex.addData("WARNING:", "component lifecylce method: initialize can throw just ACS based exceptions. Please check your code.");
        throw lex;
    }

}

void ACSComponentImpl::__execute()
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
    }catch(const std::exception &stdex){
    	ACSErrTypeCommon::StdExceptionExImpl ex(__FILE__, __LINE__, __FUNCTION__);
    	ex.setWhat(stdex.what());
    	acsErrTypeLifeCycle::LifeCycleExImpl lex(ex, __FILE__,__LINE__, __FUNCTION__);
    	lex.addData("WARNING:", "component lifecylce method: execute can throw just ACS based exceptions. Please check your code.");
    	throw lex;

    }catch( CORBA::SystemException &_ex )
    {
    	ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__, __FUNCTION__);
    	corbaProblemEx.setMinor(_ex.minor());
    	corbaProblemEx.setCompletionStatus(_ex.completed());
    	corbaProblemEx.setInfo(_ex._info().c_str());
    	acsErrTypeLifeCycle::LifeCycleExImpl lex(corbaProblemEx, __FILE__,__LINE__, __FUNCTION__);
    	lex.addData("WARNING:", "component lifecylce method: execute can throw just ACS based exceptions. Please check your code.");
    	throw lex;
    }catch( CORBA::UserException &_ex )
    {
    	ACSErrTypeCommon::CORBAUserExceptionExImpl corbaProblemEx(__FILE__, __LINE__, __FUNCTION__);
    	corbaProblemEx.setInfo(_ex._info());
    	acsErrTypeLifeCycle::LifeCycleExImpl lex(__FILE__,__LINE__, __FUNCTION__);

    	lex.addData("WARNING:", "component lifecylce method: execute can throw just ACS based exceptions. Please check your code.");
    	throw lex;
    }catch(...){

    	ACSErrTypeCommon::UnknownExImpl ex(__FILE__, __LINE__, __FUNCTION__);
    	acsErrTypeLifeCycle::LifeCycleExImpl lex(ex, __FILE__,__LINE__, __FUNCTION__);
    	lex.addData("WARNING:", "component lifecylce method: execute can throw just ACS based exceptions. Please check your code.");
        throw lex;
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
    }catch(const std::exception &stdex){
    	ACSErrTypeCommon::StdExceptionExImpl ex(__FILE__, __LINE__, __FUNCTION__);
    	ex.setWhat(stdex.what());
    	acsErrTypeLifeCycle::LifeCycleExImpl lex(ex, __FILE__,__LINE__, __FUNCTION__);
    	lex.addData("WARNING:", "component lifecylce method: aboutToAbort can throw just ACS based exceptions. Please check your code.");
    	throw lex;

    }catch( CORBA::SystemException &_ex )
    {
    	ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__, __FUNCTION__);
    	corbaProblemEx.setMinor(_ex.minor());
    	corbaProblemEx.setCompletionStatus(_ex.completed());
    	corbaProblemEx.setInfo(_ex._info().c_str());
    	acsErrTypeLifeCycle::LifeCycleExImpl lex(corbaProblemEx, __FILE__,__LINE__, __FUNCTION__);
    	lex.addData("WARNING:", "component lifecylce method: aboutToAbort can throw just ACS based exceptions. Please check your code.");
    	throw lex;
    }catch( CORBA::UserException &_ex )
    {
    	ACSErrTypeCommon::CORBAUserExceptionExImpl corbaProblemEx(__FILE__, __LINE__, __FUNCTION__);
    	corbaProblemEx.setInfo(_ex._info());
    	acsErrTypeLifeCycle::LifeCycleExImpl lex(__FILE__,__LINE__, __FUNCTION__);

    	lex.addData("WARNING:", "component lifecylce method: aboutToAbort can throw just ACS based exceptions. Please check your code.");
    	throw lex;
    }catch(...){

    	ACSErrTypeCommon::UnknownExImpl ex(__FILE__, __LINE__, __FUNCTION__);
    	acsErrTypeLifeCycle::LifeCycleExImpl lex(ex, __FILE__,__LINE__, __FUNCTION__);
    	lex.addData("WARNING:", "component lifecylce method: aboutToAbort can throw just ACS based exceptions. Please check your code.");
        throw lex;
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
  }catch(const std::exception &stdex){
	  ACSErrTypeCommon::StdExceptionExImpl ex(__FILE__, __LINE__, __FUNCTION__);
	  ex.setWhat(stdex.what());
	  acsErrTypeLifeCycle::LifeCycleExImpl lex(ex, __FILE__,__LINE__, __FUNCTION__);
	  lex.addData("WARNING:", "component lifecylce method: cleanUp can throw just ACS based exceptions. Please check your code.");
	  throw lex;

  }catch( CORBA::SystemException &_ex )
  {
	  ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__, __FUNCTION__);
	  corbaProblemEx.setMinor(_ex.minor());
	  corbaProblemEx.setCompletionStatus(_ex.completed());
	  corbaProblemEx.setInfo(_ex._info().c_str());
	  acsErrTypeLifeCycle::LifeCycleExImpl lex(corbaProblemEx, __FILE__,__LINE__, __FUNCTION__);
	  lex.addData("WARNING:", "component lifecylce method: cleanUp can throw just ACS based exceptions. Please check your code.");
	  throw lex;
  }catch( CORBA::UserException &_ex )
  {
	  ACSErrTypeCommon::CORBAUserExceptionExImpl corbaProblemEx(__FILE__, __LINE__, __FUNCTION__);
	  corbaProblemEx.setInfo(_ex._info());
	  acsErrTypeLifeCycle::LifeCycleExImpl lex(__FILE__,__LINE__, __FUNCTION__);

	  lex.addData("WARNING:", "component lifecylce method: cleanUp can throw just ACS based exceptions. Please check your code.");
	  throw lex;
  }catch(...){

	  ACSErrTypeCommon::UnknownExImpl ex(__FILE__, __LINE__, __FUNCTION__);
	  acsErrTypeLifeCycle::LifeCycleExImpl lex(ex, __FILE__,__LINE__, __FUNCTION__);
	  lex.addData("WARNING:", "component lifecylce method: cleanUp can throw just ACS based exceptions. Please check your code.");
	  throw lex;
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

