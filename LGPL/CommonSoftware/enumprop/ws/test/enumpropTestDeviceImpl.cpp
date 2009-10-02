/* @(#) $Id: enumpropTestDeviceImpl.cpp,v 1.49 2009/10/02 13:57:40 bjeram Exp $
 */
/*
*    DeviceImpl.cpp - ALMA Device interface implementation.
*    
*    ALMA - Atacama Large Millimiter Array
*    (c) Associated Universities Inc., 2001
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
*/

#include <baci.h>
#include <baciS.h>
#include <baciDB.h>
#include <logging.h>
#include <acsutil.h>

#include "enumpropTestDeviceImpl.h"
  
NAMESPACE_USE(baci)

 
// Implementation skeleton constructor
enumpropTestDeviceImpl::enumpropTestDeviceImpl (
    const ACE_CString& name,
    maci::ContainerServices* containerServices) :
    CharacteristicComponentImpl(name,containerServices),
    m_currentState(0), 
    m_currentStateRW(0)
{
  ACS_TRACE("enumpropTestDeviceImpl::enumpropTestDeviceImpl");
  
  // properties
  m_currentState = new ROEnumImpl<ACS_ENUM_T(ENUMPROP_TEST::States), POA_ENUMPROP_TEST::ROStates> (name+":currentStates", getComponent());
  CHARACTERISTIC_COMPONENT_PROPERTY(currentState,m_currentState)
 
  m_currentStateRW = new RWEnumImpl<ACS_ENUM_T(ENUMPROP_TEST::States), POA_ENUMPROP_TEST::RWStates> (name+":currentStatesRW", getComponent());
  CHARACTERISTIC_COMPONENT_PROPERTY(currentStateRW,m_currentStateRW)
 
  ACS_SHORT_LOG((LM_INFO, "enumprop Test Device: %s created", name.c_str()));

 /*
  * Here we are at the top of the hierarchy and we do not have
  * a container to use for the test.
  * Therefore we call by hand initialize() and execute()
  * to "activate" the component
  */
  __initialize();
  __execute();
}
  
// Implementation skeleton destructor
enumpropTestDeviceImpl::~enumpropTestDeviceImpl (void)
{
  ACS_TRACE("enumpropTestDeviceImpl::~enumpropTestDeviceImpl");
  if (getComponent())
      ACS_DEBUG_PARAM("::BaciTestClassImpl::~BaciTestClassImpl", "Destroying %s...", getComponent()->getName());

  // stop threads
  if (getComponent())
      getComponent()->stopAllThreads();

  // properties
  if (m_currentState) { m_currentState->destroy(); m_currentState=0; }
  if (m_currentStateRW) { m_currentStateRW->destroy(); m_currentStateRW=0; }

  ACS_DEBUG("::eumpropTestDeviceImpl::~enumpropTestDeviceImpl", "Properties destroyed");
  
  ACS_DEBUG("::enumpropTestDeviceImpl::~enumpropTestDeviceImpl", "Component destroyed");
}//~
 

/* ----------------------------------------------------------------*/
/* --------------------- [ CORBA interface ] ----------------------*/
/* ----------------------------------------------------------------*/

ENUMPROP_TEST::ROStates_ptr enumpropTestDeviceImpl::currentState (
    
  )

{
  if (!m_currentState)
    return ENUMPROP_TEST::ROStates::_nil();
  ENUMPROP_TEST::ROStates_var prop = ENUMPROP_TEST::ROStates::_narrow(m_currentState->getCORBAReference());
  return prop._retn();
}

ACSErr::Completion * enumpropTestDeviceImpl::enable () 
{
    
    ACSErr::Completion_var c = new Completion;

    printf("ENABLED\n");
    ENUMPROP_TEST::States st=ENUMPROP_TEST::ENABLED;
    m_currentState->getDevIO()->write( st, c->timeStamp );
    return c._retn();
}
 

ACSErr::Completion * enumpropTestDeviceImpl::disable ( )
{
    
    ACSErr::Completion_var c = new Completion;

    printf("DISABLE\n");

    ENUMPROP_TEST::States st = ENUMPROP_TEST::DISABLED;
    m_currentState->getDevIO()->write( st, c->timeStamp );
    return c._retn();
}
  
ACSErr::Completion * enumpropTestDeviceImpl::diagnose (  )
{
  
  ACSErr::Completion_var c = new Completion;

  printf("DIAGNOSE\n");
  ENUMPROP_TEST::States st = ENUMPROP_TEST::DIAGNOSE;
  m_currentState->getDevIO()->write( st, c->timeStamp );
  return c._retn();
}

ACSErr::Completion * enumpropTestDeviceImpl::shutdown ( )
{
    ACSErr::Completion_var c = new Completion;

    printf("SHUTDOWN\n");
    ENUMPROP_TEST::States st =  ENUMPROP_TEST::SHUTDOWN;
    m_currentState->getDevIO()->write( st, c->timeStamp );
    return c._retn();
}

ACSErr::Completion * enumpropTestDeviceImpl::init ( )
{
    ACSErr::Completion_var c = new Completion;

    printf("INITIALIZE\n");

    ENUMPROP_TEST::States st =  ENUMPROP_TEST::INITIALIZE;
    m_currentState->getDevIO()->write( st, c->timeStamp );
    return c._retn();
}

ACSErr::Completion * enumpropTestDeviceImpl::on ( )
{
    ACSErr::Completion_var c = new Completion;

    printf("ON\n");
    ENUMPROP_TEST::States st = ENUMPROP_TEST::ON; 
    m_currentState->getDevIO()->write( st, c->timeStamp );
    return c._retn();
}

ACSErr::Completion * enumpropTestDeviceImpl::off ( )
{
    ACSErr::Completion_var c = new Completion;

    printf("OFF\n");
    ENUMPROP_TEST::States st = ENUMPROP_TEST::OFF; 
    m_currentState->getDevIO()->write( st, c->timeStamp );
    return c._retn(); 
}

ENUMPROP_TEST::RWStates_ptr enumpropTestDeviceImpl::currentStateRW (
    
  )

{
  if (!m_currentStateRW)
    return ENUMPROP_TEST::RWStates::_nil();
  ENUMPROP_TEST::RWStates_var prop = ENUMPROP_TEST::RWStates::_narrow(m_currentStateRW->getCORBAReference());
  return prop._retn();
}

void  enumpropTestDeviceImpl::serverShutdown ()
{
    
#ifndef MAKE_VXWORKS
    ACS_SHORT_LOG((LM_INFO, "enumpropTestDeviceImpl::serverShutdown")); 
    BACI_CORBA::getORB()->shutdown(true);
    //LoggingProxy::done();
#endif
}


void  enumpropTestDeviceImpl::changeAlarmFFFM(const char *ff, const char *fm)
{
	m_currentState->setAlarmFaultFamily(ff);
	m_currentState->setAlarmFaultMember(fm);
}//changeAlarmFFFM

// at this point we can not use  MACI_DLL_SUPPORT_FUNCTIONS
// since it is not available yet

ACS_DLL_UNMANGLED_EXPORT PortableServer::Servant ConstructComponent(CORBA::ULong, 
							  const char * name_p, 
							  const char * type_p, 
							  maci::ContainerServices * containerServices) 
{ 
    ACE_UNUSED_ARG(type_p); 
    enumpropTestDeviceImpl* servant_p =0; 
    servant_p = new enumpropTestDeviceImpl(name_p, containerServices); 
    return servant_p; 
} 

ACS_DLL_UNMANGLED_EXPORT bool DLLOpen(int, char**) 
{ 
    return true; 
} 

ACS_DLL_UNMANGLED_EXPORT void DLLClose() 
{ 
}
