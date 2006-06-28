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
* "@(#) $Id: baciCharacteristicComponentImpl.cpp,v 1.40 2006/06/28 08:07:11 bjeram Exp $"
*
*/

#include <vltPort.h>

static char *rcsId="@(#) $Id: baciCharacteristicComponentImpl.cpp,v 1.40 2006/06/28 08:07:11 bjeram Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <baci.h>
#include <baciS.h>
#include <baciDB.h>
#include <acsutil.h>
#include <acserr.h>
#include <logging.h>
#include "baciCharacteristicComponentImpl.h"
#include <ACSErrTypeCommon.h>

NAMESPACE_USE(baci);
using namespace acscomponent;

//
// CharacteristicComponentImpl Constructor
//
CharacteristicComponentImpl::CharacteristicComponentImpl(
        const ACE_CString& name,
        maci::ContainerServices *containerServices,
	bool monitoringProperties) :
    ACSComponentImpl(name,containerServices),
    CharacteristicModelImpl(name,containerServices),
    desc_m(0),
    monitoringProperties_mp(monitoringProperties),
    component_mp(0)
{  
    // Create Component
    component_mp = new BACIComponent(getContainerServices()->getThreadManager(), name, this);
    if (component_mp==0) 
	{
	return;
	}

    // Setup most of the characteristics for this CharacteristicComponent minus properties.  
    // Those are handled in subclasses constructors.
    
    desc_m = new ACS::CharacteristicComponentDesc();
    desc_m->name            = CORBA::string_dup(component_mp->getName());
    desc_m->characteristics = get_all_characteristics(); 

    // Here initialization is not complete. The lifecycle has to be dealed with
}

//
// CharacteristicComponentImpl Destructor
//
CharacteristicComponentImpl::~CharacteristicComponentImpl()
{
  ACS_TRACE("CharacteristicComponentImpl::~CharacteristicComponentImpl");

  // delete component
  if (component_mp!=0) 
    { 
      delete component_mp; 
      component_mp = 0; 
    }
}

/* ----------------------------------------------------------------*/
/* --------------------- [ LifeCycle functions ] ------------------*/
/* ----------------------------------------------------------------*/

void CharacteristicComponentImpl::__execute() throw (ACSErr::ACSbaseExImpl)
{
  ACS_TRACE("baci::CharacteristicComponentImpl::__execute");
    // Start the threads actually created and started
    if (component_mp==NULL)
	{
	ACSErrTypeCommon::NullPointerExImpl ex(__FILE__, __LINE__, "CharacteristicComponentImpl::__execute");
	ex.setVariable("component_mp(BACIComponent)");
	throw ex;
	}//
    if( monitoringProperties_mp == true )
	{
	if (component_mp->startAllThreads() == false) 
	    {
	    throw acsErrTypeLifeCycle::StartingThreadsFailureExImpl(__FILE__,__LINE__,"ACSComponentImpl::__execute");
	    }
	}
    else
	{
	if (component_mp->startActionThread() == false) 
	    {
	    throw acsErrTypeLifeCycle::StartingThreadsFailureExImpl(__FILE__,__LINE__,"ACSComponentImpl::__execute");
	    }
	}//if-else

    ACSComponentImpl::__execute();
}//__execute
    
void CharacteristicComponentImpl::__aboutToAbort()
{
  ACS_TRACE("baci::CharacteristicComponentImpl::__aboutToAbort");
    // Stop the threads
    if (component_mp)
    {
        component_mp->stopAllThreads();
    }

    ACSComponentImpl::__aboutToAbort();
}
    
void CharacteristicComponentImpl::__cleanUp()
{
  ACS_TRACE("baci::CharacteristicComponentImpl::__cleanUp");
    // Stop the threads 
    if (component_mp)
    {
        component_mp->stopAllThreads();
    }
    ACSComponentImpl::__cleanUp();
}


void  CharacteristicComponentImpl::startPropertiesMonitoring()
{
    if ( component_mp!=NULL)
	{
	try
	    {
	    component_mp->startMonitoringThread();
	    }
	catch(ACSErr::ACSbaseExImpl &_ex)
	    {
	    acsthreadErrType::CanNotStartThreadExImpl ex(_ex, __FILE__,__LINE__,"CharacteristicComponentImpl::startPropertiesMonitoring");
	    maci::ContainerServices *cs = getContainerServices();
	    if ( cs!=NULL )
		ex.setThreadName(cs->getName()+"::monitorThread");
	    throw ex;
	    }//try-catch
	}
    else
	{
	ACSErrTypeCommon::NullPointerExImpl ex(__FILE__, __LINE__, "CharacteristicComponentImpl::startPropertiesMonitoring");
	ex.setVariable("cs(ContainerServices)");
	throw ex;
	}
}//resumePropertiesMonitoring

void  CharacteristicComponentImpl::stopPropertiesMonitoring()
{
    maci::ContainerServices *cs = getContainerServices();
    if ( cs!=NULL )
	{
	if (cs->getThreadManager()->suspend(cs->getName()+"::monitorThread")==false)
	    {
	     acsthreadErrType::CanNotSuspendThreadExImpl ex(__FILE__,__LINE__,"CharacteristicComponentImpl::stopPropertiesMonitoring");
	     ex.setThreadName(cs->getName()+"::monitorThread");
	    throw ex;
	    }
	}
    else
	{
	ACSErrTypeCommon::NullPointerExImpl ex(__FILE__, __LINE__, "CharacteristicComponentImpl::stopPropertiesMonitoring");
	ex.setVariable("cs(ContainerServices)");
	throw ex;
	}
}//suspendPropertiesMonitoring

void CharacteristicComponentImpl::addPropertyToDesc(ACS::Property_ptr prop)
{
    desc_m->properties.length(desc_m->properties.length()+1); 
    desc_m->properties[desc_m->properties.length()-1].property_ref = prop; 
    desc_m->properties[desc_m->properties.length()-1].name = prop->name(); 
    desc_m->properties[desc_m->properties.length()-1].characteristics = prop->get_all_characteristics();
}

/* ----------------------------------------------------------------*/
/* --------------------- [ CORBA interface ] ----------------------*/
/* ----------------------------------------------------------------*/

//////////////////////////////////////////////////////////////////////////////////////////
ACS::CharacteristicComponentDesc *
CharacteristicComponentImpl::descriptor ()
    throw (CORBA::SystemException)
{   
    ACS_TRACE("::CharacteristicComponentImpl::descriptor");
    
    if (getPOA().ptr() != PortableServer::POA::_nil()) 
	{ 
	try
	    {  
	    CORBA::Object_var obj = getPOA()->servant_to_reference(this); 
	     		
	    if (CORBA::is_nil(obj.in())==false) 
		{ 
		desc_m->characteristic_component_ref = ACS::CharacteristicComponent::_narrow(obj.in()); 
		} 
	    else 
		{ 
		desc_m->characteristic_component_ref = ACS::CharacteristicComponent::_nil();
		}
	    } 
	catch(...) 
	    { 
	    ACS_LOG(LM_RUNTIME_CONTEXT, "::CharacteristicComponentImpl::descriptor", 
		    (LM_ERROR, "Failed to retrieve object reference from id of Component %s.", component_mp->getName())); 
	    desc_m->characteristic_component_ref = ACS::CharacteristicComponent::_nil(); 
	    }  
	} 
    else 
	{ 
	desc_m->characteristic_component_ref = ACS::CharacteristicComponent::_nil(); 
	}   
    
    ACS::CharacteristicComponentDesc_var tDesc = desc_m;
    return tDesc._retn();
}
//////////////////////////////////////////////////////////////////////////////////////////

//
// ___oOo___
//
