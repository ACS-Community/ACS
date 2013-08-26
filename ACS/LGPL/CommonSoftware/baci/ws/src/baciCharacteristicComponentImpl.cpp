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
* "@(#) $Id: baciCharacteristicComponentImpl.cpp,v 1.53 2011/09/02 11:45:19 bjeram Exp $"
*
*/

#include <vltPort.h>

static const char*  rcsId="@(#) $Id: baciCharacteristicComponentImpl.cpp,v 1.53 2011/09/02 11:45:19 bjeram Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <baci.h>
#include <baciS.h>
#include <baciDB.h>
#include <acsutil.h>
#include <acserr.h>
#include <logging.h>
#include "baciCharacteristicComponentImpl.h"
#include <ACSErrTypeCommon.h>

 using namespace baci;
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


    CORBA::Long actionThreadStackSize, monitoringThreadStackSize;
	actionThreadStackSize = 1024;
	monitoringThreadStackSize = 2048;
// the error handling here is not the nicest because we have to handle two temporary solutions:
// CDB does not derive from CC XSD
// CC can exist w/o alma CDB part
// in general error handling should be improved for CC ctor
	cdb::DAONode* dao=0;

	try
	{
		dao = this->getDAONode();
		if (!dao) return; //Actually dao should not be 0, but it has to be verified
	}
	catch (ACSErr::ACSbaseExImpl& ex)
	{
		dao = 0;
		ex.log(LM_WARNING);
	}
	catch (...)
	{
			dao = 0;
	}

	try
	  {
		if (dao) actionThreadStackSize = dao->get_long("actionThreadStackSize");
	}
	catch (ACSErr::ACSbaseExImpl& ex)
	{
		ex.log(LM_WARNING);
	}
	catch (...)
	{
	}

	try
	{
		if (dao) monitoringThreadStackSize = dao->get_long("monitoringThreadStackSize");
	}
	catch (ACSErr::ACSbaseExImpl& ex)
	{
		ex.log(LM_WARNING);
	}
	catch (...)
	{
	}

	actionThreadStackSize *= 1024;  // that we have size in bytes
	monitoringThreadStackSize *= 1024;  // that we have size in bytes

  // Create Component
    component_mp = new BACIComponent(getContainerServices()->getThreadManager(), name, getContainerServices()->getType(), this,
										actionThreadStackSize, monitoringThreadStackSize);
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

void CharacteristicComponentImpl::__execute() 
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
	bool st;
	try
	    {
	    st = component_mp->startAllThreads();
	    }
	catch(ACSErr::ACSbaseExImpl &_ex)
	    {
	    throw acsErrTypeLifeCycle::StartingThreadsFailureExImpl(_ex, __FILE__,__LINE__,"ACSComponentImpl::__execute");
	    }//try-catch
	
	if (st == false) 
	    throw acsErrTypeLifeCycle::StartingThreadsFailureExImpl(__FILE__,__LINE__,"ACSComponentImpl::__execute");
	}
    else
	{
	try
	    {
	    component_mp->startActionThread();
	    }
	catch(ACSErr::ACSbaseExImpl &_ex)
	    {
	    throw acsErrTypeLifeCycle::StartingThreadsFailureExImpl(_ex, __FILE__,__LINE__,"ACSComponentImpl::__execute");
	    }
	}//if-else

    ACSComponentImpl::__execute();
}//__execute
    
void CharacteristicComponentImpl::__aboutToAbort()
{
  ACS_TRACE("baci::CharacteristicComponentImpl::__aboutToAbort");
    // Stop the threads with cancel that we avoid waiting for sleep - it is interrupted
    if (component_mp)
    {
    	component_mp->cancelActionThread();
        component_mp->cancelMonitoringThread();
    }

    ACSComponentImpl::__aboutToAbort();
}
    
void CharacteristicComponentImpl::__cleanUp()
{
	ACS_TRACE("baci::CharacteristicComponentImpl::__cleanUp");
	try
	{
		// we cancel the action thread in all cases because we started it
		if (component_mp!=NULL)
			component_mp->cancelActionThread();
		//component_mp->stopActionThread();

		// we cancel the monitoring thread in all cases because we started it
		if( monitoringProperties_mp == true )
		{
			// Cancel just threads that are used by properties
			// stopPropertiesMonitoring();
			component_mp->cancelMonitoringThread();
		}
	}
	catch(ACSErr::ACSbaseExImpl &ex)
	{
		//tbd: add CleanUp Error
		ex.log(LM_WARNING);
		// should we continue or throw an exception
	}

	ACSComponentImpl::__cleanUp();
}//__cleanUp

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
	ex.setVariable("component_mp (BACIComponent)");
	throw ex;
	}//if-else
}//startPropertiesMonitoring

void  CharacteristicComponentImpl::stopPropertiesMonitoring()
{
    if ( component_mp!=NULL)
	{
	component_mp->stopMonitoringThread();
	}//if
}//stopPropertiesMonitoring

bool CharacteristicComponentImpl::isPropertiesMonitoringActive()
{
    if ( component_mp!=NULL)
	{
	return component_mp->isMonitoringActive();
	}
    else
	{
	return false;
	}//if-else 
}//isPropertiesMonitoringActive

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
