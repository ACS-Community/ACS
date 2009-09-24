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
* "@(#) $Id: acsexmplFridgeImpl.cpp,v 1.134 2009/09/24 23:08:03 javarias Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni 2004-04-06 Use of smart pointer for properties
* bjeram 2003-04-10 added FRIDGE::OnOffStates state = ... DevIO::write(state, ...) since write takes reference
* david 2002-08-08 changes defines to const static int's
* bgustafs 2002-04-11 Modified for VxWorks
* gchiozzi 2002-04-04 Replaced set_sync() with getDevIO()->write<T>()
* gchiozzi 2002-03-18 Replaced includes of fridge*.* with acsexmplFridge*.*
* msekoran 2001-07-06 improved error handling
* msekoran 2001-03-10 integrated with new BACI; ALMA coding convention used
* gchiozzi 2001-02-15 Added real implementation for method descriptor()
* gchiozzi 2001-02-15 Added body of get_interface() method for Object Explorer
* gchiozzi 2001-02-15 created standard header 
*/


#include <acsexmplFridgeImpl.h>
#include <math.h>

ACE_RCSID(acsexmpl, acsexmplFridgeImpl, "$Id: acsexmplFridgeImpl.cpp,v 1.134 2009/09/24 23:08:03 javarias Exp $")
using namespace baci;

/**
 * One of these function IDs will be passed to invokeAction().
 */
const static int ON_ACTION    = 0;
const static int OFF_ACTION   = 1;
const static int OPEN_ACTION  = 2;
const static int CLOSE_ACTION = 3;

/**
 * This global variable is only used to limiting LM_INFO logging
 * to standard out.  This is done to make the modular tests involving
 * the fridge component more deterministic.
 */
int LOCALCOUNT = 0;

/**
 *
 */
ACE_Log_Priority LOCAL_LOGGING_LEVEL = LM_INFO;


/*	*
 * After the thread's run loop has exceeded MAX_LOGS, the logging
 * level goes down to LM_DEBUG.
 */
const static int MAX_LOGS = 5;


/////////////////////////////////////////////////
// FridgeThread
/////////////////////////////////////////////////

void
FridgeThread::runLoop()
{
	//Used to change the logging level to make acsexmpl's modular
	//tests more deterministic.
	if(LOCALCOUNT>MAX_LOGS)
	{
		LOCAL_LOGGING_LEVEL = LM_DEBUG;
	}
	else
	{
		LOCALCOUNT++;
	}
	ACS_SHORT_LOG((LOCAL_LOGGING_LEVEL, "Updating fridge temperature"));
	fridgeControl_p->updateTemperature();
}

/////////////////////////////////////////////////
// Fridge
/////////////////////////////////////////////////


/* ----------------------------------------------------------------*/
/* ----------------------------------------------------------------*/
/* ----------------------------------------------------------------*/
FridgeControl::FridgeControl(
			     const ACE_CString &name,
			     maci::ContainerServices * containerServices) :
    CharacteristicComponentImpl(name, containerServices),
    m_refTemperature_sp(new RWdouble(name+":refTemperature", getComponent()),this),
    m_powerStatus_sp(new ROEnumImpl<ACS_ENUM_T(FRIDGE::OnOffStates), 
		     POA_FRIDGE::ROOnOffStates> (name+":powerStatus", getComponent()),this),
    m_doorStatus_sp(new ROEnumImpl<ACS_ENUM_T(FRIDGE::OpClStates), 
		    POA_FRIDGE::ROOpClStates> (name+":doorStatus", getComponent()),this),
    m_currTemperature_sp(new ROdouble(name+":currTemperature", getComponent()),this),
    m_controlLoop_p(0),
    m_FridgeSupplier_p(0)
{
    ACS_TRACE("::FridgeControl::FridgeControl");

    // Handle notification channel creation here.
    m_FridgeSupplier_p = new nc::SimpleSupplier(FRIDGE::CHANNELNAME_FRIDGE, this);
}

FridgeControl::~FridgeControl()
{
    
    ACS_TRACE("::FridgeControl::~FridgeControl");
    ACS_DEBUG_PARAM("::FridgeControl::~FridgeControl", "Destroying %s...", getComponent()->getName());
    
    // destroy the thread via ThreadManager
    if(m_controlLoop_p!=0)
	getContainerServices()->getThreadManager()->destroy(m_controlLoop_p);

    // cleanUp will have been already called by
    // the container and threads will have been already stopped.
}

void FridgeControl::cleanUp()
{
    ACS_TRACE("::FridgeControl::cleanUp");
    
    //turn it off before destroying it
    ACSErr::Completion_var completion;
    if((m_controlLoop_p!=0) && (powerStatus()->get_sync(completion.out())!=FRIDGE::OFF))
	{
	off();
	}
    
    // Here we have to stop all threads
    getContainerServices()->getThreadManager()->stopAll(); 


    // In the past this was done in the CharacteristicComponentImpl::cleanUp();    
    // As required by the CharacteristicComponentImpl class,
    // I call explicitly the cleanUp() of the parent class.
    // This makes sure that all threads are stopped and the 
    // Component's state set.
    // Depending on what resources are used by a class implementing a 
    // Component and by the implementation of the parent class (if it does 
    // not inherit directly from acscomponent::ACSComponentImpl 
    // or baci:: CharacteristicComponentImpl) it might be
    // necessary to call the cleanuUp() method of the base class 
    // AFTER having released resources allocated by  the current class.
    // This is demonstrated in this example where we want to make sure
    // that the powerStatus is FRIDGE::OFF before letting the
    // cleanUp() of the base class stopping all the 
    // threads, including the one that regulates the temperature.
    // For an example where the cleanUp() of the parent class
    // is called before any other step, see the  Building class.
    // Always check the documentation of the parent class
    // and consider what resources are allocated by this class
    // to extablish the requirements for the execution of lifecycle
    // chained methods.

    // clean-up associated with NC
    if (m_FridgeSupplier_p != 0)
	{
	m_FridgeSupplier_p->disconnect();
	m_FridgeSupplier_p=0;
	}
}
void 
FridgeControl::updateTemperature()
{
    double temperature;
    ACS::Time timestamp;
    
    
    ACS_TRACE("::FridgeControl::updateTemperature");
    
    ACSErr::Completion_var completion;
    double currRefTemperature;
    
    currRefTemperature = m_refTemperature_sp->get_sync(completion.out());
    temperature = m_currTemperature_sp->get_sync(completion.out());

    // simulate the fridge trying to reach the reference temperature.
    ACS_SHORT_LOG((LOCAL_LOGGING_LEVEL, "::FridgeControl::updateTemperature. Curr value: %f. Ref: %f", temperature, currRefTemperature));
    if(temperature < currRefTemperature)
	{
	temperature += 0.1;
	}
    else if(temperature > currRefTemperature)
	{
	temperature -= 0.1;
	}
 
    m_currTemperature_sp->getDevIO()->write(temperature, timestamp);

    // push data onto the NC
    this->loadData();
    
    ACS_SHORT_LOG((LOCAL_LOGGING_LEVEL, "::FridgeControl::updateTemperature. New value: %f", temperature));
    
}

/* --------------------- [ FRIDGE interface ] ---------------------*/
void
FridgeControl::on ()
{
	ACS::Time timestamp;

	ACS_DEBUG_PARAM("::FridgeControl::on", "%s", getComponent()->getName());
	m_powerStatus_sp->getDevIO()->write(FRIDGE::ON, timestamp);

	// if the fridge has never been turned on before, we start the thread.  otherwise the 
	// fridge goes from off to on.
	if(m_controlLoop_p == 0)
	{
		FridgeControl * selfPtr = this;
		m_controlLoop_p = getContainerServices()->getThreadManager()->
			create<FridgeThread, FridgeControl*>("fridgeControlLoop", selfPtr);
                m_controlLoop_p->resume();
	}
	else
	{
		m_controlLoop_p->resume();
	}
}

void
FridgeControl::off ()
{
    ACS::Time timestamp;

    ACS_DEBUG_PARAM("::FridgeControl::off", "%s", getComponent()->getName());
    // if the fridge has been turned on before, it is turned off.  otherwise there
    // is no need to do anything.
    m_powerStatus_sp->getDevIO()->write(FRIDGE::OFF, timestamp);
    if(m_controlLoop_p != 0)
	{
	m_controlLoop_p->suspend();
	}
}

void
FridgeControl::open ()
{
    ACS::Time timestamp;
    
    ACS_DEBUG_PARAM("::FridgeControl::open", "%s", getComponent()->getName());
    // there is no actual door to open here: only the enum is changed
    m_doorStatus_sp->getDevIO()->write(FRIDGE::OPEN, timestamp);
}

void
FridgeControl::close ()
{
    ACS::Time timestamp;
    
    ACS_DEBUG_PARAM("::FridgeControl::close", "%s", getComponent()->getName());
    // there is no actual door to close here: only the enum is changed
    m_doorStatus_sp->getDevIO()->write( FRIDGE::CLOSE, timestamp);
}

ACS::RWdouble_ptr
FridgeControl::refTemperature ()
{
    if (m_refTemperature_sp == 0)
	{
	return ACS::RWdouble::_nil();
	}
    
    ACS::RWdouble_var prop = ACS::RWdouble::_narrow(m_refTemperature_sp->getCORBAReference());
    return prop._retn();
}

FRIDGE::ROOnOffStates_ptr
FridgeControl::powerStatus ()
{
    if (m_powerStatus_sp == 0)
	{
	return FRIDGE::ROOnOffStates::_nil();
	}
    
    FRIDGE::ROOnOffStates_var prop = FRIDGE::ROOnOffStates::_narrow(m_powerStatus_sp->getCORBAReference());
    return prop._retn();
}

FRIDGE::ROOpClStates_ptr
FridgeControl::doorStatus ()
{
    if (m_doorStatus_sp == 0)
	{
	return FRIDGE::ROOpClStates::_nil();
	}    
    
    FRIDGE::ROOpClStates_var prop = FRIDGE::ROOpClStates::_narrow(m_doorStatus_sp->getCORBAReference());
    return prop._retn();
}
 
ACS::ROdouble_ptr
FridgeControl::currTemperature ()
{
    if (m_currTemperature_sp == 0)
	{
	return ACS::ROdouble::_nil();
	}
    
    ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_currTemperature_sp->getCORBAReference());
    return prop._retn();
}
/* --------------------- [ Notification Channel ] -----------------*/

/* optional */
/* Implement event processing callback */
class FridgeEventsCallback :public nc::SimpleSupplier::EventProcessingCallback
									 <FRIDGE::temperatureDataBlockEvent>
{
	public:
		void eventDropped(FRIDGE::temperatureDataBlockEvent event)
		{
			ACS_SHORT_LOG((LM_WARNING, "Event Dropped"))
		}
		void eventSent(FRIDGE::temperatureDataBlockEvent event)
		{
			ACS_SHORT_LOG((LOCAL_LOGGING_LEVEL, "Event sent succesfully"))
		}
		void eventStoredInQueue(FRIDGE::temperatureDataBlockEvent event)
		{
			ACS_SHORT_LOG((LOCAL_LOGGING_LEVEL, "Notification Channel is down"))
		}
};

void 
FridgeControl::loadData()
{
    ACS_TRACE("::FridgeControl::loadData()");
	 
	 FridgeEventsCallback callback;

    ACSErr::Completion_var completion;
    double current=0.0;
    double reference=0.0;
    current   = currTemperature()->get_sync(completion.out() );
    reference = refTemperature()->get_sync(completion.out()  );

    // put the temperature info into a structure that will be pushed onto the NC
    FRIDGE::temperatureDataBlockEvent data;
    data.absoluteDiff = fabs(current - reference);
    if(current > reference)
	{
	data.status = FRIDGE::OVERREF;
	}
    else if(current < reference)
	{
	data.status = FRIDGE::BELOWREF;
	}
    else
	{
	data.status = FRIDGE::ATREF;
	}

    // push the data - this eventually calls setData
    if(m_FridgeSupplier_p)
	{
	//m_FridgeSupplier_p->publishData<FRIDGE::temperatureDataBlockEvent>(data);
		m_FridgeSupplier_p->publishData<FRIDGE::temperatureDataBlockEvent>
			(data, &callback);
	}
}


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(FridgeControl)
/* ----------------------------------------------------------------*/



