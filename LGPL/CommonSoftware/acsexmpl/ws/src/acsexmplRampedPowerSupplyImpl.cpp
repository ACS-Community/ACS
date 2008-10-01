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
*
* "@(#) $Id: acsexmplRampedPowerSupplyImpl.cpp,v 1.109 2008/10/01 04:30:47 cparedes Exp $"
*
* who       when      what
* --------  --------- ----------------------------------------------
* acaproni 2004-04-06 Use of the smart pointer for properties
* david 2002-08-08-changed defines to const static int's
* msekoran 2002-02-15 New Completion applied.
* msekoran 2002-02-02 Fixed DO initialization
* almamgr 2002-01-22-Replaced old include files with new axsexmpl... files
* msekoran 2001-07-07 created
*/

#include "acsexmplRampedPowerSupplyImpl.h"

ACE_RCSID(acsexmpl, acsexmplRampedPowerSupplyImpl, "$Id: acsexmplRampedPowerSupplyImpl.cpp,v 1.109 2008/10/01 04:30:47 cparedes Exp $")
using namespace baci;

/////////////////////////////////////////////////
// Ramped PowerSupply
/////////////////////////////////////////////////

RampedPowerSupply::RampedPowerSupply(
				     const ACE_CString &name,
				     maci::ContainerServices * containerServices) :
    PowerSupply(name, containerServices),
    m_rampingStep_sp(this),
    m_rampingStep_devio_p(0),
    m_rampingStatus_sp(new RWstring(name+":rampingStatus", getComponent()),this)
{
    ACS_TRACE("::RampedPowerSupply::RampedPowerSupply");
    
    // RampedPowerSupply properties

    // Create an instance of the custom devIO before it's passed to the BACI property
    m_rampingStep_devio_p = new LongDevIO();

    // This is a little different from the rest of the BACI properties throughout acsexmpl 
    // in the fact a custom devIO is being passed to the constructor.
    m_rampingStep_sp = new ROlong(name+":rampingStep", getComponent(), m_rampingStep_devio_p);
}

RampedPowerSupply::~RampedPowerSupply()
{
    
    ACS_TRACE("::RampedPowerSupply::~RampedPowerSupply");

    // devIO
    if (m_rampingStep_devio_p != 0)
	{
	delete m_rampingStep_devio_p;
	m_rampingStep_devio_p=0;
	}

    ACS_DEBUG("::RampedPowerSupply::~RampedPowerSupply", "Properties destroyed");
}

/* --------------- [ Action implementator interface ] -------------- */
ActionRequest 
RampedPowerSupply::invokeAction (int function,
				 BACIComponent *cob_p, 
				 const int &callbackID, 
				 const CBDescIn &descIn, 
				 BACIValue *value_p, 
				 Completion &completion, 
				 CBDescOut &descOut) 
{
    
    // better implementation with array is possible
    switch (function) 
	{
	case ON_ACTION:
	  {
	  return onAction(cob_p, callbackID, descIn, value_p, completion, descOut);
	  }
	case OFF_ACTION:
	  {
	  return offAction(cob_p, callbackID, descIn, value_p, completion, descOut);
	  }
	case RESET_ACTION:
	  {
	  return resetAction(cob_p, callbackID, descIn, value_p, completion, descOut);
	  }
	case START_RAMPING:
	  {
	  return startRampingAction(cob_p, callbackID, descIn, value_p, completion, descOut);
	  }
	case STOP_RAMPING:
	  {
	  return stopRampingAction(cob_p, callbackID, descIn, value_p, completion, descOut);
	  }
	default:
	  {
	  return reqDestroy;
	  }
	}
}
/* ----------------------------------------------------------------*/
/* ------------------ [ Action implementations ] ----------------- */
/* ----------------------------------------------------------------*/
/// implementation of async. startRamping() method
ActionRequest 
RampedPowerSupply::startRampingAction (BACIComponent *cob_p, 
				       const int &callbackID,
				       const CBDescIn &descIn, 
				       BACIValue *value_p,
				       Completion &completion, 
				       CBDescOut &descOut)
{
    ACE_UNUSED_ARG(descOut);
    ACE_UNUSED_ARG(value_p);
    ACE_UNUSED_ARG(descIn);
    ACE_UNUSED_ARG(callbackID);
    ACE_UNUSED_ARG(cob_p);

    ACS_DEBUG_PARAM("::RampedPowerSupply::startRampingAction", "%s", getComponent()->getName());
   completion = ACSErrTypeOK::ACSErrOKCompletion();

    try
	{
    // simulate something in hardware...
	ACE_OS::sleep(5);
	ACS::Time timestamp;
	m_status_sp->getDevIO()->write(0x00000100 | m_status_sp->getDevIO()->read(timestamp), completion.timeStamp);
	}
    catch (ACSErr::ACSbaseExImpl &ex)
	{
	// it might be better to create here another more descriptive method but .... for show how to do is is not important
	ACSErrTypeCommon::IOErrorCompletion com(ex, __FILE__, __LINE__, "PowerSupply::offAction");
	completion  = com;
	}

    // complete action requesting done invokation, 
    // otherwise return reqInvokeWorking and set descOut.estimated_timeout
    return reqInvokeDone;
}

/// implementation of async. stopRamping() method
ActionRequest 
RampedPowerSupply::stopRampingAction (BACIComponent *cob_p, 
				      const int &callbackID,
				      const CBDescIn &descIn, 
				      BACIValue *value_p,
				      Completion &completion, 
				      CBDescOut &descOut)
{
    ACE_UNUSED_ARG(descOut);
    ACE_UNUSED_ARG(value_p);
    ACE_UNUSED_ARG(descIn);
    ACE_UNUSED_ARG(callbackID);
    ACE_UNUSED_ARG(cob_p);

    ACS_DEBUG_PARAM("::RampedPowerSupply::stopRampingAction", "%s", getComponent()->getName());
    
    // simulate something in hardware...

    completion = ACSErrTypeOK::ACSErrOKCompletion();

    try
	{
	ACS::Time timestamp;
	m_status_sp->getDevIO()->write(0x11111011 & m_status_sp->getDevIO()->read(timestamp), completion.timeStamp);
	}
    catch (ACSErr::ACSbaseExImpl &ex)
	{
	// it might be better to create here another more descriptive method but .... for show how to do is is not important
	ACSErrTypeCommon::IOErrorCompletion com(ex, __FILE__, __LINE__, "PowerSupply::offAction");
	completion  = com;
	}
    ACE_OS::sleep(5);
    
    // complete action requesting done invokation, 
    // otherwise return reqInvokeWorking and set descOut.estimated_timeout
    return reqInvokeDone;
}

/* --------------------- [ CORBA interface ] ----------------------*/

void
RampedPowerSupply::startRamping (CORBA::Long rampingSteps,
				 ACS::CBvoid_ptr cb,
				 const ACS::CBDescIn &desc)
{
    //registers the action to the BACI and returns control immediately
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, START_RAMPING);
}

void
RampedPowerSupply::stopRamping (ACS::CBvoid_ptr cb,
				const ACS::CBDescIn &desc)
{
    //registers the action to the BACI and returns control immediately
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, STOP_RAMPING);
}

ACS::RWstring_ptr
RampedPowerSupply::rampingStatus ()
{
    if (m_rampingStatus_sp == 0)
	{
	return ACS::RWstring::_nil();
	}
    
    ACS::RWstring_var prop = ACS::RWstring::_narrow(m_status_sp->getCORBAReference());
    return prop._retn();
}

ACS::ROlong_ptr
RampedPowerSupply::rampingStep ()
{
    if (m_rampingStep_sp == 0)
	{
	return ACS::ROlong::_nil();
	}
    
    ACS::ROlong_var prop = ACS::ROlong::_narrow(m_rampingStep_sp->getCORBAReference());
    return prop._retn();
}

/////////////////////////////////////////////////
// MACI DLL support functions
/////////////////////////////////////////////////
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(RampedPowerSupply)


