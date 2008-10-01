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
* "@(#) $Id: acsexmplLampImpl.cpp,v 1.101 2008/10/01 04:30:47 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni 2004-04-06 Use of smart pointer for properties
* david 2002-08-08 changed defines to const static int's
* david 2002-06-16 added a few comments and deleted RESET_ACTION define
* almamgr 2002-04-07 Removed poa parameter from call to ConstructorEpilogue()
* blopez   2002-04-05 Modified for ACSDO usage. Header removed.
* almamgr 2002-01-22 Replaced old include files with new axsexmpl... files
* msekoran 2001-07-06 improved error handling
* msekoran 2001-03-10 integrated with new BACI; ALMA coding convention used
* gchiozzi 2001-02-15 Added real implementation for method descriptor()
* gchiozzi 2001-02-15 Added body of get_interface() method for Object Explorer
* gchiozzi 2001-02-15 created standard header 
*/


#include <baciDB.h>
#include <acsexmplLampImpl.h>

/**
 * One of these function IDs will be passed to invokeAction().
 */
const static int ON_ACTION  = 0;
const static int OFF_ACTION = 1;

ACE_RCSID(acsexmpl, acsexmplLampImpl, "$Id: acsexmplLampImpl.cpp,v 1.101 2008/10/01 04:30:47 cparedes Exp $")
using namespace baci;

/////////////////////////////////////////////////
// Lamp
/////////////////////////////////////////////////

Lamp::Lamp( 
	   const ACE_CString &name,
	   maci::ContainerServices * containerServices) :
    CharacteristicComponentImpl(name, containerServices),
    m_brightness_sp(new RWdouble(name+":brightness", getComponent()),this)
{
    
    ACS_TRACE("::Lamp::Lamp");
        
}

Lamp::~Lamp()
{
    
    ACS_TRACE("::Lamp::~Lamp");
    
}

/* --------------- [ Action implementator interface ] -------------- */

ActionRequest 
Lamp::invokeAction (int function,
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
	default:
	  {
	  return reqDestroy;
	  }
	}
}

/* ------------------ [ Action implementations ] ----------------- */

/// implementation of async. on() method
ActionRequest 
Lamp::onAction (BACIComponent *cob_p, 
		const int &callbackID,
		const CBDescIn &descIn, 
		BACIValue *value_p,
		Completion &completion, 
		CBDescOut &descOut)
{
    ACS_DEBUG_PARAM("::Lamp::onAction", "%s", getComponent()->getName());
    
    DBConnector::writeCommand(getComponent()->getName(), "on", getStringifiedTimeStamp());
    
    completion = ACSErrTypeOK::ACSErrOKCompletion();
    
    // complete action requesting done invocation, 
    // otherwise return reqInvokeWorking and set descOut.estimated_timeout
    return reqInvokeDone;
}



/// implementation of async. off() method
ActionRequest 
Lamp::offAction (BACIComponent *cob_p, 
		 const int &callbackID,
		 const CBDescIn &descIn, 
		 BACIValue *value_p,
		 Completion &completion, 
		 CBDescOut &descOut)
{
    ACS_DEBUG_PARAM("::Lamp::offAction", "%s", getComponent()->getName());
    
    DBConnector::writeCommand(getComponent()->getName(), "off", getStringifiedTimeStamp());
    
    completion = ACSErrTypeOK::ACSErrOKCompletion();
    
    // complete action requesting done invakation, 
    // otherwise return reqInvokeWorking and set descOut.estimated_timeout
    return reqInvokeDone;
}

/* --------------------- [ CORBA interface ] ----------------------*/

void
Lamp::on (ACS::CBvoid_ptr cb,
	  const ACS::CBDescIn &desc
	  )
{
    //registers the action to the BACI and returns control immediately
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, ON_ACTION);
}

void
Lamp::off (ACS::CBvoid_ptr cb,
	   const ACS::CBDescIn &desc
	   )
{
    //registers the action to the BACI and returns control immediately
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, OFF_ACTION);
}

ACS::RWdouble_ptr
Lamp::brightness ()
{
    if (m_brightness_sp == 0)
	{
	return ACS::RWdouble::_nil();
	}

    ACS::RWdouble_var prop = ACS::RWdouble::_narrow(m_brightness_sp->getCORBAReference());
    return prop._retn();
}


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(Lamp)
/* ----------------------------------------------------------------*/

