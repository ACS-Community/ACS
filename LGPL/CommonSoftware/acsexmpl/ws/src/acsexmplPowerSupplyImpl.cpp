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
* "@(#) $Id: acsexmplPowerSupplyImpl.cpp,v 1.115 2008/10/09 08:41:11 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni 2004-04-06 Use of smart pointer for properties
* david 2002-08-08 changed defines to const static int's
* almamgr 2002-04-07 Removed poa parameter from call to ConstructorEpilogue()
* blopez   2002-04-05 Modified for ACSDO usage. Header removed.
* msekoran 2002-02-15 New Completion applied.
* msekoran 2002-02-02 Fixed DO initialization
* almamgr 2002-01-22 Replaced old include files with new axsexmpl... files
* msekoran 2001-07-06 improved error handling
* msekoran 2001-03-10 integrated with new BACI; ALMA coding convention used
* gchiozzi 2001-02-15 Added real implementation for method descriptor()
* gchiozzi 2001-02-15 Added body of get_interface() method for Object Explorer
* gchiozzi 2001-02-15 created standard header 
*/


#include <acsexmplPowerSupplyImpl.h>

ACE_RCSID(acsexmpl, acsexmplPowerSupplyImpl, "$Id: acsexmplPowerSupplyImpl.cpp,v 1.115 2008/10/09 08:41:11 cparedes Exp $")

using namespace baci;

/////////////////////////////////////////////////
// PowerSupply
/////////////////////////////////////////////////

PowerSupply::PowerSupply( 
			 const ACE_CString &name,
			 maci::ContainerServices * containerServices) :
    CharacteristicComponentImpl(name,containerServices),
    m_status_sp(new ROpattern(name+":status", getComponent()),this),
    m_readback_sp(new ROdouble(name+":readback", getComponent()),this),
    m_current_sp(this)
{
    ACS_TRACE("::PowerSupply::PowerSupply"); 
    
    // Properties are created in two ways:
    // 1. passing to the constructor the Characteristic Component that owns the 
    //    property, the pointer to the porperty and the idl accessor function 
    // 2. passing to the constructor the Characteristic Component that owns the  
    //    property and the calling the init method of the smart pointer with
    //    the pointer to the property and the idl accessor as parameters
    // The property's name must be composed of the server's name and the
    //   property name.
    m_current_sp=new PowerSupplyCurrent(name+":current", getComponent(), m_readback_sp);
}

void
PowerSupply::execute()
{
    ACS_SHORT_LOG((LM_INFO,"PowerSupply::execute"));
    
    // The PowerSupply is initially turned off...
    ACS::Time timestamp;
    m_status_sp->getDevIO()->write(0x00000000, timestamp);
   
    ACS_SHORT_LOG((LM_INFO,"PowerSupply::COMPSTATE_OPERATIONAL"));
}


PowerSupply::~PowerSupply()
{
    ACS_TRACE("::PowerSupply::~PowerSupply");

    
    ACS_DEBUG("::PowerSupply::~PowerSupply", "Properties destroyed");
}

/* --------------- [ Action implementator interface ] -------------- */

ActionRequest 
PowerSupply::invokeAction (int function,
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
	default:
	{
	return reqDestroy;
	}
	}
}

/* ------------------ [ Action implementations ] ----------------- */

/// implementation of async. on() method
ActionRequest 
PowerSupply::onAction (BACIComponent *cob_p, 
		       const int &callbackID,
		       const CBDescIn &descIn, 
		       BACIValue *value_p,
		       Completion &completion, 
		       CBDescOut &descOut)
{
    ACS_DEBUG_PARAM("::PowerSupply::onAction", "%s", getComponent()->getName());
    
    // simulate something in hardware...
    ACE_OS::sleep(5);
    // since this method is only simulated, we just change it's state  
    completion = ACSErrTypeOK::ACSErrOKCompletion();
    try
	{
	ACS::Time timestamp;
	m_status_sp->getDevIO()->write(0x00000001 | m_status_sp->getDevIO()->read(timestamp), completion.timeStamp);
	}
    catch (ACSErr::ACSbaseExImpl &ex)
	{
	// it might be better to create here another more descriptive method but .... for show how to do is is not important
	ACSErrTypeCommon::IOErrorCompletion com(ex, __FILE__, __LINE__, "PowerSupply::onAction");
	completion  = com;
	}
    // complete action requesting done invakation,
    // otherwise return reqInvokeWorking and set descOut.estimated_timeout
    return reqInvokeDone;
}

/// implementation of async. off() method
ActionRequest 
PowerSupply::offAction (BACIComponent *cob_p, 
			const int &callbackID,
			const CBDescIn &descIn, 
			BACIValue *value_p,
			Completion &completion, 
			CBDescOut &descOut)
{
    ACS_DEBUG_PARAM("::PowerSupply::offAction", "%s", getComponent()->getName());
        
    // since this method is only simulated, we just change it's state  
    completion = ACSErrTypeOK::ACSErrOKCompletion();

    try
	{
	ACS::Time timestamp;
	m_status_sp->getDevIO()->write(0x11111110 & m_status_sp->getDevIO()->read(timestamp), completion.timeStamp);
	
	}
    catch (ACSErr::ACSbaseExImpl &ex)
	{
	// it might be better to create here another more descriptive method but .... for show how to do is is not important
	ACSErrTypeCommon::IOErrorCompletion com(ex, __FILE__, __LINE__, "PowerSupply::offAction");
	completion  = com;
	}
    // simulate something in hardware...
    ACE_OS::sleep(5);
    
    // complete action requesting done invakation, 
    // otherwise return reqInvokeWorking and set descOut.estimated_timeout
    return reqInvokeDone;
}

/// implementation of async. reset() method
ActionRequest 
PowerSupply::resetAction (BACIComponent *cob_p, 
			  const int &callbackID,
			  const CBDescIn &descIn, 
			  BACIValue *value_p,
			  Completion &completion, 
			  CBDescOut &descOut)
{
    ACS_DEBUG_PARAM("::PowerSupply::resetAction", "%s", getComponent()->getName());
    
    completion = ACSErrTypeOK::ACSErrOKCompletion();

    // simulate the PS turning off...
    try
	{
	m_status_sp->getDevIO()->write(static_cast<unsigned long>(0x11111110) & m_status_sp->getDevIO()->read(completion.timeStamp), completion.timeStamp);
	ACE_OS::sleep(5);
	// now turn it back on

	m_status_sp->getDevIO()->write(static_cast<unsigned long>(0x00000001) | m_status_sp->getDevIO()->read(completion.timeStamp), completion.timeStamp);
	}
    catch (ACSErr::ACSbaseExImpl &ex)
	{
	// it might be better to create here another more descriptive method but .... for show how to do is is not important
	ACSErrTypeCommon::IOErrorCompletion com(ex, __FILE__, __LINE__, "PowerSupply::offAction");
	completion  = com;
	}

 
    ACE_OS::sleep(5);

    // complete action requesting done invocation, 
    // otherwise return reqInvokeWorking and set descOut.estimated_timeout
    return reqInvokeDone;
}

/* --------------------- [ CORBA interface ] ----------------------*/
 
void
PowerSupply::on (ACS::CBvoid_ptr cb,
		 const ACS::CBDescIn &desc
		 )
{
    //registers the action to the BACI and returns control immediately
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, ON_ACTION);
}

void
PowerSupply::off (ACS::CBvoid_ptr cb,
		  const ACS::CBDescIn &desc
		  )
{
    //registers the action to the BACI and returns control immediately
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, OFF_ACTION);
}

void
PowerSupply::reset (ACS::CBvoid_ptr cb,
		    const ACS::CBDescIn &desc
		    )
{
    //registers the action to the BACI and returns control immediately
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, RESET_ACTION);
}

ACS::RWdouble_ptr
PowerSupply::current ()
{
    if (m_current_sp == 0)
	{
	return ACS::RWdouble::_nil();
	}
    
    ACS::RWdouble_var prop = ACS::RWdouble::_narrow(m_current_sp->getCORBAReference());
    return prop._retn();
}

ACS::ROdouble_ptr
PowerSupply::readback ()
{
    if (m_readback_sp == 0)
	{
	return ACS::ROdouble::_nil();
	}
    
    ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_readback_sp->getCORBAReference());
    return prop._retn();
}


ACS::ROpattern_ptr
PowerSupply::status ()
{
    if (m_status_sp == 0)
	{
	return ACS::ROpattern::_nil();
	}
    
    ACS::ROpattern_var prop = ACS::ROpattern::_narrow(m_status_sp->getCORBAReference());
    return prop._retn();
}


