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
* "@(#) $Id: acsexmplMountImpl.cpp,v 1.111 2008/10/01 04:30:47 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni 2004-04-06 Use of smart pointer for properties
* bjeram 2002-06-25 added  const ACS::CBDescIn & desc
* almamgr 2002-04-07 Removed poa parameter from call to ConstructorEpilogue()
* blopez   2002-04-05 Modified for ACSDO usage. Header removed.
* gchiozzi 2002-04-04 Replaced set_sync() with getDevIO()->write<T>()
* bgustafs 2002-03-04 set properties with set_sync
* msekoran 2002-02-15 New Completion applied.
* msekoran 2002-02-06 Fixed DO initialization
* almamgr 2002-01-22 Replaced old include files with new axsexmpl... files
* msekoran 2001-07-06 improved error handling
* msekoran 2001-06-23 minor changes to work with acsutil module
* msekoran 2001-04-03 new baci
* gchiozzi 2001-02-15 Added real implementation for method descriptor()
* gchiozzi 2001-02-15 Added body of get_interface() method for Object Explorer
* gchiozzi 2001-02-15 created standard header 
*/


#include <acsexmplMountImpl.h>
#include <baciDB.h>

ACE_RCSID(acsexmpl, acsexmplMountImpl, "$Id: acsexmplMountImpl.cpp,v 1.111 2008/10/01 04:30:47 cparedes Exp $")
using namespace baci;

/**
 * This structure and also __objfix_action are used in conjunction with the ActionImplementor 
 * class.  ActionImplementor uses a BACIValue (basically a void pointer) parameter for the 
 * given parameters to any asynchronous method.  This struct will be typecast to a BACIValue 
 * so that it can be used with invokeAction(...).
 */
struct __obstar_action {
    double ra;
    double dec;
    double pmRa;
    double pmDec;
    double radVel;
    double par;
    MOUNT_ACS::Mount::coordType type;
};

struct __objfix_action {
    double az;
    double elev;
};

Mount::Mount( 
	     ACE_CString _name,
	     maci::ContainerServices * containerServices) : 
    CharacteristicComponentImpl(_name, containerServices),
    m_cmdAz_sp(new ROdouble(_name+":cmdAz", getComponent()),this),
    m_cmdEl_sp(new ROdouble(_name+":cmdEl", getComponent()),this),
    m_actAz_sp(new ROdouble(_name+":actAz", getComponent()),this),
    m_actEl_sp(new ROdouble(_name+":actEl", getComponent()),this)
{
    ACS_TRACE("::Mount::Mount");

    
     
    // register actions for use with invokeAction(...)
    m_actions[0] = &Mount::obstarAction;
    m_actions[1] = &Mount::objfixAction;    
}

Mount::~Mount()
{
}

/* --------------- [ Action implementator interface ] -------------- */

ActionRequest 
Mount::invokeAction (int function,
		     BACIComponent *cob_p, 
		     const int &callbackID, 
		     const CBDescIn &descIn, 
		     BACIValue *value_p, 
		     Completion &completion, 
		     CBDescOut &descOut) 
{
    if (function < 2)
	{
	// call the asynchronous method
	return (this->*m_actions[function])(cob_p, callbackID, descIn, value_p, completion, descOut);
	}
    else
	{
	return reqDestroy;
	}
}

/* ------------------ [ Action implementations ] ----------------- */

/// implementation of async. obstar() method
ActionRequest 
Mount::obstarAction (BACIComponent *cob_p, 
		     const int &callbackID,
		     const CBDescIn &descIn, 
		     BACIValue *value_p,
		     Completion &completion, 
		     CBDescOut &descOut)
{
    ACE_UNUSED_ARG(cob_p);
    ACE_UNUSED_ARG(callbackID);
    ACE_UNUSED_ARG(descIn);
    ACE_UNUSED_ARG(descOut);
    
#ifdef debugMount
    ACS_SHORT_LOG((LM_DEBUG, "(Mount::obstarAction) %s", getComponent()->getName()));
#endif
    
    // convert the methods parameters back into something we can use
    __obstar_action *param_p = static_cast<__obstar_action *>(const_cast<void *>(value_p->pointerValue()));
    
    ACS::Time timestamp;
    
    // simulate changing the antenna's commanded and actual position
    try
	{
	m_cmdAz_sp->getDevIO()->write(param_p->ra, timestamp);
	m_actAz_sp->getDevIO()->write(param_p->ra, timestamp);
	m_cmdEl_sp->getDevIO()->write(param_p->dec, timestamp);
	m_actEl_sp->getDevIO()->write(param_p->dec, timestamp);
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	// here some other erro should be defined and used
	completion = ACSErrTypeCommon::IOErrorCompletion(ex, __FILE__, __LINE__, "Mount::obstarAction");
	return reqInvokeDone;
	}
    
#ifdef debugMount
    ACS_SHORT_LOG((LM_DEBUG, "(Mount::obstarAction) command: %s %s %s",
	       getComponent()->getName(), "obstar", getStringifiedTimeStamp().c_str()));
#endif
    
    DBConnector::writeCommand(getComponent()->getName(), "obstar", getStringifiedTimeStamp());
    
#ifdef debugMount
    ACS_SHORT_LOG((LM_DEBUG, "(Mount::obstarAction) completion"));
#endif
    
    completion = ACSErrTypeOK::ACSErrOKCompletion();

    // if OK action will be destroyed and we do not need it anymore
    if (param_p!=0) 
	{
	delete param_p;
	}

#ifdef debugMount
    ACS_SHORT_LOG((LM_DEBUG, "(Mount::obstarAction) END"));
#endif
    
    
    // complete action requesting done invokation, 
    // otherwise return reqInvokeWorking and set descOut.estimated_timeout
    return reqInvokeDone;
}


/// implementation of async. objfix() method
ActionRequest 
Mount::objfixAction (BACIComponent *cob_p, 
		     const int &callbackID,
		     const CBDescIn &descIn, 
		     BACIValue *value_p,
		     Completion &completion, 
		     CBDescOut &descOut)
{
    
    ACE_UNUSED_ARG(cob_p);
    ACE_UNUSED_ARG(callbackID);
    ACE_UNUSED_ARG(descIn);
    ACE_UNUSED_ARG(descOut);
    
#ifdef debugMount
    ACS_SHORT_LOG((LM_DEBUG, "(Mount::objfixAction) %s", getComponent()->getName()));
#endif
    
    // convert the methods parameters back into something we can use
    __objfix_action *param_p = static_cast<__objfix_action *>(const_cast<void *>(value_p->pointerValue()));
    
    ACS::Time timestamp;

    // simulate changing the antenna's commanded and actual position
    try
	{
	m_cmdAz_sp->getDevIO()->write(param_p->az, timestamp);
	m_actAz_sp->getDevIO()->write(param_p->az, timestamp);
	m_cmdEl_sp->getDevIO()->write(param_p->elev, timestamp);
	m_actEl_sp->getDevIO()->write(param_p->elev, timestamp);
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	// here some other erro should be defined and used
	completion = ACSErrTypeCommon::IOErrorCompletion(ex, __FILE__, __LINE__, "Mount::objfixAction");
	return reqInvokeDone;
	}
 
    DBConnector::writeCommand(getComponent()->getName(), "objfix", getStringifiedTimeStamp());

    completion = ACSErrTypeOK::ACSErrOKCompletion();
    
    // if OK action will be destroyed and we do not need it anymore
    if (param_p!=0) 
	{
	delete param_p;
	}
    
    // complete action requesting done invokation, 
    // otherwise return reqInvokeWorking and set descOut.estimated_timeout
    return reqInvokeDone;
}


/* --------------------- [ CORBA interface ] ----------------------*/
ACS::ROdouble_ptr
Mount::cmdAz ()
{
    if (m_cmdAz_sp == 0)
	{
	return ACS::ROdouble::_nil();
	}

    ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_cmdAz_sp->getCORBAReference());
    return prop._retn();
}

ACS::ROdouble_ptr
Mount::cmdEl ()
{
    if (m_cmdEl_sp == 0)
	{
	return ACS::ROdouble::_nil();
	}

    ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_cmdEl_sp->getCORBAReference());
    return prop._retn();
}

ACS::ROdouble_ptr
Mount::actAz ()
{
    if (m_actAz_sp == 0)
	{
	return ACS::ROdouble::_nil();
	}

    ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_actAz_sp->getCORBAReference());
    return prop._retn();
}

ACS::ROdouble_ptr
Mount::actEl ()
{
    if (m_actEl_sp == 0)
	{
	return ACS::ROdouble::_nil();
	}
    
    ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_actEl_sp->getCORBAReference());
    return prop._retn();
}

void
Mount::obstar (CORBA::Double ra,
	       CORBA::Double dec,
	       CORBA::Double pmRa,
	       CORBA::Double pmDec,
	       CORBA::Double radVel,
	       CORBA::Double par,
	       MOUNT_ACS::Mount::coordType type,
	       ACS::CBvoid_ptr callBack,
	       const ACS::CBDescIn &desc
	       )
{
    // convert this method's parameters into something ActionImplementor can use
    __obstar_action *param_p = new __obstar_action();
    param_p->ra=ra; 
    param_p->dec=dec; 
    param_p->pmRa=pmRa; 
    param_p->pmDec=pmDec;
    param_p->radVel=radVel; 
    param_p->par=par; 
    param_p->type=type;    
    
    // register the action in a queue so that control is returned immediately
    getComponent()->registerAction(BACIValue::type_null, callBack, desc, this, 0, BACIValue(param_p));    // ID = 0
}

void
Mount::objfix (CORBA::Double az,
	       CORBA::Double elev,
	       ACS::CBvoid_ptr callBack,
	       const ACS::CBDescIn &desc
	       )
{
    // convert this method's parameters into something ActionImplementor can use
    __objfix_action *param_p = new __objfix_action();
    param_p->az=az; 
    param_p->elev=elev;
    
    // register the action in a queue so that control is returned immediately
    getComponent()->registerAction(BACIValue::type_null, callBack, desc, this, 1, BACIValue(param_p));    // ID = 1
}


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(Mount)
/* ----------------------------------------------------------------*/





