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
* "@(#) $Id: acsexmplSlowMountImpl.cpp,v 1.13 2008/10/01 04:30:47 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2004--8-16 Created from acsexmplMountImpl.cpp
*/

/**
 * The component implements the Mount IDL interface but simulates
 * the movements of the antenna in a 30 secs time interval
 */

#include <acsexmplSlowMountImpl.h>
#include <baciDB.h>
#include <iostream>
#include <time.h>
#include <math.h>

ACE_RCSID(acsexmpl, acsexmplSlowMountImpl, "$Id: acsexmplSlowMountImpl.cpp,v 1.13 2008/10/01 04:30:47 cparedes Exp $")
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

SlowMount::SlowMount( 
		     ACE_CString _name,
		     maci::ContainerServices * containerServices) : 
    CharacteristicComponentImpl(_name, containerServices),
    m_cmdAz_sp(new ROdouble(_name+":cmdAz", getComponent()),this),
    m_cmdEl_sp(new ROdouble(_name+":cmdEl", getComponent()),this),
    m_actAz_sp(new ROdouble(_name+":actAz", getComponent()),this),
    m_actEl_sp(new ROdouble(_name+":actEl", getComponent()),this)
{
    ACS_TRACE("::SlowMount::SlowMount");
    
    // register actions for use with invokeAction(...)
    m_actions[0] = &SlowMount::obstarAction;
    m_actions[1] = &SlowMount::objfixAction;
}

SlowMount::~SlowMount()
{
	ACS_TRACE("::SlowMount::~SlowMount");
}

/* --------------- [ Action implementator interface ] -------------- */

ActionRequest 
SlowMount::invokeAction (int function,
		     BACIComponent *cob_p, 
		     const int &callbackID, 
		     const CBDescIn &descIn, 
		     BACIValue *value_p, 
		     Completion &completion, 
		     CBDescOut &descOut) 
{
	ACS_TRACE("SlowMount::invokeAction");
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
SlowMount::obstarAction (BACIComponent *cob_p, 
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
    
    ACS_TRACE("SlowMount::obstarAction");
    
    // convert the methods parameters back into something we can use
    __obstar_action *param_p = static_cast<__obstar_action *>(const_cast<void *>(value_p->pointerValue()));
    
    ACS::Time timestamp;
    
    // simulate changing the antenna's commanded and actual position
    m_cmdAz_sp->getDevIO()->write(param_p->ra, timestamp);
    m_actAz_sp->getDevIO()->write(param_p->ra, timestamp);
    m_cmdEl_sp->getDevIO()->write(param_p->dec, timestamp);
    m_actEl_sp->getDevIO()->write(param_p->dec, timestamp);
    
#ifdef debugMount
    ACS_SHORT_LOG((LM_DEBUG, "(SlowMount::obstarAction) command: %s %s %s",
	       getComponent()->getName(), "obstar", getStringifiedTimeStamp().c_str()));
#endif
    
    DBConnector::writeCommand(getComponent()->getName(), "obstar", getStringifiedTimeStamp());
    
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


// implementation of async. objfix() method
//
// This is the method who simulates the movement
ActionRequest 
SlowMount::objfixAction (BACIComponent *cob_p, 
		     const int &callbackID,
		     const CBDescIn &descIn, 
		     BACIValue *value_p,
		     Completion &completion, 
		     CBDescOut &descOut)
{
    
    ACE_UNUSED_ARG(cob_p);
    ACE_UNUSED_ARG(callbackID);
    //ACE_UNUSED_ARG(descIn);
    ACE_UNUSED_ARG(descOut);
    
    ACS_TRACE("SlowMount::objfixAction");
    
#ifdef debugMount
    ACS_SHORT_LOG((LM_DEBUG, "(SlowMount::objfixAction) %s", getComponent()->getName()));
#endif
    
    // convert the methods parameters back into something we can use
    __objfix_action *param_p = static_cast<__objfix_action *>(const_cast<void *>(value_p->pointerValue()));
    
    ACS::Time timestamp;
    
    // We need to know if this is the first call to this method
    // to initialize the variables for the simulation
    // This variable enhance the readability of the code
    static bool firstCall=true;
    static time_t startTime=0; // The time of the first invocation
    static time_t endTime=0; // ETA (Estimated Time of Arrival)
    static time_t lastUpdateTime=0; // The second of the last update (one second granularity)
    static double deltaAz=0.0; // AZ step
    static double deltaEl=0.0; // El step
    
    // Read the actual position of the antenna
    double actAz=m_actAz_sp->getDevIO()->read(timestamp);
    double actEl=m_actEl_sp->getDevIO()->read(timestamp);
    

	// The first call is used to set the variables 
	// to simulate the movement
    if (firstCall) {
    	firstCall=false;
    	startTime=time(NULL);
    	endTime=startTime+30; // We decide to arrive in 30 secs!!!
    	
    	// Evaluate the steps
    	deltaAz=(fabs(param_p->az)-fabs(actAz))/30;
    	if (actAz>param_p->az) deltaAz=-deltaAz;
    	deltaEl=(fabs(param_p->elev)-fabs(actEl))/30;
    	if (actEl>param_p->elev) deltaEl=-deltaEl;
    	
    	// Set the commanded positions of the antenna
	m_cmdAz_sp->getDevIO()->write(param_p->az, timestamp);
    	m_cmdEl_sp->getDevIO()->write(param_p->elev, timestamp);
    	
    	// Write the command in the database
    	DBConnector::writeCommand(getComponent()->getName(), "objfix", getStringifiedTimeStamp());
    }
    
    // Simulate the movement of the antenna
    // Granularity is the second (this method is called several
    // times per second)
    time_t now=time(NULL); // 
    if (lastUpdateTime<endTime && now!=lastUpdateTime) 
	{
	m_actAz_sp->getDevIO()->write(actAz+deltaAz, timestamp);
    	m_actEl_sp->getDevIO()->write(actEl+deltaEl, timestamp);
    	char logStr[256];
    	sprintf(logStr,
    		"SlowMount::objfixAction Moving to [az=%lf, el=%lf] @ %ld",
    		actAz+deltaAz,
    		actEl+deltaEl,
    		now);
    	ACS_SHORT_LOG((LM_INFO,logStr));
    	lastUpdateTime=now;
    } else if (lastUpdateTime>=endTime) {
    	// The time is ended: force the antenna in position
    	m_actAz_sp->getDevIO()->write(param_p->az, timestamp);
    	m_actEl_sp->getDevIO()->write(param_p->elev, timestamp);
    }
    
    completion = ACSErrTypeOK::ACSErrOKCompletion();
    
    // Check if the antenna is position
    if (actAz==param_p->az && actEl==param_p->elev) {
    	// The antennza is in position
    	ACS_SHORT_LOG((LM_INFO,
    		"SlowMount::objfixAction In position @ %d",
    		now));
    	
    	// Reset static variables to be ready for next call
    	firstCall=true;
    	lastUpdateTime=startTime=endTime=0;
    	deltaEl=deltaAz=0.0;
    	
	    // if OK action will be destroyed and we do not need it anymore
    	// Non lo distruggo perchè non ho finito
	    if (param_p!=0) 
		{
			delete param_p;
		}
    
    	// complete action requesting done invokation, 
    	// otherwise return reqInvokeWorking and set descOut.estimated_timeout
    	return reqInvokeDone;
    } else {
    	// We need a further iteration
    	return reqInvokeWorking;
    }
}


/* --------------------- [ CORBA interface ] ----------------------*/
ACS::ROdouble_ptr
SlowMount::cmdAz ()
{
	ACS_TRACE("SlowMount::cmdAz");
    if (m_cmdAz_sp == 0)
	{
	return ACS::ROdouble::_nil();
	}

    ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_cmdAz_sp->getCORBAReference());
    return prop._retn();
}

ACS::ROdouble_ptr
SlowMount::cmdEl ()
{
	ACS_TRACE("SlowMount::cmdEl");
    if (m_cmdEl_sp == 0)
	{
	return ACS::ROdouble::_nil();
	}

    ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_cmdEl_sp->getCORBAReference());
    return prop._retn();
}

ACS::ROdouble_ptr
SlowMount::actAz ()
{
	ACS_TRACE("SlowMount::actAz");
    if (m_actAz_sp == 0)
	{
	return ACS::ROdouble::_nil();
	}

    ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_actAz_sp->getCORBAReference());
    return prop._retn();
}

ACS::ROdouble_ptr
SlowMount::actEl ()
{
	ACS_TRACE("SlowMount::actEl");
    if (m_actEl_sp == 0)
	{
	return ACS::ROdouble::_nil();
	}
    
    ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_actEl_sp->getCORBAReference());
    return prop._retn();
}

void
SlowMount::obstar (CORBA::Double ra,
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
	ACS_TRACE("SlowMount::obstar");
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
SlowMount::objfix (CORBA::Double az,
	       CORBA::Double elev,
	       ACS::CBvoid_ptr callBack,
	       const ACS::CBDescIn &desc
	       )
{
	ACS_TRACE("SlowMount::objfix");
    // convert this method's parameters into something ActionImplementor can use
    __objfix_action *param_p = new __objfix_action();
    param_p->az=az; 
    param_p->elev=elev;
    
    // register the action in a queue so that control is returned immediately
    getComponent()->registerAction(BACIValue::type_null, callBack, desc, this, 1, BACIValue(param_p));    // ID = 1
}


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(SlowMount)
/* ----------------------------------------------------------------*/





