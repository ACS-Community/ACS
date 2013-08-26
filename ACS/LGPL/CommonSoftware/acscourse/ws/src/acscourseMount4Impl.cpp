/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) Associated Universities Inc., 2002 *
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
* "@(#) $Id: acscourseMount4Impl.cpp,v 1.11 2008/10/02 08:58:19 cparedes Exp $"
*
*/
 
#include <acscourseMount4Impl.h>

PositionControlThread::PositionControlThread(
    const ACE_CString& name, 
    Mount4Impl *_mount_p, 
    const ACS::TimeInterval& responseTime, 
    const ACS::TimeInterval& sleepTime) :
    ACS::Thread(name),
    mount_p(_mount_p)
{
    ACS_TRACE("PositionControlThread::PositionControlThread");
}

PositionControlThread::~PositionControlThread() 
{
    ACS_TRACE("PositionControlThread::~PositionControlThread"); 
}

void PositionControlThread::runLoop()
{
    ACS_STATIC_TRACE("PositionControlThread::runLoop");

    ACS::Time timestamp;

    double cmdAzValue = mount_p->m_cmdAz_sp->getDevIO()->read(timestamp);
    double cmdElValue = mount_p->m_cmdEl_sp->getDevIO()->read(timestamp);
    double actAzValue = mount_p->m_actAz_sp->getDevIO()->read(timestamp);
    double actElValue = mount_p->m_actEl_sp->getDevIO()->read(timestamp);

    if ( (cmdAzValue != actAzValue) ||
	 (cmdElValue != actElValue)    )
	{
	// Simulated control
	ACS_STATIC_SHORT_LOG((LM_INFO, "Moving to (%f,%f)...", cmdAzValue, cmdElValue));
	try
	    {
	    mount_p->m_actAz_sp->getDevIO()->write(cmdAzValue, timestamp);
	    mount_p->m_actEl_sp->getDevIO()->write(cmdElValue, timestamp);
	    }
	catch (...) 
	    {
	    // Here we have to better handle errors!
	    ACS_SHORT_LOG((LM_ERROR,"Error accessing devIO"));
	    }
	} 
}

/**
 * One of these function IDs will be passed to invokeAction().
 */
const static int OBJFIX_ACTION  = 1;

/* ----------------------------------------------------------------*/
Mount4Impl::Mount4Impl(const ACE_CString &_name, maci::ContainerServices *containerServices) :
    CharacteristicComponentImpl(_name, containerServices),
    m_cmdAz_sp(new baci::ROdouble(_name+":cmdAz", getComponent()),this),
    m_cmdEl_sp(new baci::ROdouble(_name+":cmdEl", getComponent()),this),
    m_actAz_sp(new baci::ROdouble(_name+":actAz", getComponent()),this),
    m_actEl_sp(new baci::ROdouble(_name+":actEl", getComponent()),this)
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::Mount4Impl::Mount4Impl");
    // Initialize control loop thread

    PositionControlThread *pct = new PositionControlThread("positionControl",this);
    getComponent()->getThreadManager()->add("positionControl",pct);

    ACS_SHORT_LOG((LM_INFO,"positionControl thread spawned.")); 
}

/* ----------------------------------------------------------------*/
Mount4Impl::~Mount4Impl()
{

    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::Mount4Impl::~Mount4Impl");
}


/* --------------- [ Action implementator interface ] -------------- */

baci::ActionRequest 
Mount4Impl::invokeAction (int function,
		    baci::BACIComponent *cob_p, 
		    const int &callbackID, 
		    const CBDescIn &descIn, 
		    baci::BACIValue *value_p, 
		    Completion &completion, 
		    CBDescOut &descOut) 
{
    
    // better implementation with array is possible
    switch (function) 
	{
	case OBJFIX_ACTION:
	  {
	  return objfixAction(cob_p, callbackID, descIn, value_p, completion, descOut);
	  }
	default:
	  {
	  return baci::reqDestroy;
	  }
	}
}

/// implementation of async. on() method
baci::ActionRequest 
Mount4Impl::objfixAction (baci::BACIComponent *cob_p, 
		const int &callbackID,
		const CBDescIn &descIn, 
		baci::BACIValue *value_p,
		Completion &completion, 
		CBDescOut &descOut)
{
    ACS_DEBUG_PARAM("::Mount4::objfixAction", "%s", getComponent()->getName());
    
    ACS::Time timestamp;
    
    // convert the methods parameters back into something we can use
    __objfix_action *param_p = 
	static_cast<__objfix_action *>(const_cast<void *>(value_p->pointerValue()));
    
    try
	{
	m_cmdAz_sp->getDevIO()->write(param_p->az,   timestamp);
	m_cmdEl_sp->getDevIO()->write(param_p->elev, timestamp);
    	}
    catch (...) 
	{
	// Here we have to better handle errors!
	ACS_SHORT_LOG((LM_ERROR,"Error accessing devIO"));
	}

    ACS_SHORT_LOG((LM_INFO, "Executed asynchronous objfix command. Az: %f, El: %f", 
		   param_p->az, param_p->elev));

    completion = ACSErrTypeOK::ACSErrOKCompletion();

    
    // if OK action will be destroyed and we do not need it anymore
    if (param_p!=0) 
	{
	delete param_p;
	}
    
     // complete action requesting done invocation, 
    // otherwise return reqInvokeWorking and set descOut.estimated_timeout
    return baci::reqInvokeDone;
}


/* --------------------- [ CORBA interface ] ----------------------*/
void 
Mount4Impl::objfix (CORBA::Double az,
		    CORBA::Double elev)
{
    ACS::Time timestamp;
    
    try
	{
	m_cmdAz_sp->getDevIO()->write(az,   timestamp);
	m_cmdEl_sp->getDevIO()->write(elev, timestamp);
    	}
    catch (...) 
	{
	// Here we have to better handle errors!
	ACS_SHORT_LOG((LM_ERROR,"Error accessing devIO"));
	}

    ACS_SHORT_LOG((LM_INFO, "Received objfix command. Az: %f, El: %f", 
		   az, elev));

}

void
Mount4Impl::objfix_async (CORBA::Double az,
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
    getComponent()->registerAction(baci::BACIValue::type_null, callBack, desc, this, OBJFIX_ACTION, baci::BACIValue(param_p)); // ID = 1
}

ACS::ROdouble_ptr
Mount4Impl::cmdAz ()
{
    if (m_cmdAz_sp == 0)
	{
	return ACS::ROdouble::_nil();
	}

    ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_cmdAz_sp->getCORBAReference());
    return prop._retn();
}


ACS::ROdouble_ptr
Mount4Impl::cmdEl ()
{
    if (m_cmdEl_sp == 0)
	{
	return ACS::ROdouble::_nil();
	}

    ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_cmdEl_sp->getCORBAReference());
    return prop._retn();
}


ACS::ROdouble_ptr
Mount4Impl::actAz ()
{
    if (m_actAz_sp == 0)
	{
	return ACS::ROdouble::_nil();
	}

    ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_actAz_sp->getCORBAReference());
    return prop._retn();
}


ACS::ROdouble_ptr
Mount4Impl::actEl ()
{
    if (m_actEl_sp == 0)
	{
	return ACS::ROdouble::_nil();
	}
    
    ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_actEl_sp->getCORBAReference());
    return prop._retn();
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(Mount4Impl)
/* ----------------------------------------------------------------*/


/*___oOo___*/




