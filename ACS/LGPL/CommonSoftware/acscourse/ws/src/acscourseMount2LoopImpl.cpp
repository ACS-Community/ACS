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
* "@(#) $Id: acscourseMount2LoopImpl.cpp,v 1.9 2006/06/22 16:26:30 gchiozzi Exp $"
*
*/
 
#include <acscourseMount2LoopImpl.h>

/**********************************
 *  Implementation for the thread *
 **********************************/

PositionControlThread::PositionControlThread(
    const ACE_CString& name, 
    Mount2LoopImpl *_mount_p, 
    const ACS::TimeInterval& responseTime, 
    const ACS::TimeInterval& sleepTime) :
    ACS::Thread(name, responseTime, sleepTime),
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
    ACS_TRACE("PositionControlThread::runLoop");

    ACS::Time timestamp;

    double cmdAzValue = mount_p->m_cmdAz_sp->getDevIO()->read(timestamp);
    double cmdElValue = mount_p->m_cmdEl_sp->getDevIO()->read(timestamp);
    double actAzValue = mount_p->m_actAz_sp->getDevIO()->read(timestamp);
    double actElValue = mount_p->m_actEl_sp->getDevIO()->read(timestamp);


    try
	{
	/*
	 * A simple algorithm to simulate the telescope
	 * mooving.
	 * Much better can be done!
	 */
	if(cmdAzValue > actAzValue)
	    mount_p->m_actAz_sp->getDevIO()->write(actAzValue+0.01, timestamp);
	if(cmdAzValue < actAzValue)
	    mount_p->m_actAz_sp->getDevIO()->write(actAzValue-0.01, timestamp);
	if(cmdElValue > actElValue)
	    mount_p->m_actEl_sp->getDevIO()->write(actElValue+0.01, timestamp);
	if(cmdElValue < actElValue)
	    mount_p->m_actEl_sp->getDevIO()->write(actElValue-0.01, timestamp);
	}
    catch (...) 
	{
	// Here we have to better handle errors!
	ACS_SHORT_LOG((LM_ERROR,"Error accessing devIO"));
	}
}

/* ----------------------------------------------------------------*/

/************************************
 * Implementation for the Component *
 ************************************/

Mount2LoopImpl::Mount2LoopImpl(const ACE_CString &_name, maci::ContainerServices *containerServices) :
    CharacteristicComponentImpl(_name, containerServices),
    Mount2Impl(_name, containerServices)
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::Mount2LoopImpl::Mount2LoopImpl");


    // Tell the ThreadManager provided to the Component
    // by the ContainerServices to instantiate our thread.
    Mount2LoopImpl *compPtr = this;
    getContainerServices()->getThreadManager()->create<PositionControlThread, Mount2LoopImpl*>("positionControl", compPtr);

    ACS_SHORT_LOG((LM_INFO,"positionControl thread spawned.")); 
}

Mount2LoopImpl::~Mount2LoopImpl()
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::Mount2LoopImpl::~Mount2LoopImpl");
}


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(Mount2LoopImpl)
/* ----------------------------------------------------------------*/


/*___oOo___*/




