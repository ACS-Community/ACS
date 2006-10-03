/* @(#) $Id: acstimeTimerImpl.cpp,v 1.17 2006/10/03 21:55:26 gchiozzi Exp $
 *
 * Copyright (C) 2001
 * Associated Universities, Inc. Washington DC, USA.
 *
 * Produced for the ALMA project
 *
 * This library is free software; you can redistribute it and/or modify it it 
 * under the terms of the GNU Library General Public License as published by 
 * the Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 *
 * This library is distributed in the hope that it will be useful but WITHOUT 
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
 * License for more details.
 *
 * You should have received a copy of the GNU Library General Public License 
 * along with this library; if not, write to the Free Software Foundation, 
 * Inc., 675 Massachusetts Ave, Cambridge, MA, 02139, USA.
 *
 * Correspondence concerning ALMA should be addressed as follows:
 * Internet email: alma-sw-admin@nrao.edu
 */

#include "acstimeTimerImpl.h"
#include "acstimeTimeUtil.h"
#include <iostream>
//------------------------------------------------------------------------------
 using namespace baci;
using namespace ACSTimeError;
//------------------------------------------------------------------------------
TimerImpl::TimerImpl(
		     const ACE_CString& name,
		     maci::ContainerServices * containerServices) :
    ACSComponentImpl(name, containerServices)
{  
    // create thread timer queue and activate it
    m_timerQueue = new Thread_Timer_Queue();
    if (m_timerQueue->activate() == -1)
	{
        std::cerr << "Timer_Queue_Adapter.activate failed" << std::endl;
	}
}
//------------------------------------------------------------------------------
TimerImpl::~TimerImpl()
{   
    // cancel thread timer queue task
    m_timerQueue->deactivate();
    m_timerQueue->wait();
    delete m_timerQueue;
}
//------------------------------------------------------------------------------
CORBA::Long 
TimerImpl::schedule(acstime::TimeoutHandler_ptr handler,
		    const acstime::Epoch &start,
		    const acstime::Duration &period)
    throw (CORBA::SystemException, ACSTimeError::ArgErrorEx)
{
    if (CORBA::is_nil(handler) == true)
        {  // nil _ptr
	throw (ArgErrorExImpl(__FILE__, __LINE__, "TimerImpl::schedule"));
	return -1;
        }
    
    if (start.value == 0 && period.value == 0)
        {  // start and period are both nil
	throw (ArgErrorExImpl(__FILE__, __LINE__, "TimerImpl::schedule"));
	return -1;
        }
    
    // convert start and period arguments to ACE_Time_Value equivalents
    ACE_Time_Value aceStart = TimeUtil::epoch2ace(start);
    ACE_Time_Value acePeriod = TimeUtil::duration2ace(period);

    // zero start time => use current time plus period
    if (aceStart.sec() == 0 && aceStart.usec() == 0)
        {
        aceStart = ACE_OS::gettimeofday() + acePeriod;
        }
    
    bool joe = acePeriod == ACE_Time_Value::zero;

    // create Handler object (derived from ACE_Event_Handler)
    // zero period => Handler should delete itself when timeout occurs
    Handler* h_p = new Handler(acstime::TimeoutHandler::_duplicate(handler), joe);

    
    // schedule timeout
    // pass Handler pointer as "act" (2nd argument)
    // so it can be retrieved by cancel() below 
    int id = m_timerQueue->schedule(h_p,
				    const_cast<const void *>(static_cast<void *>(h_p)),
				    aceStart,acePeriod);
    if (id == -1)
        {  // schedule() failed
        throw (ArgErrorExImpl(__FILE__, __LINE__, "TimerImpl::schedule"));
	return -1;
        }
    
    return id;
} 
//------------------------------------------------------------------------------
void 
TimerImpl::cancel(int id)
    throw (CORBA::SystemException, ACSTimeError::InvalidIDEx)
{
    // remove entry from queue
    // if remove is successful, delete Handler object
    // N.B. the Handler object deletes itself when "one shot" times out
    Handler *h_p;
    if (m_timerQueue->cancel(id,
			     (const void **)(&h_p))
	!= 0)
	{
        delete h_p;
	}
    else
	{
	throw(InvalidIDExImpl(__FILE__, __LINE__, "TimerImpl::cancel"));
	}
}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
TimerImpl::Handler::Handler(acstime::TimeoutHandler_ptr handler, bool flag) : 
    Logging::Loggable("acstime::Timer::Handler"),
    m_handler(handler)
   
{
    m_oneShotFlag=flag;
}
//------------------------------------------------------------------------------
TimerImpl::Handler::~Handler()
{
    ACS_SHORT_LOG((LM_INFO,"~TimerImpl::Handler::Handler Internal handler for a TimeoutHandler CORBA object being destroyed."));
}
//------------------------------------------------------------------------------
int 
TimerImpl::Handler::handle_timeout(const ACE_Time_Value &tv,
				   const void *arg)
{
    ACE_UNUSED_ARG(tv);
    ACE_UNUSED_ARG(arg);
    
    // call client timeout handler method
    acstime::Epoch e = TimeUtil::ace2epoch(ACE_OS::gettimeofday());
    m_handler->handleTimeout(e);

    if (m_oneShotFlag==true)
	{
	delete this;
	}
	
    return 0;    // don't call handle_close()
}
/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(TimerImpl)

