/* @(#) $Id: acstimeTimerImpl.h,v 1.22 2008/10/01 03:11:48 cparedes Exp $
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
////////////////////////////////////////////////////////////////////////
#ifndef ACSTIME_TIMER_IMPL_H
#define ACSTIME_TIMER_IMPL_H
////////////////////////////////////////////////////////////////////////
#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif
////////////////////////////////////////////////////////////////////////
#include <baciCharacteristicComponentImpl.h>
#include <baci.h>
#include <ace/Timer_Heap_T.h>
#include <ace/Timer_Queue_Adapters.h>
#include <loggingLogger.h>
////////////////////////////////////////////////////////////////////////
#include "acstimeS.h"
#include "ACSTimeError.h"
#include "acstimeTimeUtil.h"
#include "loggingLoggable.h"
////////////////////////////////////////////////////////////////////////
/** @file acstimeTimerImpl.h
 *  Header file for implementation of ACS Timer interface.
 */

/** 
 * @class TimerImpl
 * TimerImpl is the implementation of the <a href="../../idl/html/interfaceacstime_1_1Timer.html">Timer</a>
 * IDL interface and it is used for the sole purpose of setting one-time and
 * continuous alarms on TimeoutHandler instances.
 *
 * TODO:
 *  - nothing
 */
class TimerImpl : public virtual acscomponent::ACSComponentImpl,
		  public virtual POA_acstime::Timer
{
  public:
    /**
     * Constructor
     * @param poa Poa which will activate this and also all other components. 
     * @param name component's name.
     */
    TimerImpl(
	      const ACE_CString &name,
	      maci::ContainerServices * containerServices);
    
    /**
     * Destructor
     */
    virtual ~TimerImpl();
    ////////////////////////////////////////////////////////////////////////
    /**
     * Implementation of IDL method.
     * Please see the documenation for the Timer IDL interface.
     * @throw ACSTimeError::ArgErrorEx
     * @htmlonly
       <li><a href="../../idl/html/interfaceacstime_1_1Timer.html">IDL Documentation</a></li><br><hr>
       @endhtmlonly
     */ 
    virtual CORBA::Long 
    schedule(acstime::TimeoutHandler_ptr callBack,
	     const acstime::Epoch &time,
	     const acstime::Duration &interval);
    
    /**
     * Implementation of IDL method.
     * Please see the documenation for the Timer IDL interface.
     * @throw ACSTimeError::InvalidIDEx
     * @htmlonly
       <li><a href="../../idl/html/interfaceacstime_1_1Timer.html">IDL Documentation</a></li><br><hr>
       @endhtmlonly
     */ 
    virtual void 
    cancel(CORBA::Long id);
    
    ////////////////////////////////////////////////////////////////////////
  private:
    
    /// Event_Handler_Handle_Timeout_Upcall is defined in ace/Timer_Queue_T.h
    typedef ACE_Event_Handler_Handle_Timeout_Upcall<ACE_Null_Mutex> Upcall;
    
    /// Timer_Heap_T is defined in ace/Timer_Heap_T.h
    typedef ACE_Timer_Heap_T<ACE_Event_Handler*,Upcall,ACE_Null_Mutex> Timer_Heap;
    
    /// Timer_Heap_Iterator_T is defined in ace/Timer_Heap_T.h
    typedef ACE_Timer_Heap_Iterator_T<ACE_Event_Handler*,Upcall,ACE_Null_Mutex> Timer_Heap_Iterator;
    
    /// Thread_Timer_Queue_Adapter is defined in ace/Timer_Queue_Adapters.h
    typedef ACE_Thread_Timer_Queue_Adapter<Timer_Heap> Thread_Timer_Queue;
    
    /// thread timer queue implementation
    Thread_Timer_Queue *m_timerQueue;
    
    /// copy not allowed
    TimerImpl(const TimerImpl&);
    
    /// assignment not allowed
    void operator= (const TimerImpl&);
    
    ////////////////////////////////////////////////////////////////////////
    /**
     * timeout handler inner class derived from ACE_Event_Handler
     * Handler hides the ACE alarms/timers.
     */
    class Handler : public ACE_Event_Handler,
		    public Logging::Loggable

    {
      public:
	/**
	 * Constructor
	 * @param callBack The callback object.
	 * @param oneShotFlag Set to true only if the alarm/timer should only
	 *        be invoked once.
	 */
        Handler(acstime::TimeoutHandler_ptr callBack,
		bool oneShotFlag);

	/**
	 * Destructor
	 */
	virtual ~Handler();
	
	/**
	 * Invokes the real timeout method on the TimeoutHandler object.
	 * @param current time
	 * @param who knows???
	   @htmlonly
	   <br><hr>
	   @endhtmlonly
	 */
        int 
	handle_timeout(const ACE_Time_Value&,
		       const void*);
	
	/**
	 *  The callback handler object.
	 */
	acstime::TimeoutHandler_var m_handler;

	/**
	 *  Indicates whether this is a continuous alarm/timer or not.
	 */
        bool m_oneShotFlag;
	
    };
};
#endif

