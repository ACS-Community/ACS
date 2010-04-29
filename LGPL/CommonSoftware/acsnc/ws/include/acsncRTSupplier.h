#ifndef RT_SUPPLIER_H
#define RT_SUPPLIER_H
/*    @(#) $Id: acsncRTSupplier.h,v 1.21 2010/04/29 20:00:45 javarias Exp $
 *    ALMA - Atacama Large Millimiter Array
 *    (c) Associated Universities Inc., 2002 
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
 */

/** @file acsncRTSupplier.h
 *  Header file for the Supplier-derived class that should be used from within
 *  components.  This supplier class is designed to be used on real-time 
 *  computers.
 */

#include "acsncSupplier.h"
#include <queue>
namespace nc {
/**
 *  RTSupplier is used to publish data onto a notification channel
 *  defined by the string passed to RTSupplier's constructor. Please 
 *  do not let the name confuse you as this class does <b>not publish
 *  structured events in real-time</b>. Instead, invocations of the 
 *  publishData method merely save the structured event to an internal
 *  queue and the event will be sent across the network by a low-priority 
 *  thread sometime later. In summary, it's safe to call publishData
 *  from real-time code as it does not block until the event has been 
 *  received on manager's host.
 *
 *  TODO:
 *  - override the configQofS and configAdminProps methods for 
 *    real-time use
 *  - performance tests
 *  - integrate this class with the ACS Exception Manager because 
 *    asynchornous exceptions can be thrown by the static worker
 *    method
 *  - investigate whether a mutex is really needed for std::queues
 *    in gcc 3.3.
 */
class RTSupplier : public Supplier
{
  public:
    /**
      * Event procesing callback interface definition
      */
      class EventProcessingCallback{
          public:
              virtual void queueIsFull(::CORBA::Any_ptr event){}
              virtual void eventDropped(::CORBA::Any_ptr event){}
              virtual ~EventProcessingCallback(){}
      };


    ///////////////////////////////////////////////////////////////
    /**
     * Constructor
     * @param channlName The name of the channel events will be published to.
     * @param component A reference to a component is needed for the Event 
     * Description (which is normally hidden from Consumers).
     */
    RTSupplier(const char* channelName, 
	       acscomponent::ACSComponentImpl* component);
    
    /**
     *  Overriden from Supplier class.
     *  Ensures all events in the queue are published before exiting.
     *  @return void
     *  @htmlonly
        <br><hr>
        @endhtmlonly
     */
    virtual void
    disconnect();
    
    /**
     *  Templated method called by the developer to send ICD events to 
     *  consumers. publishData saves the ICD event to an internal queue
     *  and returns control immediately. Please note that any exceptions
     *  associated with actual CORBA calls involved with pushing events
     *  are only logged as this functionality is really performed by the
     *  worker method/thread.
     *  @throw ACSErrTypeCommon::CORBAProblemEx 
     *  @param data A user-defined IDL struct. FRIDGE::temperatureDataBlockEvent 
     *              for example
     *  @htmlonly
        <br><hr>
        @endhtmlonly
     */
    template <class T> void
    publishData(T data, EventProcessingCallback *evProcCallback=NULL);
    ///////////////////////////////////////////////////////////////

	 /**
		* Returns the size of the queue with events waiting to be sent by
		* the worker thread.
		*
		*/
	unsigned int getQueueSize();
	
  
  protected:
    /**
     * Destructor is protected.
     */
    virtual ~RTSupplier();

    /**
     *  Low priority thread method which is actually responsible for
     *  sending events across the network. Any exceptions generated
     *  by the CORBA calls are only logged here. This thread just sleeps
     *  for awhile if there are no events to publish. If the queue is 
     *  non-empty, it publishes all events before sleeping again.
     *  @param parmam_p A pointer to this instance.
     *  @return void
     *  @htmlonly
        <br><hr>
        @endhtmlonly
     */
    static void 
    worker(void* param_p);

    /**
     *  BACI thread manager used to control the worker thread.
     */
    baci::BACIThreadManager *threadManager_mp;

    /**
      * Maintains the event and the data related to the event's sending
      */
    struct unpublishedEventData{
        CosNotification::StructuredEvent event;
        EventProcessingCallback *callback;
        unsigned int tries;
    };

    /**
     *  A queque of structured events.
     */
    std::queue<unpublishedEventData>  unpublishedEvents_m;

    /**
     * A CORBA any which is used to encode/store ICD style events. This has
     * been made a member variable to improve performance of the publishData
     * method.
     */
    CORBA::Any any_m;

    /** 
     * Mutex prevents real-time code from saving half an event with the 
     * low priority thread trying to publish it. 
     */
    ACE_Thread_Mutex eventQueueMutex_m;
    
    /**
      * Repeat guard for the logger, to be used in static methods
      */
    Logging::RepeatGuardLogger<Logging::BaseLog> *rtSupGuardb;
    
    /**
      * Repeat guard for the exceptions, to be used in static methods
      */
    Logging::RepeatGuardLogger<ACSErr::ACSbaseExImpl> *rtSupGuardex;

  private:
    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const RTSupplier&);

    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    RTSupplier(const RTSupplier&);
};
 }; 


#include "acsncRTSupplier.i"

#endif
