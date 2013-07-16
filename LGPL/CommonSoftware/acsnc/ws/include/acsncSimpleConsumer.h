#ifndef SIMPLE_CONSUMER_H
#define SIMPLE_CONSUMER_H
/*    @(#) $Id: acsncSimpleConsumer.h,v 1.15 2012/02/13 21:12:59 javarias Exp $
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

/** @file acsncSimpleSupplier.h
 *  Header file for the Supplier-derived class that should be used from within
 *  components.
 */

#include "acsncConsumer.h"
#include <acsutilAnyAide.h>
#include <RepeatGuard.h>
#include "acsncErrType.h"
#include "acsncBlockingQueue.h"

namespace nc {

/**
 * SimpleConsumer is used to consume data from a notification channel
 * defined by the string passed to SimpleConsumer's constructor. In a nutshell,
 * this class has been provided so that developers do <b>not</b> have to 
 * subclass Consumer and can instead use a generic function to process
 * events.
 *
 * To begin consuming data, simply invoke the consumerReady method. After
 * that, the eventHandlerFunction will be invoked asynchronously each 
 * time an event is received.
 * 
 * TODO:
 * - add a check on the datatype in push_structured_event
 * - support subscriptions to multiple event types...may not be feasible
 */
template <class T> class SimpleConsumer : public Consumer
{
  public:
    /**
     * @typedef
     * Pointer to an event handler function.
     * @param eventData A templated ICD event structure
     * @param handlerParam A void parameter passed to the addSubscription
     * method. This can be anything the developers chooses.
     * @return void
     */
    typedef void (*eventHandlerFunction)(T eventData, void *handlerParam);
    ///////////////////////////////////////////////////////////////
    /**
     * Constructor to be used within components.
     * @param channelName The channel's name
     * @param acsNCDomain name of the ACS NC domain name. This is an optional parameter.
     *  It will default to acscommon::NAMESERVICE_BINDING_NC_DOMAIN_DEFAULT if it is not specified.
     */
    SimpleConsumer(const char* channelName, const char* acsNCDomainName = 0);
    ///////////////////////////////////////////////////////////////
    /**
     * A special version of the addSubscription method. Not only
     * subscribes to an event_type, but it also saves a function pointer
     * and an extra paremter to the function to be utilized when an event
     * of type_name is received. Currently the implementation only allows
     * this method to be invoked once (and that is done from the ACSNC_NEW_SIMPLE_CONSUMER
     * macro).
     * The template paramter is the type_name of the event to be received.
     * 
     * @param templateFunction A (static) method which will be invoked each time a type_name
     *        event is received. The method must accept a <T> structure as it's first parameter.
     * @param handlerParam A void pointer that will be passed to templateFunction (in addition
     *        to the actual ICD <T> event) each time a type_name event is received.
     * @throw ACSErrTypeCommon::CouldntPerformActionEx
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly 
     */
    template <class J> void
    addSubscription(eventHandlerFunction templateFunction, void *handlerParam=0)
	{
	    ACS_TRACE("SimpleConsumer::addSubscription");
	    
	    //yikes...the developer is trying to subscribe to multiple event
	    //types from a singly templated class.
	    if (templateFunction_mp!=0)
		{
		ACS_SHORT_LOG((LM_ERROR,
			       "SimpleConsumer::addSubscription unable to subscribe to multiple event types from this class!"));
		ACSErrTypeCommon::CouldntPerformActionExImpl err = ACSErrTypeCommon::CouldntPerformActionExImpl(__FILE__,
									    __LINE__,
									    "nc::SimpleConsumer::addSubscription");
		throw err.getCouldntPerformActionEx();
		}
	    
	    //save a reference to the eventHandlerFunction
	    templateFunction_mp = templateFunction;
	    
	    //save the parameter to handler
	    handlerParam_mp = handlerParam;
	    
	    //just call superclass implementation
	    Consumer::addSubscription<J>();
	}
    
    /**
     * This is the abstract method inherited Consumer which must be overridden.
     * In the SimpleConsumer class, it just invokes some function registered
     * via the addSubscription method. In doing this, one does not have to 
     * override SimpleConsumer. Note that this method must not be
     * invoked by your code!
     * @throw CosEventComm::Disconnected
     * @return void
     * @htmlonly
     <br><hr>
     @endhtmlonly 
    */      
    virtual void 
    push_structured_event(const CosNotification::StructuredEvent &notification);
    ///////////////////////////////////////////////////////////////

    bool stopConsumerThread();

    virtual void disconnect();

    /**
     * It just invokes some function registered
     * via the addSubscription method
     */
    void processEvent();

  protected:
    /**
     * Destructor is protected.
     */
    virtual ~SimpleConsumer();

    /**
     * This function does something with T events.
     */
    eventHandlerFunction templateFunction_mp;
    /**
     * This is a single parameter that will be passed to the handler function
     * in addition to the ICD <T> event.
     */
    void *handlerParam_mp;

  private:
    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const SimpleConsumer&);

    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    SimpleConsumer(const SimpleConsumer&);

    ///////////////////////////////////////////////////////////////
    /**
     * A special version of the addSubscription method. Not only
     * subscribes to an event_type, but it also saves a function pointer
     * and an extra paremter to the function to be utilized when an event
     * of type_name is received. Currently the implementation only allows
     * this method to be invoked once (and that is done from the ACSNC_NEW_SIMPLE_CONSUMER
     * macro).
     * @param type_name Type of the event to be received
     * @param templateFunction A (static) method which will be invoked each time a type_name
     *        event is received. The method must accept a <T> structure as it's first parameter.
     * @param handlerParam A void pointer that will be passed to templateFunction (in addition
     *        to the actual ICD <T> event) each time a type_name event is received.
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly 
     */
    void
    addSubscription(const char* type_name, eventHandlerFunction templateFunction, void *handlerParam=0);

    /**
     * Entry point for the buffer's consumer thread
     */
    static void * dispatchEvent(void *args);

    blocking_queue<T> buffer;
    bool stop_thread;
    pthread_t dispatching_thread;

	RepeatGuard receiverTooSlowLogRepeatGuard;
	unsigned int numEventsDiscarded;

};
 }; 

#include "acsncSimpleConsumer.i"

/**
 *  MACRO must be used instead of manually allocating memory for SimpleConsumer
 *  pointers.  This is done so that consumers/suppliers across the different
 *  programming languages do not confuse the type_name field of CORBA 
 *  structured events.
 *  @param simpleConsumer_p A pointer to an unallocated SimpleConsumer.
 *  @param idlStruct The IDL struct that will be subscribed to (FRIDGE::temperatureDataBlockEvent for example).
 *  @param channelName Name of the channel events will be published too.
 *  @param handlerFunction A function pointer to a function capable of processing idlStruct events.
 *  @param handlerParam A single void parameter that will be passed to the handlerFunction in addition to the ICD event.
 */
#define ACS_NEW_SIMPLE_CONSUMER(simpConsumer_p, idlStruct, channelName, handlerFunction, handlerParam) \
{ \
  simpConsumer_p = new nc::SimpleConsumer<idlStruct>(channelName); \
  simpConsumer_p->addSubscription<idlStruct>(handlerFunction, handlerParam);\
}

#endif
