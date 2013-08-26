#ifndef SIMPLE_CONSUMER_I
#define SIMPLE_CONSUMER_I
/*    @(#) $Id: acsncSimpleConsumer.i,v 1.28 2012/02/13 21:12:59 javarias Exp $
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

/** @file acsncSimpleConsumer.i
 *  Header file for the Consumer-derived class that should be used from within
 *  components to consume events.
 */
#include "acsncSimpleConsumer.h"
#include <AcsNCTraceLog.h>

namespace nc {
//---------------------------------------------------------------
template<class T>
SimpleConsumer<T>::SimpleConsumer(const char* channelName, const char* acsNCDomainName) :
        Consumer(channelName,acsNCDomainName), templateFunction_mp(0), stop_thread(false), receiverTooSlowLogRepeatGuard(
                300000000, 100), numEventsDiscarded(0) {
    // Always call init() in the constructors 
    // of concrete Consumer() classes.
    init();
    pthread_create(&dispatching_thread, NULL, SimpleConsumer<T>::dispatchEvent,
            reinterpret_cast<void *>(this));
}
//---------------------------------------------------------------
template<class T>
void SimpleConsumer<T>::push_structured_event(
        const CosNotification::StructuredEvent &notification) {
    //update the internal counter
    numEvents_m++;

    //figure out the maximum amount of time the handler is given to run here
    if (handlerTimeoutMap_m.count(
            (const char*) notification.header.fixed_header.event_type.type_name)
            == 0) {
        handlerTimeoutMap_m[(const char*) notification.header.fixed_header.event_type.type_name] =
                DEFAULT_MAX_PROCESS_TIME;
    }
    double maxProcessTime =
            handlerTimeoutMap_m[(const char*) notification.header.fixed_header.event_type.type_name];

    //extract the correct data first of all
    T *customIDLStruct_p = 0, customIDLStruct;
    customIDLStruct_p = &customIDLStruct;
    notification.filterable_data[0].value >>= customIDLStruct_p;

    if (customIDLStruct_p == NULL)
    {
        CORBA::Any tmpEvent;
        tmpEvent <<= customIDLStruct_p;
        acsncErrType::WrongEventReceivedExImpl er(__FILE__, __LINE__,
                "nc::SimpleConsumer<T>::push_structured_event");
        er.setExpectedEvent(AnyAide::getId(tmpEvent).c_str());
        er.setReceivedEvent(
                AnyAide::getId(notification.filterable_data[0].value).c_str());

        er.log();

//	throw CosEventComm::Disconnected(); // it has no effect !
        return;
    }

    acsnc::EventDescription *ed_p = 0, ed;
    ed_p = &ed;
    notification.remainder_of_body >>= ed_p;

    integrationLog(
            std::string("Channel:") + channelName_mp + ", Receiver:" + "Unknown"
                    + ", Publisher:" + (const char*) ed.name + ", Event Type:"
                    + (const char*) notification.header.fixed_header.event_type.type_name);

    event_info<T> event(
            *customIDLStruct_p,
            maxProcessTime,
            (const char*) notification.header.fixed_header.event_type.type_name);
    buffer.push(event);
    if (buffer.size() >= (ACS_NC_CONSUMER_MAX_BUFFER_SIZE / 2)) {
        AcsNCTraceLog::LOG_NC_ReceiverTooSlow TSLog(__FILE__, __LINE__,
                __PRETTY_FUNCTION__);
        TSLog.setChannelName(channelName_mp);
        TSLog.setEventName(
                std::string(
                        (const char*) notification.header.fixed_header.event_type.type_name));
        TSLog.setClientName("Unknown");
        if (buffer.size() > ACS_NC_CONSUMER_MAX_BUFFER_SIZE) {
            buffer.pop_no_block();
            ++numEventsDiscarded;
        }
        if (receiverTooSlowLogRepeatGuard.checkAndIncrement()) {
            if (numEventsDiscarded > 0)
                TSLog.setNumEventsDiscarded(numEventsDiscarded);
            TSLog.log();
            numEventsDiscarded = 0;
        }
    }
//    //now that we have it, hope and pray the user-defined
//    //function doesn't segfault when it gets it...
//    try
//	{
//	//start the profiler
//	profiler_mp->start();
//	//invoke the handler
//	templateFunction_mp(*customIDLStruct_p, handlerParam_mp);
//	//stop timing
//   if (profiler_mp == 0){
//      return;
//   }
//	ACS::Time acsTime = profiler_mp->stop();
//	//reset the profiler to avoid consuming too much memory
//	profiler_mp->reset();
//
//	//convert acsTime from 100ns units to floating point seconds
//	double secTime = acsTime / 10000000.0;
//
//	//finally check to see if the event processing time was too slow
//	if (secTime > maxProcessTime)
//	    {
//	     ACS_SHORT_LOG((LM_WARNING,
//			    "Took too long to handle an '%s' event: %f seconds.",
//			    (const char*)notification.header.fixed_header.event_type.type_name, secTime));
//
//	     ACS_SHORT_LOG((LM_INFO,
//			    "Maximum time to process an event is: %f seconds.", maxProcessTime));
//	    }
//	}
//    catch(...)
//	{
//	ACS_SHORT_LOG((LM_ERROR,
//		       "SimpleConsumer:::push_structured_event(...) the '%s' channel: handlerFunction failed!",
//		       channelName_mp));
//	}
}
//---------------------------------------------------------------
template<class T>
SimpleConsumer<T>::~SimpleConsumer() {
}
//---------------------------------------------------------------
template<class T>
void SimpleConsumer<T>::addSubscription(const char* type_name,
        eventHandlerFunction templateFunction, void *handlerParam) {
    ACS_TRACE("SimpleConsumer::addSubscription");

    //save a reference to the eventHandlerFunction
    templateFunction_mp = templateFunction;

    //save the parameter to handler
    handlerParam_mp = handlerParam;

    //just call superclass implementation
    Consumer::addSubscription(type_name);
}

template<class T>
void SimpleConsumer<T>::processEvent() {
    //now that we have it, hope and pray the user-defined
    //function doesn't segfault when it gets it...
    try {
        event_info<T> event = buffer.pop();
        //start the profiler
        profiler_mp->start();
        //invoke the handler
        templateFunction_mp(event.event, handlerParam_mp);
        //stop timing
        if (profiler_mp == 0) {
            return;
        }
        ACS::Time acsTime = profiler_mp->stop();
        //reset the profiler to avoid consuming too much memory
        profiler_mp->reset();

        //convert acsTime from 100ns units to floating point seconds
        double secTime = acsTime / 10000000.0;

        //finally check to see if the event processing time was too slow
        if (secTime > event.maxProcessTime) {
            ACS_SHORT_LOG(
                    (LM_WARNING, "Took too long to handle an '%s' event: %f seconds.", event.type_name.c_str(), secTime));

            ACS_SHORT_LOG(
                    (LM_INFO, "Maximum time to process an event is: %f seconds.", event.maxProcessTime));
        }
    } catch (interrupted_blocking_queue &ex) {
        //Time to return
        return;
    } catch (...) {
        ACS_SHORT_LOG(
                (LM_ERROR, "SimpleConsumer:::push_structured_event(...) the '%s' channel: handlerFunction failed!", channelName_mp));
    }
}

template<class T>
bool SimpleConsumer<T>::stopConsumerThread() {
    return stop_thread;
}

template<class T>
void SimpleConsumer<T>::disconnect() {
    stop_thread = true;
    buffer.unblock();
    pthread_join(dispatching_thread, NULL);
    Consumer::disconnect();
}

template<class T>
void* SimpleConsumer<T>::dispatchEvent(void * args) {
    SimpleConsumer<T> *c = reinterpret_cast<SimpleConsumer<T>*>(args);
    //Logging Initialization for the thread
    CosNaming::Name name;
    name.length(1);
    name[0].id = CORBA::string_dup("Log");
    CORBA::Object_ptr obj = c->namingContext_m->resolve(name);
    Logging::AcsLogService_ptr remote_logger =
            Logging::AcsLogService::_narrow(obj);
    LoggingProxy *m_logger = new LoggingProxy(0, 0, 31);
    LoggingProxy::init(m_logger);
    m_logger->setCentralizedLogger(remote_logger);

    while (!c->stopConsumerThread())
        c->processEvent();
    delete m_logger;
    return 0;
}

//---------------------------------------------------------------
}
;

#endif
