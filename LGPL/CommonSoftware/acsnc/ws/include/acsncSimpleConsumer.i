#ifndef SIMPLE_CONSUMER_I
#define SIMPLE_CONSUMER_I
/*    @(#) $Id: acsncSimpleConsumer.i,v 1.27 2011/12/21 18:45:24 javarias Exp $
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


namespace nc {
//---------------------------------------------------------------
template <class T>
SimpleConsumer<T>::SimpleConsumer(const char* channelName) : 
    Consumer(channelName), 
    templateFunction_mp(0) 
{
    // Always call init() in the constructors 
    // of concrete Consumer() classes.
    init();
}
//---------------------------------------------------------------
template <class T> 
void 
SimpleConsumer<T>::push_structured_event(const CosNotification::StructuredEvent &notification)
{
    //update the internal counter
    numEvents_m++;

    //figure out the maximum amount of time the handler is given to run here
    if (handlerTimeoutMap_m.count((const char*)notification.header.fixed_header.event_type.type_name)==0)
	{
	handlerTimeoutMap_m[(const char*)notification.header.fixed_header.event_type.type_name] = DEFAULT_MAX_PROCESS_TIME;
	}
    double maxProcessTime = handlerTimeoutMap_m[(const char*)notification.header.fixed_header.event_type.type_name];

    //extract the correct data first of all
    T *customIDLStruct_p = 0, customIDLStruct;
    customIDLStruct_p = &customIDLStruct;
    notification.filterable_data[0].value >>= customIDLStruct_p;

    if(customIDLStruct_p==NULL)
	{
	CORBA::Any tmpEvent;
	tmpEvent <<= customIDLStruct_p;
	acsncErrType::WrongEventReceivedExImpl er(__FILE__, 
						  __LINE__,
						  "nc::SimpleConsumer<T>::push_structured_event");
	er.setExpectedEvent(AnyAide::getId(tmpEvent).c_str());
	er.setReceivedEvent(AnyAide::getId(notification.filterable_data[0].value).c_str());

	er.log();

//	throw CosEventComm::Disconnected(); // it has no effect !
	return;
	}

    acsnc::EventDescription *ed_p = 0, ed;
    ed_p = &ed;
    notification.remainder_of_body >>= ed_p;
    

    integrationLog(std::string("Channel:") + channelName_mp +
		   ", Receiver:" + "Unknown" +
		   ", Publisher:" + (const char*)ed.name +
		   ", Event Type:" + (const char*)notification.header.fixed_header.event_type.type_name);
    
    //now that we have it, hope and pray the user-defined 
    //function doesn't segfault when it gets it...
    try
	{
	//start the profiler
	profiler_mp->start();
	//invoke the handler
	templateFunction_mp(*customIDLStruct_p, handlerParam_mp);
	//stop timing
   if (profiler_mp == 0){
      return;
   }
	ACS::Time acsTime = profiler_mp->stop();
	//reset the profiler to avoid consuming too much memory
	profiler_mp->reset();

	//convert acsTime from 100ns units to floating point seconds
	double secTime = acsTime / 10000000.0;

	//finally check to see if the event processing time was too slow
	if (secTime > maxProcessTime)
	    {
	     ACS_SHORT_LOG((LM_WARNING, 
			    "Took too long to handle an '%s' event: %f seconds.",
			    (const char*)notification.header.fixed_header.event_type.type_name, secTime));

	     ACS_SHORT_LOG((LM_INFO, 
			    "Maximum time to process an event is: %f seconds.", maxProcessTime));
	    }
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR, 
		       "SimpleConsumer:::push_structured_event(...) the '%s' channel: handlerFunction failed!",
		       channelName_mp));
	}
}
//---------------------------------------------------------------
template <class T>
SimpleConsumer<T>::~SimpleConsumer() 
{
}
//---------------------------------------------------------------
template <class T>
void
SimpleConsumer<T>::addSubscription(const char* type_name, 
				   eventHandlerFunction templateFunction, 
				   void *handlerParam)
{
    ACS_TRACE("SimpleConsumer::addSubscription");
    
    //save a reference to the eventHandlerFunction
    templateFunction_mp = templateFunction;
    
    //save the parameter to handler
    handlerParam_mp = handlerParam;
    
    //just call superclass implementation
    Consumer::addSubscription(type_name);
}
//---------------------------------------------------------------
 }; 
#endif
