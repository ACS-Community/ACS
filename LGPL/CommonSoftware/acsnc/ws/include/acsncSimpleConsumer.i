#ifndef SIMPLE_CONSUMER_I
#define SIMPLE_CONSUMER_I
/*    @(#) $Id: acsncSimpleConsumer.i,v 1.15 2005/10/21 14:23:44 dfugate Exp $
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

NAMESPACE_BEGIN(nc);
//---------------------------------------------------------------
template <class T>
SimpleConsumer<T>::SimpleConsumer(const char* channelName) : 
    Consumer(channelName), 
    templateFunction_mp(0) 
{
    init();
}
//---------------------------------------------------------------
template <class T>
SimpleConsumer<T>::SimpleConsumer(const char* channelName, 
			       CORBA::ORB_ptr orb) : 
    Consumer(channelName, orb), 
    templateFunction_mp(0) 
{
    init();
}
//---------------------------------------------------------------
template <class T> 
void 
SimpleConsumer<T>::push_structured_event(const CosNotification::StructuredEvent &notification)
    throw(CORBA::SystemException, CosEventComm::Disconnected)
{
    //update the internal counter
    numEvents_m++;
    
    //extract the correct data first of all
    T *customIDLStruct_p = 0, customIDLStruct;
    customIDLStruct_p = &customIDLStruct;
    notification.filterable_data[0].value >>= customIDLStruct_p;

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
	templateFunction_mp(*customIDLStruct_p, handlerParam_mp);
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
    throw (CORBA::SystemException)
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
NAMESPACE_END(nc);
#endif
