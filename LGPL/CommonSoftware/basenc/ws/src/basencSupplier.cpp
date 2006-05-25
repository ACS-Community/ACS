/*******************************************************************************
*     ALMA - Atacama Large Millimiter Array
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
* "@(#) $Id: basencSupplier.cpp,v 1.5 2006/05/25 16:53:58 dfugate Exp $"
*
* who       when        what
* --------  ---------   ----------------------------------------------
*/

#include "basencSupplier.h"
#include <iostream>

//-----------------------------------------------------------------------------
BaseSupplier::BaseSupplier(const char* channelName) :
    BaseHelper(channelName)
{
    //no-op
}
//-----------------------------------------------------------------------------
void
BaseSupplier::init(CosNaming::NamingContext_ptr nc_p)
{
    //delegate to common helper class
    BaseHelper::init(nc_p);
     
    //get the supplier admin
    supplierAdmin_m = notifyChannel_m->new_for_suppliers(ifgop_m, adminID_m);
    ACE_ASSERT (!CORBA::is_nil(supplierAdmin_m.in()));
    
    //connect the supplier admin which really means connect 
    //ourselves to the proxy consumer
    this->connect();
    
}
//-----------------------------------------------------------------------------
BaseSupplier::~BaseSupplier()
{
}
//-----------------------------------------------------------------------------
void
BaseSupplier::connect()
{
    //turn ourselves into a CORBA object
    corbaRef_m = acsnc::OSPushSupplier::_duplicate(getCORBARef());
    ACE_ASSERT(!CORBA::is_nil(corbaRef_m.in()));
    
    //get the proxy consumer from the supplier admin.
    CosNotifyChannelAdmin::ProxyConsumer_var proxyconsumer =
	supplierAdmin_m->obtain_notification_push_consumer(CosNotifyChannelAdmin::STRUCTURED_EVENT, 
							   proxyConsumerID_m);
    ACE_ASSERT(!CORBA::is_nil(proxyconsumer.in()));
    
    // narrow
    proxyConsumer_m = CosNotifyChannelAdmin::StructuredProxyPushConsumer::_narrow(proxyconsumer.in());
    ACE_ASSERT(!CORBA::is_nil(proxyConsumer_m.in()));

    //final piece of code connects us to the channel
    proxyConsumer_m->connect_structured_push_supplier(corbaRef_m.in());
}
//-----------------------------------------------------------------------------
void
BaseSupplier::disconnect()
{
    //take care of the proxy consumer
    ACE_ASSERT(!CORBA::is_nil(proxyConsumer_m.in()));
    //sole ownership of the proxy
    CosNotifyChannelAdmin::StructuredProxyPushConsumer_var proxyConsumer = proxyConsumer_m;
    proxyConsumer_m=CosNotifyChannelAdmin::StructuredProxyPushConsumer::_nil(); 
    //disconnect from the proxy consumer
    proxyConsumer->disconnect_structured_push_consumer();

    //take care of the supplier admin
    ACE_ASSERT(!CORBA::is_nil(supplierAdmin_m.in()));
    //sole ownership of the proxy
    CosNotifyChannelAdmin::SupplierAdmin_var supplierAdmin = supplierAdmin_m;
    supplierAdmin_m = CosNotifyChannelAdmin::SupplierAdmin::_nil();
    //destroy it
    supplierAdmin->destroy();

    //DWF-need some code here to remove our own CORBA servant???

    BaseHelper::disconnect();
}
//-----------------------------------------------------------------------------
void
BaseSupplier::subscription_change(const CosNotification::EventTypeSeq &a,
				  const CosNotification::EventTypeSeq &b)
    throw(CORBA::SystemException,
	  CosNotifyComm::InvalidEventType)
{
    //No-Op.
}
//-----------------------------------------------------------------------------
void
BaseSupplier::publishEvent(const CosNotification::StructuredEvent& event)
{
    ACE_ASSERT(!CORBA::is_nil(this->proxyConsumer_m.in()));
    //pass it on to the proxy consumer
    try
	{
	proxyConsumer_m->push_structured_event(event);
	}
    catch(CORBA::COMM_FAILURE ex)
	{
	std::cerr << "ERROR (CORBA::COMM_FAILURE): ";
	std::cerr << "failed to send an event of type '";
	std::cerr << event.header.fixed_header.event_type.type_name;
	std::cerr << "' to the '";
	std::cerr << channelName_mp << "' channel!" << std::endl;
	}
    catch(CORBA::TRANSIENT ex)
	{
	std::cerr << "ERROR (CORBA::TRANSIENT): ";
	std::cerr << "failed to send an event of type '";
	std::cerr << event.header.fixed_header.event_type.type_name;
	std::cerr << "' to the '";
	std::cerr << channelName_mp << "' channel!" << std::endl;
	}
    catch(...)
	{
	std::cerr << "ERROR (Unkwown): ";
	std::cerr << "failed to send an event of type '";
	std::cerr << event.header.fixed_header.event_type.type_name;
	std::cerr << "' to the '";
	std::cerr << channelName_mp << "' channel!" << std::endl;
	}
}
//-----------------------------------------------------------------------------
void
BaseSupplier::disconnect_structured_push_supplier()
    throw(CORBA::SystemException)
{
    // No-Op.
}
//-----------------------------------------------------------------------------
acsnc::OSPushSupplier_ptr
BaseSupplier::getCORBARef()
{
    acsnc::OSPushSupplier_var retVal = this->_this();
    return retVal._retn();
}
//-----------------------------------------------------------------------------
void
BaseSupplier::populateHeader(CosNotification::StructuredEvent& event)
{
    //just delegate this to other functions
    event.header.fixed_header.event_type.domain_name = CORBA::string_dup(getChannelDomain());
    event.header.fixed_header.event_type.type_name   = CORBA::string_dup(getEventType());    
    event.header.fixed_header.event_name             = CORBA::string_dup(getEventName());

    //no reason to put anything in the variable header.
    event.header.variable_header.length(0);
}

