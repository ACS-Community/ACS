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
* "@(#) $Id: basencSupplier.cpp,v 1.12 2009/05/19 17:35:42 javarias Exp $"
*
* who       when        what
* --------  ---------   ----------------------------------------------
*/

#include "basencSupplier.h"
#include <iostream>
#include <logging.h>
#include <ACSErrTypeCORBA.h>

//-----------------------------------------------------------------------------
BaseSupplier::BaseSupplier(const char* channelName, const char* notifyServiceDomainName) :
    BaseHelper(channelName, notifyServiceDomainName)
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
    if(CORBA::is_nil(supplierAdmin_m.in())){
      ACSErrTypeCORBA::CORBAReferenceNilExImpl ex(__FILE__, __LINE__, "BaseSupplier::init");
      ex.setVariable("supplierAdmin_m");
      throw ex;
   }

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
    corbaRef_m = getCORBARef();
	 if(CORBA::is_nil(corbaRef_m.in())){
		 ACSErrTypeCORBA::CORBAReferenceNilExImpl 
			 ex(__FILE__, __LINE__, "BaseSupplier::connect");
		 ex.setVariable("corbaRef_m");
		 throw ex;
	 }
    
    //get the proxy consumer from the supplier admin.
    CosNotifyChannelAdmin::ProxyConsumer_var proxyconsumer =
	supplierAdmin_m->obtain_notification_push_consumer(CosNotifyChannelAdmin::STRUCTURED_EVENT, 
							   proxyConsumerID_m);
	 if(CORBA::is_nil(proxyconsumer.in())){
		 ACSErrTypeCORBA::CORBAReferenceNilExImpl 
			 ex(__FILE__, __LINE__, "BaseSupplier::connect");
		 ex.setVariable("proxyconsumer");
		 throw ex;
	 }

    
    // narrow
    proxyConsumer_m = CosNotifyChannelAdmin::StructuredProxyPushConsumer::_narrow(proxyconsumer.in());
	 if(CORBA::is_nil(proxyConsumer_m.in())){
		 ACSErrTypeCORBA::NarrowFailedExImpl 
			 ex(__FILE__, __LINE__, "BaseSupplier::connect");
		 ex.setNarrowType("CosNotifyChannelAdmin::StructuredProxyPushConsumer");
		 throw ex;
	 }

    //final piece of code connects us to the channel
    proxyConsumer_m->connect_structured_push_supplier(corbaRef_m.in());
}
//-----------------------------------------------------------------------------
void
BaseSupplier::disconnect()
{
    //take care of the proxy consumer
	 if(CORBA::is_nil(proxyConsumer_m.in())){
		 ACSErrTypeCORBA::CORBAReferenceNilExImpl 
			 ex(__FILE__, __LINE__, "BaseSupplier::disconnect");
		 ex.setVariable("proxyConsumer_m");
		 throw ex;
	 }
    //sole ownership of the proxy
    CosNotifyChannelAdmin::StructuredProxyPushConsumer_var proxyConsumer = proxyConsumer_m;
    proxyConsumer_m=CosNotifyChannelAdmin::StructuredProxyPushConsumer::_nil(); 
    //disconnect from the proxy consumer
    proxyConsumer->disconnect_structured_push_consumer();

    //take care of the supplier admin
	 if(CORBA::is_nil(supplierAdmin_m.in())){
		 ACSErrTypeCORBA::CORBAReferenceNilExImpl 
			 ex(__FILE__, __LINE__, "BaseSupplier::disconnect");
		 ex.setVariable("supplierAdmin_m");
		 throw ex;
	 }

    //sole ownership of the proxy
    CosNotifyChannelAdmin::SupplierAdmin_var supplierAdmin = supplierAdmin_m;
    supplierAdmin_m = CosNotifyChannelAdmin::SupplierAdmin::_nil();
    //destroy it
    supplierAdmin->destroy();

    // deactivate our own CORBA servant
    PortableServer::ObjectId_var objectId = this->_default_POA()->servant_to_id(this);
    this->_default_POA()->deactivate_object(*objectId.ptr());

    BaseHelper::disconnect();
}
//-----------------------------------------------------------------------------
void
BaseSupplier::subscription_change(const CosNotification::EventTypeSeq &a,
				  const CosNotification::EventTypeSeq &b)
{
    //No-Op.
}
//-----------------------------------------------------------------------------
void
BaseSupplier::publishEvent(const CosNotification::StructuredEvent& event)
{
	if(CORBA::is_nil(proxyConsumer_m.in())){
		ACSErrTypeCORBA::CORBAReferenceNilExImpl 
			ex(__FILE__, __LINE__, "BaseSupplier::publishEvent");
		ex.setVariable("proxyConsumer_m");
		throw ex;
	}
	//pass it on to the proxy consumer
	try
	{
		proxyConsumer_m->push_structured_event(event);
	}
	catch(CORBA::COMM_FAILURE &ex)
	{
		char msg[512];
		Logging::Logger::LoggerSmartPtr logger = getLogger();
		sprintf(msg, "Failed to send an event of type '%s' to the '%s' channel - CORBA::COMM_FAILURE!",
				event.header.fixed_header.event_type.type_name.in(),
				channelName_mp);
		logger->log(Logging::Logger::LM_ERROR, msg, __FILE__, __LINE__, "BaseSupplier::publishEvent");
	}
	catch(CORBA::TRANSIENT &ex)
	{
		char msg[512];
		Logging::Logger::LoggerSmartPtr logger = getLogger();
		sprintf(msg, "Failed to send an event of type '%s' to the '%s' channel - CORBA::TRANSIENT!",
				event.header.fixed_header.event_type.type_name.in(),
				channelName_mp);
		logger->log(Logging::Logger::LM_ERROR, msg, __FILE__, __LINE__, "BaseSupplier::publishEvent");
	}
	catch(CORBA::UserException &ex)
	{
		char msg[512];
		Logging::Logger::LoggerSmartPtr logger = getLogger();
		sprintf(msg, "Failed to send an event of type '%s' to the '%s' channel - CORBA::UserException(%s)!",
				event.header.fixed_header.event_type.type_name.in(),
				channelName_mp,
				ex._info().c_str());
		logger->log(Logging::Logger::LM_ERROR, msg, __FILE__, __LINE__, "BaseSupplier::publishEvent");
	}
	catch(CORBA::SystemException &ex)
	{
		char msg[512];
		Logging::Logger::LoggerSmartPtr logger = getLogger();
		sprintf(msg, "Failed to send an event of type '%s' to the '%s' channel - CORBA::SystemException(%s)!",
				event.header.fixed_header.event_type.type_name.in(),
				channelName_mp,
				ex._info().c_str());
		logger->log(Logging::Logger::LM_ERROR, msg, __FILE__, __LINE__, "BaseSupplier::publishEvent");
	}
	catch(...)
	{
		char msg[512];
		Logging::Logger::LoggerSmartPtr logger = getLogger();
		sprintf(msg, "Failed to send an event of type '%s' to the '%s' channel - UNKNOWN!",
				event.header.fixed_header.event_type.type_name.in(),
				channelName_mp);
		logger->log(Logging::Logger::LM_ERROR, msg, __FILE__, __LINE__, "BaseSupplier::publishEvent");

	}//try-catch
}
//-----------------------------------------------------------------------------
void
BaseSupplier::disconnect_structured_push_supplier()
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

    event.header.fixed_header.event_type.domain_name = getChannelDomain();
    event.header.fixed_header.event_type.type_name   = getEventType();    
    event.header.fixed_header.event_name             = getEventName();

    //no reason to put anything in the variable header.
    event.header.variable_header.length(0);
}

