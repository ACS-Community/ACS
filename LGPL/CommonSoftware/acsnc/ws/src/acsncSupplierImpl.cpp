/* @(#) $Id: acsncSupplierImpl.cpp,v 1.90 2012/02/02 15:03:01 rtobar Exp $
 *
 *    Structured event push supplier implementation.
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

#include "acsncSupplier.h"
#include <baciCORBA.h>
#include <acscommonC.h>
#include <acsncErrType.h>
#include <AcsNCTraceLog.h>

using namespace ACSErrTypeCommon;

namespace nc {
//-----------------------------------------------------------------------------
Supplier::Supplier(const char* channelName, acscomponent::ACSComponentImpl* component) :
    Helper(channelName),
    SupplierAdmin_m(CosNotifyChannelAdmin::SupplierAdmin::_nil()),
    proxyConsumer_m(CosNotifyChannelAdmin::StructuredProxyPushConsumer::_nil()),
    reference_m(0),
    component_mp(component),
    typeName_mp(0),
    count_m(0),
    guardbl(10000000,50),
    antennaName("")
{
    ACS_TRACE("Supplier::Supplier");
    init(static_cast<CORBA::ORB_ptr>(0));
}
//-----------------------------------------------------------------------------
Supplier::Supplier(const char* channelName, CORBA::ORB_ptr orb_mp, acscomponent::ACSComponentImpl* component) :
    Helper(channelName),
    SupplierAdmin_m(CosNotifyChannelAdmin::SupplierAdmin::_nil()),
    proxyConsumer_m(CosNotifyChannelAdmin::StructuredProxyPushConsumer::_nil()),
    reference_m(0),
    component_mp(component),
    typeName_mp(0),
    count_m(0),
    guardbl(10000000,50)
{
    ACS_TRACE("Supplier::Supplier");
    init(orb_mp);
}
//-----------------------------------------------------------------------------
Supplier::Supplier(const char* channelName, int argc, char *argv[], acscomponent::ACSComponentImpl* component) :
    Helper(channelName),
    SupplierAdmin_m(CosNotifyChannelAdmin::SupplierAdmin::_nil()),
    proxyConsumer_m(CosNotifyChannelAdmin::StructuredProxyPushConsumer::_nil()),
    reference_m(0),
    component_mp(component),
    typeName_mp(0),
    count_m(0),
    guardbl(10000000,50)
{
    ACS_TRACE("Supplier::Supplier");

    // Create our own ORB which will in turn give us a reference to the Naming
    // Service using either the arguments passed in or environment variables.
    if(argc!=0 && (orbHelper_mp==0))
	{
	orbHelper_mp = new ORBHelper(argc, argv);
	}
    else if(orbHelper_mp==0)
	{
	orbHelper_mp = new ORBHelper();
	}
    // Run the orb on a separate thread
    orbHelper_mp->runOrb();

    init(orbHelper_mp->getORB());
}
//-----------------------------------------------------------------------------
void
Supplier::init(CORBA::ORB_ptr orb)
{
    //things we only have to do once.
    event_m.filterable_data.length (1);
    event_m.filterable_data[0].name = acscommon::DEFAULTDATANAME;

    // Resolve the naming service using the orb
    resolveNamingService(orb);

    // If a notification channel already exists, then use it, otherwise
    // Create the NC
    if(!resolveInternalNotificationChannel())
        ACS_SHORT_LOG((LM_ERROR,"NC '%s' couldn't be created nor resolved", channelName_mp));  
    //Finally we can create the supplier admin, consumer proxy, etc.
    createSupplier();
    if(notifyFactory_m == 0)
       resolveNotificationFactory();
    if (orbHelper_mp !=0 )
        callback_m->init(orbHelper_mp->getORB(), notifyFactory_m);
    else
        callback_m->init(orb, notifyFactory_m);
}
//-----------------------------------------------------------------------------
Supplier::~Supplier()
{
    ACS_TRACE("Supplier::~Supplier");
    disconnect();
}
//-----------------------------------------------------------------------------
// TAO Developer's Guide p. 595
void
Supplier::disconnect()
{
    ACS_TRACE("Supplier::disconnect");

    callback_m->disconnect();

    /**
     *  proxyConsumer_m->disconnect_structured_push_consumer should really disconnect the consumer.
     */

    //Take sole ownership of the proxy.
    CosNotifyChannelAdmin::StructuredProxyPushConsumer_var proxyConsumer = proxyConsumer_m;
    proxyConsumer_m=CosNotifyChannelAdmin::StructuredProxyPushConsumer::_nil();

    try
	{
	    if(CORBA::is_nil(proxyConsumer.in()) == false)
		{
		proxyConsumer->disconnect_structured_push_consumer();
		}

	    if(CORBA::is_nil(SupplierAdmin_m.in()) == false)
		{
		    SupplierAdmin_m->destroy();
		    SupplierAdmin_m=CosNotifyChannelAdmin::SupplierAdmin::_nil();
		}
	    BACI_CORBA::DestroyTransientCORBAObject(reference_m.in());
	    if(reference_m.in()!=0)
		{
		reference_m=0;
		//delete this;
		}
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR, "Supplier::disconnect failed for the '%s' channel!",
		       channelName_mp));
	}
}
//-----------------------------------------------------------------------------
void
Supplier::publishEvent(const CORBA::Any &eventData)
{
    populateHeader(eventData);
    publishEvent(event_m);
}
//-----------------------------------------------------------------------------
void
Supplier::publishEvent(const CosNotification::StructuredEvent &event)
{
    //First a sanity check.
    if(CORBA::is_nil(proxyConsumer_m.in()) == true)
	{
    	acsncErrType::PublishEventFailureExImpl ex(__FILE__, __LINE__, "nc::Supplier::publishEvent");
    	ex.setChannelName(channelName_mp);
    	ex.log(LM_DEBUG);
    	throw ex;
	}//if

    CosNotification::StructuredEvent *tmp;
    try
    {
       while( (tmp = eventBuff.front()) != NULL){
          proxyConsumer_m->push_structured_event(*tmp);
          eventBuff.pop();
          delete tmp;
       }
    	// Invoke a method on consumer proxy
    	proxyConsumer_m->push_structured_event(event);
    }
    catch(CORBA::TRANSIENT &ex)
    {
       /* Probably the Notify Service is down.
        * It can be recovery starting the service again.
        * The supplier will store the events.
        */
       eventBuff.push(event);
	   throw ex;
    }
    catch(CosEventComm::Disconnected &ex)
    {
    	ACSErrTypeCommon::CORBAProblemExImpl cex(__FILE__,
    			__LINE__,
    			"nc::SimpleSupplier::publishEvent");
    	cex.setInfo(ex._info().c_str());

    	acsncErrType::PublishEventFailureExImpl ex(cex, __FILE__, __LINE__, "nc::Supplier::publishEvent");
    	ex.setChannelName(channelName_mp);
    	ex.log(LM_DEBUG);
    	throw ex;
    }
    catch(CORBA::SystemException &ex)
    {
    	ACSErrTypeCommon::CORBAProblemExImpl cex(__FILE__,
    			__LINE__,
    			"nc::SimpleSupplier::publishEvent");
    	cex.setMinor(ex.minor());
    	cex.setCompletionStatus(ex.completed());
    	cex.setInfo(ex._info().c_str());

    	acsncErrType::PublishEventFailureExImpl ex(cex, __FILE__, __LINE__, "nc::Supplier::publishEvent");
    	ex.setChannelName(channelName_mp);
    	ex.log(LM_DEBUG);
    	throw ex;
    }
    catch(...)
    {
    	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__, "nc::Supplier::publishEvent");

    	acsncErrType::PublishEventFailureExImpl ex(uex, __FILE__, __LINE__, "nc::Supplier::publishEvent");
    	ex.setChannelName(channelName_mp);
    	ex.log(LM_DEBUG);
    	throw ex;
    }//try-catch
}//publishEvent
//-----------------------------------------------------------------------------
void
Supplier::populateHeader(const CORBA::Any &any)
{
    if (any.type()->kind()!=CORBA::tk_sequence)
	{
	setEventType(any.type()->name());
	}
    else
	{
	std::string etName= acsnc::SEQUENCE_EVENT_TYPE_PREFIX; //_SequenceOf_
	CORBA::Any a;
	a._tao_set_typecode(any.type()->content_type());
	etName+=a.type()->name();
	setEventType(etName.c_str());
	}
    populateHeader(event_m);
    event_m.filterable_data[0].value = any;
}
//-----------------------------------------------------------------------------
void
Supplier::populateHeader(CosNotification::StructuredEvent &event)
{
    event.header.fixed_header.event_type.domain_name = getChannelDomain();
    event.header.fixed_header.event_type.type_name = typeName_mp;//CORBA::string_dup(typeName_mp);

    event.header.fixed_header.event_name = "";

    // if Names has a filterable data entry, then add it here
    event.header.variable_header.length(0);    // put nothing here

    //fill out the event description requested by Executive subsystem
    acsnc::EventDescription descrip;
    //pack in the timestamp
    descrip.timestamp = getTimeStamp();
    //next get the total number of events sent by this supplier
    count_m++;
    descrip.count = count_m;
    //get the component's name.
    if(component_mp == 0)
	{
	descrip.name = "Unknown";
	}
    else
	{
        // here we do not have to do CORBA::string_dup, ....
	// because it is done already in ACSComponentImpl::name()
	descrip.name = component_mp->name();
	}

    integrationLog(std::string("Channel:") + channelName_mp +
		   ", Publisher:" + (const char*)descrip.name +
		   ", Event Type:" + typeName_mp);

    //pack the event description into the event
    event.remainder_of_body <<= descrip;




}
//-----------------------------------------------------------------------------
void
Supplier::destroyNotificationChannel()
{
    ACS_TRACE("Supplier::destroyNotificationChannel");

    //Sanity check
    if (CORBA::is_nil(notifyChannel_m.in())==true)
	{
	ACS_SHORT_LOG((LM_ERROR, "Supplier::destroyNotificationChannel() error occured for the '%s' channel!",
		       channelName_mp));
	CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Supplier::destroyNotificationChannel");
	throw err.getCORBAProblemEx();
	}

    try
	{
	//Destroy the remote object and dereference it's pointer.
	notifyChannel_m->destroy();
	notifyChannel_m = 0;

	// Unbind notification channel from Naming service
	CosNaming::Name name(1);
	name.length(1);
	name[0].id = CORBA::string_dup(getCombinedChannelAndDomainName().c_str());
	name[0].kind = acscommon::NC_KIND;
	ACE_ASSERT(CORBA::is_nil(namingContext_m.in()) == false);
	namingContext_m->unbind(name);
	}
    catch(CORBAProblemEx)
	{
	ACS_SHORT_LOG((LM_ERROR, "Supplier::destroyNotificationChannel() failed for the '%s' channel with an ACS-based exception!",
		       channelName_mp));
	//exception thrown by us...OK to rethrow
	throw;
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR, "Supplier::destroyNotificationChannel() error occured for the '%s' channel!",
		       channelName_mp));
	CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Supplier::destroyNotificationChannel");
	throw err.getCORBAProblemEx();
	}
}
//-----------------------------------------------------------------------------
void
Supplier::createSupplier()
{
    ACS_TRACE("Supplier::createSupplier");

    // TAO Developer's Guide p. 599 - Create Supplier Admin object
    if(CORBA::is_nil(notifyChannel_m.in()) == true)
	{
	ACS_SHORT_LOG((LM_ERROR, "Supplier::createSupplier error occured for the '%s' channel!",
		       channelName_mp));
	CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Supplier::createSupplier");
	throw err.getCORBAProblemEx();
	}

    // CosNotifyChannelAdmin::AdminID adminid;
    // CosNotifyChannelAdmin::ProxyID proxyConsumerID;

    try
	{
	//get a supplier admin
	SupplierAdmin_m = notifyChannel_m->new_for_suppliers(ifgop_m, adminid);
	//sanity check on the supplier admin
	if(CORBA::is_nil(SupplierAdmin_m.in()) == true)
	    {
	    ACS_SHORT_LOG((LM_ERROR, "Supplier::createSupplier error occured for the '%s' channel!",
			   channelName_mp));
	    CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Supplier::createSupplier");
	    throw err.getCORBAProblemEx();
	    }

	// Check if our admin is a proper TAO extension
	// so we can create a named proxy consumer
	bool isAdminExt = false;
	try {
		NotifyMonitoringExt::SupplierAdmin_var supplierAdminExt = NotifyMonitoringExt::SupplierAdmin::_narrow(SupplierAdmin_m);
		isAdminExt = (supplierAdminExt != 0);
	} catch(...) {}

	//get a proxy consumer
	CosNotifyChannelAdmin::ProxyConsumer_var proxyconsumer = 0;
	if( isAdminExt ) {

		char *name = 0;
		if( component_mp != 0 )
			name = component_mp->name();
		else
			name = "Unknown";

		std::stringstream ss;
		ss << name << "-" << ACE_OS::rand();
		ACE_CString	proxyName = ss.str().c_str();
		ACE_OS::srand((unsigned int)ACE_OS::gettimeofday().msec());
		NotifyMonitoringExt::SupplierAdmin_var supplierAdminExt = NotifyMonitoringExt::SupplierAdmin::_narrow(SupplierAdmin_m);

		while( proxyconsumer == 0 ) {
			try {
				proxyconsumer = supplierAdminExt->obtain_named_notification_push_consumer(CosNotifyChannelAdmin::STRUCTURED_EVENT, proxyConsumerID, proxyName.c_str());
	    		//ACS_SHORT_LOG((LM_INFO,"Consumer::createConsumer Got named proxy supplier '%s' with proxyID %d", proxyName.c_str(), proxyconsumerID));
			} catch (NotifyMonitoringExt::NameAlreadyUsed &ex) {
				// If the original name is already in use, append "-<tries>" and try again
				// until we find a free name
				std::stringstream ss;
				ss << name << "-" << ACE_OS::rand();
				proxyName = ss.str().c_str();
			} catch (...) {
				// If any unexpected problem appears, try the unnamed version
				proxyconsumer = SupplierAdmin_m->obtain_notification_push_consumer(CosNotifyChannelAdmin::STRUCTURED_EVENT, proxyConsumerID);
	    		//ACS_SHORT_LOG((LM_INFO,"Consumer::createConsumer Created unnamed proxy supplier"));
			}
		}

	}
	else {
		// Just the unnamed version if we don't have the TAO extensions
		proxyconsumer = SupplierAdmin_m->obtain_notification_push_consumer(CosNotifyChannelAdmin::STRUCTURED_EVENT, proxyConsumerID);
	}

	//sanity check on the consumer admin
	if(CORBA::is_nil(proxyconsumer.in()) == true)
	    {
	    ACS_SHORT_LOG((LM_ERROR, "Supplier::createSupplier error occured for the '%s' channel!",
			   channelName_mp));
	    CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Supplier::createSupplier");
	    throw err.getCORBAProblemEx();
	    }

	//narrow the consumer to a structured proxy
	proxyConsumer_m = CosNotifyChannelAdmin::StructuredProxyPushConsumer::_narrow(proxyconsumer.in());
	//sanity check
	if(CORBA::is_nil(proxyConsumer_m.in()) == true)
	    {
	    ACS_SHORT_LOG((LM_ERROR, "Supplier::createSupplier error occured for the '%s' channel!",
			   channelName_mp));
	    CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Supplier::createSupplier");
	    throw err.getCORBAProblemEx();
	    }

	//activate ourself as a CORBA object
	reference_m = BACI_CORBA::ActivateTransientCORBAObject<CosNotifyComm::StructuredPushSupplier>(this);
	if (reference_m.in()==0)
	    {
	    reference_m = this->_this();
	    }

	//sanity check
	if(CORBA::is_nil(reference_m.in()) == true)
	    {
	    ACS_SHORT_LOG((LM_ERROR, "Supplier::createSupplier error occured for the '%s' channel!",
			   channelName_mp));
	    CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Supplier::createSupplier");
	    throw err.getCORBAProblemEx();
	    }

	//finally attach ourself to the proxy consumer
	proxyConsumer_m->connect_structured_push_supplier(reference_m.in());
	}
    catch(CORBAProblemEx)
	{
	//exception thrown by us...OK to rethrow
	ACS_SHORT_LOG((LM_TRACE, "Supplier::createSupplier nil pointer error occured for the '%s' channel!",
		       channelName_mp));
	throw;
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR, "Supplier::createSupplier unknown error occured for the '%s' channel!",
		       channelName_mp));
	CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Supplier::createSupplier");
	throw err.getCORBAProblemEx();
	}
}

void Supplier::reconnect(::NotifyMonitoringExt::EventChannelFactory *ecf)
{
   Helper::reconnect(ecf);

   if (::CORBA::is_nil(SupplierAdmin_m))
      SupplierAdmin_m = notifyChannel_m->get_supplieradmin(adminid);

	SupplierAdmin_m->set_qos(getAdminProps());

   if(::CORBA::is_nil(proxyConsumer_m))
      proxyConsumer_m = 
         CosNotifyChannelAdmin::StructuredProxyPushConsumer::_narrow(
               SupplierAdmin_m->get_proxy_consumer(proxyConsumerID));
}

void Supplier::setAntennaName(std::string antennaName) {
    //If the antenna name is already set, do nothing
    if (this->antennaName.compare("") != 0)
        return;
    this->antennaName = antennaName;
    if (antennaName.compare("") != 0) {
        std::cout << "Adding filter field antenna_name" << std::endl;
        event_m.filterable_data.length(2);
        event_m.filterable_data[1].name = CORBA::string_dup("antenna_name");
        event_m.filterable_data[1].value <<= antennaName.c_str();
    }
}

//-----------------------------------------------------------------------------
void
Supplier::disconnect_structured_push_supplier()
{
    ACS_TRACE("Supplier::disconnect_structured_push_supplier");
}
//-----------------------------------------------------------------------------
/*
* @throw CosNotifyComm::InvalidEventType
*/
void
Supplier::subscription_change(const CosNotification::EventTypeSeq &added,
			      const CosNotification::EventTypeSeq &removed)
{
    ACS_TRACE("Supplier::subscription_change");
    ACE_UNUSED_ARG(added);
    ACE_UNUSED_ARG(removed);
}
//-----------------------------------------------------------------------------
void
Supplier::setEventType(const char* typeName)
{
    typeName_mp = CORBA::string_dup(typeName);
}
//-----------------------------------------------------------------------------

 };

















