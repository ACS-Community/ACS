/* @(#) $Id: acsncConsumerImpl.cpp,v 1.80 2011/12/15 12:54:53 rtobar Exp $
 *
 *    Implementation of abstract base class Consumer.
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
//-----------------------------------------------------------------------------
#include "acsncConsumer.h"
#include <baciThread.h>
#include <baciCORBA.h>
#include <acscommonC.h>
using namespace ACSErrTypeCommon;
//-----------------------------------------------------------------------------
namespace nc {
//-----------------------------------------------------------------------------
double Consumer::DEFAULT_MAX_PROCESS_TIME = 2.0;
//-----------------------------------------------------------------------------
Consumer::Consumer(const char* channelName, const char* acsNCDomainName) :
    Helper(channelName,acsNCDomainName),
    consumerAdmin_m(0),
    proxySupplier_m(0),
    numEvents_m(0),
    reference_m(0),
    profiler_mp(0),
    antennaName(""),
    orb_mp(0)
{
    ACS_TRACE("Consumer::Consumer");
    orb_mp = static_cast<CORBA::ORB_ptr>(0);
}
//-----------------------------------------------------------------------------
Consumer::Consumer(const char* channelName, CORBA::ORB_ptr orb, const char* acsNCDomainName) : 
    Helper(channelName,acsNCDomainName),
    consumerAdmin_m(0),
    proxySupplier_m(0),
    numEvents_m(0),
    reference_m(0),
    profiler_mp(0),
    orb_mp(0)
{
    ACS_TRACE("Consumer::Consumer");
    orb_mp = orb;
}
//-----------------------------------------------------------------------------
Consumer::Consumer(const char* channelName, int argc, char *argv[], const char* acsNCDomainName) : 
    Helper(channelName,acsNCDomainName),
    consumerAdmin_m(0),
    proxySupplier_m(0),
    numEvents_m(0),
    reference_m(0),
    profiler_mp(0),
    orb_mp(0)
{
    ACS_TRACE("Consumer::Consumer");
    
    //Create an ORB to discover where the Naming Service is running for 
    //ourselves
    if(argc!=0 && (orbHelper_mp==0))
	{
	orbHelper_mp = new ORBHelper(argc, argv);
	}
    else if(orbHelper_mp == 0)
	{
	orbHelper_mp = new ORBHelper();
	}
    orbHelper_mp->runOrb();
    orb_mp = orbHelper_mp->getORB();
}
//-----------------------------------------------------------------------------
void
Consumer::init()
{    
    //just delegate to other signature
    init(orb_mp);
}
//-----------------------------------------------------------------------------
void
Consumer::init(CORBA::ORB_ptr orb)
{    
    //setup profiling stuff here
    handlerTimeoutMap_m = CDBProperties::getEventHandlerTimeoutMap(channelName_mp);
    profiler_mp = new Profiler();

    // Must call resolveNamingService B-4 resolveNotifyChannel!
    // using activator's orb
    resolveNamingService(orb);
    // Create the NC
    if(!resolveInternalNotificationChannel())
        ACS_SHORT_LOG((LM_ERROR,"NC '%s' couldn't be created nor resolved", channelName_mp));  
    //create consumer corba objects
    createConsumer();

    if(notifyFactory_m == 0)
       resolveNotificationFactory();

    if (orbHelper_mp != 0 )
        callback_m->init(orbHelper_mp->getORB(), notifyFactory_m);
    else
       callback_m->init(orb, notifyFactory_m);
}
//-----------------------------------------------------------------------------
void 
Consumer::disconnect()
{
    ACS_TRACE("Consumer::disconnect");

    callback_m->disconnect();

    if(reference_m.in()!=0)
	{  
	//suspend the connection first
	try
	    {
	    suspend();
	    }
	catch(...)
	    {
	    //if an exception is thrown, we don't really care
	    ACS_SHORT_LOG((LM_INFO,"Consumer::disconnect failed to suspend subscriptions for the '%s' channel!",
			   channelName_mp));
	    }
	
	//remove all subscriptions
	try
	    {
	    CosNotification::EventTypeSeq added(0);
	    CosNotification::EventTypeSeq removed(1);
	    added.length(0);
	    removed.length(1);
	    // remove subscription from this publisher
	    removed[0].domain_name = CORBA::string_dup("*");
	    removed[0].type_name   = CORBA::string_dup("*");
	    consumerAdmin_m->subscription_change(added, removed);
	    //just in case...
	    added.length(0);
	    removed.length(0);
	    }
	catch(...)
	    {
	    //if an exception is thrown, we don't really care
	    ACS_SHORT_LOG((LM_INFO,"Consumer::disconnect failed to remove subscriptions for the '%s' channel!",
			   channelName_mp));
	    }
	
	//Take sole ownership of the supplier proxy.
	CosNotifyChannelAdmin::StructuredProxyPushSupplier_var proxySupplier = proxySupplier_m;
	proxySupplier_m=CosNotifyChannelAdmin::StructuredProxyPushSupplier::_nil();
	
	try
	    {
	    if(CORBA::is_nil(proxySupplier.in()) == false)
		{
		proxySupplier->disconnect_structured_push_supplier();
		}
	    if(CORBA::is_nil(consumerAdmin_m.in()) == false)
		{
		consumerAdmin_m->destroy();
		consumerAdmin_m=CosNotifyChannelAdmin::ConsumerAdmin::_nil();
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
	    ACS_SHORT_LOG((LM_ERROR,"Consumer::disconnect failed for the '%s' channel!",
			   channelName_mp));
	    }
	}

    if (profiler_mp!=0)
	{
	delete profiler_mp;
	profiler_mp = 0;
	}
    ACS_TRACE("Consumer::disconnect");
}
//-----------------------------------------------------------------------------
void 
Consumer::consumerReady()
{
    ACS_TRACE("Consumer::consumerReady");
    
    resolveNotifyChannel();
    
    try
	{
	//activate ourself as a CORBA object
	reference_m = BACI_CORBA::ActivateTransientCORBAObject<CosNotifyComm::StructuredPushConsumer>(this);
	if (reference_m.in()==0)
	    {
	    CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Consumer::consumerReady");
	    throw err.getCORBAProblemEx();
	    }
	
	// Connect consumer to the proxy supplier & we should be ready to receive events
	proxySupplier_m->connect_structured_push_consumer(reference_m.in());
	}
    catch(...)
	{
	CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Consumer::consumerReady");
	throw err.getCORBAProblemEx();
	}
}
//-----------------------------------------------------------------------------
void 
Consumer::resume()
{
    ACS_TRACE("Consumer::resume");
    
    try
	{
	proxySupplier_m->resume_connection();
	}
    catch(CosNotifyChannelAdmin::ConnectionAlreadyActive e)
	{
	//OK to ignore
	ACS_SHORT_LOG((LM_INFO,"Consumer::resume failed because already resumed for the '%s' channel!",
		       channelName_mp));
	}
    catch(CosNotifyChannelAdmin::NotConnected e)
	{
	//OK to ignore
	ACS_SHORT_LOG((LM_INFO,"Consumer::resume failed because not connected to the '%s' channel!",
		       channelName_mp));
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_INFO,"Consumer::resume failed for the '%s' channel!",
		       channelName_mp));
	CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Consumer::resume");
	throw err.getCORBAProblemEx();
	}
}
//-----------------------------------------------------------------------------
void 
Consumer::suspend()
{
    ACS_TRACE("Consumer::suspend");
    
    try
	{
	proxySupplier_m->suspend_connection();
	}
    catch(CosNotifyChannelAdmin::ConnectionAlreadyInactive e)
	{
	//OK to ignore
	ACS_SHORT_LOG((LM_INFO,"Consumer::suspend failed because already suspended for the '%s' channel!",
		       channelName_mp));
	}
    catch(CosNotifyChannelAdmin::NotConnected e)
	{
	//OK to ignore
	ACS_SHORT_LOG((LM_INFO,"Consumer::suspend failed because not connected to the '%s' channel!",
		       channelName_mp));
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_INFO,"Consumer::suspend failed for the '%s' channel!",
		       channelName_mp));
	CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Consumer::suspend");
	throw err.getCORBAProblemEx();
	}
}
//-----------------------------------------------------------------------------
void 
Consumer::addSubscription(const char* type_name)
{
	ACS_TRACE("Consumer::addSubscription");

	CosNotification::EventTypeSeq added(1);
	CosNotification::EventTypeSeq removed(0);
	added.length(1);
	removed.length(0);
   
	added[0].domain_name = getChannelDomain();
	added[0].type_name   = CORBA::string_dup(type_name);
   
	ACS_SHORT_LOG((LM_INFO, "Consumer::addSubscription subscribing to '%s' events for the '%s' channel!",
	static_cast<const char *>(added[0].type_name), channelName_mp));
	try
	{
		if(CORBA::is_nil(consumerAdmin_m)) 
		{
			// log an error, then throw an exception to prevent segfault dereferencing nil consumerAdmin_m 
			ACS_SHORT_LOG((LM_ERROR,"Consumer::addSubscription failing due to nil consumerAdmin_m - was init() called?"));
			throw new std::invalid_argument("Consumer::addSubscription failing due to nil consumerAdmin_m");
		}

		consumerAdmin_m->subscription_change(added, removed);
	} 
	catch(...)
	{
		ACS_SHORT_LOG((LM_ERROR,"Consumer::addSubscription failed for the '%s' channel and '%s' event type!",
		channelName_mp, type_name));
		CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Consumer::addSubscription");
		throw err.getCORBAProblemEx();
	}
}
//-----------------------------------------------------------------------------
void 
Consumer::removeSubscription(const char* type_name)
{
    ACS_TRACE("Consumer::removeSubscription");
    
    
    CosNotification::EventTypeSeq added(0);
    CosNotification::EventTypeSeq removed(1);
    added.length(0);
    removed.length(1);
    
    removed[0].domain_name = getChannelDomain();
    removed[0].type_name   = CORBA::string_dup(type_name);
    
    ACS_SHORT_LOG((LM_INFO, "Consumer::removeSubscription unsubscribing from '%s' events for the '%s' channel!",
		   static_cast<const char *>(added[0].type_name), channelName_mp));
    
    try
	{
	consumerAdmin_m->subscription_change(added, removed);
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"Consumer::removeSubscription failed for the '%s' channel and '%s' event type!",
		       channelName_mp, type_name));
	CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Consumer::removeSubscription");
	throw err.getCORBAProblemEx();
	}
}
//-----------------------------------------------------------------------------
int
Consumer::addFilter(const char* type_name,
		    const char* filterString)
{
    ACS_TRACE("Consumer::addFilter");
    
    try
	{
	//Create a temporary filter factory
	CosNotifyFilter::FilterFactory_var filterFactory = notifyChannel_m->default_filter_factory();
	if(CORBA::is_nil(filterFactory.in()) == true)
	    {
	    CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Consumer::addFilter");
	    throw err.getCORBAProblemEx();
	    }
	
	//Create a filter
	CosNotifyFilter::Filter_var filter = 0;
	filter = filterFactory->create_filter(getFilterLanguage());
	if(filter.in() == 0)
	    {
	    CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Consumer::addFilter");
	    throw err.getCORBAProblemEx();
	    }
	
	//Create the constraint expression sequence
	CosNotifyFilter::ConstraintExpSeq cexp(1);
	cexp.length(1);
	cexp[0].event_types.length(1);
	cexp[0].event_types[0].domain_name = getChannelDomain();
	cexp[0].event_types[0].type_name   = CORBA::string_dup(type_name);
	cexp[0].constraint_expr            = CORBA::string_dup(filterString);
	
	try
	    {
	    filter->add_constraints(cexp);
	    }
	catch(CosNotifyFilter::InvalidConstraint e)
	    {
	    if (filter.in() != 0)
		{
		filter->destroy();
		filter = 0;
		}
	    
	    ACS_SHORT_LOG((LM_ERROR,"Consumer::addFilter failed for the '%s' channel, '%s' event type, and '%s' filter!",
			   channelName_mp, type_name, filterString));
	    CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Consumer::addFilter");
	    throw err.getCORBAProblemEx();
	    }
	
	return proxySupplier_m->add_filter(filter._retn());
	}
    catch(CORBAProblemEx)
	{
	ACS_SHORT_LOG((LM_TRACE,"Consumer::addFilter failed for the '%s' channel, '%s' event type, and '%s' filter with a nil pointer!",
		       channelName_mp, type_name, filterString));
	throw;
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"Consumer::addFilter failed for the '%s' channel, '%s' event type, and '%s' filter!",
		       channelName_mp, type_name, filterString));
	CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Consumer::addFilter");
	throw err.getCORBAProblemEx();
	}
}
//-----------------------------------------------------------------------------
bool
Consumer::removeFilter(int filter_id)
{
    ACS_TRACE("Consumer::removeFilter");
    
    try
	{
	proxySupplier_m->remove_filter(filter_id);    
	return true;
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR, "Consumer::removeFilter failed for the '%s' channel and '%d' filter ID!",
		       channelName_mp, filter_id));
	CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Consumer::removeFilter");
	throw err.getCORBAProblemEx();
	return false;
	}
    
}
//-----------------------------------------------------------------------------
void 
Consumer::disconnect_structured_push_consumer()
{
    ACS_TRACE("Consumer::disconnect_structured_push_consumer");
}
//-----------------------------------------------------------------------------
void 
Consumer::createConsumer()
{
    ACS_TRACE("Consumer::createConsumer");
    
    try
	{
	// Get ConsumerAdmin object
	//CosNotifyChannelAdmin::AdminID adminid;
	consumerAdmin_m = notifyChannel_m->new_for_consumers(ifgop_m, adminid);
	
	if(CORBA::is_nil(consumerAdmin_m.in()) == true)
	    {
	    CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Consumer::createConsumer");
	    throw err.getCORBAProblemEx();
	    }


	// get the the proxySupplier (named if possible)
	bool isAdminExt = false;
	try {
		NotifyMonitoringExt::ConsumerAdmin_var consumerAdminExt = NotifyMonitoringExt::ConsumerAdmin::_narrow(consumerAdmin_m);
		isAdminExt = (consumerAdminExt != 0);
	} catch(...) {}

	CosNotifyChannelAdmin::ProxySupplier_var proxySupplier = 0;
	if( isAdminExt && (callback_m->services_ != 0) ) {

		ACE_OS::srand((unsigned int)ACE_OS::gettimeofday().msec());
		std::stringstream ss;
		ss << callback_m->services_->getName() << "-" << ACE_OS::rand();
		ACE_CString proxyName = ss.str().c_str();
		NotifyMonitoringExt::ConsumerAdmin_var consumerAdminExt = NotifyMonitoringExt::ConsumerAdmin::_narrow(consumerAdmin_m);

		while( proxySupplier == 0 ) {
			try {
				proxySupplier = consumerAdminExt->obtain_named_notification_push_supplier(CosNotifyChannelAdmin::STRUCTURED_EVENT, proxySupplierID, proxyName.c_str());
	    		//ACS_SHORT_LOG((LM_INFO,"Consumer::createConsumer Got named proxy supplier '%s' with proxyID %d", proxyName.c_str(), proxySupplierID));
			} catch (NotifyMonitoringExt::NameAlreadyUsed &ex) {
				// If the original name is already in use, append "-<tries>" and try again
				// until we find a free name
				std::stringstream ss;
				ss << callback_m->services_->getName() << "-" << ACE_OS::rand();
				proxyName = ss.str().c_str();
			} catch (...) {
				// If any unexpected problem appears, try the unnamed version
				proxySupplier = consumerAdmin_m->obtain_notification_push_supplier(CosNotifyChannelAdmin::STRUCTURED_EVENT, proxySupplierID);
	    		//ACS_SHORT_LOG((LM_INFO,"Consumer::createConsumer Created unnamed proxy supplier"));
			}
		}
	}
	else {
		// Just the unnamed version if we don't have the TAO extensions
		proxySupplier = consumerAdmin_m->obtain_notification_push_supplier(CosNotifyChannelAdmin::STRUCTURED_EVENT, proxySupplierID);
		//ACS_SHORT_LOG((LM_INFO,"Consumer::createConsumer Created unnamed proxy supplier"));
	}

	if(CORBA::is_nil(proxySupplier.in()) == true)
	{
		CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Consumer::createConsumer");
		throw err.getCORBAProblemEx();
	}
	
	//narrow to a STRUCTURED proxy supplier
	proxySupplier_m = CosNotifyChannelAdmin::StructuredProxyPushSupplier::_narrow(proxySupplier.in());
	}
    catch(CORBAProblemEx)
	{
	ACS_SHORT_LOG((LM_ERROR,"Consumer::createConsumer failed for the '%s' channel due to a nil pointer!",
		       channelName_mp));
	throw;
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"Consumer::createConsumer failed for the '%s' channel due to some unknown reason!",
		       channelName_mp));
	CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Consumer::createConsumer");
	throw err.getCORBAProblemEx();
	}
    
    //now the developer must call consumerReady() to receive events.
}

void Consumer::reconnect(::NotifyMonitoringExt::EventChannelFactory *ecf)
{
   Helper::reconnect(ecf);
   
   if (::CORBA::is_nil(consumerAdmin_m))
      consumerAdmin_m = notifyChannel_m->get_consumeradmin(adminid);
  
	consumerAdmin_m->set_qos(getAdminProps());

   if(::CORBA::is_nil(proxySupplier_m))
      proxySupplier_m = 
         CosNotifyChannelAdmin::StructuredProxyPushSupplier::_narrow(
               consumerAdmin_m->get_proxy_supplier(proxySupplierID));

}

void Consumer::setAntennaName(std::string antennaName) {
    //If the antenna name is already set, do nothing
    if (this->antennaName.compare("") != 0)
        return;
    this->antennaName = antennaName;
    if (antennaName.compare("") != 0) {
        std::cout << "Adding filter" << std::endl;
        CosNotifyFilter::FilterFactory_var filter_factory =
                notifyChannel_m->default_filter_factory();
        CosNotifyFilter::Filter_var filter = filter_factory->create_filter(
                "ETCL");
        if (CORBA::is_nil(filter)) {
            ACS_SHORT_LOG(
                    (LM_ERROR,"Consumer::createConsumer failed for the '%s' channel due the filter cannot be created!", channelName_mp));
        }
        CosNotifyFilter::ConstraintExpSeq constraint_list;
        constraint_list.length(1);
        constraint_list[0].event_types.length(0);
        std::string filter_expr = "$antenna_name == '" + antennaName + "'";
        std::cout << filter_expr << std::endl;
        constraint_list[0].constraint_expr = CORBA::string_dup(
                filter_expr.c_str());
        filter->add_constraints(constraint_list);
        proxySupplier_m->add_filter(filter.in());
    }
}

//-----------------------------------------------------------------------------
/*
* @throw CosNotifyComm::InvalidEventType
*/
void 
Consumer::offer_change(const CosNotification::EventTypeSeq &added,
		       const CosNotification::EventTypeSeq &removed)
{
    ACS_TRACE("Consumer::offer_change");
    ACE_UNUSED_ARG(added);
    ACE_UNUSED_ARG(removed);
    //This method should be overriden to be used.
}
//-----------------------------------------------------------------------------
const char* 
Consumer::getFilterLanguage()
{
    //return a constant defined in acsnc.idl to be portable in the other 
    //programming languages supported by ACS.
    return acsnc::FILTER_LANGUAGE_NAME;
}
//-----------------------------------------------------------------------------
 }; 
