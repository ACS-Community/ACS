/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
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
* "@(#) $Id: loggingClient.cpp,v 1.42 2005/12/12 21:13:10 dfugate Exp $"
*
* who       when        what
* --------  ---------   ----------------------------------------------
* msekoran  2002-02-03  disconnection added
* msekoran  2001-12-23  using LoggingHelper::resolveNameService
* msekoran  2001-07-12  created
*/

/// TBD: proper shutdown (disconnection of the consumers) && cleanup

#include <vltPort.h>
#include <loggingClient.h>

#include <loggingHelper.h>

#include <acscommonC.h>

#include <acsutilAnyAide.h>
#include <acsutilTimeStamp.h>

Subscribe::Subscribe (void)
{
  // No-Op.
  ifgop_ = CosNotifyChannelAdmin::OR_OP;
}

Subscribe::~Subscribe ()
{
}

void
Subscribe::init (int argc, char *argv [])
{
  init_ORB (argc, argv);
  
  /*
   * Here we use directly ACS_DEBUG macros.
   * We do not want to use ACS logging macros, because
   * our purpose in life is to show the messages coming from
   * the ACS logging system, and not to add output in that 
   * very same place
   */
  ACE_DEBUG((LM_DEBUG, "Resolving Naming Service...\n"));
  resolve_naming_service ();
  

  ACE_DEBUG((LM_DEBUG, "Resolving Notify Channel...\n"));
  resolve_notify_channel (argv[1]);
  

  create_consumeradmin ();
  

  create_consumers ();
  

  setup_events();
  
}

void
Subscribe::run ()
{
  ACE_ERROR ((LM_DEBUG, "ACS Notify Push Subscriber Client is running...\n"));

  try
    {
      this->orb_->run ();
      
    }
  catch(...)
    {
    }
}

void
Subscribe::init_ORB (int argc,
                      char *argv []
                      )
{
  this->orb_ = CORBA::ORB_init (argc,
                                argv,
                                ""
                                );
  

  CORBA::Object_ptr poa_object  =
    this->orb_->resolve_initial_references("RootPOA"
                                           );
  

  if (CORBA::is_nil (poa_object))
    {
      ACE_ERROR ((LM_ERROR, " (%P|%t) Unable to initialize the POA.\n"));
      return;
    }
  this->root_poa_ =
    PortableServer::POA::_narrow (poa_object);
  

  PortableServer::POAManager_var poa_manager =
    root_poa_->the_POAManager ();
  

  poa_manager->activate ();
  
}

void
Subscribe::shutdown ()
{
    try
      {
	this->consumer_->disconnect ();
	
      }
    catch(...)
      {
      ACE_ERROR((LM_ERROR, "Failed to disconnect CosNotifyComm::StructuredPushConsumer from the Notify Service."));
      }
    
  // shutdown the ORB.
  if (!CORBA::is_nil (this->orb_.in ()))
    {
      this->orb_->shutdown (true);
      
    }
}

void
Subscribe::resolve_naming_service ()
{
    this->naming_context_ = LoggingHelper::resolveNameService(this->orb_.in());
    if (naming_context_.ptr() == CosNaming::NamingContext::_nil())
	throw CORBA::UNKNOWN();

}

void
Subscribe::resolve_notify_channel (const char * channel_name)
{
  ACE_DEBUG((LM_DEBUG, "Notify Channel: %s\n", channel_name));

  CosNaming::Name name (1);
  name.length (1);
  name[0].id = CORBA::string_dup (channel_name);
  
  if(ACE_OS::strcmp(channel_name, acscommon::LOGGING_CHANNEL_NAME)==0)
      {
      name[0].kind = acscommon::LOGGING_CHANNEL_KIND;
      }
  else if(ACE_OS::strcmp(channel_name, acscommon::ARCHIVING_CHANNEL_NAME)==0)
      {
      name[0].kind = acscommon::ARCHIVING_CHANNEL_KIND;
      }
  else
      {
      //just pass
      }


  CORBA::Object_var obj =
    this->naming_context_->resolve (name
                                   );
  

  this->ec_ =
    CosNotifyChannelAdmin::EventChannel::_narrow (obj.in ()
						  );
  
}

void
Subscribe::create_consumeradmin ()
{
  CosNotifyChannelAdmin::AdminID adminid;

  consumer_admin_ =
    ec_->new_for_consumers (this->ifgop_, adminid);
  

  ACE_ASSERT (!CORBA::is_nil (consumer_admin_.in ()));
}

void
Subscribe::create_consumers ()
{
  consumer_ = new ACSStructuredPushConsumer (this);
  consumer_->connect (this->consumer_admin_.in ()
                        );
  
}

void
Subscribe::setup_events ()
{
  // Setup the CA to receive event_type
  CosNotification::EventTypeSeq added(1);
  CosNotification::EventTypeSeq removed (0);
  added.length (1);
  removed.length (0);

  // We will listen only specified domain.
  added[0].domain_name =  CORBA::string_dup ("*");
  added[0].type_name = CORBA::string_dup ("*");

  this->consumer_admin_->subscription_change (added, removed);
  

  // Setup the Consumer to receive event_type
  //  this->consumer_->get_proxy_supplier ()->subscription_change (added, removed,
  //						       );
  //

}

/*****************************************************************/
ACSStructuredPushConsumer::ACSStructuredPushConsumer (Subscribe* subscribe)
  : subscribe_ (subscribe)
{
}

ACSStructuredPushConsumer::~ACSStructuredPushConsumer ()
{
}

void
ACSStructuredPushConsumer::connect (CosNotifyChannelAdmin::ConsumerAdmin_ptr consumer_admin
				    )
{
  // Activate the consumer with the default_POA_
  CosNotifyComm::StructuredPushConsumer_var objref =
    this->_this ();
  

  CosNotifyChannelAdmin::ProxySupplier_var proxysupplier =
    consumer_admin->obtain_notification_push_supplier (CosNotifyChannelAdmin::STRUCTURED_EVENT, proxy_supplier_id_);
  

  ACE_ASSERT (!CORBA::is_nil (proxysupplier.in ()));

  // narrow
  this->proxy_supplier_ =
    CosNotifyChannelAdmin::StructuredProxyPushSupplier::
    _narrow (proxysupplier.in ());
  

  ACE_ASSERT (!CORBA::is_nil (proxy_supplier_.in ()));

  proxy_supplier_->connect_structured_push_consumer (objref.in ()
                                                     );
  
}

void
ACSStructuredPushConsumer::disconnect ()
{
  this->proxy_supplier_->
    disconnect_structured_push_supplier();
  
}

void
ACSStructuredPushConsumer::offer_change (const CosNotification::EventTypeSeq & /*added*/,
					 const CosNotification::EventTypeSeq & /*removed*/
					 
)
      throw (
        CORBA::SystemException,
        CosNotifyComm::InvalidEventType
      )
{
  // No-Op.
}

void
ACSStructuredPushConsumer::push_structured_event (const CosNotification::StructuredEvent & notification
						  )
  throw (
                   CORBA::SystemException,
                   CosEventComm::Disconnected
                   )
{
	
 // "Logging" or "Archiving"
  const char * domain_name =
    notification.header.fixed_header.event_type.domain_name;

  // if "Logging" -> ""
  // if "Archiving" -> "string" | "long" | "double"
  const char * type_name =
    notification.header.fixed_header.event_type.type_name;

  //ACE_OS::printf("\nReceived event, domain = %s, type = %s\n", domain_name, type_name);

  if (ACE_OS::strcmp(domain_name, "Logging")==0)
  {
    // for logging
    const char * xmlLog;
    notification.remainder_of_body >>= xmlLog;
    if (xmlLog)
	{
	ACE_OS::printf("%s\n", xmlLog);
	ACE_OS::fflush (stdout);
	}
  }
  else if (ACE_OS::strcmp(domain_name, "Archiving")==0)
      {
      std::string eventName = (const char *)notification.header.fixed_header.event_name;
      std::string containerName = "";
      std::string deviceName    = "";
      std::string parameterName = "";
      
      //if the first element of eventName is not ':'
      if(eventName.at(0) != ':')
	  {
	  //then it must be the container name.
	  containerName = eventName.substr(0, eventName.find(':'));
	  }
      //remove container name and the ':'
      eventName = eventName.substr(eventName.find(':')+1);
      
      //get the device name
      deviceName = eventName.substr(0, eventName.find(':'));
      //remove device name and the ':'
      eventName = eventName.substr(eventName.find(':')+1);
      
      //whatever is left must be the property name
      parameterName = eventName;
      
      ACS::Time timeStamp = 0ULL;
      if((notification.filterable_data[0].value >>= timeStamp) == false)
	  {
	  ACE_OS::printf("ACSStructuredPushConsumer::::push_structured_event(...) - failed to get the timestamp!");
	  }
      std::string stringifiedTimeStamp = getStringifiedUTC(timeStamp).c_str();

      
      std::string typeName = AnyAide::getId(notification.filterable_data[1].value);
      
      std::string stringifiedValue = "";
      try
	  {
	  stringifiedValue = AnyAide::anyToString(notification.filterable_data[1].value);
	  }
      catch(...)
	  {
	  stringifiedValue = "?";
	  }
      
      
      ACE_OS::printf("%s %s.%s (%s) = %s\n", stringifiedTimeStamp.c_str(), 
		     deviceName.c_str(), 
		     parameterName.c_str(), 
		     typeName.c_str(),
		     stringifiedValue.c_str());
      
      ACE_OS::fflush (stdout);
      
      }
  else
      {
      ACE_OS::printf("Structured Subscribe Consumer %d received unknown event, domain = %s, type = %s\n", 
		     this->proxy_supplier_id_, domain_name, type_name);
      }
}

void
ACSStructuredPushConsumer::disconnect_structured_push_consumer ()
  throw (
                   CORBA::SystemException
                   )
{
  // No-Op.
}

CosNotifyChannelAdmin::StructuredProxyPushSupplier_ptr
ACSStructuredPushConsumer::get_proxy_supplier (void)
{
  return proxy_supplier_.in ();
}

// ************************************************************************ //
// ************************  [  main method  ]  *************************** //
// ************************************************************************ //

// globals
bool g_blockTermination = false;
Subscribe * g_client = 0;

void TerminationSignalHandler(int)
{
  if (g_blockTermination) return;
  g_blockTermination=true;

  LoggingHelper::terminateResolving();

  ACE_ERROR ((LM_INFO, "Stopping the Logging Client...\n"));

  if (g_client)
    {
      g_client->shutdown ();
    }
}

int
main (int argc, char *argv [])
{
  
  if (argc < 2 ||
      (ACE_OS::strcmp(argv[1], acscommon::LOGGING_CHANNEL_NAME)!=0 &&
      ACE_OS::strcmp(argv[1], acscommon::ARCHIVING_CHANNEL_NAME)!=0))
    {
      ACE_OS::printf("\n\tusage: %s <%s | %s> <ORB options>\n\n", 
		     argv[0], 
		     acscommon::LOGGING_CHANNEL_NAME, 
		     acscommon::ARCHIVING_CHANNEL_NAME);
      return 1;
    }

  Subscribe client;
  try
    {
      client.init (argc, argv
                   ); // Init the Client
      

      g_client = &client;
      
      ACE_OS::signal(SIGINT, TerminationSignalHandler);  // Ctrl+C
      ACE_OS::signal(SIGTERM, TerminationSignalHandler); // termination request

      client.run ();
      

      if (!g_blockTermination)
	{
	  g_blockTermination=true;
	  client.shutdown ();
	  
	}

    }
  catch(CORBA::UserException &ue)
    {
    ACE_ERROR((LM_ERROR,"LoggingClient user error: "));
    return 1;
    }
  catch(CORBA::SystemException &se)
    {
    ACE_ERROR((LM_ERROR, "Filter system error: "));
    return 1;
    }
  
  ACE_ERROR ((LM_DEBUG, "ACS Notify Push Subscriber Client stopped.\n"));

  return 0;
}









