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
* "@(#) $Id: loggingClient.cpp,v 1.55 2009/07/31 16:22:37 javarias Exp $"
*
* who       when        what
* --------  ---------   ----------------------------------------------
* msekoran  2002-02-03  disconnection added
* msekoran  2001-12-23  using LoggingHelper::resolveNameService
* msekoran  2001-07-12  created
*/

/// TBD: proper shutdown (disconnection of the consumers) && cleanup
#include <iostream>

 #include <syslog.h>
 #include <unistd.h>
 #include <getopt.h>

#include <vltPort.h>
#include <loggingClient.h>

#include <loggingHelper.h>

#include <acscommonC.h>

#include <acsutilAnyAide.h>
#include <acsutilTimeStamp.h>
#include <acsutilPorts.h>
#include <loggingLoggingProxy.h>
#include <logging_idlC.h>

Subscribe::Subscribe (void)
{
  // No-Op.
  ifgop_ = CosNotifyChannelAdmin::OR_OP;
}

Subscribe::~Subscribe ()
{
}

void
Subscribe::init (int argc, char *argv [], std::string channel)
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


  ACE_DEBUG((LM_DEBUG, "Resolving Notify Channel... %d %s\n",argc, channel.c_str()));

  	resolve_notify_channel (channel.c_str());



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
      teardown_events();
	this->consumer_->disconnect ();
	this->consumer_admin_->destroy();
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
    if (CORBA::is_nil(naming_context_.ptr()))
    	throw CORBA::UNKNOWN();
}

void
Subscribe::resolve_notify_channel (const char * channel_name)
{
  ACE_DEBUG((LM_DEBUG, "Notify Channel: %s\n", channel_name));

  CosNaming::Name name (1);
  name.length (1);
  name[0].id = CORBA::string_dup (channel_name);

  if(ACE_OS::strcmp(channel_name, acscommon::LOGGING_CHANNEL_NAME)==0 ||
        ACE_OS::strcmp(channel_name, acscommon::LOGGING_CHANNEL_XML_NAME)==0 )
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
}//setup_events

void
Subscribe::teardown_events ()
{
  // Remove subscription from the
  CosNotification::EventTypeSeq added(0);
  CosNotification::EventTypeSeq removed (1);
  added.length (0);
  removed.length (1);

  removed[0].domain_name =  CORBA::string_dup ("*");
  removed[0].type_name = CORBA::string_dup ("*");

  this->consumer_admin_->subscription_change (added, removed);

  //just in case...
  added.length(0);
  removed.length(0);
}//teardown_events

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
  m_logBin = false;
  char *acsLogType = getenv("ACS_LOG_BIN");
  if (acsLogType && *acsLogType){
    if(strcmp("true", acsLogType) == 0)
        m_logBin = true;
  }

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
{
  // No-Op.
}

ACE_TCHAR* ACSStructuredPushConsumer::m_LogEntryTypeName[] =
{
    ACE_TEXT ("Unknown"),		// not in specs
    ACE_TEXT ("Shutdown"), 	// not in specs
    ACE_TEXT ("Trace"),
    ACE_TEXT ("Debug"),
    ACE_TEXT ("Info"),
    ACE_TEXT ("Notice"),
    ACE_TEXT ("Warning"),
    ACE_TEXT ("Startup"),		// not in specs
    ACE_TEXT ("Error"),
    ACE_TEXT ("Critical"),
    ACE_TEXT ("Alert"),
    ACE_TEXT ("Emergency")
};

void
ACSStructuredPushConsumer::push_structured_event (const CosNotification::StructuredEvent & notification
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
    if(!m_logBin){
        // for logging
        const char * xmlLog;
        notification.remainder_of_body >>= xmlLog;
	Logging::XmlLogRecordSeq *reclist;
	notification.remainder_of_body >>= reclist;

        if (xmlLog)
        {
            if (toSyslog)
            {
                writeSyslogMsg(xmlLog);
            }
            else
            {
                if(toFile){
                    ACE_OS::fprintf(outputFile,"%s\n", xmlLog);
                    ACE_OS::fflush (outputFile);
                }else{
                    ACE_OS::printf("%s\n", xmlLog);
                    ACE_OS::fflush (stdout);
                }
            }
        }
	else if (reclist)
	{
		for(unsigned int i = 0; i < reclist->length(); i++){
			xmlLog = (*reclist)[i].xml;

			if (toSyslog)
			{
				writeSyslogMsg(xmlLog);
			}
			else
			{
				if(toFile){
					ACE_OS::fprintf(outputFile,"%s\n", xmlLog);
					ACE_OS::fflush (outputFile);
				}else{
					ACE_OS::printf("%s\n", xmlLog);
					ACE_OS::fflush (stdout);
				}
			}
		}
	}
    }else{
        // for logging
        ACSLoggingLog::LogBinaryRecord *log;
        notification.remainder_of_body >>= log;
        if (log)
        {
            if (toSyslog)
            {
                writeSyslogMsg(LoggingProxy::BinToXml(log).c_str());
            }
            else
            {
                if(toFile){
                    ACE_OS::fprintf(outputFile,"%s\n",LoggingProxy::BinToXml(log).c_str());
                    ACE_OS::fflush (outputFile);
                }else{
                    ACE_OS::printf("%s\n",LoggingProxy::BinToXml(log).c_str());
                    ACE_OS::fflush (stdout);
                }
            }
        }

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

    if(toFile){
        ACE_OS::fprintf(outputFile,"</Log>\n");
        ACE_OS::fclose (outputFile);
    }
  ACE_ERROR ((LM_INFO, "Stopping the Logging Client...\n"));

  if (g_client)
    {
      g_client->shutdown ();
    }
}


int
main (int argc, char *argv [])
{
  getParams(argc,argv);

  if ( (ACE_OS::strcmp(argv[argc-1], "Logging")!=0 &&
       ACE_OS::strcmp(argv[argc-1], "Archiving")!=0))
    {
      printUsage(argv[0]);
      return 1;
    }


    if (toSyslog) {
    	syslogFacility=getSyslogFacility();
    }

  Subscribe client;
  try
    {
      client.init (argc, argv,channelName); // Init the Client

      g_client = &client;

      ACE_OS::signal(SIGINT, TerminationSignalHandler);  // Ctrl+C
      ACE_OS::signal(SIGTERM, TerminationSignalHandler); // termination request
      if(toFile){
        outputFile = ACE_OS::fopen (fileName.c_str(),"w");

        if(outputFile == NULL) toFile=false;
        else
            ACE_OS::fprintf(outputFile, "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?> \n<Log> \n<Header Name=\"NameForXmlDocument\" Type=\"LOGFILE\" />\n");
      }

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
    ACE_PRINT_EXCEPTION(ue, "TerminationSignalHandler");
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


int getSyslogFacility() {
	int instance = (int)ACSPorts::getBasePort();
	switch (instance) {
		case 0:
			return LOG_LOCAL0|LOG_INFO;
		case 1:
			return LOG_LOCAL1|LOG_INFO;
		case 2:
			return LOG_LOCAL2|LOG_INFO;
		case 3:
			return LOG_LOCAL3|LOG_INFO;
		case 4:
			return LOG_LOCAL4|LOG_INFO;
		default:
			return LOG_LOCAL5|LOG_INFO;
	}
}

// Write a message to the kernel log
void writeSyslogMsg(const char* msg) {
	syslog(syslogFacility,"%s",msg);
}

// Read the command line parameters and fill the loacl variables
void getParams(int argc, char *argv []) {
	toSyslog=false;
	int c;
	int orb_flag;
	struct option long_options[] =
             {
               /* These options set a flag. */
               {"ORBInitRef", required_argument, &orb_flag, 1},
               {"file", 1, 0, 0}
             };
    int option_index=0;
	while ((c = getopt_long(argc, argv, "shf:t:c:",long_options,&option_index)) != -1) {
		switch(c) {
			case 's': toSyslog=true; break;
			case 'h': printUsage(argv[0]); exit(0);
            case 'f': toFile=true; fileName = optarg;
			default: break;
		}
	}
	// There is only one non-option argument: the name of the channel
	if (argc==optind)
	{
		// The name of the channel is not in the coomand line
		printUsage(argv[0]);
		exit(-1);
	}
	else
	{
        if(strcmp("Archiving",argv[argc-1]) == 0)
            channelName = acscommon::ARCHIVING_CHANNEL_NAME;
        else if(strcmp("Logging",argv[argc-1]) == 0){
            channelName = acscommon::LOGGING_CHANNEL_XML_NAME;
            char *acsLogType = getenv("ACS_LOG_BIN");
            if (acsLogType && *acsLogType){
              if(strcmp("true", acsLogType) == 0)
                channelName = acscommon::LOGGING_CHANNEL_NAME;
             }
        }else{
            printUsage(argv[0]);
            exit(-1);
        }
	}
}

/**
 * Prints the usage message on the stdout
 */
void printUsage(const char* prgName) {
    ACE_OS::printf("\n\tusage: %s [-f <filename>] [-s] <Logging| Archiving> <ORB options>\n",
	     prgName);
	  ACE_OS::printf("-s: write the logs into the syslog\n");
	  ACE_OS::printf("-f: print the resulting xml to filename\n");
	  ACE_OS::printf("-h: print this help\n\n");
}
