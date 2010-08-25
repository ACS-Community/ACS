/*******************************************************************************
 * E.S.O. - ACS project
 *
 * "@(#) $Id: acsServices80Test.cpp,v 1.10 2010/08/25 09:04:16 hsommer Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 */

/** @file acsServices80Test.cpp
 *  acsServices80Test is used to test ACS 8.0 Services Deamon.
 *  @htmlonly
 *  <br><hr>
 *  @endhtmlonly
 */   


#include <acsutilPorts.h>
#include <logging.h>
#include <acsdaemonS.h>
#include <ACSErrTypeCommon.h>
#include <acsdaemonErrType.h>
#include <maciErrType.h>
#include <getopt.h>
#include <unistd.h>
#include <tao/IORTable/IORTable.h>

#define DAEMONHOST ACSPorts::getIP()

static struct option long_options[] = {
        {"help",        no_argument,       0, 'h'},
        {"instance",    required_argument, 0, 'i'},
        {"host",        required_argument, 0, 'H'},
        {"daemon",      required_argument, 0, 'd'},
        {0, 0, 0, '\0'}};

void 
usage(const char *argv)
{
    ACE_OS::printf ("\n\tusage: %s {-h}", argv);
    ACE_OS::printf ("\t   -h, --help         show this help message\n");
    ACE_OS::printf ("\t   -i, --instance     ACS instance to use\n");
    ACE_OS::printf ("\t   -H, --host         Host where to stop the daemon\n");
    ACE_OS::printf ("\t   -d, --daemon       Daemon reference\n");
}

class TestDaemonSequenceCallback : public POA_acsdaemon::DaemonSequenceCallback
{
  bool startup, complete;
  ::acsdaemon::DaemonSequenceCallback_var cobj;
  public:
    acsdaemon::ServicesDaemon_var daemon;
    char *services_definition;
    TestDaemonSequenceCallback() : startup(true), complete(false), cobj(NULL), services_definition(NULL) {}
    ~TestDaemonSequenceCallback() {
      ACS_SHORT_LOG((LM_DEBUG, "DESTROYING TestDaemonSequenceCallback!"));
      if (services_definition != NULL) {
        free(services_definition);
      }
    }
    ::acsdaemon::DaemonSequenceCallback_ptr ptr() {
      if (cobj == NULL)
        cobj = this->_this();
      return cobj.in();
    }
    bool isComplete() { return complete; }
    void done (const ::ACSErr::Completion & comp) {
      if (startup) {
        ACS_SHORT_LOG((LM_INFO, "DONE STARTING UP SERVICES."));
        
	ACE_CString managerRef = ACE_CString("corbaloc::") + ACSPorts::getIP() + ":" + ACSPorts::getManagerPort(1).c_str() + "/Manager";
        daemon->set_manager_reference(1, managerRef.c_str()); 

//        printf("Please, press a key to start shutting down the services!");
//        getchar();
	ACE_OS::sleep(5);
        ACS_SHORT_LOG((LM_INFO, "SHUTTING DOWN THE SERVICES."));
        daemon->stop_services(services_definition, ptr());
        startup = false;
      } else {
        ACS_SHORT_LOG((LM_INFO, "DONE SHUTTING DOWN SERVICES."));
        PortableServer::POA_var poa = this->_default_POA();
        PortableServer::ObjectId_var oid = poa->servant_to_id(this);
        poa->deactivate_object(oid.in());
        complete = true;
      }
    }
    void working (const char * service, const char * host, ::CORBA::Short instance_number, const ::ACSErr::Completion & comp) {
      ACSErr::CompletionImpl compi = comp;
      if (compi.isErrorFree()) {
        ACS_SHORT_LOG((LM_INFO, "Successfully %s service '%s' on host '%s' (instance: %d).", startup ? "started" : "stopped", service, host, instance_number));
      } else {
        compi.log();
        ACS_SHORT_LOG((LM_INFO, "Failed to %s service '%s' on host '%s' (instance: %d).", startup ? "start" : "stop", service, host, instance_number));
      }
    }
};


/*class TestDaemonSequenceCallback2 : public POA_acsdaemon::DaemonSequenceCallback
{
  public:
    TestDaemonSequenceCallback2() {}
    ~TestDaemonSequenceCallback2() {
      ACS_SHORT_LOG((LM_DEBUG, "DESTROYING TestDaemonSequenceCallback2!"));
    }
    void done (const ::ACSErr::Completion & comp) {
    }
    void working (const char * service, const char * host, ::CORBA::Short instance_number, const ::ACSErr::Completion & comp) {
    }
};
*/

int
main (int argc, char *argv[])
{
/*    {
	TestDaemonSequenceCallback2 *cb = new TestDaemonSequenceCallback2(); // refcount = 1
	acsdaemon::DaemonSequenceCallback_ptr cbp = cb->_this();         // refcount++
//        acsdaemon::DaemonSequenceCallback_var cbv1 = cbp;
//        acsdaemon::DaemonSequenceCallback_var cbv2 = acsdaemon::DaemonSequenceCallback::_duplicate(cbp);
//        acsdaemon::DaemonSequenceCallback_var cbv3 = cbp;
        printf("a0\n");

        PortableServer::POA_var poa = cb->_default_POA();
        PortableServer::ObjectId_var oid = poa->servant_to_id(cb);
        poa->deactivate_object(oid.in()); // refcount--
        printf("a1\n");
        cb->_remove_ref(); // refcount--
        printf("endblock\n");
        CORBA::release(cbp);
    }
    printf("a2\n");
    exit(0);
*/
    int c;
    ACE_CString daemonRef;
    ACE_CString hostName;
    for(;;)
        {
        int option_index = 0;
        c = getopt_long (argc, argv, "hd:H:",
                         long_options, &option_index); 
        if (c==-1) break;
        switch(c)
            {
                case 'h':
                    usage(argv[0]);
                    return 0;
                case 'd':
                    daemonRef = optarg;
                    break;
                case 'H':
                    hostName = optarg;
                    break;
            }
        }

	LoggingProxy * logger = new LoggingProxy(0, 0, 31);
	if (logger)
	{
		LoggingProxy::init(logger);
		LoggingProxy::ProcessName(argv[0]);
		LoggingProxy::ThreadName("main");
	}
	else
		ACS_SHORT_LOG((LM_INFO, "Failed to initialize logging."));

	try
	{
		// Initialize the ORB.
		CORBA::ORB_var orb = CORBA::ORB_init (argc,argv,"TAO");

/// ENABLE CALLBACKS ///////////////////////////////////////////////////////

	TestDaemonSequenceCallback cb;

	// get a reference to the RootPOA
	CORBA::Object_var pobj = orb->resolve_initial_references("RootPOA");
	PortableServer::POA_var root_poa = PortableServer::POA::_narrow(pobj.in());
	PortableServer::POAManager_var poa_manager = root_poa->the_POAManager();
      
	// create policies
	CORBA::PolicyList policy_list;
	policy_list.length(5);
	policy_list[0] = root_poa->create_request_processing_policy(PortableServer::USE_DEFAULT_SERVANT);
	policy_list[1] =  root_poa->create_id_uniqueness_policy(PortableServer::MULTIPLE_ID);
	policy_list[2] = root_poa->create_id_assignment_policy(PortableServer::USER_ID); 
	policy_list[3] = root_poa->create_servant_retention_policy(PortableServer::NON_RETAIN); 
	policy_list[4] =  root_poa->create_lifespan_policy(PortableServer::PERSISTENT);
      
	// create a ACSDaemon POA with policies 
	PortableServer::POA_var poa = root_poa->create_POA("DaemonSequenceCallback", poa_manager.in(), policy_list);

	// destroy policies
	for (CORBA::ULong i = 0; i < policy_list.length(); ++i)
	    {
	    CORBA::Policy_ptr policy = policy_list[i];
	    policy->destroy();
	    }

	// set as default servant
	poa->set_servant(&cb);

	// create reference
	PortableServer::ObjectId_var oid = PortableServer::string_to_ObjectId("DaemonSequenceCallback");
	pobj = poa->create_reference_with_id (oid.in(), cb._interface_repository_id());
	CORBA::String_var m_ior = orb->object_to_string(pobj.in());

	// bind to IOR table
      	CORBA::Object_var table_object = orb->resolve_initial_references("IORTable");
	IORTable::Table_var adapter = IORTable::Table::_narrow(table_object.in());

	if (CORBA::is_nil(adapter.in()))
	    {
	    ACS_SHORT_LOG ((LM_ERROR, "Nil IORTable"));
	    return -1;
	    }
	else
	    {
	    adapter->bind("DaemonSequenceCallback", m_ior.in());
	    }

	// activate POA
	poa_manager->activate();

	ACS_SHORT_LOG((LM_INFO, "%s is waiting for incoming requests.", "DaemonSeqeuenceCallback"));
//////////////////////////////////////////////////////////

		// construct default one
      if (daemonRef.length() == 0)
	  {
          if(hostName.length() == 0)
          {
	      hostName = ACSPorts::getIP();
          } 
	  daemonRef = "corbaloc::";
	  daemonRef = daemonRef + hostName + ":" + ACSPorts::getServicesDaemonPort().c_str() + "/" + ::acsdaemon::servicesDaemonServiceName;
	  ACS_SHORT_LOG((LM_INFO, "Using local Services Daemon reference: '%s'", daemonRef.c_str()));
	  
	  }
      else
          {
          ACS_SHORT_LOG((LM_INFO, "Services Daemon reference obtained via command line: '%s'", daemonRef.c_str()));
          }


		CORBA::Object_var obj = orb->string_to_object(daemonRef.c_str());
		if (CORBA::is_nil(obj.in()))
		{
			ACS_SHORT_LOG((LM_ERROR, "Failed to resolve reference '%s'.", daemonRef.c_str()));
			return -1;
		}

		acsdaemon::ServicesDaemon_var daemon = acsdaemon::ServicesDaemon::_narrow(obj.in());
		if (CORBA::is_nil(daemon.in()))
		{
			ACS_SHORT_LOG((LM_ERROR, "Failed to narrow reference '%s'.", daemonRef.c_str()));
			return -1;
		}

                ACS_SHORT_LOG((LM_INFO, "BUILDING SERVICE DEFINITION."));

		acsdaemon::ServiceDefinitionBuilder *sdb = daemon->create_service_definition_builder(1);
		sdb->add_naming_service(DAEMONHOST);
		sdb->add_interface_repository(DAEMONHOST, false, false);
		sdb->add_notification_service("NotifyEventChannelFactory", DAEMONHOST);
		sdb->add_notification_service("LoggingNotifyEventChannelFactory", DAEMONHOST);
		sdb->add_notification_service("AlarmNotifyEventChannelFactory", DAEMONHOST);
                sdb->add_notification_service("ArchiveNotifyEventChannelFactory", DAEMONHOST);
		sdb->add_logging_service(DAEMONHOST, "Log");
		sdb->add_acs_log(DAEMONHOST);
                sdb->add_xml_cdb(DAEMONHOST, true, getenv("ACS_CDB"));
                sdb->add_alarm_service(DAEMONHOST);
                sdb->add_manager(DAEMONHOST, "", true);

		printf("SERVICE DEFINITION XML:\n%s\n", sdb->get_services_definition());
                cb.daemon = daemon;
                cb.services_definition = sdb->get_services_definition();
                ACS_SHORT_LOG((LM_INFO, "STARTING UP THE SERVICES."));
		daemon->start_services(cb.services_definition, true, cb.ptr());
		sdb->close();

		while (!cb.isComplete()) {
			sleep(1);
			if (orb->work_pending())
				orb->perform_work();
		};
		ACS_SHORT_LOG((LM_INFO, "TEST COMPLETE."));
	}
	catch( maciErrType::NoPermissionEx &ex )
	{
		ACS_SHORT_LOG((LM_WARNING, "Daemon is running in protected mode and cannot be shut down remotely!\n"));
		return -1;
	}
	catch( CORBA::Exception &ex )
	{
		ACS_SHORT_LOG((LM_ERROR, "Failed."));
		ACE_PRINT_EXCEPTION (ex, ACE_TEXT ("Caught unexpected exception:"));
		return -1;
	}

	return 0;
}





