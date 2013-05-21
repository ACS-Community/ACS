#ifndef _ACS_DAEMON_IMPL_H_
#define _ACS_DAEMON_IMPL_H_

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
* "@(#) $Id: acsDaemonImpl.h,v 1.12 2012/05/15 09:06:34 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran 2006-06-21 created 
* agrimstr 2007-11-07 refactored common daemon implementation code to
*                     template classes
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "acsdaemonS.h"
#include <ace/SString.h>
#include "logging.h"
#include <getopt.h>
#include <acsutilPorts.h>
#include <tao/IORTable/IORTable.h>
#include <acserr.h>
#include <acsdaemonErrType.h>
#include <ACSErrTypeCommon.h>
#include "acsdaemonORBTask.h"

// thread-pool
#include "acsRequest.h"

/**
 *  Service management class for daemon.
 *
 *  The ACSDaemonServiceImpl class manages the lifecycle of the
 *  CORBA interface that the daemon is to provide.
 */
template <typename T>
class ACSDaemonServiceImpl {

  public:
    
   /**
    * Constructor
    */
    ACSDaemonServiceImpl(LoggingProxy &logProxy, bool isProtected);
  
    /**
     * Destructor
     */
    virtual ~ACSDaemonServiceImpl();
    
    /**
     * Initalization status
     */
    bool 
    isInitialized() { return m_isInitialized; }
    
    /**
     * Tells if daemon was started in protected mode
     */
    bool 
    isProtected() { return m_isProtected; }
    

    /**
     * Retrieve the port for this service
     */
    std::string
    getPort() { return handler.getPort(); }

    /**
     * Retrieve the name for this service
     */
    std::string
    getName() { return handler.getName(); }

    /**
     * Initializes the service.
     */
    int
    startup (int argc, char *argv[]);

    /**
     * Run the service.
     * @return Returns 0 on success, -1 on error.
     */
    int 
    run ();

    /**
     * Shutdown the service.
     */
    void
    shutdown (bool wait_for_completition); 

    /**
     * Get CORBA IOR.
     */
    const char*
    getIOR() const { return m_ior.in(); };
    
  protected:
    /**
     *  initialize the ORB.
     */
    virtual int 
    init_ORB (int& argc, char *argv []);

    //--Common data members-------------------------------------

    /** Initialization status */
    bool m_isInitialized;

    /** Protected mode */
    bool m_isProtected;

    /** Daemon shutdown in progress flag **/
    bool m_blockTermination;

    /** The ORB that we use. */
    CORBA::ORB_var m_orb;

    /** Logging. proxy **/
    LoggingProxy &m_logProxy;

    /** CORBA IOR **/
    CORBA::String_var m_ior;

    /** Implementation of the CORBA interface this service provides **/
    T handler;
};

template <typename T>
ACSDaemonServiceImpl<T>::ACSDaemonServiceImpl (LoggingProxy &logProxy, bool isProtected) :
    m_isInitialized(false), m_logProxy(logProxy)
{
    // noop here

    m_isInitialized = true;

    m_isProtected = isProtected;

    m_blockTermination = false;

    ACS_CHECK_LOGGER;
    // daemon is a standalone process, replace global logger with named logger
    Logging::Logger::setGlobalLogger(getNamedLogger(handler.getName()));

    handler.setService(this);
}

template <typename T>
ACSDaemonServiceImpl<T>::~ACSDaemonServiceImpl (void)
{
}

template <typename T>
int ACSDaemonServiceImpl<T>::startup (int argc, char *argv[])
{
    ACS_SHORT_LOG ((LM_INFO, "Starting up the %s...", handler.getName().c_str()));

    // Initalize the ORB.
    if (init_ORB (argc, argv) != 0)
	{
	return -1;
	}

    // Initialize AES.
    if (!ACSError::init(m_orb.in()))
	{
	ACS_SHORT_LOG ((LM_ERROR, "Failed to initalize the ACS Error System."));
	return -1;
	}

    ACS_SHORT_LOG ((LM_INFO, "%s is initialized.", handler.getName().c_str()));

    return 0;
}

template <typename T>
int ACSDaemonServiceImpl<T>::run (void)
{
    ACS_SHORT_LOG ((LM_INFO, "%s is up and running...", handler.getName().c_str()));

  
    try
	{
	handler.initialize(m_orb.in());
//	this->m_orb->run ();

      // constrcut CORBA ORB request handler task
      ORBTask task (this->m_orb.in(), &m_logProxy);
      const int m_serverThreads = 5;

      // activate task, i.e. spawn threads and handle requests
      if (task.activate (THR_NEW_LWP | THR_JOINABLE, m_serverThreads) == 0)
          // wait until CORBA ORB is shutdown or destroyed
          task.thr_mgr()->wait ();
      else
        {
          // failed to spawn threads
          ACS_LOG(LM_RUNTIME_CONTEXT, "ACSDaemonServiceImpl<T>::run", (LM_INFO, "Failed to activate CORBA ORB request handler threads..."));
          return -1;
        }

	}
    catch(...)
	{
	return -1;
	}

    return 0;
}

template <typename T>
void ACSDaemonServiceImpl<T>::shutdown (bool wait_for_completition)
{
    if (!m_blockTermination)
    {
	ACS_SHORT_LOG ((LM_INFO, "Stopping the %s...", this->getName().c_str()));
	m_blockTermination=true;

	AsyncRequestThreadPool::destroy();

	// shutdown the ORB.
	if (!CORBA::is_nil (m_orb.in ()))
	{
	    handler.dispose(m_orb.in());
	    this->m_orb->shutdown (wait_for_completition);
	}
	// shutdown AES
	ACSError::done();
    }
}

template <typename T>
int ACSDaemonServiceImpl<T>::init_ORB  (int& argc, char *argv [])
{
    m_orb = CORBA::ORB_init(argc, argv, "TAO");

    try
	{
	// get a reference to the RootPOA
	CORBA::Object_var obj = m_orb->resolve_initial_references("RootPOA");
	PortableServer::POA_var root_poa = PortableServer::POA::_narrow(obj.in());
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
	PortableServer::POA_var poa = root_poa->create_POA(handler.getType().c_str(), poa_manager.in(), policy_list);

	// destroy policies
	for (CORBA::ULong i = 0; i < policy_list.length(); ++i)
	    {
	    CORBA::Policy_ptr policy = policy_list[i];
	    policy->destroy();
	    }

	// set as default servant
	poa->set_servant(&handler);

	// create reference
	PortableServer::ObjectId_var oid = PortableServer::string_to_ObjectId(handler.getType().c_str());
	obj = poa->create_reference_with_id (oid.in(), handler._interface_repository_id());
	m_ior = m_orb->object_to_string(obj.in());

	// bind to IOR table
      	CORBA::Object_var table_object = m_orb->resolve_initial_references("IORTable");
	IORTable::Table_var adapter = IORTable::Table::_narrow(table_object.in());
      
	if (CORBA::is_nil(adapter.in()))
	    {
	    ACS_SHORT_LOG ((LM_ERROR, "Nil IORTable"));
	    return -1;
	    }
	else
	    {
	    adapter->bind(handler.getType().c_str(), m_ior.in());
	    }

	// activate POA
	poa_manager->activate();

	ACS_SHORT_LOG((LM_INFO, "%s is waiting for incoming requests.", handler.getName().c_str()));
      
	}
    catch( CORBA::Exception &ex )
	{
	ACE_PRINT_EXCEPTION (ex, "EXCEPTION CAUGHT");
	return -1;
	}
  
    return 0;
}

/**
 * ACS Daemon Implementation class.
 *
 * acsDaemonImpl provides a common implementation for the container and
 * service daemons of ACS.  
 */

template <typename T>
class acsDaemonImpl
{

  public:

    /**
     * Constructor.
     */
    acsDaemonImpl(int argc, char *argv[]);
    
    /**
     * Destructor.
     */
    ~acsDaemonImpl();
    
    /**
     * Display help information for the daemon.
     */
    void usage(const char *argv);

    /**
     * Process client requests.
     */
    int run();

    /**
     * Terminate the daemon.
     */
    void shutdown();

  private:

    /** Manager for the service provided by this daemon **/
    ACSDaemonServiceImpl<T> *service;

    /** File name where the IOR information is to be written **/
    ACE_CString iorFile;

    /** Description of where the provided service listens for requests **/
    ACE_CString ORBEndpoint;

    /** Configuration information for the service **/
    int nargc;
    char** nargv;
    
    /** logger **/
    LoggingProxy *m_logger;
};


/** Valid command line options for daemons **/
static struct option long_options[] = {
    {"help",        no_argument,       0, 'h'},
    {"outfile",     required_argument, 0, 'o'},
    {"ORBEndpoint", required_argument, 0, 'O'},
    {"unprotected", no_argument,       0, 'u'},
    {0, 0, 0, '\0'}};

template <typename T>
void acsDaemonImpl<T>::usage(const char *argv)
{
    ACE_OS::printf ("\n\tusage: %s {-h} [-O iiop://ip:port] [-o iorfile]\n", argv);
    ACE_OS::printf ("\t   -h, --help         show this help message\n");
    ACE_OS::printf ("\t   -O, --ORBEndpoint  ORB end point\n");
    ACE_OS::printf ("\t   -o, --outfile      IOR output file\n");
    ACE_OS::printf ("\t   -u, --unprotected  start in unprotected mode\n");
}

template <typename T>
acsDaemonImpl<T>::acsDaemonImpl(int argc, char *argv[])
{
    nargc = 0;
    nargv = 0;
    service = 0;
    m_logger = 0;
    bool unprotected = false;

    // Extract and validate command line arguments
    int c;
    for(;;)
        {
        int option_index = 0;
        c = getopt_long (argc, argv, "ho:O:u",
                         long_options, &option_index); 
        if (c==-1) break;
        switch(c)
            {
                case 'h':
                    usage(argv[0]);
                    return;
                case 'o':
                    iorFile = optarg;
                    break;
                case 'O':
                    ORBEndpoint = optarg;
                    break;
                case 'u':
                    unprotected = true;
                    break;
                default:
                    ACE_OS::printf("Ignoring unrecognized option %s", 
                                    argv[option_index]);
            }
        }

    // Host IP information is needed to initialize the logging system
    // and for ORBEndpoint creation
    const char* hostName = ACSPorts::getIP();

    // store env. var. value and disable logging to local file cache
    char * acsLogFileEnv = ACE_OS::getenv("ACS_LOG_FILE");
    ACE_CString acsLogFileValue(acsLogFileEnv);
    ACE_OS::setenv("ACS_LOG_FILE", "/dev/null", 1);

    // create logging proxy
    LoggingProxy::ProcessName(argv[0]);
    LoggingProxy::ThreadName("main");
    ACE_Log_Msg::instance()->local_host(hostName);
    // for daemons/imp we set some cache by default
    m_logger = new LoggingProxy (1024, 0, 31, 0);
    
    LoggingProxy::init (m_logger);  

    // reset ACS_LOG_FILE back
    if (acsLogFileEnv)
        ACE_OS::setenv("ACS_LOG_FILE", acsLogFileValue.c_str(), 1);
    else
        ACE_OS::unsetenv("ACS_LOG_FILE");

    AsyncRequestThreadPool::configure(argv[0], m_logger, 5);	// 5 threads for async

    // Ready the service manager
    service = new ACSDaemonServiceImpl<T>(*m_logger, !unprotected);

    // Generate the CORBA configuration for the service
    ACE_CString argStr;
    
    if(ORBEndpoint.length()<=0)
        {
        argStr = ACE_CString("-ORBEndpoint iiop://") + hostName + ":";
        argStr = argStr + service->getPort().c_str();
        }
    else
        {
        argStr = ACE_CString("-ORBEndpoint ") + ORBEndpoint;
        }

    ACS_SHORT_LOG((LM_INFO, "Command line is: %s", argStr.c_str()));
    ACE_OS::string_to_argv ((ACE_TCHAR*)argStr.c_str(), nargc, nargv);
}

template <typename T>
acsDaemonImpl<T>::~acsDaemonImpl()
{
    if (service != 0) delete service;
    if (m_logger != 0)
    {
	LoggingProxy::done();
	delete m_logger;
    }
}


template <typename T>
int acsDaemonImpl<T>::run()
{
    ACS_TRACE("acsDaemonImpl<T>::run");
    if (!service || !service->isInitialized())
	{
	return -1;
	}
    try
	{
	if (service->startup (nargc, nargv) != 0)
	    {
	    return -1;
	    }

	// write IOR to file, if necessary
	if (iorFile.length() > 0)
	    {
	    FILE *output_file = ACE_OS::fopen (iorFile.c_str(), "w");
	    if (output_file == 0) 
		{
		ACS_SHORT_LOG ((LM_ERROR, "Cannot open output file '%s' to write IOR.", iorFile.c_str()));
		return  -1;
		}

	    int result = ACE_OS::fprintf (output_file, "%s", service->getIOR());
	    if (result < 0) 
		{
		ACS_SHORT_LOG ((LM_ERROR, "ACE_OS::fprintf failed to write IOR."));
		return  -1;
		}

	    ACE_OS::fclose (output_file);
	    ACS_SHORT_LOG((LM_INFO, "%s IOR has been written into file '%s'.", service->getName().c_str(), iorFile.c_str()));
	    }

	// run, run, run...
	if (service->run () == -1)
	    {
	    this->shutdown ();
	    ACS_SHORT_LOG ((LM_ERROR, "Failed to run the %s.", service->getName().c_str()));
	    return  1;
	    }
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR, "Failed to start the %s.", service->getName().c_str()));
	return 1;
	}
  

    this->shutdown();
  
    ACS_SHORT_LOG ((LM_INFO, "%s stopped.", service->getName().c_str()));

    return 0;
}

template <typename T>
void acsDaemonImpl<T>::shutdown()
{
    service->shutdown(true);
}

#endif
