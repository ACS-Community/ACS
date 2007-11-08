#include "acsdaemonS.h"
#include <ace/SString.h>
#include "logging.h"
#include <getopt.h>
#include <acsutilPorts.h>
#include <tao/IORTable/IORTable.h>
#include <acserr.h>
#include <acsdaemonErrType.h>
#include <ACSErrTypeCommon.h>

template <typename T>
class ACSBaseDaemonImpl {

  public:
    
   /**
    * Constructor
    */
    ACSBaseDaemonImpl(LoggingProxy &logProxy);
  
    /**
     * Destructor
     */
    virtual ~ACSBaseDaemonImpl();
    
    /**
     * Initalization status
     */
    bool 
    isInitialized() { return m_isInitialized; }
    

    /**
     * Retrieve the port for this type of daemon
     */
    const char* getPort() { return handler.getPort(); }

    /**
     * Retrieve the name for this daemon
     */
    const char* getName() { return handler.getName(); }

    /**
     * Initializes the daemon.
     */
    int
    startup (int argc, char *argv[]);

    /**
     * Run the daemon.
     * @return Returns 0 on success, -1 on error.
     */
    int 
    run ();

    /**
     * Shutdown the daemon.
     */
    void shutdown (); 

    /**
     * Get CORBA IOR.
     */
    const char* getIOR() const { return m_ior.in(); };
    
  protected:
    /**
     *  initialize the ORB.
     */
    virtual int 
    init_ORB (int& argc, char *argv []);

    //--Common data members-------------------------------------

    /** Initialization status */
    bool m_isInitialized;

    /** The ORB that we use. */
    CORBA::ORB_var m_orb;

    /** Logging. proxy **/
    LoggingProxy &m_logProxy;

    /** CORBA IOR **/
    CORBA::String_var m_ior;

    T handler;
};

template <typename T>
ACSBaseDaemonImpl<T>::ACSBaseDaemonImpl (LoggingProxy &logProxy) :
    m_isInitialized(false), m_logProxy(logProxy)
{
    // noop here

    m_isInitialized = true;
}

template <typename T>
ACSBaseDaemonImpl<T>::~ACSBaseDaemonImpl (void)
{
}

template <typename T>
int ACSBaseDaemonImpl<T>::startup (int argc, char *argv[])
{
    ACS_SHORT_LOG ((LM_INFO, "Starting up the %s...", handler.getName()));

//     ACS_SHORT_LOG ((LM_INFO, m_startmsg));

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

    ACS_SHORT_LOG ((LM_INFO, "%s is initialized.", handler.getName()));

    return 0;
}

template <typename T>
int ACSBaseDaemonImpl<T>::run (void)
{
    ACS_SHORT_LOG ((LM_INFO, "%s is up and running...", handler.getName()));

  
    try
	{
	this->m_orb->run ();
	}
    catch(...)
	{
	return -1;
	}

    return 0;
}

template <typename T>
void ACSBaseDaemonImpl<T>::shutdown ()
{

    // shutdown the ORB.
    if (!CORBA::is_nil (m_orb.in ()))
	{
	this->m_orb->shutdown (true);
      
	}

    // shutdown AES
    ACSError::done();
}

template <typename T>
int ACSBaseDaemonImpl<T>::init_ORB  (int& argc, char *argv [])
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
	PortableServer::POA_var poa = root_poa->create_POA(handler.getType(), poa_manager.in(), policy_list);

	// destroy policies
	for (CORBA::ULong i = 0; i < policy_list.length(); ++i)
	    {
	    CORBA::Policy_ptr policy = policy_list[i];
	    policy->destroy();
	    }

	// set as default servant
	poa->set_servant(&handler);

	// create reference
	PortableServer::ObjectId_var oid = PortableServer::string_to_ObjectId(handler.getType());
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
	    adapter->bind(handler.getType(), m_ior.in());
	    }

	// activate POA
	poa_manager->activate();

	ACS_SHORT_LOG((LM_INFO, "%s is waiting for incoming requests.", handler.getName()));
      
	}
    catch( CORBA::Exception &ex )
	{
	ACE_PRINT_EXCEPTION (ex, "EXCEPTION CAUGHT");
	return -1;
	}
  
    return 0;
}

template <typename T>
class acsDaemonImpl
{
    bool blockTermination;
    ACSBaseDaemonImpl<T> *service;
    ACE_CString iorFile;
    ACE_CString ORBEndpoint;
    int nargc;
    char** nargv;



  public:
    acsDaemonImpl(int argc, char *argv[]);
    
    ~acsDaemonImpl();
    
    void usage(const char *argv);

    int run();

    void shutdown();
};

static struct option long_options[] = {
    {"help",        no_argument,       0, 'h'},
    {"outfile",     required_argument, 0, 'o'},
    {"ORBEndpoint", required_argument, 0, 'O'},
    {0, 0, 0, '\0'}};

template <typename T>
void acsDaemonImpl<T>::usage(const char *argv)
{
    ACE_OS::printf ("\n\tusage: %s {-h} [-O iiop://ip:port] [-o iorfile]\n", argv);
    ACE_OS::printf ("\t   -h, --help         show this help message\n");
    ACE_OS::printf ("\t   -O, --ORBEndpoint  ORB end point\n");
    ACE_OS::printf ("\t   -o, --outfile      IOR output file\n");
}

template <typename T>
acsDaemonImpl<T>::acsDaemonImpl(int argc, char *argv[])
{
    blockTermination = false;
    nargc = 0;
    nargv = 0;
    service = 0;


    int c;
    for(;;)
        {
        int option_index = 0;
        c = getopt_long (argc, argv, "ho:O:",
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
                default:
                    ACE_OS::printf("Ignoring unrecognized option %s", 
                                    argv[option_index]);
            }
        }

   const char* hostName = ACSPorts::getIP();

    // create logging proxy
    LoggingProxy::ProcessName(argv[0]);
    LoggingProxy::ThreadName("main");
    ACE_Log_Msg::instance()->local_host(hostName);

    LoggingProxy m_logger (0, 0, 31, 0);
    LoggingProxy::init (&m_logger);  


    ACE_CString argStr;
    
    service = new ACSBaseDaemonImpl<T>(m_logger);

    if(ORBEndpoint.length()<=0)
        {
        argStr = ACE_CString("-ORBEndpoint iiop://") + hostName + ":";
        argStr = argStr + service->getPort();
        }
    else
        {
        argStr = ACE_CString("-ORBEndpoint ") + ORBEndpoint;
        }

    // create new argv
    ACS_SHORT_LOG((LM_INFO, "Command line is: %s", argStr.c_str()));
    ACE_OS::string_to_argv ((ACE_TCHAR*)argStr.c_str(), nargc, nargv);
}

template <typename T>
acsDaemonImpl<T>::~acsDaemonImpl()
{
    delete service;
    LoggingProxy::done();
}


template <typename T>
int acsDaemonImpl<T>::run()
{
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
	    ACS_SHORT_LOG((LM_INFO, "%s IOR has been written into file '%s'.", service->getName(), iorFile.c_str()));
	    }

	// run, run, run...
	if (service->run () == -1)
	    {
	    service->shutdown ();
	    ACS_SHORT_LOG ((LM_ERROR, "Failed to run the %s.", service->getName()));
	    return  1;
	    }
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR, "Failed to start the %s.", service->getName()));
	return 1;
	}
  

    this->shutdown();
  
    ACS_SHORT_LOG ((LM_INFO, "%s stopped.", service->getName()));

    return 0;
}

template <typename T>
void acsDaemonImpl<T>::shutdown()
{
    if (!blockTermination)
	{
	ACS_SHORT_LOG ((LM_INFO, "Stopping the %s...", service->getName()));
	blockTermination=true;
	service->shutdown ();
	}
}
