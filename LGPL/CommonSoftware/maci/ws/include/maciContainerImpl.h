#ifndef maciContainerImpl_h
#define maciContainerImpl_h

/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciContainerImpl.h,v 1.68 2012/05/18 06:54:45 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram 2003-10-27 template methods are supported on VxWorks too (since 2.96)
* msekoran 2003-09-12 ported to new maci DIL
* oat 2002-12-17 added get_object template method
* jib/blo 2002-04-02  get_object method added
* msekoran 2002-02-07 Added shutdown helpers.
* almamgr 2001-07-23 Added getManager() method
* almamgr 2001-07-19 Added forward class definitions for MACIServantManager and LibraryManager
* almamgr 2001-07-19 created
* msekoran  2001/05/19  redesigned
*/

#include <acsutil.h>
#include <maciExport.h>

#include <maciS.h>

#include <cdb.h>

#include <logging.h>

#include <AlarmSourceThread.h>

#include <ace/Synch.h>
#include <ace/Hash_Map_Manager.h>
#include <ace/Unbounded_Set.h>

#include <acsContainerServices.h>
#include "maciContainerThreadHook.h"
#ifndef MAKE_VXWORKS
#include "maciLogThrottleAlarmImpl.h"
#endif

#include <map>

#include <ace/Activation_Queue.h>
#include <ace/Method_Request.h>
#include <ace/Task.h>
#include <ace/Auto_Ptr.h>
#include <ACSErrTypeOK.h>

// #include <maciContainerServices.h> commented out: forward decl

/// 0 - reload the container
#define CONTAINER_RELOAD 0
/// 1 - reboot the computer
#define CONTAINER_REBOOT 1
/// 2 - exit the container
#define CONTAINER_EXIT 2

namespace maci {


/**
 * Each DLL is expected to export a function with public name "ConstructComponentFunc",
 * returning a pointer of a created servant.
 * @param h handle of the component
 * @param poa reference of the poa activating components
 * @param name name of the component
 * @param type type of the component
 * @return newly created servant
 */

typedef PortableServer::Servant (*ConstructComponentFunc)(
						    maci::Handle h,
						    const char * name,
						    const char * type,
                            ContainerServices * containerServices
						    );

/*
 * Forward declarations
 */
class ContainerServices; // This is defined in maciContainerServics.h
class MACIServantManager;
class LibraryManager;

    
    
class MethodRequestThreadPool : public ACE_Task_Base, public Logging::Loggable
{
public:
    MethodRequestThreadPool (int n_threads = 1);
        
    virtual int svc (void);
        
    int enqueue (ACE_Method_Request *request);
        
    void shutdown();
        
private:
    ACE_Activation_Queue activation_queue_;

	int m_threads;
};

    
/**
 * Container is an agent of MACI that is installed on every computer of the control system.
 * There can be more than one Container living on the same computer, but there can be only one Container per process.
 * It has the following responsibilities: 
 * <OL> 
 * <LI>Constructs Controlled Objects (<B>components</B>) when the Manager instructs it to (see activate_component and deactivate_component).</LI> 
 * <LI>Provides the components that it hosts with basic MACI services, such as: 
 * <UL> 
 * <LI>access to the Manager</LI> 
 * <LI>access to the Local Database</LI> 
 * <LI>access to other components (indirectly through the Manager)</LI> 
 * <LI>access to the ORB and POA</LI> 
 * </UL> 
 * <LI>Handles intricacies of working with the ORB and the POA, such as connecting the newly created components to the POA.</LI> 
 * <LI>Maintains code-base of components that can be instantiated on the computer (see get_executables, get_executable_info and put_executable_code.</LI> 
 * <LI>Is responsive to a shutdown directive from the Manager, which can shutdown only the Container's process, or reboot the computer (see shutdown).</LI> 
 * </OL> 
 *
 * The Container could be easily extended to assist the Manager in fault detection:
 * the Container could respond to periodic pings issued by the Manager,
 * and if the responses stop, the Manager could assume a failure of the Container's computer.
 *
 * @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 * Jozef Stefan Institute, Slovenia<br>
 * @version "@(#) $Id: maciContainerImpl.h,v 1.68 2012/05/18 06:54:45 bjeram Exp $"
 */

class maci_EXPORT ContainerImpl :
  public virtual POA_maci::Container,
  public virtual PortableServer::RefCountServantBase
{
public:

  /// Get container instance
  /// Discouraged direct usage of this reference, use CORBA proxy instead: ContainerImpl::getContainer()->getContainerCORBAProxy()
  /// @return container instance
  static ContainerImpl * getContainer()
  {
    return m_container;
  }

  /// Get container CORBA proxy instance
  /// @return container CORBA proxy instance
  maci::Container_ptr getContainerCORBAProxy()
  {
    return m_container_ref.ptr();
  }

  /// Get logging proxy instance
  /// @return logging proxy instance
  static LoggingProxy * getLoggerProxy()
  {
    return m_loggerProxy;
  }

  /// Get container's proces name
  /// @return  container's proces name
  char * getProcessName()
  {
    return m_argv[0];
  }

  /// Constructor
  ContainerImpl();

  /// Destructor
  virtual ~ContainerImpl();


  /// Get manager instance
  /// @return manager instance.
  /// Returns the reference to this domain's Manager. If the container
  /// is not logged in or if the manager could not be found, NULL is returned.
  ///
  ///   NOTE: The returned pointer is NOT duplicated. This means that you
  ///     should not assign it to a _var object! The object pointed to
  ///     is freed during Container's destruction. If you still need the
  ///     pointer after that, duplicate it.
  ///
  /// EXAMPLE:
  ///
  ///   Attempt an operation on the Manager.
  ///
  ///     try
  ///     {
  ///       getContainer()->getManager()->release_component("mycomponent");
  ///     }
  ///     catch(const CORBA::SystemException&)
  ///     {
  ///     }
  ///
  maci::Manager_ptr getManager();

  bool init(int argc, char *argv[]);
  bool connect();
  bool run();
  bool done();

  /// Get status of container (main() return value)
  int getStatus() { return m_status; }
  /// Set status of container (main() return value)
  void setStatus(int status) { m_status = status; }

  /// Thread initialization method. Has to be called by each thread to initialize it.
  static void initThread(const char * threadName = 0);

  /// Thread finalization method. Has to be called by each thread to finalize it.
  static void doneThread();

  /// Called by the servant manager - used to etherealize component when references to it dropped to 0.
  void etherealizeComponent(const char * id, PortableServer::Servant servant);

  /// Get shutdown action.
  /// @return 0 - reload the container, 1 - reboot the computer, 2 - exit the container
  int getShutdownAction() { return m_shutdownAction; }

  /// Set shutdown action.
  /// @param 0 - reload the container, 1 - reboot the computer, 2 - exit the container
  void setShutdownAction(int action) { m_shutdownAction=action; }

  /// Get container handle
  /// @return The container handle
  maci::Handle getHandle() { return m_handle; }

  /// Get container POA
  /// @return The Container POA
  PortableServer::POA_var getContainerPOA() { return poaContainer; }

  /// Get POA Manager
  /// @return The POA Manager
  PortableServer::POAManager_var getPOAManager() { return poaManager; }

  /// Get container services
  /// @return the container services
  ContainerServices* getContainerServices() { return m_containerServices; }

    /// Get container ORB
    /// @return The Container ORB
    CORBA::ORB_var getContainerORB() { return orb; }

  /* ----------------------------------------------------------------*/
  /* --------------------- [ CORBA interface ] ----------------------*/
  /* ----------------------------------------------------------------*/

  /**
   * Return a handle that was given to the Container at login into the Manager.
   * If the container is not logged in (or never was) the method must return 0.
   * @return the Container's handle.
   */
  virtual ::maci::Handle get_handle ();

  /**
   * Activate a component whose type (class) and name (instance) are given. 
   *
   * In the process of activation, component's code-base is loaded into memory if it is not there already. 
   * The code-base resides in an executable file (usually a dynamic-link library or a shared library -- DLL). 
   *
   * On platforms that do not automatically load dependent executables (e.g., VxWorks),
   * the Container identifies the dependancies by querying the executable and loads them automatically. 
   *
   * Once the code is loaded, it is asked to construct a servant of a given type. 
   *
   * The servant is then initialized with the Configuration Database (CDB) and
   * Persistance Database (PDB) data. The servant is attached to the component, and a
   * reference to it is returned. 
   *
   * @param h Handle of the component that is being activated. This handle is used by the component when it will present itself to the Manager. The component is expected to remember this handle for its entire life-time.
   * @param name Name of the component to instantiate.
   * @param exe Path to the executable file (a DLL or a shared library) in which the component's code resides. The path is relative to the root directory in which all executable code is stored. The path must not contain dots, and uses slashes (not backslashes) to separate components of the path. The path must not include the extension, or any prefixes, so that it is platform independent.
   * @param type The type of the component to instantiate. The interpretation of this field depends on the executable. Type should uniquely identify the code-base which the component will be executing. <B>Note:</B> Type name is NOT CORBA repository id.
   * @throw maciErrType::CannotActivateComponentEx
   * @return Returns the reference to the object that has just been activated.
   *		  If the component could not the activated, a nil reference is returned. 
   */
  virtual maci::ComponentInfo * activate_component (maci::Handle h,
					maci::ExecutionId execution_id,
					const char * name,
					const char * exe,
					const char * type
					);

  // TODO
  virtual void activate_component_async(maci::Handle, maci::ExecutionId, const char*, const char*, const char*, maci::CBComponentInfo*, const ACS::CBDescIn&);
  
  /**
   * Deactivate a component whose handle is given.
   *
   * Deactivation is the inverse process of activation:
   * component is detached from the POA, and thus made unavailable through CORBA,
   * and its resources are freed. If it's code-base is no longer used,
   * it is unloaded from memory.
   * @param h A handle identifying a component to be released.
   */
  virtual void deactivate_component (maci::Handle h
				      );

  /**
   * Restarts an component.
   * @param h a handle identifying component to be restarted.
   * @return a new reference of the restarted component.
   */
  virtual CORBA::Object_ptr restart_component (maci::Handle h
					       );

  /**
   * Shutdown the Container. 
   * @param Action to take after shutting down. Bits 8 thru 15 of this parameter denote the action, which can be one of: <UL><LI>0 -- reload the container</LI><LI>1 -- reboot the computer</LI><LI>2 -- exit the container</LI></UL> The bits 0 thru 7 (values 0 to 255) are the return value that the Container will pass to the operating system.
   */
  virtual void shutdown (CORBA::ULong action
			 );
	
  /**
   * Returns information about a subset of components that are currently hosted by the Container. 
   *
   * <B>Note:</B> If the list of handles is empty, information about all components hosted by the container is returned! 
   * @param Handles of the components whose information should be retrieved.
   * @return Information about the selected components.
   */
  virtual maci::ComponentInfoSeq * get_component_info (const maci::HandleSeq & h
					   );

  /**
   * Container name
   * @return name of the container. The caller has to take care for the releaseing of the memory!
   * The memory is allocated with CORBA::string_dup.
   */
  virtual char * name ();

  /**
   * Disconnect notification.
   * The disconnect method is called by the Manager to notify the client that it will be unavailable and that the client should log off. 
   */
  virtual void disconnect ();
	
  /**
   * Authentication method.
   * Method authenticate is the challenge issued to the client after it tries to login. The login will be successful if the client's authenticate() produces the expected result. Only in this case will the Manager's login method return a valid handle, which the client will later use as the id parameter with all calls to the Manager. 
   * @param The question posed by the Manager.
   * @return Answer to the question. The first character of the answer identifies the type of the client: 
   * <TT>A</TT> An container (implements the Container interface)
   */
  virtual maci::AuthenticationData * authenticate (maci::ExecutionId execution_id, const char * question
			       );
	
  /**
   * The Manager and administrators use this method for sending textual messages to the client.
   * @param type Can be either MSG_ERROR or MSG_INFORMATION.
   * @param message Contents of the message. The contents are human readable.
   */
  virtual void message (CORBA::Short type,
			const char * message
			);

  /**
   * The Manager and administrators use this method for sending tagged textual messages to the client.
   * @param type Can be either MSG_ERROR or MSG_INFORMATION.
   * @param tag  Additional information tage
   * @param message Contents of the message. The contents are human readable.
   */
  virtual void taggedmessage (CORBA::Short type,
			CORBA::Short tag,
			const char * message
			);

  /**
   * Notify client about the change (availability) of the components currently in use by this client. For administrative clients, notification is issued for the change of availability of any component in the domain.
   * @param components A sequence of ComponentInfo structures identifying the affected components. Regular clients receive the name, the type, the handle and the reference of the newly activated component. Administrative clients also receive the handle of the Container where the component was activated.
   */
  virtual void components_available (const maci::ComponentInfoSeq & components
			       );

  /** 
   * Notify client that some of the components currently in use by client have become unavailable.
   * @param component_names CURLs of the unavailable components  
   */
  virtual void components_unavailable (const maci::stringSeq & component_names
				 );

  /**
   * Notify container about component shutdown order.
   * @param h ordered list of components' handles.
   */
  virtual void set_component_shutdown_order(const maci::HandleSeq & h
					   );

  /** 
   * Get a component, activating it if necessary.
   * The client must have adequate access rights to access the component. This is untrue of components: NameService, Log, LogFactory, 
   * NotifyEventChannelFactory, ArchivingChannel@ARCHIVING.channels, LoggingChannel@LOGGING.channels, InterfaceRepository, CDB.
   * @param name name of the component (e.g. MOUTN1)
   * @param domain domain name, 0 for default domain
   * @param activate true to activate component, false to leave it in the current state 
   * @return reference to the component. If the component could not be activated, a CORBA::Object::_nil() reference is returned.
   * @deprecated getContainerService()->getComponent() should be used instead
   */
  virtual CORBA::Object_ptr get_object(const char *name, 
                                       const char *domain,
                                       bool activate
			               );

  /**
   * get_object template method
   * @param name name of the component
   * @param doman domain name (could be 0)
   * @param activate true to activate component, false to leave it in the current state 
   * @return reference to the component
   * it just redirect call to #getComponent, so #getComponent should be used instead
   * @deprecated getContainerService()->getComponent<T>() should be used instead
   */
    template<class T>
    T* get_object(const char *name, const char *domain, bool activate)
	{
	    return getComponent<T>(name, domain, activate);
	}

    /**
     * getComponent template method
     * @param name name of the component
     * @param doman domain name (could be 0)
     * @param activate true to activate component, false to leave it in the current state 
     * @return reference to the component
     * @deprecated getContainerService()->getComponent<T>() should be used instead
   */
    template<class T>
    T* getComponent(const char *name, const char *domain, bool activate);

    /**
     * getService template method
     * @param name name of the service
     * @param doman domain name (could be 0)
     * @param activate true to activate service, false to leave it in the current state 
     * @return reference to the service
     * @throw maciErrType::CannotGetServiceExImpl
     * @tod Could be this method decalred as deprecated since getContainerServices()->getComponent can be used instead ?
     */
    template<class T>
    T* getService(const char *name, const char *domain, bool activate);

  /**
   * Releases the specified component.
   *
   * @param The name of the component instance to be released
   * @return void
   * @htmlonly
   * <br><hr>
   * @endhtmlonly
   */
  void releaseComponent(const char *name);
    
  /**
   * Manager pings its clients (both GUI clients, as well as Containers) repeatedly to verify that they still exist.
   * The return value can be either <code>true</code>, indicating that everything is OK with the client, of <code>false</code>, indicating that client is malfunctioning.
   * If CORBA::TRANSIENT exception is thrown, the Manager should retry the ping several times, and only then shall the client be assumed to be malfunctioning.
   * If another exception is thrown, the client may be immediately assumed to be malfunctioning.
   * Once the client is found to be malfunctioning, the Manager makes an implicit logout of the client.
   * @return <code>true</code>, indicating that everything is OK with the client, of <code>false</code>, indicating that client is malfunctioning.
   */
  virtual CORBA::Boolean ping ();

    /**
     * Logging configurable methods
     */

    virtual Logging::LoggingConfigurable::LogLevels get_default_logLevels();

    virtual void set_default_logLevels(const Logging::LoggingConfigurable::LogLevels&);

    virtual Logging::stringSeq* get_logger_names();

    /*
    * @throw maciErrType::LoggerDoesNotExistEx
    */
    virtual Logging::LoggingConfigurable::LogLevels get_logLevels(const char*);

    /* 
    * @throw maciErrType::LoggerDoesNotExistEx
    */
    virtual void set_logLevels(const char*, const Logging::LoggingConfigurable::LogLevels&);

    virtual void refresh_logging_config();

    static void configureLogger(const std::string& loggerName);

    void loadLoggerConfiguration(const std::string& loggerName);

  protected:
    /**
     * Returns an ACS Logger created for this container.
     * @return an ACS Logger
     */
    Logging::Logger::LoggerSmartPtr
    getLogger() {return m_logger;}
    
    
  private:
    
  /**
   * Parses command-line argument
   * @param argc
   * @param argv
   */
  int parseArgs (int argc, char *argv[]);

  /// Show usage
  void showUsage(int argc, char *argv[]);
  
  /**
   * Build a new ContainerServices object (this object
   * implements the abstract class acsContainerServices)
   * 
   * @param h The handle of the component
   * @param name The name of the component
   * @param poa The POA
   * 
   * @return The ContainerServices object 
   *         (the default is the object defined in maciContainerServices.h)
   */
   ContainerServices* instantiateContainerServices(
        maci::Handle h, 
        ACE_CString& name, 
        ACE_CString& type, 
        PortableServer::POA_ptr poa);

  /// File to output the process id.
  const char * m_pid_file_name;

  /// Manager cmd-ln reference
  const char * m_manager_ref;

  // Container name;
  const char * m_container_name;

  // static pointer to the container
  static ContainerImpl * m_container;

  /// library manager
  static LibraryManager * m_dllmgr;

  /// logger
  static LoggingProxy * m_loggerProxy;

#ifndef MAKE_VXWORKS
  // It is used by the logging proxy to clear/raise alarms
  LogThrottleAlarmImpl* m_logThrottleAlarm_p;
#endif

  static int m_logLevelRefresh;
  static int m_logLevelConfigure;

  /// servant manager
  MACIServantManager * m_servant_mgr; 

  /// Database access
  cdb::Table * m_database;

  // CORBA reference to the Container.
  maci::Container_var m_container_ref;

  //
  // CORBA vars
  //
  CORBA::ORB_var orb;
  PortableServer::POAManager_var poaManager;
  PortableServer::POA_var poaRoot;
  PortableServer::POA_var poaContainer;
  PortableServer::POA_var poaPersistent;
  PortableServer::POA_var poaTransient;

  /// Invocation timeout in milliseconds (0 - disabled)
  static CORBA::ULong m_invocationTimeout;

  /// Reference to the manager
  maci::Manager_var m_manager;

  /// Handle of the container (given from Mamager at logon)
  maci::Handle m_handle;

  /// Status of the container, later used as the return value from the
  /// main() function.
  int m_status;

  /// Is manager shutting down?
  bool m_shutdown;

  /// Structure to hold components information
  struct ContainerComponentInfo
  {
    int lib;		
    maci::ComponentInfo info;
  };
	
  typedef ACE_Hash_Map_Manager <maci::Handle, ContainerComponentInfo, ACE_Recursive_Thread_Mutex> COMPONENT_HASH_MAP;
  typedef ACE_Hash_Map_Iterator <maci::Handle, ContainerComponentInfo, ACE_Recursive_Thread_Mutex> COMPONENT_HASH_MAP_ITER;
  typedef ACE_Hash_Map_Entry <maci::Handle, ContainerComponentInfo> COMPONENT_HASH_MAP_ENTRY;

  /// Data about all active components
  COMPONENT_HASH_MAP m_activeComponents;

  // NOTE: Preserves FIFO order.
  typedef ACE_Unbounded_Set <maci::Handle> COMPONENT_LIST;

  /// Component list (preserves order of activation).
  COMPONENT_LIST m_activeComponentList;

  /// Component shutdown order (given from the manager).
  maci::HandleSeq m_componentShutdownOrder;

  /// Initializes componentA
  bool initializeCORBA(int &argc, char *argv[]);

  /// Finalizes CORBA
  bool doneCORBA();
  public:
  /// Activates CORBA obejct
  CORBA::Object_ptr activateCORBAObject(PortableServer::Servant srvnt,
					const char * name);
 
  /// Deactivates servant
  bool deactivateCORBAObject(PortableServer::Servant servant);

  /// Deactivates servant
  bool deactivateCORBAObject(CORBA::Object_ptr servant);
  private:
  /// Helper method to loadDLL
  int loadDLL(const char * bame);

  /// Resolve manager
  maci::Manager_ptr resolveManager(int nSecTimeout);

  // Logout from Manager
  void logout ();

  // DB prefix (for container)
  ACE_CString m_dbPrefix;

  // DB prefix (for MACI)
  ACE_CString m_dbRootPrefix;

  // argc
  int m_argc;

  // argc w/o ORB, CDB decrement
  int m_fullargc;

  // argv
  char** m_argv;

  /// Shutdown action
  int m_shutdownAction;

  /// Is InterfaceRepository present?
  bool m_hasIFR;

  /// Recovery switch.
  bool m_recovery;

  //
  // shutdown helpers
  //

  /// The mutual exclusion mechanism which is required to use the <condition_>.
  ACE_SYNCH_MUTEX m_shutdownMutex;

  /// Condition used to wait until Container shutdown is finished
  ACE_SYNCH_CONDITION m_shutdownDone;

  /// Signaling state (to avoid waiting for already signaled signal).
  bool m_shutdownDoneSignaled;

  /// Number of server threads to handle CORBA ORB requests.
  int m_serverThreads;

  /// Dynamic container (i.e. without CDB configuration)
  bool m_dynamicContainer;

  /// ContainerServices
  ContainerServices *m_containerServices;

  /// threads' standard start-up hook 
  maci::ContainerThreadHook m_containerThreadHook;

  /// Logger for this container;
  Logging::Logger::LoggerSmartPtr m_logger;

  // default logging levels
  Logging::LoggingConfigurable::LogLevels m_defaultLogLevels;
  
  // loggers configuration 
  std::map<std::string, Logging::LoggingConfigurable::LogLevels> m_logLevels;

  // executionId
  maci::ExecutionId m_executionId;

  // startup timestamp
  ACS::Time m_startTime;

  //Logging stuff
  unsigned long cacheSize;
  unsigned long minCachePriority;
  unsigned long maxCachePriority;
  unsigned int flushPeriodSeconds;
  int maxLogsPerSecond;
    
  // activation thread-pool
  MethodRequestThreadPool* m_methodRequestThreadPool;

  // The thread to update the AlarmSource's
  std::auto_ptr<acsalarm::AlarmSourceThread> m_alarmSourceThread_ap;

  const acsalarm::AlarmSourceThread* getAlarmSourceThread() {
	  if (m_alarmSourceThread_ap.get()==NULL)
	  {
		  m_alarmSourceThread_ap=std::auto_ptr<acsalarm::AlarmSourceThread>(new acsalarm::AlarmSourceThread());
	  }
	  return m_alarmSourceThread_ap.get();
  }
};

///////////////////////////////////////////////////////////////////////////////////////
/**
 * @todo getComponent should throw an exception in case of error
 */
template<class T>
T* ContainerImpl::getComponent(const char *name, const char *domain, bool activate)
{   
    T* object = T::_nil();

    if(!name) // Check if <name> is null
	{
	ACS_SHORT_LOG((LM_DEBUG, "Name parameter is null."));
	return T::_nil();
	}
    
    /**
     * First creates the CURL and query the Manager for the component
     */
    ACE_CString curl = "curl://";
    if (domain)
	curl += domain;

    curl += ACE_CString("/");

    curl += name;

    ACS_SHORT_LOG((LM_DEBUG, "Getting component: '%s'.",  curl.c_str()));
    
    // wait that m_handle become !=0
    while (m_handle==0)
	{
	ACS_SHORT_LOG((LM_DEBUG, "Waiting for m_handle"));
	ACE_OS::sleep(1);
	}
    
    try
	{
	CORBA::Object_var obj = m_manager->get_component(m_handle, curl.c_str(), activate);
	
	if (CORBA::is_nil(obj.in()))
	    {
	    ACS_SHORT_LOG((LM_DEBUG, "Failed to create '%s'.",  curl.c_str()));
	    maciErrType::CannotGetComponentExImpl ex(__FILE__, __LINE__, 
					"ContainerImpl::getComponent&lt;&gt;");
	    ex.setCURL(name);
	    ex.log();
	    // @todo throw ex;
	    return T::_nil();
	    }
	object = T::_narrow(obj.in());
	
	return object;
	}
    catch(maciErrType::ComponentNotAlreadyActivatedEx &_ex)
	{
	maciErrType::CannotGetComponentExImpl ex(_ex, __FILE__, __LINE__, 
					       "maci::ContainerImpl::getComponent&lt;&gt;");
	ex.setCURL(name);
	ex.log();
	/// @todo throw ex;
	return T::_nil();
	}
    catch(maciErrType::ComponentConfigurationNotFoundEx &_ex)
	{
	maciErrType::CannotGetComponentExImpl ex(_ex, __FILE__, __LINE__, 
					       "maci::ContainerImpl::getComponent&lt;&gt;");
	ex.setCURL(name);
	ex.log();
	/// @todo throw ex;
	return T::_nil();
	}
    catch( CORBA::SystemException &_ex )
	{
	ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__,
							    "ContainerServices::getComponent&lt;&gt;");
	corbaProblemEx.setMinor(_ex.minor());
	corbaProblemEx.setCompletionStatus(_ex.completed());
	corbaProblemEx.setInfo(_ex._info().c_str());
	maciErrType::CannotGetComponentExImpl ex(corbaProblemEx, __FILE__, __LINE__,
					       "ContainerImpl::getComponent&lt;&gt;");
	ex.setCURL(name);
	ex.log();
	/// @todo throw ex;
	return T::_nil();
	}
    catch(...)
	{
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__, 
							"ContainerImpl::getComponent&lt;&gt;");
	maciErrType::CannotGetComponentExImpl ex(uex, __FILE__, __LINE__,
					       "ContainerImpl::getComponent&lt;&gt;");
	ex.setCURL(name);
	ex.log();
	/// @todo throw ex;
	return T::_nil();
	}
}//getComponent
////////////////////////////////////////////////////////////////////////////
/** 
 * Implementation for getService template method
 */
template<class T>
T* ContainerImpl::getService(const char *name, const char *domain, bool activate) 
{   
    ACS_TRACE("ContainerImpl::getService&lt;&gt;");
    T* object = T::_nil();
   
    if(!name)  // Check if <name> is null
	{
	ACSErrTypeCommon::NullPointerExImpl nullEx(__FILE__, __LINE__,
						   "ContainerImpl::getService&lt;&gt;");
	nullEx.setVariable("(parameter) name");
	maciErrType::CannotGetServiceExImpl ex(nullEx, __FILE__, __LINE__,
					       "ContainerImpl::getService&lt;&gt;");
	ex.setCURL("NULL");
	throw ex;
	}//if
    
//     First creates the CURL and query the Manager for the component
    ACE_CString curl = "curl://";
    if (domain)
	curl += domain;

    curl += ACE_CString("/");

    curl += name;

    ACS_SHORT_LOG((LM_DEBUG, "Getting service: '%s'.",  curl.c_str()));
    
    // wait that m_handle become !=0
    while (m_handle==0)
	{
	ACS_SHORT_LOG((LM_DEBUG, "Waiting for m_handle"));
	ACE_OS::sleep(1);
	}

    try
	{
	CORBA::Object_var obj = m_manager->get_service(m_handle, curl.c_str(), activate);
	
	if (CORBA::is_nil(obj.in()))
	    {
	    maciErrType::CannotGetServiceExImpl ex(__FILE__, __LINE__, 
						   "ContainerImpl::getService&lt;&gt;");
	    ex.setCURL(name);
	    throw ex;
	    }//if
	object = T::_narrow(obj.in());
	
	return object;
	}
    catch(maciErrType::CannotGetComponentEx &_ex)
	{
	 maciErrType::CannotGetServiceExImpl ex(_ex, __FILE__, __LINE__,
						"ContainerImpl::getService&lt;&gt;");
	 ex.setCURL(name);
	 throw ex;
	}
    catch(maciErrType::ComponentNotAlreadyActivatedEx &_ex)
	{
	 maciErrType::CannotGetServiceExImpl ex(_ex, __FILE__, __LINE__, 
						"ContainerImpl::getService&lt;&gt;");
	 ex.setCURL(name);
	 throw ex;
	}
    catch(maciErrType::ComponentConfigurationNotFoundEx &_ex)
	{
	maciErrType::CannotGetServiceExImpl ex(_ex, __FILE__, __LINE__, 
					       "ContainerImpl::getService&lt;&gt;");
	ex.setCURL(name);
	throw ex;
	}
    catch( CORBA::SystemException &_ex )
	{
	ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__,
							    "ContainerImpl::getService&lt;&gt;");
	corbaProblemEx.setMinor(_ex.minor());
	corbaProblemEx.setCompletionStatus(_ex.completed());
	corbaProblemEx.setInfo(_ex._info().c_str());
	maciErrType::CannotGetServiceExImpl ex(corbaProblemEx, __FILE__, __LINE__,
					       "ContainerImpl::getService&lt;&gt;");
	ex.setCURL(name);
	throw ex;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__, 
							"ContainerImpl::getService&lt;&gt;");
	maciErrType::CannotGetServiceExImpl ex(uex, __FILE__, __LINE__,
					       "ContainerImpl::getService&lt;&gt;");
	ex.setCURL(name);
	throw ex;
	}//try-catch
}//getService

}; 

#endif // maciContainerImpl_h

