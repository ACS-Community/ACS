#ifndef maciSimpleClient_H
#define maciSimpleClient_H

/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciSimpleClient.h,v 1.92 2006/10/09 06:13:17 gchiozzi Exp $"
*
* who       when        what
* --------  --------    ----------------------------------------------
* msekoran  2001/12/24  documented, improved
* bjeram    2001-11-20  added get_object() template method
* msekoran  2001/03/14  created
* msekoran  2001/05/16  modified to work with new maci
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsutil.h>
#include <maciExport.h>

#include <logging.h>

#include <maciS.h>
#include <ace/SString.h>

#include "maciSimpleClientThreadHook.h"

namespace maci {

/**
 * The class SimpleClient is the base class for a ACS C++ client.
 * It hides most of the CORBA interface to the implementation 
 * of the real client.
 */

class maci_EXPORT SimpleClient : 
    public virtual POA_maci::Client,
    public virtual PortableServer::RefCountServantBase
{

public:

  /**
   * Constructor.
   */
  SimpleClient ();

  /**
   * Destructor.
   */
  virtual ~SimpleClient ();

  /**
   * Destroys Client.
   * @return 0 on failure
   */
  int destroy();
  
  /**
   * Initializes CORBA.
   * @return 0 on failure
   */
  int initCORBA(int argc, char * argv[]);

  /**
   * Get ORB
   */
  CORBA::ORB_ptr getORB();

  /**
   * Finalizes CORBA.
   * @return 0 on failure
   */
  int doneCORBA();

  /**
   * Login method
   * Logins client to the manager.
   * @return 0 on failure
   */
  int login();

  /**
   * Logout method.
   * @return 0 on failure
   */
  int logout();

  /**
   * The init method logs in to the requested manager.
   * Default is the local manager (if no arguments are given).
   * To specify a remote manager give the parameter
   * -m corbaloc::<host name>:<port number>/<manager>
   *
   * example:
   * -m corbaloc::te1.hq.eso.org:xxxx/Manager
   *     
   * @return 0 on failure
   */
  int init(int argc, char *argv[]);

  /**
   * Run the ORB event loop with the specified <tv> time value.
   * @param tv time to run
   * @return 0 on failure
   */
  int run (ACE_Time_Value &tv 
	   );

  /**
   * Run the ORB event loop until terminated or shutdown.
   * @return 0 on failure
   */
  int run ();

    static void initThread(const char * threadName);
    static void doneThread();
  
  /**
   * Get manager CORBA reference.
   * @return manager CORBA reference
   */
  maci::Manager_ptr manager();

  /**
   * Get handle of the client.
   * @return handle
   */
  maci::Handle handle();
  
  /** 
   * Get a component, activating it if necessary.
   * The client must have adequate access rights to access the component. This is untrue of components: NameService, Log, LogFactory, 
   * NotifyEventChannelFactory, ArchivingChannel, LoggingChannel, InterfaceRepository, CDB and PDB.
   * @param name name of the component (e.g. MOUTN1)
   * @param domain domain name, 0 for default domain
   * @param activate true to activate component, false to leave it in the current state 
   * @return reference to the component. If the component could not be activated, a CORBA::Object::_nil() reference is returned,
   * @see template<class T> T* get_object
   */
  CORBA::Object_ptr get_object(const char *name, const char *domain, bool activate
			       );

  /** 
   * Get a component, activating it if necessary and directly narrows it to the type
   * declared in the template definition.
   * The client must have adequate access rights to access the component. This is untrue of components: NameService, Log, LogFactory, 
   * NotifyEventChannelFactory, ArchivingChannel, LoggingChannel, InterfaceRepository, CDB and PDB.
   * @param name name of the component (e.g. MOUNT1)
   * @param domain domain name, 0 for default domain
   * @param activate true to activate component, false to leave it in the current state 
   * @return reference to the component. If the component could not be activated, a CORBA::Object::_nil() reference is returned,
   * For example:
   * @code
   *    MACI_TEST::MaciTestClass_var maciTestDO = client.get_object<MACI_TEST::MaciTestClass>(argv[1], 0, true);
   * @endcode
   * @see get_object()
   */
    template<class T>
    T* get_object(const char *name, const char *domain, bool activate
		  );

  /* ----------------------------------------------------------------*/
  /* ------------------ [ CORBA Client interface ] ------------------*/
  /* ----------------------------------------------------------------*/

  /**
   * Client name
   */
  virtual char * name ()
      throw (CORBA::SystemException);

  /**
   * Disconnect notification.
   * The disconnect method is called by the Manager to notify the client that it will be unavailable and that the client should log off. 
   */
  virtual void disconnect ()
    throw (CORBA::SystemException);
	
  /**
   * Authentication method.
   * Method authenticate is the challenge issued to the client after it tries to login. The login will be successful if the client's authenticate() produces the expected result. Only in this case will the Manager's login method return a valid handle, which the client will later use as the id parameter with all calls to the Manager. 
   * @param The question posed by the Manager.
   * @return Answer to the question. The first character of the answer identifies the type of the client: 
   * <TT>A</TT> An container (implements the Container interface)
   * <TT>C</TT> A client (implements the Client interface)
   * <TT>S</TT> A supervisor (implements the Administrator interface)
   */
   virtual char * authenticate (const char * question)
       throw (CORBA::SystemException);
	
  /**
   * The Manager and administrators use this method for sending textual messages to the client.
   * @param type Can be either MSG_ERROR or MSG_INFORMATION.
   * @param message Contents of the message. The contents are human readable.
   */
  virtual void message (CORBA::Short type,
			const char * message
			)
    throw (CORBA::SystemException);

  /**
   * Notify client about the change (availability) of the components currently in use by this client. For administrative clients, notification is issued for the change of availability of any component in the domain.
   * @param cobs A sequence of ComponentInfo structures identifying the affected components. Regular clients receive the name, the type, the handle and the reference of the newly activated component. Administrative clients also receive the handle of the Container where the component was activated.
   */
  virtual void components_available (const maci::ComponentInfoSeq & cobs
			       )
    throw (CORBA::SystemException);

  /**
   * Notify client that some of the components currently in use by client have become unavailable.
   * @param cob_names CURLs of the unavailable components  
   */
  virtual void components_unavailable (const maci::stringSeq & cob_names
				 )
    throw (CORBA::SystemException);
  
			
  /**
   * Manager pings its clients (both GUI clients, as well as Containers) repeatedly to verify that they still exist.
   * The return value can be either <code>true</code>, indicating that everything is OK with the client, of <code>false</code>, indicating that client is malfunctioning.
   * If CORBA::TRANSIENT exception is thrown, the Manager should retry the ping several times, and only then shall the client be assumed to be malfunctioning.
   * If another exception is thrown, the client may be immediately assumed to be malfunctioning.
   * Once the client is found to be malfunctioning, the Manager makes an implicit logout of the client.
   * @return <code>true</code>, indicating that everything is OK with the client, of <code>false</code>, indicating that client is malfunctioning.
   */
  virtual CORBA::Boolean ping ()
      throw (CORBA::SystemException);

    /// Get logging proxy instance
    /// @return logging proxy instance
    static LoggingProxy * getLoggerProxy()
	{
	    return m_logger;
	}

    /// Get SimpleClient's proces name
    /// @return  SimpleClient's proces name
    static const char * getProcessName()
	{
	    return m_processName.c_str();
	}

private:
  
  /// Reference to the Manager.
  maci::Manager_var m_manager;

  /// Handle.
  maci::Handle m_handle;
  
  /// Initialization status.
  bool m_initialized;

  /// Root POA.
  PortableServer::POA_var m_poaRoot;

  /// Persistent POA.
  PortableServer::POA_var m_poaPersistent;

  /// Persistent POA.
  PortableServer::POA_var m_poaTransient;

  /// The CORBA ORB.
  CORBA::ORB_var m_orb;

  /// Logger.
  static LoggingProxy* m_logger;

  /// The name of the process
  static ACE_CString m_processName;

  /// threads' standard start-up hook 
  maci::SimpleClientThreadHook m_simpleClientThreadHook;
}; /* end class SimpleClient */

/*
 * INLINE METHODS
 */

/* 
 * Implementation for get_object template method
 */

template<class T>
T* SimpleClient::get_object(const char *name, const char *domain, bool activate
			    )
{
    T* object = T::_nil();
    
    /**
     * Check first if the client is initialized
     */
    if(!m_initialized)
	{
	ACS_SHORT_LOG((LM_DEBUG, "Client not initialized."));
	return T::_nil();
	}
    
    /**
     * Check if <name> is null
     */
    if(!name)
	{
	ACS_SHORT_LOG((LM_DEBUG, "Name parameter is null."));
	return T::_nil();
	}
    
    /**
     * First creates the CURL, if not already a CURL,
     * and query the Manager for the component
     */
    char *curl_str = "curl://";
    
    ACE_CString curl = "";
    if(strncmp(name, curl_str, strlen(curl_str)) != 0 )
	{
	curl += curl_str;
	if (domain)
	    curl += domain;
	
	curl += ACE_CString("/");
	}
    curl += name;
    
    ACS_SHORT_LOG((LM_DEBUG, "Getting device: '%s'. Creating it...",  curl.c_str()));
    
    try
	{
	CORBA::Object_var obj = manager()->get_service(m_handle, curl.c_str(), activate);
	
	if (CORBA::is_nil(obj.in()))
	    {
	    ACS_SHORT_LOG((LM_DEBUG, "Failed to create '%s'",  curl.c_str()));
	    return T::_nil();
	    }
	object = T::_narrow(obj.in());
	
	return object;
	}
    catch( CORBA::Exception &ex )
	{
	ACE_PRINT_EXCEPTION(ex,
			    "maci::SimpleClient::get_object");
	return T::_nil();
	}
    catch(...)
	{
	return T::_nil();
	}
    return T::_nil();
}

 }; 

#endif  /* maciSimpleClient_H */
