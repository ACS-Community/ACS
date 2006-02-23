#ifndef maciTestClientImpl_h
#define maciTestClientImpl_h

/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciTestClientImpl.h,v 1.83 2005/04/13 13:00:14 acaproni Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* kzager   2002-02-15 Created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */


#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif


#include <maciS.h>
#include <SString.h>

//Class MaciTestClientImpl
class  MaciTestClientImpl : public virtual POA_maci::Client
{
public:
  //Constructor
  MaciTestClientImpl (ACE_CString name, maci::Manager_ptr mgr, int onPing);

  //Destructor
  virtual ~MaciTestClientImpl (void);

  void setHandle(maci::Handle h) { m_handle = h; };

virtual char * name (
    
  )
  throw (
    CORBA::SystemException
  );

virtual void disconnect (
    
  )
  throw (
    CORBA::SystemException
  );

virtual char * authenticate (
    const char * question
    
  )
  throw (
    CORBA::SystemException
  );

virtual void message (
    CORBA::Short type,
    const char * message
    
  )
  throw (
    CORBA::SystemException
  );

virtual CORBA::Boolean ping (
    
  )
  throw (
    CORBA::SystemException
  );

virtual void components_available (
    const maci::ComponentInfoSeq & cobs
    
  )
  throw (
    CORBA::SystemException
  );

virtual void components_unavailable (
    const maci::stringSeq & cob_names
    
  )
  throw (
    CORBA::SystemException
  );
protected:
  ACE_CString m_name;
  maci::Manager_ptr m_manager;
  maci::Handle m_handle;
  int m_onPing;
};

//Class MaciTestContainerImpl
class  MaciTestContainerImpl :
  public virtual POA_maci::Container,
  public virtual MaciTestClientImpl
{
public:
  //Constructor
  MaciTestContainerImpl (ACE_CString name, maci::Manager_ptr mgr,
    PortableServer::POA_ptr poa, int onPing, int onActivate,
    bool haveRecovery);

  //Destructor
  virtual ~MaciTestContainerImpl (void);

virtual maci::ComponentInfo * activate_component (
    maci::Handle h,
    const char * name,
    const char * exe,
    const char * type
    
  )
  throw (
    CORBA::SystemException
  );

virtual void deactivate_components (
    const maci::HandleSeq & h
    
  )
  throw (
    CORBA::SystemException
  );

virtual void shutdown (
    CORBA::ULong action
    
  )
  throw (
    CORBA::SystemException
  );

virtual maci::ComponentInfoSeq * get_component_info (
    const maci::HandleSeq & h
    
  )
  throw (
    CORBA::SystemException
  );

virtual char * name (
    
  )
  throw (
    CORBA::SystemException
  );

virtual void disconnect (
    
  )
  throw (
    CORBA::SystemException
  );

virtual char * authenticate (
    const char * question
    
  )
  throw (
    CORBA::SystemException
  );

virtual void message (
    CORBA::Short type,
    const char * message
    
  )
  throw (
    CORBA::SystemException
  );

virtual CORBA::Boolean ping (
    
  )
  throw (
    CORBA::SystemException
  );

virtual void components_available (
    const maci::ComponentInfoSeq & cobs
    
  )
  throw (
    CORBA::SystemException
  );

virtual void components_unavailable (
    const maci::stringSeq & cob_names
    
  )
  throw (
    CORBA::SystemException
  );

protected:
  int m_onActivate;
  PortableServer::POA_ptr m_poa;
  bool m_haveRecovery;
};

//Class MaciTestAdministratorImpl
class  MaciTestAdministratorImpl :
  public virtual POA_maci::Administrator,
  public virtual MaciTestClientImpl
{
public:
  //Constructor
  MaciTestAdministratorImpl (ACE_CString name, maci::Manager_ptr mgr, int onPing);

  //Destructor
  virtual ~MaciTestAdministratorImpl (void);

virtual void client_logged_in (
    const maci::ClientInfo & info
    
  )
  throw (
    CORBA::SystemException
  );

virtual void client_logged_out (
    maci::Handle h
    
  )
  throw (
    CORBA::SystemException
  );

virtual void container_logged_in (
    const maci::ContainerInfo & info
    
  )
  throw (
    CORBA::SystemException
  );

virtual void container_logged_out (
    maci::Handle h
    
  )
  throw (
    CORBA::SystemException
  );

virtual void components_requested (
    const maci::HandleSeq & clients,
    const maci::HandleSeq & COBs
    
  )
  throw (
    CORBA::SystemException
  );

virtual void components_released (
    const maci::HandleSeq & clients,
    const maci::HandleSeq & COBs
    
  )
  throw (
    CORBA::SystemException
  );

virtual char * name (
    
  )
  throw (
    CORBA::SystemException
  );

virtual void disconnect (
    
  )
  throw (
    CORBA::SystemException
  );

virtual char * authenticate (
    const char * question
    
  )
  throw (
    CORBA::SystemException
  );

virtual void message (
    CORBA::Short type,
    const char * message
    
  )
  throw (
    CORBA::SystemException
  );

virtual CORBA::Boolean ping (
    
  )
  throw (
    CORBA::SystemException
  );

virtual void components_available (
    const maci::ComponentInfoSeq & cobs
    
  )
  throw (
    CORBA::SystemException
  );

virtual void components_unavailable (
    const maci::stringSeq & cob_names
    
  )
  throw (
    CORBA::SystemException
  );

};

#endif /* maciTestClientImpl_h */



