#ifndef maciTestClientImpl_h
#define maciTestClientImpl_h

/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciTestClientImpl.h,v 1.87 2012/01/21 22:48:11 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* kzagar   2002-02-15 Created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */


#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif


#include <maciS.h>
#include <ace/SString.h>

//Class MaciTestClientImpl
class  MaciTestClientImpl : public virtual POA_maci::Client
{
public:
  //Constructor
  MaciTestClientImpl (ACE_CString name, maci::Manager_ptr mgr, int onPing);

  //Destructor
  virtual ~MaciTestClientImpl (void);

  void setHandle(maci::Handle h) { m_handle = h; };

virtual char * name ();

virtual void disconnect ();

virtual maci::AuthenticationData * authenticate (
    maci::ExecutionId execution_id,
    const char * question);

virtual void message (
    CORBA::Short type,
    const char * message
  );

virtual void taggedmessage (
    CORBA::Short type,
    CORBA::Short tag,
    const char * message
  );

virtual CORBA::Boolean ping ();

virtual void components_available (
    const maci::ComponentInfoSeq & cobs);

virtual void components_unavailable (
    const maci::stringSeq & cob_names);
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
    maci::ExecutionId execution_id,
    const char * name,
    const char * exe,
    const char * type
  );

virtual void deactivate_components (
    const maci::HandleSeq & h
  );

virtual void shutdown (
    CORBA::ULong action
  );

virtual maci::ComponentInfoSeq * get_component_info (
    const maci::HandleSeq & h
  );

virtual char * name ();

virtual void disconnect ();

virtual maci::AuthenticationData * authenticate (
    maci::ExecutionId execution_id,
    const char * question
  );

virtual void message (
    CORBA::Short type,
    const char * message
  );

virtual CORBA::Boolean ping (
  );

virtual void components_available (
    const maci::ComponentInfoSeq & cobs
  );

virtual void components_unavailable (
    const maci::stringSeq & cob_names
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
    const maci::ClientInfo & info,
    ACS::Time timestamp,
    maci::ExecutionId execution_id
  );

virtual void client_logged_out (
    maci::Handle h,
    ACS::Time timestamp
  );

virtual void container_logged_in (
    const maci::ContainerInfo & info,
    ACS::Time timestamp,
    maci::ExecutionId execution_id
  );

virtual void container_logged_out (
    maci::Handle h,
    ACS::Time timestamp
  );

virtual void components_requested (
    const maci::HandleSeq & clients,
    const maci::HandleSeq & COBs,
    ACS::Time timestamp
  );

virtual void components_released (
    const maci::HandleSeq & clients,
    const maci::HandleSeq & COBs,
    ACS::Time timestamp
  );

virtual void component_activated (
    const maci::ComponentInfo & info,
    ACS::Time timestamp,
    maci::ExecutionId execution_id
  );

virtual void component_deactivated (
    maci::Handle h,
    ACS::Time timestamp
  );

virtual char * name ();

virtual void disconnect ();

virtual maci::AuthenticationData * authenticate (
    maci::ExecutionId execution_id,
    const char * question
  );

virtual void message (
    CORBA::Short type,
    const char * message
  );

virtual void taggedmessage (
    CORBA::Short type,
    CORBA::Short tag,
    const char * message
  );

virtual CORBA::Boolean ping (
  );

virtual void components_available (
    const maci::ComponentInfoSeq & cobs
  );

virtual void components_unavailable (
    const maci::stringSeq & cob_names
  );

};

#endif /* maciTestClientImpl_h */



