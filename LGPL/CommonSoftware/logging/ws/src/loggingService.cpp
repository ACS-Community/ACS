/*******************************************************************************
*     ALMA - Atacama Large Millimiter Array
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
* "@(#) $Id: loggingService.cpp,v 1.61 2008/10/28 13:46:13 msekoran Exp $"
*
* who       when        what
* --------  ---------   ----------------------------------------------
* bjeram    2003-03-10  some changes in create_with_id(...) accoridng to changes in TAO x.3
* bjeram    2003-03-10  sdded #include <strstream>
* bjeram    2003-03-10  LogMgr_i -> TAO_LogMgr_i, BasicLogFactory_i -> TAO_BasicLogFactory_i
* msekoran  2001-12-23  using LoggingHelper::resolveNameService
* msekoran  2001-07-08  added implementation of sending logs to Notify Service
* msekoran  2001-06-17  created
*/

#include "loggingService.h"

#include "loggingHelper.h"
#include "logging.h"

#include "loggingACSStructuredPushSupplierBin.h"
#include "loggingACSStructuredPushSupplierXml.h"

#include <tao/IORTable/IORTable.h>

#include <acscommonC.h>

#define LOG_NAME "Log"
/*****************************************************************/
LoggingService::LoggingService (void)
  : m_basic_log_factory_name (acscommon::LOG_FACTORY_NAME),
    m_basic_log_name (LOG_NAME),
    m_isInitialized(false)
{
  // No-Op.
  m_ifgop = CosNotifyChannelAdmin::OR_OP;

  m_isInitialized = true;
  m_logBin = false;
  char *acsLogType = getenv("ACS_LOG_BIN");
  if (acsLogType && *acsLogType){
    if(strcmp("true", acsLogType) == 0)
        m_logBin = true;
  }
}

LoggingService::~LoggingService (void)
{
}

void
LoggingService::init_ORB  (int& argc, char *argv []
			   )
{
    for(int i=0; i<argc; i++)
	ACE_OS::printf("argv[%d]=%s\n", i, argv[i]);

    this->m_orb = CORBA::ORB_init (argc,
				 argv,
				 "TAO"
				 );


  CORBA::Object_var m_poaobject  =
    this->m_orb->resolve_initial_references("RootPOA"
                                           );

  /*
    PortableServer::POA_var poaRoot =
    PortableServer::POA::_narrow (m_poaobject.in (),
    );
    /
  */

  this->m_poa =
    PortableServer::POA::_narrow (m_poaobject.in ()
                                  );


  PortableServer::POAManager_var m_poamanager =
    m_poa->the_POAManager ();

  /*
    //
    // Prepare policies our POAs will be using.
    //
    PortableServer::IdAssignmentPolicy_var user_id_policy =
    poaRoot->create_id_assignment_policy(PortableServer::USER_ID);

    PortableServer::LifespanPolicy_var persistent_policy =
    poaRoot->create_lifespan_policy(PortableServer::PERSISTENT);

    PortableServer::ServantRetentionPolicy_var servant_retention_policy  =
    poaRoot->create_servant_retention_policy (PortableServer::RETAIN);


    CORBA::PolicyList policies;
    policies.length(3);

    policies[0] = PortableServer::LifespanPolicy::_duplicate(persistent_policy.in());
    policies[1] = PortableServer::IdAssignmentPolicy::_duplicate(user_id_policy.in());
    policies[2] = PortableServer::ServantRetentionPolicy::_duplicate(servant_retention_policy.in());

    this->m_poa = poaRoot->create_POA("PersistentPOA",
    m_poamanager.in(),
    policies);


    // We're done using the policies.
    user_id_policy->destroy();
    persistent_policy->destroy();
    servant_retention_policy->destroy();

  */
  if (CORBA::is_nil(m_poa.ptr()))
      {
      throw CORBA::NO_RESOURCES();
      }

  m_poamanager->activate ();

}

void
LoggingService::startup (int argc, char *argv[]
			 )
{
  ACS_SHORT_LOG ((LM_INFO,
              "Starting up the ACS Centralized Logger..."));

  // Tnitalize the ORB.
  init_ORB (argc, argv);


  ACS_SHORT_LOG ((LM_INFO,
              "Resolving Naming Service..."));

  // Resolve the naming service.
  resolve_naming_service ();


  ACS_SHORT_LOG ((LM_INFO,
              "Resolving Notify Service..."));

  // Resolve the notify factory
  resolve_notify_factory ();


  ACS_SHORT_LOG ((LM_INFO,
              "Creating Event Channels..."));

  // Create Event channels (LoggingChannel)
  create_EC ();


  ACS_SHORT_LOG ((LM_INFO,
              "Creating Supplier Admins..."));

  // Create Supplier Admin
  create_supplieradmin ();


  ACS_SHORT_LOG ((LM_INFO,
              "Creating Suppliers..."));

  // Create Suppliers
  create_suppliers ();


  ACS_SHORT_LOG ((LM_INFO,
              "Creating Log Factory..."));

  // Create the Basic Log Factory
  create_basic_log_factory();


  ACS_SHORT_LOG ((LM_INFO,
              "Creating Log..."));

  // Create the Basic Log
  create_basic_log();


  ACS_SHORT_LOG ((LM_INFO, "ACS Centralized Logger is initialized."));

}

void
LoggingService::resolve_naming_service ()
{
/*
  CORBA::Object_var naming_obj =
    this->m_orb->resolve_initial_references (acscommon::NAMING_SERVICE_NAME,
                                            );

  // Need to check return value for errors.
  if (CORBA::is_nil (naming_obj.in ()))
    ACE_THROW (CORBA::UNKNOWN ());

  this->m_naming_context =
    CosNaming::NamingContext::_narrow (naming_obj.in (),
                                       );
*/

    this->m_naming_context = LoggingHelper::resolveNameService(this->m_orb.in());
    if (CORBA::is_nil(m_naming_context.ptr()))
	{
    	throw CORBA::UNKNOWN ();
	}
}

void
LoggingService::resolve_notify_factory ()
{
  // first check inital references (to support non-TAO implementations of NotifyService)
    try
    {
      CORBA::Object_var obj =
	this->m_orb->resolve_initial_references (acscommon::LOGGING_NOTIFICATION_FACTORY_NAME);


      this->m_notify_factory =
	CosNotifyChannelAdmin::EventChannelFactory::_narrow (obj.in ()
							     );


     if (!CORBA::is_nil(this->m_notify_factory.in()))
	  return;

    }
  catch(...)
    {
    }

  // then try with naming service
  ACE_ASSERT(!CORBA::is_nil (this->m_naming_context.in ()));

  CosNaming::Name name (1);
  name.length (1);
  name[0].id = CORBA::string_dup (acscommon::LOGGING_NOTIFICATION_FACTORY_NAME);

  CORBA::Object_var obj =
    this->m_naming_context->resolve (name
				     );


    this->m_notify_factory =
      CosNotifyChannelAdmin::EventChannelFactory::_narrow (obj.in ()
							   );


  if (CORBA::is_nil(this->m_notify_factory.in()))
    {
      ACS_SHORT_LOG ((LM_DEBUG, "Failed to resolve %s using ORB's initial references and naming service!", acscommon::LOGGING_NOTIFICATION_FACTORY_NAME));
      throw CORBA::NO_RESOURCES();
    }

}

int
LoggingService::run (void)
{
    //CP: This message is important/expected from acs scripts in order to start ACS
  //ACS_SHORT_LOG ((LM_INFO, "ACS Centralized Logger is running..."));
  ACE_OS::printf("ACS Centralized Logger is running...");
  ACE_OS::fflush (stdout);
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

void
LoggingService::shutdown ()
{

  // Destroy Basic Log.
  if (!CORBA::is_nil(m_basic_log.ptr()))
    {
    try
	{
	  m_basic_log->destroy();
	}
    catch(...)
	{
	  ACS_SHORT_LOG((LM_ERROR, "Failed to destroy DsLogAdmin::BasicLog instance."));
	}
   }

  // Destroy ECs.
  if (!CORBA::is_nil(this->m_logging_ec.in()))
    {
    try
	{
	  this->m_logging_ec->destroy ();
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR, "Failed to destroy Logging EventChannel instance."));
	}
    }

  // Deactivate.
  if (m_poa.ptr() != PortableServer::POA::_nil())
  {
  try
	{
	  PortableServer::ObjectId_var oid =
	      this->m_poa->servant_to_id (&this->m_basic_log_factory
					  );

	  // Deactivate from the POA.
	  this->m_poa->deactivate_object (oid.in ()
					  );

	}
  catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR, "Failed to destroy DsLogAdmin::BasicLogFactory instance."));
	}
  }

  if (!CORBA::is_nil(this->m_naming_context.in()))
    {
      // Unbind from the naming service.
      CosNaming::Name name (1);
      name.length (1);

      try
	{
	  name[0].id = CORBA::string_dup (this->m_basic_log_factory_name);
	  this->m_naming_context->unbind (name);
	}
      catch(...)
	{
	  ACS_SHORT_LOG((LM_ERROR, "Failed to unbind DsLogAdmin::BasicLogFactory."));
	}

      try
	{
	  name[0].id = CORBA::string_dup (this->m_basic_log_name);
	  this->m_naming_context->unbind (name);
	}
      catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR, "Failed to unbind DsLogAdmin::BasicLog."));
	}

      try
	{
        if(!m_logBin)
	        name[0].id = CORBA::string_dup (acscommon::LOGGING_CHANNEL_XML_NAME);
	    else
	        name[0].id = CORBA::string_dup (acscommon::LOGGING_CHANNEL_NAME);


      name[0].kind = CORBA::string_dup (acscommon::LOGGING_CHANNEL_KIND);
	  this->m_naming_context->unbind (name);
	}
      catch(...)
	{
	  ACS_SHORT_LOG((LM_ERROR, "Failed to unbind Logging EventChannel."));
	}
    }

  // shutdown the ORB.
  if (!CORBA::is_nil (this->m_orb.in ()))
    {
      this->m_orb->shutdown (true);

    }
}

/*****************************************************************/

void
LoggingService::create_basic_log_factory()
{

  // Set suppliers.
  this->m_basic_log_factory.set_logging_supplier(m_logging_supplier);

  DsLogAdmin::BasicLogFactory_var obj =
      this->m_basic_log_factory.activate (this->m_orb.in(),
					  this->m_poa.in ()
	  );


  ACE_ASSERT (!CORBA::is_nil (obj.in ()));

  // Register the Basic Log Factory
  ACE_ASSERT(!CORBA::is_nil (this->m_naming_context.in ()));

  CosNaming::Name name (1);
  name.length (1);
  name[0].id = CORBA::string_dup (this->m_basic_log_factory_name);

  this->m_naming_context->rebind (name,
				  obj.in ()
				  );


  ACS_SHORT_LOG ((LM_DEBUG,
              "Log Factory registered with the naming service as: %s",
              this->m_basic_log_factory_name));
}

void
LoggingService::create_basic_log()
{
  DsLogAdmin::LogFullActionType logfullaction = DsLogAdmin::halt;
  CORBA::ULongLong max_size = 0; // 0 means "infinite"

  DsLogAdmin::LogId logid = 0;

  m_basic_log =
    this->m_basic_log_factory.create (logfullaction,
				      max_size,
				      logid
				      );


  ACE_ASSERT ((!CORBA::is_nil(m_basic_log.ptr())));

  // Register the Basic Log
  ACE_ASSERT(!CORBA::is_nil (this->m_naming_context.in ()));

  CosNaming::Name name (1);
  name.length (1);
  name[0].id = CORBA::string_dup (this->m_basic_log_name);

  this->m_naming_context->rebind (name,
				  m_basic_log.in ()
				  );


  ACS_SHORT_LOG ((LM_DEBUG,
              "Log registered with the naming service as: %s",
              this->m_basic_log_name));

  // and make it corbaloc accessible
  CORBA::Object_var table_object =
    this->m_orb->resolve_initial_references ("IORTable");

  IORTable::Table_var adapter =
    IORTable::Table::_narrow (table_object.in ());
  if (CORBA::is_nil (adapter.in ()))
    {
    ACS_SHORT_LOG ((LM_ERROR, "Nil IORTable. corbaloc support not enabled."));
    }
  else
    {
    CORBA::String_var ior =
      this->m_orb->object_to_string (this->m_basic_log.in ());
        adapter->bind (this->m_basic_log_name, ior.in ());
    }

}

void
LoggingService::create_EC ()
{
    CosNaming::Name name (1);
    name.length (1);
    if(!m_logBin)
        name[0].id = CORBA::string_dup (acscommon::LOGGING_CHANNEL_XML_NAME);
    else
        name[0].id = CORBA::string_dup (acscommon::LOGGING_CHANNEL_NAME);

    name[0].kind = CORBA::string_dup (acscommon::LOGGING_CHANNEL_KIND);

    try
	{
	//use the naming service to get our object
	CORBA::Object_var ec_obj =  this->m_naming_context->resolve(name);
	ACE_ASSERT(!CORBA::is_nil(ec_obj.in()));

	//narrow it
	m_logging_ec = CosNotifyChannelAdmin::EventChannel::_narrow(ec_obj.in());
	ACE_ASSERT(!CORBA::is_nil(m_logging_ec.in()));
	}

    catch(CosNaming::NamingContext::NotFound ex)
	{
	// Logging Channel
	CosNotifyChannelAdmin::ChannelID id;

	m_logging_ec = m_notify_factory->create_channel (m_initial_qos,
							 m_initial_admin,
							 id);

	ACE_ASSERT (!CORBA::is_nil (m_logging_ec.in ()));
	ACE_ASSERT(!CORBA::is_nil (this->m_naming_context.in ()));


	this->m_naming_context->rebind (name,
					m_logging_ec.in());


    if(m_logBin){

        ACS_SHORT_LOG ((LM_DEBUG,
			"Logging EC registered with the naming service as: %s",
			acscommon::LOGGING_CHANNEL_XML_NAME));
    }else{
	    ACS_SHORT_LOG ((LM_DEBUG,
			"Logging EC registered with the naming service as: %s",
			acscommon::LOGGING_CHANNEL_NAME));
	}
	//ACE_ASSERT(!CORBA::is_nil (this->m_naming_context.in ()));

	//CosNaming::Name name (1);
	//name.length (1);
	}
}
void
LoggingService::create_supplieradmin ()
{
  CosNotifyChannelAdmin::AdminID adminid;

  m_logging_supplier_admin =
    m_logging_ec->new_for_suppliers (this->m_ifgop, adminid);


  ACE_ASSERT (!CORBA::is_nil (m_logging_supplier_admin.in ()));
}

void
LoggingService::create_suppliers ()
{
    if(!m_logBin)
        m_logging_supplier = new ACSStructuredPushSupplierXml ();
    else
        m_logging_supplier = new ACSStructuredPushSupplierBin ();

    ACE_ASSERT (m_logging_supplier);

  m_logging_supplier->connect (this->m_logging_supplier_admin.in ()
			       );
}

/*****************************************************************/
/**************************  main()  *****************************/
/*****************************************************************/

// globals
bool g_blockTermination = false;
LoggingService * g_service = 0;

void TerminationSignalHandler(int)
{
  if (g_blockTermination) return;
  g_blockTermination=true;

  LoggingHelper::terminateResolving();

  ACS_SHORT_LOG ((LM_INFO, "Stopping the ACS Centralized Logger..."));

  if (g_service)
    {
      g_service->shutdown ();
    }
}

int
main (int argc, char *argv[])
{
  LoggingService service;
  if (!service.isInitialized())
    {
      return -1;
    }
  g_service = &service;

  ACE_OS::signal(SIGINT, TerminationSignalHandler);  // Ctrl+C
  ACE_OS::signal(SIGTERM, TerminationSignalHandler); // termination request
  ACS_SHORT_LOG ((LM_INFO, "ACS Centralized Logger starting."));

  try
      {
      service.startup (argc,argv);

      if (service.run () == -1)
	  {
          service.shutdown ();
          ACS_SHORT_LOG ((LM_ERROR, "Failed to run the ACS Centralized Logger."));
	  return  1;
	  }
      }
  catch(...)
      {
      ACS_SHORT_LOG((LM_ERROR, "Failed to start the ACS Centralized Logger"));
      return 1;
      }


  if (!g_blockTermination)
      {
      g_blockTermination=true;
      service.shutdown ();
      }


  ACS_SHORT_LOG ((LM_INFO, "ACS Centralized Logger stopped."));

  return 0;
}

// ************************************************************************
//
// REVISION HISTORY:
//
// $Log: loggingService.cpp,v $
// Revision 1.61  2008/10/28 13:46:13  msekoran
// corbaloc support
//
// Revision 1.60  2008/08/04 11:24:39  bjeram
// Replaced non portable comparing with _nil() with CORBA::is_nil(x).
// COMP-2596
//
// Revision 1.59  2008/07/15 06:55:52  bjeram
// Replaced non portable comparing with _nil() with CORBA::is_nil(x). COMP-
//
// Revision 1.58  2008/02/21 09:20:45  cparedes
// Changing the message to printf
//
// Revision 1.57  2008/02/21 08:47:36  cparedes
// Change it from emergency to info
//
// Revision 1.56  2008/02/21 08:39:36  cparedes
// Incrementing the message level of the logging service start up
//
// Revision 1.55  2007/05/28 06:23:39  cparedes
// Adding the new alternate method to log binaries
//
// Revision 1.54.6.3  2007/04/03 07:46:03  cparedes
// Changing from ACS_LOG_TYPE to ACS_LOG_BIN
//
// Revision 1.54.6.2  2007/03/12 10:14:40  cparedes
// Fully functional cpp implementation of structured logs (not yet well tested, but it
// passed the normal workflow)
//
// Revision 1.54.6.1  2007/03/05 06:16:24  cparedes
// First attempt, work well with old things, but seg fault with the new things. To debug
//
// Revision 1.54  2006/08/08 11:14:04  bjeram
// ported to ACE+TAO x.5.2:
//   * change dur to cahnge in TAO_BasicLogFactory_i and TAO_BasicLog_i
//   and TAO_LogMgr_i
//   * prioties in ACE has been changed due to fix of ACE::log2(..)
//
// Revision 1.53  2006/07/19 16:57:28  dfugate
// Regressed all files back to July  16, 2006 for ACS 5.0.4.
//
// Revision 1.50  2006/06/20 21:47:44  dfugate
// Use pre-existing logging channel if it exists.
//
// Revision 1.49  2006/01/28 00:03:51  dfugate
// The LoggingChannel is now created using the LoggingNotifyEventChannelFactory instead of NotifyEventChannelFactory.
//
// Revision 1.48  2005/09/12 19:02:15  dfugate
// Stripped out all code dealing with the archiving channel EXCEPT that used to
// create the notification channel.
//
// Revision 1.47  2005/09/12 17:57:06  dfugate
// Converted plain C++ comments to Doxygen-style.
// Split loggingService.h into four headers (three new) as it's an ALMA C++
// coding violation to have more than one class declaration per header.
//
// Revision 1.46  2005/09/09 21:33:45  dfugate
// Decoupled a generic event supplier from loggingService.cpp.
//
// Revision 1.45  2005/04/18 20:51:02  dfugate
// Use the "kind" field of Naming Service references as well.
//
// Revision 1.44  2005/02/17 20:54:09  dfugate
// Use constants defined in acscommon.idl.
//
// Revision 1.43  2004/10/14 21:03:32  gchiozzi
// No change. Simply add a commet for the previous change.
// Fixed nasty bug with ACE_CString allocation.
// The wrong costructor was used producing well hidden memory allocation and
// corruption problems:
// -      ACE_CString strCmdLn((const char *)0, 512);
// +      ACE_CString strCmdLn(size_t(512));
//
// Revision 1.42  2004/10/14 20:46:17  gchiozzi
// Cleaned up logging messages:
// - uses now only ACS_* macros and not any more ACE_* macros
// - Removed all new line characters
// - fixed problem with ACS logging that was not putting proper new line
//   characters when logging messages on stdout BEFORE the logging itself
//   was initialised.
//
// Revision 1.41  2004/03/17 07:38:33  bjeram
// ported to ACE 5.4 and TAO 1.4
//
// Revision 1.40  2003/10/24 19:27:07  dfugate
// Fixed a few serious bugs and now use native exceptions.
//
// Revision 1.39  2003/10/23 07:39:08  acaproni
// True native exception handling. No more extra parameters
//
// Revision 1.38  2003/08/14 14:33:26  bjeram
// added TAO to ORB_init
//
// Revision 1.37  2003/07/28 09:46:57  bjeram
// modification for native exception
//
// Revision 1.36  2003/07/08 13:39:58  bjeram
// ported to gcc 3.2
//
// Revision 1.35  2003/03/14 10:24:49  rgeorgie
// LGPL
//
// Revision 1.34  2003/03/10 14:29:29  bjeram
// changes according to the changes in TAO x.3
//
// Revision 1.33  2002/09/23 12:45:09  vltsccm
// msekoran: loggingXMLParser fixed, memory leak removed and tat test added.
// ************************************************************************








