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
* 
*
* who       when        what
* --------  ---------   ----------------------------------------------
*/

#include "acsServicesDaemonImpl.h"
#include <tao/IORTable/IORTable.h>
#include <acserr.h>
#include <acsdaemonErrType.h>
#include <ACSErrTypeCommon.h>

/*****************************************************************/

ACSServicesDaemonImpl::ACSServicesDaemonImpl (LoggingProxy &logProxy) :
    m_isInitialized(false),
    m_logProxy(logProxy)
{
    // noop here

    m_isInitialized = true;
}

ACSServicesDaemonImpl::~ACSServicesDaemonImpl (void)
{
}

int
ACSServicesDaemonImpl::init_ORB  (int& argc, char *argv [])
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
	PortableServer::POA_var poa = root_poa->create_POA("ACSServicesDaemon", poa_manager.in(), policy_list);

	// destroy policies
	for (CORBA::ULong i = 0; i < policy_list.length(); ++i)
	    {
	    CORBA::Policy_ptr policy = policy_list[i];
	    policy->destroy();
	    }

	// set as default servant
	poa->set_servant(this);

	// create reference
	PortableServer::ObjectId_var oid = PortableServer::string_to_ObjectId("ACSServicesDaemon");
	obj = poa->create_reference_with_id (oid.in(), _interface_repository_id());
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
	    adapter->bind("ACSServicesDaemon", m_ior.in());
	    }

	// activate POA
	poa_manager->activate();

	ACS_SHORT_LOG((LM_INFO, "ACS Services Daemon is waiting for incoming requests."));
      
	}
    catch( CORBA::Exception &ex )
	{
	ACE_PRINT_EXCEPTION (ex, "EXCEPTION CAUGHT");
	return -1;
	}
  
    return 0;
}


int
ACSServicesDaemonImpl::startup (int argc, char *argv[])
{
    ACS_SHORT_LOG ((LM_INFO, "Starting up the ACS Services Daemon..."));

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

    ACS_SHORT_LOG ((LM_INFO, "ACS Services Daemon is initialized."));

    return 0;
}

int
ACSServicesDaemonImpl::run (void)
{
    ACS_SHORT_LOG ((LM_INFO, "ACS Services Daemon is up and running..."));

  
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
ACSServicesDaemonImpl::shutdown ()
{

    // shutdown the ORB.
    if (!CORBA::is_nil (m_orb.in ()))
	{
	this->m_orb->shutdown (true);
      
	}

    // shutdown AES
    ACSError::done();
}

/************************** CORBA interface ****************************/

void
ACSServicesDaemonImpl::start_acs (
    ::CORBA::Short instance_number,
    const char * additional_command_line
    )
    ACE_THROW_SPEC ((
			CORBA::SystemException,
			::acsdaemonErrType::FailedToStartAcsEx,
			::ACSErrTypeCommon::BadParameterEx
			))
{

    const char * cmdln = (additional_command_line ? additional_command_line : "");

    // execute: "acsStart  -b <instance> <args>"
    // TODO checks for ';', '&', '|' chars, they can run any other command!
    char command[1000];
    snprintf(command, 1000, "acsStart -b %d %s &", instance_number, cmdln);

    ACS_SHORT_LOG ((LM_INFO, "Executing: '%s'.", command));

    int result = ACE_OS::system(command);

    if (result < 0)
	{
	throw ::acsdaemonErrType::FailedToStartAcsExImpl(
	    __FILE__, __LINE__, 
	    "::ACSServicesDaemonImpl::start_acs").getFailedToStartAcsEx();
	}
}



void
ACSServicesDaemonImpl::stop_acs (
    ::CORBA::Short instance_number,
    const char * additional_command_line
    )
    ACE_THROW_SPEC ((
			CORBA::SystemException,
			::acsdaemonErrType::FailedToStopAcsEx,
			::ACSErrTypeCommon::BadParameterEx
			))
{
    const char * cmdln = (additional_command_line ? additional_command_line : "");

    // execute: "acsStop -b <instance> <args>"
    // TODO checks for ';', '&', '|' chars, they can run any other command!
    char command[1000];
    snprintf(command, 1000, "acsStop -b %d %s &", instance_number, cmdln);

    ACS_SHORT_LOG ((LM_INFO, "Executing: '%s'.", command));

    int result = ACE_OS::system(command);

    if (result < 0)
	{
	throw ::acsdaemonErrType::FailedToStopAcsExImpl(
	    __FILE__, __LINE__, 
	    "::ACSServicesDaemonImpl::stop_acs").getFailedToStopAcsEx();
	}
   
}





