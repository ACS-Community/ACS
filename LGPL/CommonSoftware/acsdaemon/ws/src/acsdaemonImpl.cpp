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
* "@(#) $Id: acsdaemonImpl.cpp,v 1.2 2006/06/23 12:23:05 msekoran Exp $"
*
* who       when        what
* --------  ---------   ----------------------------------------------
* msekoran  2006-06-21  created
*/

#include "acsdaemonImpl.h"
#include <tao/IORTable/IORTable.h>
#include <acserr.h>
#include <acsdaemonErrType.h>
#include <ACSErrTypeCommon.h>

/*****************************************************************/

ACSDaemonImpl::ACSDaemonImpl (LoggingProxy &logProxy) :
    m_isInitialized(false),
    m_logProxy(logProxy)
{
    // noop here

    m_isInitialized = true;
}

ACSDaemonImpl::~ACSDaemonImpl (void)
{
}

int
ACSDaemonImpl::init_ORB  (int& argc, char *argv [])
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
	PortableServer::POA_var poa = root_poa->create_POA("ACSDaemon", poa_manager.in(), policy_list);

	// destroy policies
	for (CORBA::ULong i = 0; i < policy_list.length(); ++i)
	    {
	    CORBA::Policy_ptr policy = policy_list[i];
	    policy->destroy();
	    }

	// set as default servant
	poa->set_servant(this);

	// create reference
	PortableServer::ObjectId_var oid = PortableServer::string_to_ObjectId("ACSDeamon");
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
	    adapter->bind("ACSDaemon", m_ior.in());
	    }

	// activate POA
	poa_manager->activate();

	ACS_SHORT_LOG((LM_INFO, "ACS Daemon is waiting for incoming requests."));
      
	}
    catch( CORBA::Exception &ex )
	{
	ACE_PRINT_EXCEPTION (ex, "EXCEPTION CAUGHT");
	return -1;
	}
  
    return 0;
}


int
ACSDaemonImpl::startup (int argc, char *argv[])
{
    ACS_SHORT_LOG ((LM_INFO, "Starting up the ACS Daemon..."));

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

    ACS_SHORT_LOG ((LM_INFO, "ACS Daemon is initialized."));

    return 0;
}

int
ACSDaemonImpl::run (void)
{
    ACS_SHORT_LOG ((LM_INFO, "ACS Daemon is up and running..."));

  
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
ACSDaemonImpl::shutdown ()
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
ACSDaemonImpl::start_container (
    const char * container_type,
    const char * container_name,
    ::CORBA::Short instance_number,
    const char * additional_command_line
    )
    ACE_THROW_SPEC ((
			CORBA::SystemException,
			::acsdaemonErrType::FailedToStartContainerEx,
			::ACSErrTypeCommon::BadParameterEx
			))
{
    if (container_type == 0 ||
	*container_type == 0)
	{
	::ACSErrTypeCommon::BadParameterExImpl ex(__FILE__, __LINE__, 
						  "::ACSDaemonImpl::start_container");
	ex.setParameter("container_type");
	throw ex.getBadParameterEx();
	}

    if (container_name == 0 ||
	*container_name == 0)
	{
	::ACSErrTypeCommon::BadParameterExImpl ex(__FILE__, __LINE__, 
						  "::ACSDaemonImpl::start_container");
	ex.setParameter("container_name");
	throw ex.getBadParameterEx();
	}

    const char * cmdln = (additional_command_line ? additional_command_line : "");

    // execute: "acsStartContainer -<type> -b <instance> <name> <args>"
    // TODO checks for ';', '&', '|' chars, they can run any other command!
    char command[200];
    snprintf(command, 200, "acsStartContainer -%s -b %d %s %s &", container_type, instance_number, container_name, cmdln);

    ACS_SHORT_LOG ((LM_INFO, "Executing: '%s'.", command));

    int result = ACE_OS::system(command);

    if (result < 0)
	{
	throw ::acsdaemonErrType::FailedToStartContainerExImpl(
	    __FILE__, __LINE__, 
	    "::ACSDaemonImpl::start_container").getFailedToStartContainerEx();
	}
   
}








