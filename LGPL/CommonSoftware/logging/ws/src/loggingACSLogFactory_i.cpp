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
* "@(#) $Id: loggingACSLogFactory_i.cpp,v 1.3 2006/08/08 11:14:04 bjeram Exp $"
*
* who       when        what
* --------  ---------   ----------------------------------------------
*/

#include "loggingACSLogFactory_i.h"
#include "loggingACSLog_i.h"

/*****************************************************************/

PortableServer::ServantBase*
ACSLogFactory_i::create_log_servant(DsLogAdmin::LogId id)
{
    ACSLog_i* basic_log_i=0;
    
    try
	{
	basic_log_i = new ACSLog_i(this->orb_.in(), 
				   this->log_poa_.in(),
				   *this,
				   this->log_mgr_.in(),
				   id);
	}
    catch(...)
	{
	return basic_log_i;
	}
    
    if (basic_log_i==0)
	{
	errno = ENOMEM;
	throw CORBA::NO_MEMORY ();
	}  
    
    // Set suppliers
    basic_log_i->set_logging_supplier(m_logging_supplier);
    
    try
	{
	basic_log_i->init ();
	}
    catch(...)
	{
	delete basic_log_i;
	return 0;
	}
    
    return basic_log_i;
}//create_log_servant

/*
DsLogAdmin::BasicLog_ptr
ACSLogFactory_i::create_with_id (DsLogAdmin::LogId id,
				 DsLogAdmin::LogFullActionType full_action,
				 CORBA::ULongLong max_size)
    throw(CORBA::SystemException,
	  DsLogAdmin::LogIdAlreadyExists,
	  DsLogAdmin::InvalidLogFullAction)
{
    // Make sure the id not used up.
    if (exists (id) )
	{
	
	throw DsLogAdmin::LogIdAlreadyExists();
	return DsLogAdmin::BasicLog::_nil();
	}
    
    DsLogAdmin::BasicLog_var basic_log = DsLogAdmin::BasicLog::_nil();
    // Object to return.
    
    ACSLog_i* basic_log_i;
    
    try
	{
	basic_log_i = new ACSLog_i(*this, this->log_mgr_.in (), id, full_action, max_size);
	}
    catch(...)
	{
	return basic_log._retn();
	}
    
    if (basic_log_i==0)
	{
	errno = ENOMEM;
	throw CORBA::NO_MEMORY ();
	}  
    
    // Set suppliers
    basic_log_i->set_logging_supplier(m_logging_supplier);
    
    PortableServer::ServantBase_var safe_basic_log_i = basic_log_i;
    // Transfer ownership to the POA.
    

    try
	{
	basic_log_i->init ();
	// Register with the poa
	basic_log = basic_log_i->_this ();
	}
    catch(...)
	{
	return DsLogAdmin::BasicLog::_nil ();
	}
    
    // widening a BasicLog_var to a Log_var
    DsLogAdmin::Log_var log = DsLogAdmin::BasicLog::_duplicate (basic_log.in ());
    
    
    // Add to the Hash table..
    if (hash_map_.bind (id, log) == -1)
	{
	throw CORBA::INTERNAL();
	return DsLogAdmin::BasicLog::_nil();
	}
    
    return basic_log._retn ();
}
*/
ACSLogFactory_i::ACSLogFactory_i (void) : TAO_BasicLogFactory_i(), 
					  m_logging_supplier(0)
{}

ACSLogFactory_i::~ACSLogFactory_i ()
{
}

/*****************************************************************/
