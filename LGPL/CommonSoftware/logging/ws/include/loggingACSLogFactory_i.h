#ifndef logging_ACS_LOG_FACTORY_I_H
#define logging_ACS_LOG_FACTORY_I_H

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
* "@(#) $Id: loggingACSLogFactory_i.h,v 1.4 2009/06/03 23:16:28 javarias Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsutil.h>
#include <logging_idlS.h>

#include <orbsvcs/Log/BasicLogFactory_i.h>

#include "loggingACSStructuredPushSupplier.h"

/**
 * Implementation of the ACSLogFactory interface (extending BasicLogFactory interface).
 */
class ACSLogFactory_i : public TAO_BasicLogFactory_i, 
public POA_Logging::ACSLogFactory
{
    
  public:
    
    /**
     * Ctor
     */
    ACSLogFactory_i();
    
    /**
     * Dtor
     */
    ~ACSLogFactory_i();
    
    /**
     * Set the logging supplier to which puch logs
     */
    void
    set_logging_supplier(ACSStructuredPushSupplier* supplier)
	{
	    m_logging_supplier = supplier;
	}
    
    PortableServer::ServantBase* create_log_servant(DsLogAdmin::LogId id);

    
    Logging::ACSLogFactory_ptr
	    activate (CORBA::ORB_ptr orb,
			    PortableServer::POA_ptr poa);

    Logging::AcsLogService_ptr
	    create (DsLogAdmin::LogFullActionType full_action,
			    CORBA::ULongLong max_size,
			    DsLogAdmin::LogId_out id_out);

    Logging::AcsLogService_ptr
	    create_with_id (DsLogAdmin::LogId id,
			    DsLogAdmin::LogFullActionType full_action,
			    CORBA::ULongLong max_size);

    /**
     * Same as BasicLogFactory implementation, except it creates ACSLog
     */
/*    DsLogAdmin::BasicLog_ptr
    create_with_id (DsLogAdmin::LogId id,
                    DsLogAdmin::LogFullActionType full_action,
                    CORBA::ULongLong max_size)
	throw (CORBA::SystemException,
	       DsLogAdmin::LogIdAlreadyExists,
	       DsLogAdmin::InvalidLogFullAction);
*/  
  protected:
    
    /** The logging supplier */
    ACSStructuredPushSupplier* m_logging_supplier;

    CORBA::RepositoryId create_repositoryid ();
    
};

#endif // logging_ACS_LOG_FACTORY_I_H
