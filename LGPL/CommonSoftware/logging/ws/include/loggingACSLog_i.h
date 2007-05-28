#ifndef logging_acs_log_i_H
#define logging_acs_log_i_H

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
* "@(#) $Id: loggingACSLog_i.h,v 1.4 2007/05/28 06:23:39 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsutil.h>

#include <orbsvcs/Log/BasicLog_i.h>

#include "loggingACSStructuredPushSupplier.h"

#define LOG_BIN_TYPE 0
#define LOG_XML_TYPE 1
/**
 * Implementation of the ACSLog interface (extending BasicLog interface).
 */
class ACSLog_i : public TAO_BasicLog_i
{
    
  public:
    
    /**
     * Constructor
     */
    ACSLog_i (CORBA::ORB_ptr orb,
	      PortableServer::POA_ptr poa,
	      TAO_LogMgr_i &logmgr_i,
	      DsLogAdmin::LogMgr_ptr factory,
	      DsLogAdmin::LogId id);

    /**
     * Destructor
     */
    ~ACSLog_i();
    
    /**
     * Set the logging supplier to which puch logs
     */
    void
    set_logging_supplier(ACSStructuredPushSupplier* supplier)
	{
	    m_logging_supplier = supplier;
	}
    
    /**
     * Write a list of record ids to storage. Raises DsLogAdmin::LogFull
     * and DsLogAdmin::LogLocked
     * Same as BasicLogFactory implementation, except it writes to file (no memory store is used)
     */
    void
    write_recordlist (const DsLogAdmin::RecordList & list)
	throw (CORBA::SystemException,
	       DsLogAdmin::LogFull,
	       DsLogAdmin::LogLocked); 
    
  protected:
   
    bool m_logBin; 
    /** The logging supplier */
    ACSStructuredPushSupplier* m_logging_supplier;
};

#endif /* logging_acs_log_i_H */
