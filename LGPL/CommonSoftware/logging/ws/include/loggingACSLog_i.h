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
* "@(#) $Id: loggingACSLog_i.h,v 1.7 2009/06/24 22:52:54 javarias Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <time.h>

#include <acsutil.h>

#include <orbsvcs/Log/BasicLog_i.h>

#include <logging_idlC.h>

#include "loggingACSStructuredPushSupplier.h"

#define LOG_BIN_TYPE 0
#define LOG_XML_TYPE 1

class LoggingServiceMessageCounter
{
	private:
		unsigned long long *messages;
		struct timeval i_time;
	public:

		LoggingServiceMessageCounter(unsigned long long *m);

		void resetCounter();
		
		// Number of messages / elapsed time
		double getMessagesMetric();

		// Get the number of Log messages registered
		unsigned long long getNMessages();

		void operator++(int);
};


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
     * \throw DsLogAdmin::LogFull 
     * \throw DsLogAdmin::LogLocked
     */
    void
    write_recordlist (const DsLogAdmin::RecordList & list);
    
  protected:
   
    bool m_logBin; 
    /** The logging supplier */
    ACSStructuredPushSupplier* m_logging_supplier;
    Logging::LogStatistics logStat;
    LoggingServiceMessageCounter *counter;

};


#endif /* logging_acs_log_i_H */
