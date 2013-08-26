/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) NRAO, 2009
*    Copyright by NRAO (in the framework of the ALMA collaboration)
*    All rights reserved
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
* "@(#) $Id:"
*/

#ifndef _ACS_LOG_SERVICE_IMPL_H_
#define _ACS_LOG_SERVICE_IMPL_H_

#include <ace/Synch.h>
#include <logging_idlS.h>
#include "loggingACSLog_i.h"

/**
  * Implement the extensions of the Telecom Log service, these extensions
  * are particular to ALMA and ACS usage.
  * @see Logging::AcsLogService
  *
  */
class AcsLogServiceImpl: public ACSLog_i,
	public POA_Logging::AcsLogService
{
	public :
		AcsLogServiceImpl (CORBA::ORB_ptr orb,
				PortableServer::POA_ptr poa,
				TAO_LogMgr_i &logmgr_i,
				DsLogAdmin::LogMgr_ptr factory,
				DsLogAdmin::LogId id);

		~AcsLogServiceImpl();

		void writeRecords (const ::Logging::XmlLogRecordSeq & xmlLogRecords);
		Logging::LogStatistics getStatistics();
      /** override */
      void set_logging_supplier(ACSStructuredPushSupplier* supplier);


   protected:
      /**
        * This class implements the functions necessary to send the log records
        * in a batch through the logging Notification Channel.
        * It implements the batch cache as triple buffer to improve
        * the overall throughput of the service.
        *
        * This class uses a worker thread to dispatch the log records through
        * the Notification Channel when buffer is full or each 1 second
        */
      class LogRecordBatch
      {
         private:
            /** reference to the current buffer in use*/
            ::Logging::XmlLogRecordSeq *buffer_;
            /** triple buffer*/
            ::Logging::XmlLogRecordSeq buffer[3];
            /** current size of the batch*/
            volatile unsigned int size_;
            ACSStructuredPushSupplier* loggingSupplier_;
            /** mutex used by the wait condition*/
            ACE_SYNCH_MUTEX mutex_;
            /** mutex used to protect the batch*/
            ACE_SYNCH_MUTEX batchMutex_;
            ACE_SYNCH_CONDITION waitCond_;
            volatile bool shutdown_;
            /** number of the current buffer in use*/
            int nBuff;
            /** Internal usage*/
            ::Logging::XmlLogRecordSeq *tmpBuffer;
            /** Internal usage*/
            unsigned int tmpSize;

            /** Send the records through the Notification Channel*/
            void sendRecords(::Logging::XmlLogRecordSeq *reclist);
            /** Entry point for worker thread*/
            int svc();

         public:
            /** Initializes the buffers and the worker thread*/
            LogRecordBatch();
            /** Stop the worker thread*/
            ~LogRecordBatch();
            /** Function used by the thread to send the log records to the NC*/
            void sendRecords();
            /** Add log records to the buffer, if the buffer is full will
              dispatch to the worker thread to send the records through NC*/
            void add(const ::Logging::XmlLogRecordSeq *reclist);
            /** function to initilizate the thread*/
            static void* worker(void *);
            void set_logging_supplier(ACSStructuredPushSupplier* supplier);
      };
      
      LogRecordBatch recordBatch;

};

#endif
