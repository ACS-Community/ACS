#ifndef _ACS_LOG_SERVICE_IMPL_H_
#define _ACS_LOG_SERVICE_IMPL_H_

#include <ace/Synch.h>
#include <logging_idlS.h>
#include "loggingACSLog_i.h"

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
      void set_logging_supplier(ACSStructuredPushSupplier* supplier);


   protected:
      class LogRecordBatch
      {
         private:
            ::Logging::XmlLogRecordSeq cache_;
            unsigned int size_;
            ACSStructuredPushSupplier* loggingSupplier_;
            ACE_SYNCH_MUTEX mutex_;
            ACE_SYNCH_MUTEX batchMutex_;
            ACE_SYNCH_CONDITION waitCond_;
            volatile bool shutdown_;

            void sendRecords(::Logging::XmlLogRecordSeq *reclist);
            int svc();

         public:
            LogRecordBatch();
            ~LogRecordBatch();
            void sendRecords();
            void add(const ::Logging::XmlLogRecordSeq *reclist);
            static void* worker(void *);
            void set_logging_supplier(ACSStructuredPushSupplier* supplier);
      };
      
      LogRecordBatch recordBatch;

};



#endif
