#ifndef _ACS_LOG_SERVICE_IMPL_H_
#define _ACS_LOG_SERVICE_IMPL_H_

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

		void writeRecord (const ::Logging::XmlLogRecordSeq & xmlLogRecords);
		CORBA::ULong getNumberOfLogs ();

};

#endif
