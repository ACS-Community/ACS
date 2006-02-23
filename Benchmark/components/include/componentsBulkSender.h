#ifndef _COMPONENTS_BULK_SENDER_H_
#define _COMPONENTS_BULK_SENDER_H_

#include "perftestBDS.h"
#include "bulkDataSenderImpl.h"

#include <acstimeProfiler.h>

class BulkDataSenderEx1Impl : public virtual BulkDataSenderDefaultImpl,
			      public virtual POA_perftestBD::BulkDataSenderEx1
{    
  public:
    
    BulkDataSenderEx1Impl(const ACE_CString& name,ContainerServices* containerServices);
    virtual ~BulkDataSenderEx1Impl();
    
    virtual void startSend()
	throw (CORBA::SystemException, AVStartSendErrorEx);
    
    virtual void paceData ()
        throw (CORBA::SystemException, AVPaceDataErrorEx);
    
    virtual void stopSend()
	throw (CORBA::SystemException, AVStopSendErrorEx);
    
    virtual void setSize(unsigned long long newSize)
	throw (CORBA::SystemException)
	{ size_m = newSize; }
    
  private:
    unsigned long long size_m;
    Profiler *profiler_m;
};

#endif
