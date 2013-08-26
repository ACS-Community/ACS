#ifndef _COMPONENTS_BULK_SENDER_H_
#define _COMPONENTS_BULK_SENDER_H_

#include "perftestBDS.h"
#include "bulkDataSenderImpl.h"

#include <acstimeProfiler.h>

class BulkDataSenderEx1Impl : public virtual BulkDataSenderDefaultImpl,
			      public virtual POA_perftestBD::BulkDataSenderEx1
{    
  public:
    
    BulkDataSenderEx1Impl(const ACE_CString& name,maci::ContainerServices* containerServices);
    virtual ~BulkDataSenderEx1Impl();
    
    /*
     * @throw ACSBulkDataError::AVStartSendErrorEx
     */
    virtual void startSend();
    
    /*
     * @throw ACSBulkDataError::AVPaceDataErrorEx
     */
    virtual void paceData ();
    
    /*
     * @throw ACSBulkDataError::AVStopSendErrorEx
     */
    virtual void stopSend();
    
    virtual void setSize(::CORBA::ULongLong newSize);
    
  private:
    unsigned long long size_m;
    Profiler *profiler_m;
};

#endif
