#ifndef _RECEIVER_PT_IMPL_H
#define _RECEIVER_PT_IMPL_H

#include "receiverPTS.h"
#include "bulkDataReceiverImpl.h"
#include "receiverPTCb.h"

class ReceiverPTImpl : public virtual BulkDataReceiverImpl<ReceiverPTCb>,
		      public virtual POA_TEST_M::ReceiverPT
{
  public:
    
    ReceiverPTImpl(const ACE_CString& name,maci::ContainerServices* containerServices);
  
    virtual ~ReceiverPTImpl();

    virtual void setTestName(const char *);

    void cleanUp();

};


#endif
