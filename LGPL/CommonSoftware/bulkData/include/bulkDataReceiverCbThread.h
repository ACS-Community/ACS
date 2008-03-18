#ifndef _BULKDATA_RECEIVER_CB_THREAD_H_
#define _BULKDATA_RECEIVER_CB_THREAD_H_

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "bulkDataCallback.h"
#include "bulkDataReceiverImpl.h"

class BulkDataReceiverCbThread : public BulkDataCallback
{

 public:

  BulkDataReceiverCbThread();

  ~BulkDataReceiverCbThread();

  virtual int cbStart(ACE_Message_Block * userParam_p = 0);

  virtual int cbReceive(ACE_Message_Block * frame_p);

  virtual int cbStop();

  private:
    
    CORBA::ULong count1_m;
    CORBA::ULong count2_m;
    CORBA::ULong count3_m;
    CORBA::ULong count4_m;
};



#endif /* _BULKDATA_RECEIVER_CB_THREAD_H_ */
