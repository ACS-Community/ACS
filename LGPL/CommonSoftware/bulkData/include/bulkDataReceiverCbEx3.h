#ifndef _BULKDATA_RECEIVER_CB_EX3_H
#define _BULKDATA_RECEIVER_CB_EX3_H

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "bulkDataCallback.h"
#include "bulkDataReceiverEx3Impl.h"

class BulkDataReceiverCbEx3 : public BulkDataCallback
{

 public:
  BulkDataReceiverCbEx3();

  ~BulkDataReceiverCbEx3();

  virtual int cbStart(ACE_Message_Block * userParam_p = 0);

  virtual int cbReceive(ACE_Message_Block * frame_p);

  virtual int cbStop();

  private:
        
    CORBA::ULong count1_m;

    CORBA::Boolean timeout_m;
};

#endif /*!_BULKDATA_RECEIVER_CB_EX3_H*/
