#ifndef _BULKDATA_RECEIVER_CB_EX2_H
#define _BULKDATA_RECEIVER_CB_EX2_H

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "bulkDataCallback.h"
#include "bulkDataReceiverEx2Impl.h"

class BulkDataReceiverCbEx2 : public BulkDataCallback
{

 public:
  BulkDataReceiverCbEx2();

  ~BulkDataReceiverCbEx2();

  virtual int cbStart(ACE_Message_Block * userParam_p = 0);

  virtual int cbReceive(ACE_Message_Block * frame_p);

  virtual int cbStop();

  virtual void setReceiverImpl(BulkDataReceiverEx2Impl<BulkDataReceiverCbEx2> *recv);

  private:
    
    CORBA::ULong count1_m;
};



#endif /*!_BULKDATA_RECEIVER_CB_EX2_H*/
