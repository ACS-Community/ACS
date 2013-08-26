#ifndef _BULKDATA_RECEIVER_CB_DISTR1_H
#define _BULKDATA_RECEIVER_CB_DISTR1_H

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "bulkDataCallback.h"
#include "bulkDataReceiverDistr1Impl.h"

class BulkDataReceiverCbDistr1 : public BulkDataCallback
{

 public:
  BulkDataReceiverCbDistr1();

  ~BulkDataReceiverCbDistr1();

  virtual int cbStart(ACE_Message_Block * userParam_p = 0);

  virtual int cbReceive(ACE_Message_Block * frame_p);

  virtual int cbStop();

  private:
    
    CORBA::ULong count1_m;
};



#endif /*!_BULKDATA_RECEIVER_CB_DISTR1_H*/
