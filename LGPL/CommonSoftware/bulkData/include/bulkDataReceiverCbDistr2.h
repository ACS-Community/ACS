#ifndef _BULKDATA_RECEIVER_CB_DISTR2_H
#define _BULKDATA_RECEIVER_CB_DISTR2_H

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "bulkDataCallback.h"

class BulkDataReceiverCbDistr2 : public BulkDataCallback
{

 public:
  BulkDataReceiverCbDistr2();

  ~BulkDataReceiverCbDistr2();

  virtual int cbStart(ACE_Message_Block * userParam_p = 0);

  virtual int cbReceive(ACE_Message_Block * frame_p);

  virtual int cbStop();

  private:
    
    CORBA::ULong count1_m;
    FILE *fp_p;
};



#endif /*!_BULKDATA_RECEIVER_CB_DISTR2_H*/
