#ifndef _BULKDATA_RECEIVER_CB_EX1_H
#define _BULKDATA_RECEIVER_CB_EX1_H

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "bulkDataCallback.h"
#include "bulkDataReceiverImpl.h"

class BulkDataReceiverCbEx1 : public BulkDataCallback
{

 public:
  BulkDataReceiverCbEx1();

  ~BulkDataReceiverCbEx1();

  virtual int cbStart(ACE_Message_Block * userParam_p = 0);

  virtual int cbReceive(ACE_Message_Block * frame_p);

  virtual int cbStop();

  virtual int myMethod();

  private:
    
    CORBA::ULong count1_m;
    CORBA::ULong count2_m;
    CORBA::ULong count4_m;
    
    FILE *fp_p;
};



#endif /*!_BULKDATA_RECEIVER_CB_EX1_H*/
