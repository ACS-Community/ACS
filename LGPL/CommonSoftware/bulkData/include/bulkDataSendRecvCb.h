#ifndef _BULKDATA_SENDRECV_CB_H
#define _BULKDATA_SENDRECV_CB_H

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "bulkDataCallback.h"
#include "bulkDataSendRecvImpl.h"

class BulkDataSendRecvCb : public BulkDataCallback
{

 public:
  BulkDataSendRecvCb();

  ~BulkDataSendRecvCb();

  virtual int cbStart(ACE_Message_Block * userParam_p);

  virtual int cbReceive(ACE_Message_Block * frame_p);

  virtual int cbStop();

  private:

    CORBA::ULong count1_m;
    CORBA::ULong count2_m;
};



#endif /*!_BULKDATA_SENDRECV_CB_H*/
