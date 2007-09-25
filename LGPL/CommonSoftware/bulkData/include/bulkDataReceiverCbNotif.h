#ifndef _BULKDATA_RECEIVER_CB_NOTIF_H
#define _BULKDATA_RECEIVER_CB_NOTIF_H

#include "bulkDataCallback.h"

#include "bulkDataReceiverImpl.h"


class BulkDataReceiverCbNotif : public BulkDataCallback
{
  public:

    BulkDataReceiverCbNotif();

    ~BulkDataReceiverCbNotif();

    virtual int cbStart(ACE_Message_Block * userParam_p = 0);

    virtual int cbReceive(ACE_Message_Block * frame_p);

    virtual int cbStop();

    // Method used to set the receiver reference into the callback (called by the receiver during the
    // notification subscription phase)
    void setReceiver(AcsBulkdata::BulkDataReceiver<BulkDataReceiverCbNotif> *recv);

  private:
        
    CORBA::ULong count1_m;
    CORBA::ULong count2_m;

    AcsBulkdata::BulkDataReceiver<BulkDataReceiverCbNotif> *recv_p;
};

#endif /*_BULKDATA_RECEIVER_CB_NOTIF_H*/
