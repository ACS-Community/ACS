#ifndef _BULKDATA_RECEIVER1_CB_NOTIF_H
#define _BULKDATA_RECEIVER1_CB_NOTIF_H

#include "bulkDataCallback.h"

#include "bulkDataReceiverImpl.h"


class BulkDataReceiver1CbNotif : public BulkDataCallback
{
  public:

    BulkDataReceiver1CbNotif();

    ~BulkDataReceiver1CbNotif();

    virtual int cbStart(ACE_Message_Block * userParam_p = 0);

    virtual int cbReceive(ACE_Message_Block * frame_p);

    virtual int cbStop();

    // Method used to set the receiver reference into the callback (called by the receiver during the
    // notification subscription phase)
    void setReceiver(AcsBulkdata::BulkDataReceiver<BulkDataReceiver1CbNotif> *recv);

  private:
        
    CORBA::ULong count1_m;
    CORBA::ULong count2_m;

    AcsBulkdata::BulkDataReceiver<BulkDataReceiver1CbNotif> *recv_p;
};

#endif /*_BULKDATA_RECEIVER1_CB_NOTIF_H*/
