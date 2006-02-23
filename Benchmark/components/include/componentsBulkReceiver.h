#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "bulkDataCallback.h"
#include "bulkDataReceiverImpl.h"

/** @class BulkDataServer_StreamEndPoint
 *  Defines the server callback.
 */
class BulkDataReceiver : public BulkDataCallback
{
    
  public:
    BulkDataReceiver();

    ~BulkDataReceiver()
	{
	}

    virtual int cbStart(ACE_Message_Block * userParam_p = 0);
    virtual int cbReceive(ACE_Message_Block * frame_p);
    virtual int cbStop();

    
  private:
};
