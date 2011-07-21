#include "bulkDataNTSenderImpl.h"



class bdNTSenderImplTest : public BulkDataNTSenderImpl
{

public:

	bdNTSenderImplTest(const ACE_CString& name,maci::ContainerServices* containerServices);

   virtual ~bdNTSenderImplTest();

    virtual void startSend();

    virtual void paceData ();

    virtual void stopSend();

};
