#ifndef _SENDER_PT_IMPL_H
#define _SENDER_PT_IMPL_H

#include "senderPTS.h"
#include "bulkDataSenderImpl.h"


class SenderPTImpl : public virtual BulkDataSenderDefaultImpl,
		     public virtual POA_TEST_M::SenderPT
{    
 public:
    SenderPTImpl(const ACE_CString& name,maci::ContainerServices* containerServices);
  
    virtual ~SenderPTImpl();
  
    /**
     *  @throw AVStartSendErrorEx
     */ 
    virtual void startSend();
    
    /**
     *  @throw AVPaceDataErrorEx
     */ 
    virtual void paceData ();

    /**
     *  @throw AVStopSendErrorEx
     */ 
    virtual void stopSend();

    /**
     *  @throw AVStartSendErrorEx
     */ 
    virtual void startSendNew(CORBA::Long flowNumber, CORBA::Long size);
    
    /**
     *  @throw AVPaceDataErrorEx
     */ 
    virtual void paceDataNew(CORBA::Long flowNumber, CORBA::Long size);

    /**
     *  @throw AVStopSendErrorEx
     */ 
    virtual void stopSendNew(CORBA::Long flowNumber);

};

#endif /* _SENDER_PT_IMPL_H */
