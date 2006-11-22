#ifndef _SENDER_PT_IMPL_H
#define _SENDER_PT_IMPL_H

#include "senderPTS.h"
#include "bulkDataSenderImpl.h"


class SenderPTImpl : public virtual BulkDataSenderDefaultImpl,
		     public virtual POA_TEST_M::SenderPT
{    
 public:
    SenderPTImpl(const ACE_CString& name,ContainerServices* containerServices);
  
    virtual ~SenderPTImpl();
  
    virtual void startSend()
	throw (CORBA::SystemException, AVStartSendErrorEx);
    
    virtual void paceData ()
        throw (CORBA::SystemException, AVPaceDataErrorEx);

    virtual void stopSend()
	throw (CORBA::SystemException, AVStopSendErrorEx);

    virtual void startSendNew(CORBA::Long flowNumber, CORBA::Long size)
	throw (CORBA::SystemException, AVStartSendErrorEx);
    
    virtual void paceDataNew(CORBA::Long flowNumber, CORBA::Long size)
        throw (CORBA::SystemException, AVPaceDataErrorEx);

    virtual void stopSendNew(CORBA::Long flowNumber)
	throw (CORBA::SystemException, AVStopSendErrorEx);

};

#endif /* _SENDER_PT_IMPL_H */
