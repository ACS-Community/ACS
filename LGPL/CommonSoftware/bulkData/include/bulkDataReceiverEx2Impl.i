template<class TCallback>
BulkDataReceiverEx2Impl<TCallback>::BulkDataReceiverEx2Impl(const ACE_CString& name,maci::ContainerServices* containerServices) :
    BulkDataReceiverImpl<TCallback>(name,containerServices)
{
    ACS_TRACE("BulkDataReceiverEx2Impl<>::BulkDataReceiverEx2Impl");
}


template<class TCallback>
BulkDataReceiverEx2Impl<TCallback>::~BulkDataReceiverEx2Impl()
{
    ACS_TRACE("BulkDataReceiverEx2Impl<>::~BulkDataReceiverEx2Impl");
}


template<class TCallback>
void BulkDataReceiverEx2Impl<TCallback>::cleanUp()
{
    ACS_TRACE("BulkDataReceiverEx2Impl<>::cleanUp");
}


template<class TCallback>
void BulkDataReceiverEx2Impl<TCallback>::setParam(const CORBA::Any &param) 
{
    ACS_TRACE("BulkDataReceiverEx2Impl<>::setParam");

    TCallback *cb = 0;
    ACE_CString flw = "Flow1";

    CORBA::Long longParam;
    param >>= longParam;
    ACS_SHORT_LOG((LM_INFO,"BulkDataReceiverEx2Impl<>::setParam param: %d",longParam));

    this->getReceiver()->getFlowCallback(flw, cb);
    if(cb == 0)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataReceiverEx2Impl<>::setParam: receiver callback = 0"));
	} 
    else
	{
	cb->setReceiverImpl(this);
	}
}


template<class TCallback>
void BulkDataReceiverEx2Impl<TCallback>::myMethod()
{
    ACS_TRACE("BulkDataReceiverEx2Impl<>::myMethod");
    ACS_SHORT_LOG((LM_INFO,"BulkDataReceiverEx2Impl<>::myMethod"));
}
