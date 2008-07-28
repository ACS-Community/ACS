template<class TCallback>
BulkDataReceiverDistr1Impl<TCallback>::BulkDataReceiverDistr1Impl(const ACE_CString& name,maci::ContainerServices* containerServices) :
    BulkDataReceiverImpl<TCallback>(name,containerServices)
{
    ACS_TRACE("BulkDataReceiverDistr1Impl<>::BulkDataReceiverDistr1Impl");
}


template<class TCallback>
BulkDataReceiverDistr1Impl<TCallback>::~BulkDataReceiverDistr1Impl()
{
    ACS_TRACE("BulkDataReceiverDistr1Impl<>::~BulkDataReceiverDistr1Impl");
}


template<class TCallback>
void BulkDataReceiverDistr1Impl<TCallback>::cleanUp()
{
    ACS_TRACE("BulkDataReceiverDistr1Impl<>::cleanUp");
}
