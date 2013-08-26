template<class TCallback>
BulkDataReceiverEx3Impl<TCallback>::BulkDataReceiverEx3Impl(const ACE_CString& name,maci::ContainerServices* containerServices) :
    BulkDataReceiverImpl<TCallback>(name,containerServices)
{
    ACS_TRACE("BulkDataReceiverEx3Impl<>::BulkDataReceiverEx3Impl");
}


template<class TCallback>
BulkDataReceiverEx3Impl<TCallback>::~BulkDataReceiverEx3Impl()
{
    ACS_TRACE("BulkDataReceiverEx3Impl<>::~BulkDataReceiverEx3Impl");
}


template<class TCallback>
void BulkDataReceiverEx3Impl<TCallback>::cleanUp()
{
    ACS_TRACE("BulkDataReceiverEx3Impl<>::cleanUp");
}
