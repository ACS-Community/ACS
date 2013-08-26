template<class TCallback>
BulkDataReceiver1PerfImpl<TCallback>::BulkDataReceiver1PerfImpl(const ACE_CString& name,maci::ContainerServices* containerServices) :
    BulkDataReceiverImpl<TCallback>(name,containerServices)
{
    ACS_TRACE("BulkDataReceiver1PerfImpl<>::BulkDataReceiver1PerfImpl");
}


template<class TCallback>
BulkDataReceiver1PerfImpl<TCallback>::~BulkDataReceiver1PerfImpl()
{
    ACS_TRACE("BulkDataReceiver1PerfImpl<>::~BulkDataReceiver1PerfImpl");
}


template<class TCallback>
void BulkDataReceiver1PerfImpl<TCallback>::cleanUp()
{
    ACS_TRACE("BulkDataReceiver1PerfImpl<>::cleanUp");
}
