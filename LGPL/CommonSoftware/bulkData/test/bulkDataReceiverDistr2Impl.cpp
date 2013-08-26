#include "bulkDataReceiverDistr2Impl.h"

BulkDataReceiverDistr2Impl::BulkDataReceiverDistr2Impl(const ACE_CString& name,maci::ContainerServices* containerServices) :
    BulkDataReceiverImpl<BulkDataReceiverCbDistr2>(name,containerServices)
{
    ACS_TRACE("BulkDataReceiverDistr2Impl::BulkDataReceiverDistr2Impl");
}


BulkDataReceiverDistr2Impl::~BulkDataReceiverDistr2Impl()
{
    ACS_TRACE("BulkDataReceiverDistr2Impl::~BulkDataReceiverDistr2Impl");
}


void BulkDataReceiverDistr2Impl::cleanUp()
{
    ACS_TRACE("BulkDataReceiverDistr2Impl::cleanUp");
}


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(BulkDataReceiverDistr2Impl)
/* ----------------------------------------------------------------*/

    
