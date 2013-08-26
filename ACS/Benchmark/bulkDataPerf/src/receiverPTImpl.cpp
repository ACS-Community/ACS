#include "receiverPTImpl.h"
#include <OS_NS_time.h>

ReceiverPTImpl::ReceiverPTImpl(const ACE_CString& name,maci::ContainerServices* containerServices) :
    BulkDataReceiverImpl<ReceiverPTCb>(name,containerServices)
{
    ACS_TRACE("ReceiverPTImpl::ReceiverPTImpl");
}


ReceiverPTImpl::~ReceiverPTImpl()
{
    ACS_TRACE("ReceiverPTImpl::~ReceiverPTImpl");
}


void ReceiverPTImpl::cleanUp()
{
    ACS_TRACE("ReceiverPTImpl::cleanUp");
}


void ReceiverPTImpl::setTestName(const char *name)
{
    ACS_TRACE("ReceiverPTImpl::setTestName");

    ACE_TCHAR ctp[20];
    time_t ut(ACE_OS::gettimeofday().sec());
    struct tm *utc = ACE_OS::gmtime(&ut);
    ACE_OS::strftime(ctp, sizeof(ctp), "%Y-%m-%dT%H.%M.%S", utc);

//    ReceiverPTCb::testName_m = ACE_CString(name) + "-" +
//      this->name() + "-" + ctp +  ".dat";

    ReceiverPTCb::testName_m = ACE_CString(name) + "-" +
	this->name() + "-" + ctp;
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(ReceiverPTImpl)
/* ----------------------------------------------------------------*/
