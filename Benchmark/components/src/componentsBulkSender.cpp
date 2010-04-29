#include "componentsBulkSender.h"
#include <sstream> 

using namespace std;


BulkDataSenderEx1Impl::BulkDataSenderEx1Impl(const ACE_CString& name,maci::ContainerServices* containerServices) :
    BulkDataSenderDefaultImpl(name,containerServices)
{
    ACS_TRACE("BulkDataSenderEx1Impl::BulkDataSenderEx1Impl");
    profiler_m = new Profiler();
}

BulkDataSenderEx1Impl::~BulkDataSenderEx1Impl()
{
    ACS_TRACE("BulkDataSenderEx1Impl::~BulkDataSenderEx1Impl");
    std::ostringstream profilerMsg;
    profilerMsg << "Time Needed to Send " << size_m << " KB of Data from the Bulk Data API";
    profiler_m->fullDescription(const_cast<char *>(profilerMsg.str().c_str()));

    delete profiler_m;
}

void BulkDataSenderEx1Impl::startSend()
{
    unsigned long long realSize = size_m * 1000ULL;
    
    ACE_Message_Block *mb;
    mb = new ACE_Message_Block(realSize);
    
    for (unsigned long long j=0; j<realSize; j++)
	{
	*mb->wr_ptr()='1';
	mb->wr_ptr(sizeof(char));
	}
    
    ACS_SHORT_LOG ((LM_DEBUG,"ACE Message block length = %d", mb->length()));

    profiler_m->start();
    getSender()->sendData(1UL, mb);  
    profiler_m->stop();

    mb->release();
}

void BulkDataSenderEx1Impl::paceData()
{
}

void BulkDataSenderEx1Impl::setSize(::CORBA::ULongLong newSize)
{
	size_m = newSize;
}

void BulkDataSenderEx1Impl::stopSend()
{
    ACS_TRACE("BulkDataSenderImpl::stopSend");
    getSender()->stopSend(1UL);
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(BulkDataSenderEx1Impl)
/* ----------------------------------------------------------------*/
