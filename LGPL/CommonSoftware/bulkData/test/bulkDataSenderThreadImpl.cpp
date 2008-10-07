#include "bulkDataSenderThreadImpl.h"

using namespace ACSBulkDataError;

BulkDataSenderThreadImpl::BulkDataSenderThreadImpl(const ACE_CString& name,maci::ContainerServices* containerServices) :
    BulkDataSenderDefaultImpl(name,containerServices)
{
    ACS_TRACE("BulkDataSenderThreadImpl::BulkDataSenderThreadImpl");

    numberOfFlows = 4;

    CORBA::ULong flow;

    for(CORBA::ULong i = 0; i < numberOfFlows; i++)
	{
	std::string str = "Flow";
	flow = i + 1;
	std::stringstream out;
	out << flow;
	str = str + out.str();
	ACE_CString str1 = str.c_str();
	SenderThread *thread = new SenderThread(str1, this, flow, ACS::ThreadBase::defaultResponseTime, ACS::ThreadBase::defaultSleepTime*10 /*=1s*/);
	thread_p.push_back(thread);
	}

}


BulkDataSenderThreadImpl::~BulkDataSenderThreadImpl()
{
    ACS_TRACE("BulkDataSenderThreadImpl::~BulkDataSenderThreadImpl");

    for(CORBA::ULong i = 0; i < numberOfFlows; i++)
	{
	if(thread_p[i])
	    delete thread_p[i];
	}
}


void BulkDataSenderThreadImpl::startSend()
{
    //empty
}


void BulkDataSenderThreadImpl::paceData()
{
    ACS_TRACE("BulkDataSenderThreadImpl::paceData");

    for(CORBA::ULong i = 0; i < numberOfFlows; i++)
	{
	thread_p[i]->resume();
	}

    ACE_Time_Value tv(20);
    BACI_CORBA::getORB()->run(tv); 
}


void BulkDataSenderThreadImpl::stopSend()
{
    ACS_TRACE("BulkDataSenderThreadImpl::stopSend");

    try
	{
	for(CORBA::ULong flowNumber = 1; flowNumber <= numberOfFlows; flowNumber++)
	    {
	    getSender()->stopSend(flowNumber);
	    }
	}

    catch (AVInvalidFlowNumberExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderThreadImpl::stopSend AVInvalidFlowNumberExImpl exception catched !"));
	AVStopSendErrorExImpl err = AVStopSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderThreadImpl::stopSend");
	throw err.getAVStopSendErrorEx();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderThreadImpl::stopSend UNKNOWN exception"));
	AVStopSendErrorExImpl err = AVStopSendErrorExImpl(__FILE__,__LINE__,"BulkDataSenderThreadImpl::stopSend");
	throw err.getAVStopSendErrorEx();
	}
}



//////////////////////////////////////////////////////////////////////
///////////////////////////// thread /////////////////////////////////
//////////////////////////////////////////////////////////////////////


SenderThread::SenderThread(const ACE_CString& name, 
			   BulkDataSenderThreadImpl *sender,
			   CORBA::ULong flowNumber,
			   const ACS::TimeInterval& responseTime, 
			   const ACS::TimeInterval& sleepTime) :
    ACS::Thread(name, responseTime, sleepTime)
{
    sender_p = sender;

    flowNumber_m = flowNumber;

    int size = flowNumber_m * 10000000;

    mb_p = new ACE_Message_Block(size);
	    
    for (CORBA::Long j = 0; j < (size-1); j++)
	{
	*mb_p->wr_ptr()='d';
	mb_p->wr_ptr(sizeof(char));
	}
    *mb_p->wr_ptr()='\0';
    mb_p->wr_ptr(sizeof(char));
	       	    
    ACS_SHORT_LOG((LM_INFO, "%s: Created thread", getName().c_str()));
}


SenderThread::~SenderThread() 
{ 
    if(mb_p)
	mb_p->release();
}


void SenderThread::run()
{
    try
	{
	sender_p->getSender()->sendData(flowNumber_m, mb_p);
	ACS_SHORT_LOG ((LM_INFO,"flow %d length sent data = %d",flowNumber_m,mb_p->length()));
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"Error!"));
	}

    setStopped();
    ACS_SHORT_LOG((LM_INFO, "%s: Stopped thread", getName().c_str()));   
}




/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(BulkDataSenderThreadImpl)
/* ----------------------------------------------------------------*/
