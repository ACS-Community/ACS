#include "bulkDataNTSenderFlow.h"
#include "bulkDataNTSenderImpl.h"
#include <ACSBulkDataError.h>   // error definition  ??


BulkDataNTSenderImpl::BulkDataNTSenderImpl(const ACE_CString& name,maci::ContainerServices* containerServices):
CharacteristicComponentImpl(name,containerServices)
{
	ACS_TRACE("BulkDataNTSenderImpl::BulkDataNTSenderImpl");

	containerServices_p = containerServices;

	receiverObj_m = 0;
	senderStream_m =0;
}


BulkDataNTSenderImpl::~BulkDataNTSenderImpl()
{
	ACS_TRACE("BulkDataNTSenderImpl::~BulkDataNTSenderImpl");
}


void BulkDataNTSenderImpl::initialize()
{
	 senderStream_m = new AcsBulkdata::BulkDataNTSenderStream("TestFlow");
	 //senderStream_m->initialize();
}

void BulkDataNTSenderImpl::cleanUp()
{
	delete senderStream_m;
}

void BulkDataNTSenderImpl::connect(bulkdata::BulkDataReceiver_ptr receiverObj_p)
{
	ACS_TRACE("BulkDataNTSenderImpl::connect - deprecated");
// here we do not do any contact with receiver

	char buf[BUFSIZ];

	CDB::DAL_ptr dal_p = containerServices_p->getCDB();
	if (CORBA::is_nil(dal_p))
	{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataSenderImpl::connect error getting CDB reference"));
		ACSBulkDataError::AVConnectErrorExImpl err = ACSBulkDataError::AVConnectErrorExImpl(__FILE__,__LINE__,"BulkDataSenderImpl::connect");
		throw err.getAVConnectErrorEx();
	}

	ACE_CString CDBpath="alma/";
	CDBpath += name();

	CDB::DAO_ptr dao_p = dal_p->get_DAO_Servant(CDBpath.c_str());
	if (CORBA::is_nil(dao_p))
	{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataSenderImpl::connect error getting DAO reference"));
		ACSBulkDataError::AVConnectErrorExImpl err = ACSBulkDataError::AVConnectErrorExImpl(__FILE__,__LINE__,"BulkDataSenderImpl::connect");
		throw err.getAVConnectErrorEx();
	}

	ACE_OS::strcpy(buf,dao_p->get_string("sender_protocols"));

	try
	{
//		getSenderStream()->initialize();
		getSenderStream()->createMultipleFlowsFromConfig(buf);

//		receiverObj_p->openReceiver();
//		bulkdata::BulkDataReceiverConfig *receiverConfig = receiverObj_p->getReceiverConfig();

//		getSender()->connectToPeer(receiverConfig);

//??		receiverObj_p->setRecvName(receiverObj_p->name());

	}
	catch(ACSErr::ACSbaseExImpl &ex)
	{
		ACSBulkDataError::AVConnectErrorExImpl err = ACSBulkDataError::AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataNTSenderImpl::connect");
		err.log(LM_DEBUG);
	}
	catch(...)
	{
		ACSBulkDataError::AVConnectErrorExImpl err = ACSBulkDataError::AVConnectErrorExImpl(__FILE__,__LINE__,"BulkDataNTSenderImpl::connect");
				err.log(LM_DEBUG);
	}//try-catch

}//connect

void BulkDataNTSenderImpl::disconnect()
{
	ACS_TRACE("BulkDataNTSenderImpl::disconnect - deprecated");
	try
	{
		//getSenderStream()->destroyFlows();
	}
	catch(ACSErr::ACSbaseExImpl &ex)
	{
		ACSBulkDataError::AVConnectErrorExImpl err = ACSBulkDataError::AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataNTSenderImpl::disconnect");
		err.log(LM_DEBUG);
	}
	catch(...)
	{
		ACSBulkDataError::AVConnectErrorExImpl err = ACSBulkDataError::AVConnectErrorExImpl(__FILE__,__LINE__,"BulkDataNTSenderImpl::disconnect");
		err.log(LM_DEBUG);
	}//try-catch
}
