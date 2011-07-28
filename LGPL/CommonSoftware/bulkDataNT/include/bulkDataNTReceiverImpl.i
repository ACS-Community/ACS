

template<class TCallback>
BulkDataNTReceiverImpl<TCallback>::BulkDataNTReceiverImpl(const ACE_CString& name,maci::ContainerServices* containerServices) :
	CharacteristicComponentImpl(name,containerServices)
{
    ACS_TRACE("BulkDataNTReceiverImpl<>::BulkDataNTReceiverImpl");
    containerServices_p = containerServices;
}//BulkDataNTReceiverImpl

template<class TCallback>
BulkDataNTReceiverImpl<TCallback>::~BulkDataNTReceiverImpl()
{
    ACS_TRACE("BulkDataNTReceiverImpl<>::~BulkDataNTReceiverImpl");
}//~BulkDataNTReceiverImpl


template<class TCallback>
void BulkDataNTReceiverImpl<TCallback>::initialize()
{
	ACS_TRACE("BulkDataNTReceiverImpl<>::initialize");
	receiverStream_m = new AcsBulkdata::BulkDataNTReceiverStream<TCallback>("DefaultStream");
}//cleanUp




template<class TCallback>
void BulkDataNTReceiverImpl<TCallback>::cleanUp()
{
	ACS_TRACE("BulkDataNTReceiverImpl<>::cleanUp");
	delete receiverStream_m;
}//cleanUp

template<class TCallback>
void BulkDataNTReceiverImpl<TCallback>::openReceiver()
{
	ACS_TRACE("BulkDataNTReceiverImpl<>::openReceiver");

	char buf[BUFSIZ];

	// here we read CDB just that we are backward compatible, this has to be changed !!!
	CDB::DAL_ptr dal_p = containerServices_p->getCDB();
	if(CORBA::is_nil(dal_p))
	{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataNTReceiverImpl<>::openReceiver error getting DAL reference"));
		ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(__FILE__,__LINE__,"BulkDataNTReceiverImpl::openReceiver");
		throw err.getAVOpenReceiverErrorEx();
	}

	ACE_CString CDBpath="alma/";
	CDBpath += name();

	CDB::DAO_ptr dao_p = dal_p->get_DAO_Servant(CDBpath.c_str());
	if(CORBA::is_nil(dao_p))
	{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataNTReceiverImpl<>::openReceiver error getting DAO reference"));
		ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(__FILE__,__LINE__,"BulkDataNTReceiverImpl::openReceiver");
		throw err.getAVOpenReceiverErrorEx();
	}

	ACE_OS::strcpy(buf,dao_p->get_string("recv_protocols"));

	try
	{
		//receiverStream_m->initialize();

		receiverStream_m->createMultipleFlowsFromConfig(buf); // actaully here we need just number of flows !!!
		receiverStream_m->setReceiverName(name());

	}
	catch(ACSErr::ACSbaseExImpl &ex)
	{
		ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataNTReceiverImpl::openReceiver");
		err.log(LM_DEBUG);
		throw err.getAVOpenReceiverErrorEx();
	}
	catch(...)
	{
		ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataNTReceiverImpl::openReceiver");
		ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataNTReceiverImpl::openReceiver");
		throw err.getAVOpenReceiverErrorEx();
	}

}//openReceiver

template<class TCallback>
void BulkDataNTReceiverImpl<TCallback>::closeReceiver()
{
	ACS_TRACE("BulkDataNTReceiverImpl<>::closeReceiver");

	try
	{
//		receiverStream_m->closeReceiver();
	}
	catch(ACSErr::ACSbaseExImpl &ex)
	{
		ACSBulkDataError::AVCloseReceiverErrorExImpl err = ACSBulkDataError::AVCloseReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataNTReceiverImpl::closeReceiver");
		err.log(LM_DEBUG);
		throw err.getAVCloseReceiverErrorEx();
	}
	catch(...)
	{
		ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataNTReceiverImpl::closeReceiver");
		ACSBulkDataError::AVCloseReceiverErrorExImpl err = ACSBulkDataError::AVCloseReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataNTReceiverImpl::closeReceiver");
		throw err.getAVCloseReceiverErrorEx();
	}
}//closeReceiver


template<class TCallback>
bulkdata::BulkDataReceiverConfig * BulkDataNTReceiverImpl<TCallback>::getReceiverConfig()
{
	//TBD
	return NULL;
}//getReceiverConfig


