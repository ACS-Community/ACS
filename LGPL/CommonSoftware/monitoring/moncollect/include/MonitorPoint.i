
template<class T, class TBLOB_SEQ, class TPROP, class TCB>
MonitorPoint<T, TBLOB_SEQ, TPROP, TCB>::MonitorPoint(const char *propertyName, const ACS::TimeInterval &archivingInterval,
		                                             ACS::Property* property, TMCDB::DataValueType typeOfData, MonitorBlob& mb) :
	MonitorPointBase(propertyName, archivingInterval, typeOfData, mb)
{
	AUTO_TRACE("MonitorPoint<>::MonitorPoint");
	try
	{

		property_m = TPROP::_narrow(property);

		if (property_m->default_timer_trigger()==0)
		{
//TBD: here we should throw an exception !!
		}//if

		blobDataSeq_m.length(prealocSeqLen_m);
		seqLen_m = prealocSeqLen_m;
	}
	catch(CORBA::Exception &cex)
	{
//TBD: improve error handling
		ACE_PRINT_EXCEPTION(cex, "in MonitorPoint<>::MonitorPoint");
	}
}//MonitorPoint

template<class T, class TBLOB_SEQ, class TPROP, class TCB>
MonitorPoint<T, TBLOB_SEQ, TPROP, TCB>::~MonitorPoint()
{
	AUTO_TRACE("MonitorPoint<>::~MonitorPoint");
	CORBA::release(property_m);
}//~MonitorPoint


template<class T, class TBLOB_SEQ, class TPROP, class TCB>
void MonitorPoint<T, TBLOB_SEQ, TPROP, TCB>::startMonitoring()
{
	ACS::CBDescIn cbDescIn;
	AUTO_TRACE("MonitorPoint<T, TPROP, TCB>::startMonitoring");

	try
	{

		//printf("cb1 %d %d\n", callback_m->_refcount_value(), _refcount_value());
		if ( !CORBA::is_nil(monitor_m) )
			return; // we are already monitoring

		CORBA::Request_var req;
		req = property_m->_request("create_monitor");
		req->add_in_arg ("CB") <<= callback_m.in();
		req->add_in_arg ("CBDescIn") <<= cbDescIn;
		req->set_return_type (ACS::_tc_Monitor);

		req->invoke();

		//printf("cb2 %d %d\n", callback_m->_refcount_value(), _refcount_value());

		if (req->response_received ())
		{
			ACS::Monitor *m;
			req->return_value() >>= m;
			monitor_m = m->_duplicate(m);

			monitor_m->set_timer_trigger(archivingInterval_m);

		}else
			printf("DII problems\n");

		//monitor_m = property_m->create_monitor(this->callback_m, cbDescIn);
	}
	catch(CORBA::Exception &cex)
	{
		//TBD: improve error handling
		ACE_PRINT_EXCEPTION(cex, "in MonitorPoint<>::startMonitoring");
	}
}//startMonitoring

template<class T, class TBLOB_SEQ, class TPROP, class TCB>
void MonitorPoint<T, TBLOB_SEQ, TPROP, TCB>::stopMonitoring()
{
	AUTO_TRACE("MonitorPoint<>::stopMonitoring");
	try
	{
	if ( !CORBA::is_nil(monitor_m) )
	    monitor_m->destroy();
	monitor_m = ACS::Monitor::_nil();
	}
	catch(CORBA::Exception &cex)
	{
		//TBD: improve error handling
		ACE_PRINT_EXCEPTION(cex, "in MonitorPoint<>::stopMonitoring");
	}
}//stopMonitoring


template<class T, class TBLOB_SEQ, class TPROP, class TCB>
void MonitorPoint<T, TBLOB_SEQ, TPROP, TCB>::fillSeq()
{
	ACE_GUARD(ACE_Thread_Mutex, mut, switchMutex_m);

	// we adjust the length
	blobDataSeq_m.length(curSeqPos_m);

	// put to the any
	monitorBlob_m.blobDataSeq <<= this->blobDataSeq_m;

	//set a length of a sequence back
	blobDataSeq_m.length(seqLen_m);

/* or we do like that
	seqLen_m = curSeqPos_m;
*/
	// start for the beginning
	curSeqPos_m = 0;
}//fillSeq



template<class T, class TBLOB_SEQ, class TPROP, class TCB>
void MonitorPoint<T, TBLOB_SEQ, TPROP, TCB>::working(T value, const ACSErr::Completion& comp, const ACS::CBDescOut& cbdescout)
{
//	std::cout  << "Got value for property:" << propertyName_m << " at pos:" << curSeqPos_m << " " << value << std::endl;
//	std::cout  << "Got value for property:" << propertyName_m << " at pos:"  << curSeqPos_m << std::endl;
	ACE_GUARD(ACE_Thread_Mutex, mut, switchMutex_m);

	if ( curSeqPos_m>=seqLen_m )
	{
		seqLen_m = curSeqPos_m+prealocSeqLen_m;
		blobDataSeq_m.length(seqLen_m);
		/*ACS_LOG(LM_FULL_INFO ,"MonitorPoint<>::working",
						(LM_DEBUG, "Increased blobData sequence size for property: %s to: %d", monitorBlob_m.propertyName.in(), seqLen_m));*/
	}//if

	blobDataSeq_m[curSeqPos_m].value = value;
	blobDataSeq_m[curSeqPos_m].time = comp.timeStamp;

	curSeqPos_m++;
}

template<class T, class TBLOB_SEQ, class TPROP, class TCB>
void MonitorPoint<T, TBLOB_SEQ, TPROP, TCB>::done(T value, const ACSErr::Completion& comp, const ACS::CBDescOut& cbdescout)
{

}

