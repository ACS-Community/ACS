
template<class T, class TBLOB_SEQ, class TPROP, class TCB, class TBASE>
MonitorPoint<T, TBLOB_SEQ, TPROP, TCB, TBASE>::MonitorPoint(const char *propertyName, const ACS::TimeInterval &archivingInterval,
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
   valueTrigger_m = 0;
	try
	{
		CORBA::Any *anyCharacteristic;
		char *strCharacteristic;
		anyCharacteristic = property_m->get_characteristic_by_name("archive_suppress");
		*anyCharacteristic >>= CORBA::Any::to_string(strCharacteristic, 0);
      std::cout << strCharacteristic << std::endl;
		if ( strcmp(strCharacteristic, "false")!=0 ) {
			ACS_LOG(LM_FULL_INFO ,"MonitorPoint::MonitorPoint", (LM_DEBUG, "Values from property %s (%s) will NOT be collected, because archive_suppress is set to 'false', but to: %s.",
					property_m->name(),
					property_m->_repository_id(),
					strCharacteristic
			));
			suppressed_m = true;
		}
      double archiveMaxInt;
		anyCharacteristic = property_m->get_characteristic_by_name("archive_max_int");
		*anyCharacteristic >>= CORBA::Any::to_string(strCharacteristic, 0);
		std::istringstream i(strCharacteristic);
		i >> archiveMaxInt;
		archiveMaxInt *= static_cast<double>(10000000.0); //we have to convert to 100s nsec.
		if ( archiveMaxInt==0.0 )
		{
			ACS_LOG(LM_FULL_INFO ,"MonitorPoint::MonitorPoint", (LM_DEBUG, "Values from property %s (%s) will NOT be collected by time interval, because archive_max_int is 0 (%f).",
					property_m->name(),
					property_m->_repository_id(),
					archiveMaxInt
			));
			archivingInterval_m = 0;
		}//if
      TBASE val;// = static_cast<TBASE>(0);
		anyCharacteristic = property_m->get_characteristic_by_name("archive_delta");
		*anyCharacteristic >>= CORBA::Any::to_string(strCharacteristic, 0);
		std::istringstream i1(strCharacteristic);
		i1 >> val;
      std::cout << typeid(val).name() << std::endl;
      std::cout << val << std::endl;
      std::cout << strCharacteristic << std::endl;
		if ( val == 0 ) {
			ACS_LOG(LM_FULL_INFO ,"MonitorPoint::MonitorPoint", (LM_DEBUG, "Values from property %s (%s) will NOT be collected on value change, because archive_delta is set to '%s'.",
					property_m->name(),
					property_m->_repository_id(),
					strCharacteristic
			));
		} else {
			valueTrigger_m = val;
		}
		anyCharacteristic = property_m->get_characteristic_by_name("archive_delta_percent");
      double valPer;// = 0.0;
		*anyCharacteristic >>= CORBA::Any::to_string(strCharacteristic, 0);
		std::istringstream i2(strCharacteristic);
		i2 >> valPer;
      std::cout << valPer << std::endl;
      std::cout << strCharacteristic << std::endl;
		if ( valPer == 0 ) {
			ACS_LOG(LM_FULL_INFO ,"MonitorPoint::MonitorPoint", (LM_DEBUG, "Values from property %s (%s) will NOT be collected on value percentage change, because archive_delta_percent is set to '%s'.",
					property_m->name(),
					property_m->_repository_id(),
					strCharacteristic
			));
		} else {
			valuePercentTrigger_m = valPer;
		}
	} catch(CORBA::SystemException &ex) {
		ACE_PRINT_EXCEPTION(ex, "CORBA problem in MonitorComponent::addProperty");
	} catch(...) {
		printf("problem in MonitorComponent::addProperty!!!\n");
	}//try-catch
}//MonitorPoint

template<class T, class TBLOB_SEQ, class TPROP, class TCB, class TBASE>
MonitorPoint<T, TBLOB_SEQ, TPROP, TCB, TBASE>::~MonitorPoint()
{
	AUTO_TRACE("MonitorPoint<>::~MonitorPoint");
	CORBA::release(property_m);
}//~MonitorPoint


template<class T, class TBLOB_SEQ, class TPROP, class TCB, class TBASE>
void MonitorPoint<T, TBLOB_SEQ, TPROP, TCB, TBASE>::startMonitoring()
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
			if(suppressed_m == true)
				monitor_m->suspend();

			monitor_m->set_timer_trigger(archivingInterval_m);
			//monitor_m->set_value_trigger(valueTrigger_m);
			req = monitor_m->_request("set_value_trigger");
			req->add_in_arg ("delta") <<= valueTrigger_m;
			if(valueTrigger_m == 0) req->add_in_arg ("enable") <<= false;
			else req->add_in_arg ("enable") <<= true;
			req->set_return_type (CORBA::_tc_void);
			req->invoke();
			//monitor_m->set_value_percent_trigger(valuePercentTrigger_m);
			req = monitor_m->_request("set_value_percent_trigger");
			req->add_in_arg ("delta") <<= valuePercentTrigger_m;
			if(valuePercentTrigger_m == 0) req->add_in_arg ("enable") <<= false;
			else req->add_in_arg ("enable") <<= true;
			req->set_return_type (CORBA::_tc_void);
			req->invoke();

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

template<class T, class TBLOB_SEQ, class TPROP, class TCB, class TBASE>
void MonitorPoint<T, TBLOB_SEQ, TPROP, TCB, TBASE>::stopMonitoring()
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

template<class T, class TBLOB_SEQ, class TPROP, class TCB, class TBASE>
void MonitorPoint<T, TBLOB_SEQ, TPROP, TCB, TBASE>::fillSeq()
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

template<class T, class TBLOB_SEQ, class TPROP, class TCB, class TBASE>
void MonitorPoint<T, TBLOB_SEQ, TPROP, TCB, TBASE>::set_archiving_interval(ACS::TimeInterval time)
{
	AUTO_TRACE("MonitorPoint<T, TPROP, TCB>::set_archiving_interval");
	archivingInterval_m = time;
	if ( CORBA::is_nil(monitor_m) )
		return; // The monitor does not exist.
	monitor_m->set_timer_trigger(archivingInterval_m);
}

template<class T, class TBLOB_SEQ, class TPROP, class TCB, class TBASE>
void MonitorPoint<T, TBLOB_SEQ, TPROP, TCB, TBASE>::suppress_archiving()
{
	AUTO_TRACE("MonitorPoint<T, TPROP, TCB>::suppress_archiving");
	suppressed_m = true;
	if ( CORBA::is_nil(monitor_m) )
		return; // The monitor does not exist.
	monitor_m->suspend();
}

template<class T, class TBLOB_SEQ, class TPROP, class TCB, class TBASE>
void MonitorPoint<T, TBLOB_SEQ, TPROP, TCB, TBASE>::enable_archiving()
{
	AUTO_TRACE("MonitorPoint<T, TPROP, TCB>::enable_archiving");
	suppressed_m = false;
	if ( CORBA::is_nil(monitor_m) )
		return; // The monitor does not exist.
	monitor_m->resume();
}

template<class T, class TBLOB_SEQ, class TPROP, class TCB, class TBASE>
void MonitorPoint<T, TBLOB_SEQ, TPROP, TCB, TBASE>::working(T value, const ACSErr::Completion& comp, const ACS::CBDescOut& cbdescout)
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

template<class T, class TBLOB_SEQ, class TPROP, class TCB, class TBASE>
void MonitorPoint<T, TBLOB_SEQ, TPROP, TCB, TBASE>::done(T value, const ACSErr::Completion& comp, const ACS::CBDescOut& cbdescout)
{

}

