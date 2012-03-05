
template<class T, class TBLOB_SEQ, class TPROP, class TCB, class TBASE>
MonitorPoint<T, TBLOB_SEQ, TPROP, TCB, TBASE>::MonitorPoint(const char *propertyName, const ACS::TimeInterval &archivingInterval,
		                                             ACS::Property* property, TMCDB::DataValueType typeOfData, MonitorBlob& mb) :
	MonitorPointBase(propertyName, archivingInterval, typeOfData, mb)
{
	AUTO_TRACE("MonitorPoint<>::MonitorPoint");
	try
	{

		monitorServant_m = NULL;
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
		if ( strcmp(strCharacteristic, "false")!=0 ) {
			ACS_LOG(LM_FULL_INFO ,"MonitorPoint<>::MonitorPoint", (LM_DEBUG, "Values from property %s (%s) will NOT be collected, because archive_suppress is set to 'false', but to: %s.",
					property_m->name(),
					property_m->_repository_id(),
					strCharacteristic
			));
			monitorSuppressed_m = true;
		}
		double archiveMaxInt;
		anyCharacteristic = property_m->get_characteristic_by_name("archive_max_int");
		*anyCharacteristic >>= CORBA::Any::to_string(strCharacteristic, 0);
		std::istringstream i(strCharacteristic);
		i >> archiveMaxInt;
		archiveMaxInt *= static_cast<double>(10000000.0); //we have to convert to 100s nsec.
		if ( archiveMaxInt==0.0 )
		{
			ACS_LOG(LM_FULL_INFO ,"MonitorPoint<>::MonitorPoint", (LM_DEBUG, "Values from property %s (%s) will NOT be collected by time interval, because archive_max_int is 0 (%f).",
					property_m->name(),
					property_m->_repository_id(),
					archiveMaxInt
			));
			archivingInterval_m = 0;
		}//if
		TBASE val(0);
		anyCharacteristic = property_m->get_characteristic_by_name("archive_delta");
		*anyCharacteristic >>= CORBA::Any::to_string(strCharacteristic, 0);
      val = initValue<TBASE>(strlen(strCharacteristic));
		std::istringstream i1(strCharacteristic);
		i1 >> val;
		if ( val == 0 ) {
			ACS_LOG(LM_FULL_INFO ,"MonitorPoint<>::MonitorPoint", (LM_DEBUG, "Values from property %s (%s) will NOT be collected on value change, because archive_delta is set to '%s'.",
					property_m->name(),
					property_m->_repository_id(),
					strCharacteristic
			));
		} else {
			valueTrigger_m = val;
		}
		anyCharacteristic = property_m->get_characteristic_by_name("archive_delta_percent");
		double valPer;
		*anyCharacteristic >>= CORBA::Any::to_string(strCharacteristic, 0);
		std::istringstream i2(strCharacteristic);
		i2 >> valPer;
		if ( valPer == 0 ) {
			ACS_LOG(LM_FULL_INFO ,"MonitorPoint<>::MonitorPoint", (LM_DEBUG, "Values from property %s (%s) will NOT be collected on value percentage change, because archive_delta_percent is set to '%s'.",
					property_m->name(),
					property_m->_repository_id(),
					strCharacteristic
			));
		} else {
			valuePercentTrigger_m = valPer;
		}
	} catch(CORBA::SystemException &ex) {
		ACE_PRINT_EXCEPTION(ex, "CORBA problem in MonitorPoint<>::MonitorPoint");
	} catch(...) {
		printf("problem in MonitorPoint<>::MonitorPoint!!!\n");
	}//try-catch
}//MonitorPoint

template<class T>
T initValue(unsigned int len)
{
   return 0;
}

template<class T, class TBLOB_SEQ, class TPROP, class TCB, class TBASE>
MonitorPoint<T, TBLOB_SEQ, TPROP, TCB, TBASE>::~MonitorPoint()
{
	AUTO_TRACE("MonitorPoint<>::~MonitorPoint");
	CORBA::release(property_m);
	if(monitorServant_m != NULL)
		delete monitorServant_m;
}//~MonitorPoint

template<class T, class TBLOB_SEQ, class TPROP, class TCB, class TBASE>
void MonitorPoint<T, TBLOB_SEQ, TPROP, TCB, TBASE>::setMonitorServant(TCB *servant)
{
   monitorServant_m = servant;
}

template<class T, class TBLOB_SEQ, class TPROP, class TCB, class TBASE>
void MonitorPoint<T, TBLOB_SEQ, TPROP, TCB, TBASE>::activate(maci::ContainerServices *cs)
{
	AUTO_TRACE("MonitorPoint<>::activate");
	try
	{
		this->monitorCallback_m = cs->activateOffShoot(monitorServant_m);
		monitorServant_m->_remove_ref(); //Decrease ref count to 1
	}
	catch(CORBA::Exception &ex)
	{
		ACE_PRINT_EXCEPTION(ex, "MonitorPoint<>::activate");
	}
}

template<class T, class TBLOB_SEQ, class TPROP, class TCB, class TBASE>
void MonitorPoint<T, TBLOB_SEQ, TPROP, TCB, TBASE>::deactivate(maci::ContainerServices *cs)
{
	AUTO_TRACE("MonitorPoint<>::deactivate");
	try
	{
		cs->deactivateOffShoot(monitorServant_m);
	}
	catch(CORBA::Exception &ex)
	{
		ACE_PRINT_EXCEPTION(ex, "MonitorPoint<>::deactivate");
	}
}

template<class T, class TBLOB_SEQ, class TPROP, class TCB, class TBASE>
void MonitorPoint<T, TBLOB_SEQ, TPROP, TCB, TBASE>::startMonitoring()
{
	ACS::CBDescIn cbDescIn;
	AUTO_TRACE("MonitorPoint<>::startMonitoring");

	try
	{

		//printf("cb1 %d %d\n", monitorCallback_m->_refcount_value(), _refcount_value());
		if ( !CORBA::is_nil(monitor_m) )
			return; // we are already monitoring

		CORBA::Request_var req;
		req = property_m->_request("create_monitor");
		req->add_in_arg ("CB") <<= monitorCallback_m.in();
		req->add_in_arg ("CBDescIn") <<= cbDescIn;
		req->set_return_type (ACS::_tc_Monitor);

		req->invoke();

		//printf("cb2 %d %d\n", monitorCallback_m->_refcount_value(), _refcount_value());

		if (req->response_received ())
		{
			ACS::Monitor *m;
			req->return_value() >>= m;
			monitor_m = m->_duplicate(m);
			if(monitorSuppressed_m == true)
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

		//monitor_m = property_m->create_monitor(this->monitorCallback_m, cbDescIn);
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
	AUTO_TRACE("MonitorPoint<>::set_archiving_interval");
	archivingInterval_m = time;
	if ( CORBA::is_nil(monitor_m) )
		return; // The monitor does not exist.
	monitor_m->set_timer_trigger(archivingInterval_m);
}

template<class T, class TBLOB_SEQ, class TPROP, class TCB, class TBASE>
void MonitorPoint<T, TBLOB_SEQ, TPROP, TCB, TBASE>::suppress_archiving()
{
	AUTO_TRACE("MonitorPoint<>::suppress_archiving");
	monitorSuppressed_m = true;
	if ( CORBA::is_nil(monitor_m) )
		return; // The monitor does not exist.
	monitor_m->suspend();
}

template<class T, class TBLOB_SEQ, class TPROP, class TCB, class TBASE>
void MonitorPoint<T, TBLOB_SEQ, TPROP, TCB, TBASE>::enable_archiving()
{
	AUTO_TRACE("MonitorPoint<>::enable_archiving");
	monitorSuppressed_m = false;
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
		/*ACS_LOG(LM_FULL_INFO ,"MonitorPoint<>::working", (LM_DEBUG, "Increased blobData sequence size for property: %s to: %d", monitorBlob_m.propertyName.in(), seqLen_m));*/
	}//if

	blobDataSeq_m[curSeqPos_m].value = value;
	blobDataSeq_m[curSeqPos_m].time = comp.timeStamp;

	curSeqPos_m++;
}

template<class T, class TBLOB_SEQ, class TPROP, class TCB, class TBASE>
void MonitorPoint<T, TBLOB_SEQ, TPROP, TCB, TBASE>::done(T value, const ACSErr::Completion& comp, const ACS::CBDescOut& cbdescout)
{

}

template<class T, class TBLOB_SEQ, class TPROP, class TMCB, class TACB, class TBASE, class TSEQ, class TALARM>
ROMonitorPoint<T, TBLOB_SEQ, TPROP, TMCB, TACB, TBASE, TSEQ, TALARM>::ROMonitorPoint(const char *propertyName, const ACS::TimeInterval &monitoringInterval, ACS::Property* property, TMCDB::DataValueType typeOfData, MonitorBlob& mb) :
	MonitorPoint<T, TBLOB_SEQ, TPROP, TMCB, TBASE>(propertyName, monitoringInterval, property, typeOfData, mb)
{
	try
	{
		alarmServant_m = NULL;
		CORBA::Any *anyCharacteristic;
		char *strCharacteristic;
		anyCharacteristic = this->property_m->get_characteristic_by_name("alarm_timer_trig");
		double valPer;
		this->alarmTimerTrigger_m = 0;
		*anyCharacteristic >>= CORBA::Any::to_string(strCharacteristic, 0);
		std::istringstream i2(strCharacteristic);
		i2 >> valPer;
		if ( valPer == 0 ) {
			ACS_LOG(LM_FULL_INFO ,"ROMonitorPoint<>::ROMonitorPoint", (LM_DEBUG, "Values from property %s (%s) will NOT be collected when alarms are raised, because alarm_timer_trig is set to '%s'.",
					this->property_m->name(),
					this->property_m->_repository_id(),
					strCharacteristic
			));
		} else {
			this->alarmTimerTrigger_m = valPer;
		}
	} catch(CORBA::SystemException &ex) {
		ACE_PRINT_EXCEPTION(ex, "CORBA problem in ROMonitorPoint<>::MonitorPoint");
	} catch(...) {
		printf("problem in ROMonitorPoint<>::MonitorPoint!!!\n");
	}//try-catch
}

template<class T, class TBLOB_SEQ, class TPROP, class TMCB, class TACB, class TBASE, class TSEQ, class TALARM>
ROMonitorPoint<T, TBLOB_SEQ, TPROP, TMCB, TACB, TBASE, TSEQ, TALARM>::~ROMonitorPoint()
{
	if(alarmServant_m != NULL)
		delete alarmServant_m;
}

template<class T, class TBLOB_SEQ, class TPROP, class TMCB, class TACB, class TBASE, class TSEQ, class TALARM>
void ROMonitorPoint<T, TBLOB_SEQ, TPROP, TMCB, TACB, TBASE, TSEQ, TALARM>::setAlarmServant(TACB *servant)
{
   alarmServant_m = servant;
}

template<class T, class TBLOB_SEQ, class TPROP, class TMCB, class TACB, class TBASE, class TSEQ, class TALARM>
void ROMonitorPoint<T, TBLOB_SEQ, TPROP, TMCB, TACB, TBASE, TSEQ, TALARM>::activate(maci::ContainerServices *cs)
{
	AUTO_TRACE("ROMonitorPoint<>::activate");
   MonitorPoint<T, TBLOB_SEQ, TPROP, TMCB, TBASE>::activate(cs);
	try
	{
		this->alarmCallback_m = cs->activateOffShoot(alarmServant_m);
		alarmServant_m->_remove_ref(); //Decrease ref count to 1
	}
	catch(CORBA::Exception &ex)
	{
		ACE_PRINT_EXCEPTION(ex, "ROMonitorPoint<>::activate");
	}
}

template<class T, class TBLOB_SEQ, class TPROP, class TMCB, class TACB, class TBASE, class TSEQ, class TALARM>
void ROMonitorPoint<T, TBLOB_SEQ, TPROP, TMCB, TACB, TBASE, TSEQ, TALARM>::deactivate(maci::ContainerServices *cs)
{
	AUTO_TRACE("ROMonitorPoint<>::deactivate");
   MonitorPoint<T, TBLOB_SEQ, TPROP, TMCB, TBASE>::deactivate(cs);
	try
	{
		cs->deactivateOffShoot(alarmServant_m);
	}
	catch(CORBA::Exception &ex)
	{
		ACE_PRINT_EXCEPTION(ex, "ROMonitorPoint<>::deactivate");
	}
}

template<class T, class TBLOB_SEQ, class TPROP, class TMCB, class TACB, class TBASE, class TSEQ, class TALARM>
void ROMonitorPoint<T, TBLOB_SEQ, TPROP, TMCB, TACB, TBASE, TSEQ, TALARM>::startMonitoring()
{
   MonitorPoint<T, TBLOB_SEQ, TPROP, TMCB, TBASE>::startMonitoring();
	ACS::CBDescIn cbDescIn;
	AUTO_TRACE("ROMonitorPoint<>::startMonitoring");

	try
	{
		if(this->alarmTimerTrigger_m == 0)
			return; //Alarms can't be configured with alarm_timer_trig=0

		//printf("cb1 %d %d\n", alarmCallback_m->_refcount_value(), _refcount_value());
		if ( !CORBA::is_nil(this->subscription_m) )
			return; // we are already checking alarms

		CORBA::Request_var req;
		req = this->property_m->_request("new_subscription_Alarm");
		req->add_in_arg ("CB") <<= this->alarmCallback_m.in();
		req->add_in_arg ("CBDescIn") <<= cbDescIn;
		req->set_return_type (ACS::_tc_Subscription);
		req->invoke();

		//printf("cb2 %d %d\n", alarmCallback_m->_refcount_value(), _refcount_value());
		if (req->response_received ())
		{
			ACS::Subscription *s;
			req->return_value() >>= s;
			this->subscription_m = s->_duplicate(s);
			if(this->alarmSuppressed_m == true)
				this->subscription_m->suspend();
		}else
			printf("DII problems\n");
		//subscription_m = property_m->new_subscription_Alarm(this->alarmCallback_m, cbDescIn);
	}
	catch(CORBA::Exception &cex)
	{
		//TBD: improve error handling
		ACE_PRINT_EXCEPTION(cex, "in ROMonitorPoint<>::startMonitoring");
	}
}//startMonitoring

template<class T, class TBLOB_SEQ, class TPROP, class TMCB, class TACB, class TBASE, class TSEQ, class TALARM>
void ROMonitorPoint<T, TBLOB_SEQ, TPROP, TMCB, TACB, TBASE, TSEQ, TALARM>::stopMonitoring()
{
   MonitorPoint<T, TBLOB_SEQ, TPROP, TMCB, TBASE>::stopMonitoring();
	AUTO_TRACE("ROMonitorPoint<>::stopMonitoring");
	try
	{
	if ( !CORBA::is_nil(this->subscription_m) )
	   this->subscription_m->destroy();
	   this->subscription_m = ACS::Subscription::_nil();
	}
	catch(CORBA::Exception &cex)
	{
		//TBD: improve error handling
		ACE_PRINT_EXCEPTION(cex, "in ROMonitorPoint<>::stopMonitoring");
	}
}//stopMonitoring

template<class T, class TBLOB_SEQ, class TPROP, class TMCB, class TACB, class TBASE, class TSEQ, class TALARM>
void ROMonitorPoint<T, TBLOB_SEQ, TPROP, TMCB, TACB, TBASE, TSEQ, TALARM>::suppress_archiving()
{
   MonitorPoint<T, TBLOB_SEQ, TPROP, TMCB, TBASE>::suppress_archiving();
	AUTO_TRACE("ROMonitorPoint<>::suppress_archiving");
	this->alarmSuppressed_m = true;
	if ( CORBA::is_nil(this->subscription_m) )
		return; // The monitor does not exist.
	this->subscription_m->suspend();
}

template<class T, class TBLOB_SEQ, class TPROP, class TMCB, class TACB, class TBASE, class TSEQ, class TALARM>
void ROMonitorPoint<T, TBLOB_SEQ, TPROP, TMCB, TACB, TBASE, TSEQ, TALARM>::enable_archiving()
{
   MonitorPoint<T, TBLOB_SEQ, TPROP, TMCB, TBASE>::enable_archiving();
	AUTO_TRACE("ROMonitorPoint<>::enable_archiving");
	this->alarmSuppressed_m = false;
	if ( CORBA::is_nil(this->subscription_m) )
		return; // The monitor does not exist.
	this->subscription_m->resume();
}

template<class T, class TBLOB_SEQ, class TPROP, class TMCB, class TACB, class TBASE, class TSEQ, class TALARM>
void ROMonitorPoint<T, TBLOB_SEQ, TPROP, TMCB, TACB, TBASE, TSEQ, TALARM>::alarm_raised(TALARM value, const ACSErr::Completion& comp, const ACS::CBDescOut& cbdescout)
{
	ACE_GUARD(ACE_Thread_Mutex, mut, this->switchMutex_m);

	if ( this->curSeqPos_m>=this->seqLen_m )
	{
		this->seqLen_m = this->curSeqPos_m+this->prealocSeqLen_m;
		this->blobDataSeq_m.length(this->seqLen_m);
	}//if

	this->blobDataSeq_m[this->curSeqPos_m].value = value;
	this->blobDataSeq_m[this->curSeqPos_m].time = comp.timeStamp;

	this->curSeqPos_m++;

   std::cout << "Error while reading property:" << this->propertyName_m << " with description:" << std::endl;
   for(unsigned int i = 0; i < comp.previousError.length(); i++)
      std::cout << comp.previousError[i].shortDescription << std::endl;
   //Obtain back-log and archive it.
   ACS::TimeSeq_var ts;
   TSEQ seq;
   CORBA::Long len = this->property_m->get_history(this->backLogSize_m, seq.out(), ts.out());
   for(int i = 0; i < len; i++)
   {
     	if ( this->curSeqPos_m>=this->seqLen_m )
     	{
     		this->seqLen_m = this->curSeqPos_m+this->prealocSeqLen_m;
     		this->blobDataSeq_m.length(this->seqLen_m);
     	}//if
  
     	this->blobDataSeq_m[this->curSeqPos_m].value = seq.in()[i];
     	this->blobDataSeq_m[this->curSeqPos_m].time = ts.in()[i];
  
     	this->curSeqPos_m++;
   }
}

template<class T, class TBLOB_SEQ, class TPROP, class TMCB, class TACB, class TBASE, class TSEQ, class TALARM>
void ROMonitorPoint<T, TBLOB_SEQ, TPROP, TMCB, TACB, TBASE, TSEQ, TALARM>::alarm_cleared(TALARM value, const ACSErr::Completion& comp, const ACS::CBDescOut& cbdescout)
{
}
