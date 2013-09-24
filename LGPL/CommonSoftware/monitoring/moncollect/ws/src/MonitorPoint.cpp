/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2009
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: MonitorPoint.cpp,v 1.5 2012/10/10 09:48:54 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2009-02-11  created
*/

#include "vltPort.h"

static char *rcsId="@(#) $Id: MonitorPoint.cpp,v 1.5 2012/10/10 09:48:54 bjeram Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "MonitorPoint.h"

using namespace TMCDB;


MonitorPointBase::MonitorPointBase(const char *propertyName, const ACS::TimeInterval &archivingInterval, TMCDB::DataValueType typeOfData, MonitorBlob& mb) :
	propertyName_m(propertyName),
	archivingInterval_m(archivingInterval),
	monitorBlob_m(mb)
{
	AUTO_TRACE("MonitorPointBase::MonitorPointBase");

	monitor_m = ACS::Monitor::_nil();
	subscription_m = ACS::Subscription::_nil();
	curSeqPos_m = 0;
	//monitorBlob_m = new MonitorBlob;
	monitorBlob_m.propertyName = CORBA::string_dup(propertyName);
	monitorBlob_m.typeOfValue = typeOfData;
	monitorSuppressed_m = false;
	alarmSuppressed_m = false;
	valuePercentTrigger_m = 0;
	backLogSize_m = 32;
}

void MonitorPointBase::setPropertySerialNumber(serialNumberTypeSeq& sn)
{
		monitorBlob_m.propertySerialNumber = sn;
}//setSerialNumber

MonitorPointBase::~MonitorPointBase()
{
	AUTO_TRACE("MonitorPointBase::~MonitorPointBase");
}

template<>
char* initValue<char*>(unsigned int len)
{
   return new char[len+1];
}


#include <tao/IFR_Client/IFR_BasicC.h>
EnumMonitorPoint::EnumMonitorPoint(const char *propertyName, const ACS::TimeInterval &archivingInterval, ACS::Property* property, TMCDB::DataValueType typeOfData, MonitorBlob& mb) :
	MonitorPointBase(propertyName, archivingInterval, typeOfData, mb)
{
	AUTO_TRACE("EnumMonitorPoint::EnumMonitorPoint");
	try
	{
		monitorServant_m = NULL;
		property_m = ACS::TypelessProperty::_narrow(property);
		CORBA::Request_var req;
		req = property_m->_request("_get_default_timer_trigger");
		req->set_return_type (ACS::_tc_TimeInterval);
		req->invoke();
		CORBA::Double timer = 0;
		if(req->response_received())
			req->return_value() >>= timer;
		else
			printf("DII problems\n");
		if (timer == 0)
		{
//TBD: here we should throw an exception !!
		}//if

		blobDataSeq_m.length(prealocSeqLen_m);
		seqLen_m = prealocSeqLen_m;
	}
	catch(CORBA::Exception &cex)
	{
//TBD: improve error handling
	cex._tao_print_exception("in EnumMonitorPoint::EnumMonitorPoint");
	}
	valueTrigger_m = 0;
	try
	{
		CORBA::Any *anyCharacteristic;
		char *strCharacteristic;
		anyCharacteristic = property_m->get_characteristic_by_name("archive_suppress");
		*anyCharacteristic >>= CORBA::Any::to_string(strCharacteristic, 0);
		if ( strcmp(strCharacteristic, "false")!=0 ) {
			ACS_LOG(LM_FULL_INFO ,"EnumMonitorPoint::EnumMonitorPoint", (LM_DEBUG, "Values from property %s (%s) will NOT be collected, because archive_suppress is set to 'false', but to: %s.",
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
			ACS_LOG(LM_FULL_INFO ,"EnumMonitorPoint::EnumMonitorPoint", (LM_DEBUG, "Values from property %s (%s) will NOT be collected by time interval, because archive_max_int is 0 (%f).",
					property_m->name(),
					property_m->_repository_id(),
					archiveMaxInt
			));
			archivingInterval_m = 0;
		}//if
		CORBA::ULong val(0);
		anyCharacteristic = property_m->get_characteristic_by_name("archive_delta");
		*anyCharacteristic >>= CORBA::Any::to_string(strCharacteristic, 0);
		std::istringstream i1(strCharacteristic);
		i1 >> val;
		if ( val == 0 ) {
			ACS_LOG(LM_FULL_INFO ,"EnumMonitorPoint::EnumMonitorPoint", (LM_DEBUG, "Values from property %s (%s) will NOT be collected on value change, because archive_delta is set to '%s'.",
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
			ACS_LOG(LM_FULL_INFO ,"EnumMonitorPoint::EnumMonitorPoint", (LM_DEBUG, "Values from property %s (%s) will NOT be collected on value percentage change, because archive_delta_percent is set to '%s'.",
					property_m->name(),
					property_m->_repository_id(),
					strCharacteristic
			));
		} else {
			valuePercentTrigger_m = valPer;
		}
	} catch(CORBA::SystemException &ex) {
	ex._tao_print_exception("CORBA problem in EnumMonitorPoint::EnumMonitorPoint");
	} catch(...) {
		printf("problem in EnumMonitorPoint::EnumMonitorPoint!!!\n");
	}//try-catch
}//EnumMonitorPoint

EnumMonitorPoint::~EnumMonitorPoint()
{
	AUTO_TRACE("EnumMonitorPoint::~EnumMonitorPoint");
	CORBA::release(property_m);
	if(monitorServant_m != NULL)
		delete monitorServant_m;
}//~EnumMonitorPoint

void EnumMonitorPoint::setMonitorServant(POA_ACS::CBuLong *servant)
{
   monitorServant_m = servant;
}

void EnumMonitorPoint::activate(maci::ContainerServices *cs)
{
	AUTO_TRACE("EnumMonitorPoint::activate");
	try
	{
		this->monitorCallback_m = cs->activateOffShoot(monitorServant_m);
		monitorServant_m->_remove_ref(); //Decrease ref count to 1
	}
	catch(CORBA::Exception &ex)
	{
	ex._tao_print_exception("EnumMonitorPoint::activate");
	}
}

void EnumMonitorPoint::deactivate(maci::ContainerServices *cs)
{
	AUTO_TRACE("EnumMonitorPoint::deactivate");
	try
	{
		cs->deactivateOffShoot(monitorServant_m);
	}
	catch(CORBA::Exception &ex)
	{
	ex._tao_print_exception("EnumMonitorPoint::deactivate");
	}
}

void EnumMonitorPoint::startMonitoring()
{
	ACS::CBDescIn cbDescIn;
	AUTO_TRACE("EnumMonitorPoint::startMonitoring");
	try
	{
		if ( !CORBA::is_nil(monitor_m) )
			return; // we are already monitoring
		CORBA::Request_var req;
		req = property_m->_request("create_monitor");
		req->add_in_arg ("CB") <<= monitorCallback_m.in();
		req->add_in_arg ("CBDescIn") <<= cbDescIn;
		req->set_return_type (ACS::_tc_Monitor);
		req->invoke();
		if (req->response_received ())
		{
			ACS::Monitor *m;
			req->return_value() >>= m;
			monitor_m = m->_duplicate(m);
			if(monitorSuppressed_m == true)
				monitor_m->suspend();
			monitor_m->set_timer_trigger(archivingInterval_m);
			////monitor_m->set_value_trigger(valueTrigger_m);
			//req = monitor_m->_request("set_value_trigger");
			//req->add_in_arg ("delta") <<= valueTrigger_m;
			//if(valueTrigger_m == 0) req->add_in_arg ("enable") <<= false;
			//else req->add_in_arg ("enable") <<= true;
			//req->set_return_type (CORBA::_tc_void);
			//req->invoke();
			////monitor_m->set_value_percent_trigger(valuePercentTrigger_m);
			//req = monitor_m->_request("set_value_percent_trigger");
			//req->add_in_arg ("delta") <<= valuePercentTrigger_m;
			//if(valuePercentTrigger_m == 0) req->add_in_arg ("enable") <<= false;
			//else req->add_in_arg ("enable") <<= true;
			//req->set_return_type (CORBA::_tc_void);
			//req->invoke();
		}else
			printf("DII problems\n");
	}
	catch(CORBA::Exception &cex)
	{
		//TBD: improve error handling
	cex._tao_print_exception("in EnumMonitorPoint::startMonitoring");
	}
}//startMonitoring

void EnumMonitorPoint::stopMonitoring()
{
	AUTO_TRACE("EnumMonitorPoint::stopMonitoring");
	try
	{
		if ( !CORBA::is_nil(monitor_m) )
			monitor_m->destroy();
		monitor_m = ACS::Monitor::_nil();
	}
	catch(CORBA::Exception &cex)
	{
		//TBD: improve error handling
	cex._tao_print_exception("in EnumMonitorPoint::stopMonitoring");
	}
}//stopMonitoring

void EnumMonitorPoint::fillSeq()
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

void EnumMonitorPoint::set_archiving_interval(ACS::TimeInterval time)
{
	AUTO_TRACE("EnumMonitorPoint::set_archiving_interval");
	archivingInterval_m = time;
	if ( CORBA::is_nil(monitor_m) )
		return; // The monitor does not exist.
	monitor_m->set_timer_trigger(archivingInterval_m);
}

void EnumMonitorPoint::suppress_archiving()
{
	AUTO_TRACE("EnumMonitorPoint::suppress_archiving");
	monitorSuppressed_m = true;
	if ( CORBA::is_nil(monitor_m) )
		return; // The monitor does not exist.
	monitor_m->suspend();
}

void EnumMonitorPoint::enable_archiving()
{
	AUTO_TRACE("EnumMonitorPoint::enable_archiving");
	monitorSuppressed_m = false;
	if ( CORBA::is_nil(monitor_m) )
		return; // The monitor does not exist.
	monitor_m->resume();
}
//
void EnumMonitorPoint::working(CORBA::ULong value, const ACSErr::Completion& comp, const ACS::CBDescOut& cbdescout)
{
	ACE_GUARD(ACE_Thread_Mutex, mut, switchMutex_m);

	if ( curSeqPos_m>=seqLen_m )
	{
		seqLen_m = curSeqPos_m+prealocSeqLen_m;
		blobDataSeq_m.length(seqLen_m);
	}//if

	blobDataSeq_m[curSeqPos_m].value = value;
	blobDataSeq_m[curSeqPos_m].time = comp.timeStamp;

	curSeqPos_m++;
}

void EnumMonitorPoint::done(CORBA::ULong value, const ACSErr::Completion& comp, const ACS::CBDescOut& cbdescout)
{

}

ROEnumMonitorPoint::ROEnumMonitorPoint(const char *propertyName, const ACS::TimeInterval &monitoringInterval, ACS::Property* property, TMCDB::DataValueType typeOfData, MonitorBlob& mb) :
	EnumMonitorPoint(propertyName, monitoringInterval, property, typeOfData, mb)
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
			ACS_LOG(LM_FULL_INFO ,"ROEnumMonitorPoint::ROEnumMonitorPoint", (LM_DEBUG, "Values from property %s (%s) will NOT be collected when alarms are raised, because alarm_timer_trig is set to '%s'.",
					this->property_m->name(),
					this->property_m->_repository_id(),
					strCharacteristic
			));
		} else {
			this->alarmTimerTrigger_m = valPer;
		}
	} catch(CORBA::SystemException &ex) {
	ex._tao_print_exception("CORBA problem in ROEnumMonitorPoint::EnumMonitorPoint");
	} catch(...) {
		printf("problem in ROEnumMonitorPoint::EnumMonitorPoint!!!\n");
	}//try-catch
}

ROEnumMonitorPoint::~ROEnumMonitorPoint()
{
	if(alarmServant_m != NULL)
		delete alarmServant_m;
}

void ROEnumMonitorPoint::setAlarmServant(POA_ACS::AlarmuLong *servant)
{
   alarmServant_m = servant;
}

void ROEnumMonitorPoint::activate(maci::ContainerServices *cs)
{
	AUTO_TRACE("ROEnumMonitorPoint::activate");
   EnumMonitorPoint::activate(cs);
	try
	{
		this->alarmCallback_m = cs->activateOffShoot(alarmServant_m);
		alarmServant_m->_remove_ref(); //Decrease ref count to 1
	}
	catch(CORBA::Exception &ex)
	{
	ex._tao_print_exception("ROEnumMonitorPoint::activate");
	}
}

void ROEnumMonitorPoint::deactivate(maci::ContainerServices *cs)
{
	AUTO_TRACE("ROEnumMonitorPoint::deactivate");
   EnumMonitorPoint::deactivate(cs);
	try
	{
		cs->deactivateOffShoot(alarmServant_m);
	}
	catch(CORBA::Exception &ex)
	{
	ex._tao_print_exception("ROEnumMonitorPoint::deactivate");
	}
}

void ROEnumMonitorPoint::startMonitoring()
{
   EnumMonitorPoint::startMonitoring();
	ACS::CBDescIn cbDescIn;
	AUTO_TRACE("ROEnumMonitorPoint::startMonitoring");

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
	cex._tao_print_exception("in ROEnumMonitorPoint::startMonitoring");
	}
}//startMonitoring

void ROEnumMonitorPoint::stopMonitoring()
{
   EnumMonitorPoint::stopMonitoring();
	AUTO_TRACE("ROEnumMonitorPoint::stopMonitoring");
	try
	{
	if ( !CORBA::is_nil(this->subscription_m) )
	   this->subscription_m->destroy();
	   this->subscription_m = ACS::Subscription::_nil();
	}
	catch(CORBA::Exception &cex)
	{
		//TBD: improve error handling
	cex._tao_print_exception("in ROEnumMonitorPoint::stopMonitoring");
	}
}//stopMonitoring

void ROEnumMonitorPoint::suppress_archiving()
{
   EnumMonitorPoint::suppress_archiving();
	AUTO_TRACE("ROEnumMonitorPoint::suppress_archiving");
	this->alarmSuppressed_m = true;
	if ( CORBA::is_nil(this->subscription_m) )
		return; // The monitor does not exist.
	this->subscription_m->suspend();
}

void ROEnumMonitorPoint::enable_archiving()
{
   EnumMonitorPoint::enable_archiving();
	AUTO_TRACE("ROEnumMonitorPoint::enable_archiving");
	this->alarmSuppressed_m = false;
	if ( CORBA::is_nil(this->subscription_m) )
		return; // The monitor does not exist.
	this->subscription_m->resume();
}

void ROEnumMonitorPoint::alarm_raised(CORBA::ULong& value, const ACSErr::Completion& comp, const ACS::CBDescOut& cbdescout)
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

   for(unsigned int i = 0; i < comp.previousError.length(); i++)
      std::cout << comp.previousError[i].shortDescription << std::endl;
   //Obtain back-log and archive it.
   ACS::TimeSeq_var ts;
   ACS::uLongSeq_var seq;
	CORBA::Request_var req;
	req = property_m->_request("get_history");
	req->add_in_arg ("n_last_values") <<= this->backLogSize_m;
	req->add_out_arg ("vs") <<= seq.out();
	req->add_out_arg ("ts") <<= ts.out();
	req->set_return_type (CORBA::_tc_long);
	req->invoke();
	CORBA::Long len = 0;
	if(req->response_received())
		req->return_value() >>= len;
   //CORBA::Long len = this->property_m->get_history(this->backLogSize_m, seq.out(), ts.out());
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

void ROEnumMonitorPoint::alarm_cleared(CORBA::ULong& value, const ACSErr::Completion& comp, const ACS::CBDescOut& cbdescout)
{
}

/*___oOo___*/
