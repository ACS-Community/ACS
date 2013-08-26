#ifndef MONITOR_POINT_IMPL_H
#define MONITOR_POINT_IMPL_H
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
* "@(#) $Id: MonitorPoint.h,v 1.5 2012/10/10 09:48:54 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2009-02-11  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <tao/DynamicInterface/Request.h>

#include "MonitorCollectorS.h"
#include <baciCharacteristicComponentImpl.h>
#include <TMCDBCOMMON_IDLS.h>
namespace TMCDB
{


/**
 * Class to hold data of a monitor point
 */
class MonitorPointBase : public virtual POA_ACS::Callback
{
public:
	MonitorPointBase(const char *propertyName, const ACS::TimeInterval &archivingInterval, TMCDB::DataValueType typeOfData, MonitorBlob& mb);

	virtual ~MonitorPointBase();

	void setPropertySerialNumber(serialNumberTypeSeq& sn);

	ACE_CString &getPropertyName() { return  propertyName_m; }

	CORBA::Boolean negotiate(ACS::TimeInterval, const ACS::CBDescOut&){ return true; }

	/// activate corba object
	virtual void activate(maci::ContainerServices *cs)=0;
	virtual void deactivate(maci::ContainerServices *cs)=0;

	///start monitoring the property (monitor point)
	virtual void startMonitoring()=0;

	///stop monitoring the property (monitor point)
	virtual void stopMonitoring()=0;

	/// method that puts sequence of data blobDataSeq to the any
	virtual void fillSeq()=0;

	virtual void set_archiving_interval(ACS::TimeInterval time)=0;

	virtual void suppress_archiving()=0;

	virtual void enable_archiving()=0;

protected:

	ACE_CString propertyName_m; /// property name

	ACS::TimeInterval archivingInterval_m; // interval in which the value should be archived (and so monitored)

	double valuePercentTrigger_m; // Delta value percentage a value can change before the value should be archived (and so monitored)

	MonitorBlob& monitorBlob_m; ///here we put the values

	ACS::Monitor_var monitor_m; /// monitor of the property

	ACS::Subscription_var subscription_m; /// Subscription for the alarm of the property

	ACS::OffShoot_var monitorCallback_m; ///callback CORBA reference
	ACS::OffShoot_var alarmCallback_m; ///callback CORBA reference

	unsigned int curSeqPos_m; ///current position to write in the sequence

	unsigned int seqLen_m; ///sequence length

	ACE_Thread_Mutex switchMutex_m; //when we switch the collection sequence

	bool monitorSuppressed_m;
	bool alarmSuppressed_m;
	double alarmTimerTrigger_m;

	static const unsigned int prealocSeqLen_m = 100; // preallocated length of the seqnece. This is the step that the sequence will grow

	CORBA::Long backLogSize_m;
	//TBD: do we need also type (as string or ..)?
};//MonitorPointBase

template <class T, class TBLOB_SEQ, class TPROP, class TCB, class TBASE>
class MonitorPoint : public MonitorPointBase
{
public:
	MonitorPoint(const char *propertyName, const ACS::TimeInterval &monitoringInterval, ACS::Property* property, TMCDB::DataValueType typeOfData, MonitorBlob& mb);

	virtual ~MonitorPoint();

	//Sets the servant for the property monitor
	void setMonitorServant(TCB *servant);

	virtual void activate(maci::ContainerServices *cs);
	virtual void deactivate(maci::ContainerServices *cs);

	///start monitoring the property (monitor point)
	virtual void startMonitoring();

	///stop monitoring the property (monitor point)
	virtual void stopMonitoring();

	void fillSeq();

	void set_archiving_interval(ACS::TimeInterval time);

	virtual void suppress_archiving();

	virtual void enable_archiving();

	/// implementig CB interface
	void working(T value, const ACSErr::Completion& comp, const ACS::CBDescOut& cbdescout);

	/// implementig CB interface
	void done(T value, const ACSErr::Completion& comp, const ACS::CBDescOut& cbdescout);
protected:
	TPROP* property_m;
	TBLOB_SEQ blobDataSeq_m;
	TBASE valueTrigger_m; // Delta value describing how much a value can change before the value should be archived (and so monitored)
	TCB * monitorServant_m;
};//MonitorPoint

template <class T, class TBLOB_SEQ, class TPROP, class TMCB, class TACB, class TBASE, class TSEQ, class TALARM>
class ROMonitorPoint : public MonitorPoint<T, TBLOB_SEQ, TPROP, TMCB, TBASE>
{
  public:
	ROMonitorPoint(const char *propertyName, const ACS::TimeInterval &monitoringInterval, ACS::Property* property, TMCDB::DataValueType typeOfData, MonitorBlob& mb);
	~ROMonitorPoint();
	//Sets the servant for the alarm monitor
	void setAlarmServant(TACB *servant);
	void activate(maci::ContainerServices *cs);
	void deactivate(maci::ContainerServices *cs);
	///start/stop monitoring the property (monitor point) and its alarms
	void startMonitoring();
	void stopMonitoring();
	void suppress_archiving();
	void enable_archiving();
	//Implementing Alarm interface
	void alarm_raised(TALARM value, const ACSErr::Completion& comp, const ACS::CBDescOut& cbdescout);
	void alarm_cleared(TALARM value, const ACSErr::Completion& comp, const ACS::CBDescOut& cbdescout);
  protected:
	TACB * alarmServant_m;
};

class EnumMonitorPoint : public MonitorPointBase
{
  public:
	EnumMonitorPoint(const char *propertyName, const ACS::TimeInterval &monitoringInterval, ACS::Property* property, TMCDB::DataValueType typeOfData, MonitorBlob& mb);
	~EnumMonitorPoint();
	//Sets the servant for the property monitor
	void setMonitorServant(POA_ACS::CBuLong *servant);

	virtual void activate(maci::ContainerServices *cs);
	virtual void deactivate(maci::ContainerServices *cs);

	///start monitoring the property (monitor point)
	virtual void startMonitoring();

	///stop monitoring the property (monitor point)
	virtual void stopMonitoring();

	void fillSeq();

	void set_archiving_interval(ACS::TimeInterval time);

	virtual void suppress_archiving();

	virtual void enable_archiving();

	/// implementig CB interface
	void working(CORBA::ULong value, const ACSErr::Completion& comp, const ACS::CBDescOut& cbdescout);

	/// implementig CB interface
	void done(CORBA::ULong value, const ACSErr::Completion& comp, const ACS::CBDescOut& cbdescout);
protected:
	ACS::TypelessProperty* property_m;
	enumBlobDataSeq blobDataSeq_m;
	CORBA::ULong valueTrigger_m; // Delta value describing how much a value can change before the value should be archived (and so monitored)
	POA_ACS::CBuLong * monitorServant_m;
};

class ROEnumMonitorPoint : public EnumMonitorPoint
{
  public:
	ROEnumMonitorPoint(const char *propertyName, const ACS::TimeInterval &monitoringInterval, ACS::Property* property, TMCDB::DataValueType typeOfData, MonitorBlob& mb);
	~ROEnumMonitorPoint();
	//Sets the servant for the alarm monitor
	void setAlarmServant(POA_ACS::AlarmuLong *servant);
	void activate(maci::ContainerServices *cs);
	void deactivate(maci::ContainerServices *cs);
	///start/stop monitoring the property (monitor point) and its alarms
	void startMonitoring();
	void stopMonitoring();
	void suppress_archiving();
	void enable_archiving();
	//Implementing Alarm interface
	void alarm_raised(CORBA::ULong& value, const ACSErr::Completion& comp, const ACS::CBDescOut& cbdescout);
	void alarm_cleared(CORBA::ULong& value, const ACSErr::Completion& comp, const ACS::CBDescOut& cbdescout);
  protected:
	POA_ACS::AlarmuLong * alarmServant_m;
};

template <class T>
T initValue(unsigned int len);
template <>
char* initValue(unsigned int len);
#include "MonitorPoint.i"

};

#endif
