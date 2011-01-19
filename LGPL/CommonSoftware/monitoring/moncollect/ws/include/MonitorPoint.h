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
* "@(#) $Id: MonitorPoint.h,v 1.1 2011/01/19 21:20:41 tstaig Exp $"
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
	virtual void activate(maci::ContainerServices *cs);

	///start monitoring the property (monitor point)
	virtual void startMonitoring()=0;

	///stop monitoring the property (monitor point)
	virtual void stopMonitoring()=0;

	/// method that puts sequence of data blobDataSeq to the any
	virtual void fillSeq()=0;


protected:

	ACE_CString propertyName_m; /// property name

	ACS::TimeInterval archivingInterval_m; // interval in which the value should be archived (and so monitored)

	MonitorBlob& monitorBlob_m; ///here we put the values

	ACS::Monitor_var monitor_m; /// monitor of the property

	ACS::OffShoot_var callback_m; ///callback CORBA reference

	unsigned int curSeqPos_m; ///current position to write in the sequence

	unsigned int seqLen_m; ///sequence length

	ACE_Thread_Mutex switchMutex_m; //when we switch the collection sequence

	static const unsigned int prealocSeqLen_m = 100; // preallocated length of the seqnece. This is the step that the sequence will grow
	//TBD: do we need also type (as string or ..)?
};//MonitorPointBase

template <class T, class TBLOB_SEQ, class TPROP, class TCB>
class MonitorPoint : public MonitorPointBase, public virtual TCB
{
public:
	MonitorPoint(const char *propertyName, const ACS::TimeInterval &monitoringInterval, ACS::Property* property, TMCDB::DataValueType typeOfData, MonitorBlob& mb);

	~MonitorPoint();

	///start monitoring the property (monitor point)
	void startMonitoring();

	///stop monitoring the property (monitor point)
	void stopMonitoring();


	void fillSeq();

	/// implementig CB interface
	void working(T value, const ACSErr::Completion& comp, const ACS::CBDescOut& cbdescout);

	/// implementig CB interface
	void done(T value, const ACSErr::Completion& comp, const ACS::CBDescOut& cbdescout);
protected:
	TPROP* property_m;
	TBLOB_SEQ blobDataSeq_m;
};//MonitorPoint

#include "MonitorPoint.i"

};

#endif
