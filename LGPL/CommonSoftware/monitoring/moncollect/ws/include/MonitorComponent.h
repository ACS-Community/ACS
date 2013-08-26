#ifndef MONITOR_COMPONENT_H
#define MONITOR_COMPONENT_H
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
 * "@(#) $Id: MonitorComponent.h,v 1.3 2012/03/02 14:03:10 tstaig Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * bjeram  2009-02-11  created
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "MonitorPoint.h"

#define ROMONITORPOINTNS(NS, Type, type) ROMonitorPoint<NS::Type, TMCDB::type##BlobDataSeq, ACS::P##type, POA_ACS::CB##type, POA_ACS::Alarm##type, NS::Type, ACS::type##Seq_var, NS::Type>
#define ROMONITORPOINT(Type, type) ROMonitorPoint<const Type, TMCDB::type##BlobDataSeq, ACS::P##type, POA_ACS::CB##type, POA_ACS::Alarm##type, Type, ACS::type##Seq_var, const Type>
#define ROMONITORPOINTNSSEQ(NS, Type, type) ROMonitorPoint<const ACS::type##Seq&, TMCDB::type##SeqBlobDataSeq, ACS::P##type##Seq, POA_ACS::CB##type##Seq, POA_ACS::Alarm##type, NS::Type, ACS::type##SeqSeq_var, NS::Type>
#define ROMONITORPOINTSEQ(Type, type) ROMonitorPoint<const ACS::type##Seq&, TMCDB::type##SeqBlobDataSeq, ACS::P##type##Seq, POA_ACS::CB##type##Seq, POA_ACS::Alarm##type, Type, ACS::type##SeqSeq_var, const Type>
#define MONITORPOINTNS(NS, Type, type) MonitorPoint<NS::Type, TMCDB::type##BlobDataSeq, ACS::P##type, POA_ACS::CB##type, NS::Type>
#define MONITORPOINT(Type, type) MonitorPoint<const Type, TMCDB::type##BlobDataSeq, ACS::P##type, POA_ACS::CB##type, Type>
#define MONITORPOINTNSSEQ(NS, Type, type) MonitorPoint<const ACS::type##Seq&, TMCDB::type##SeqBlobDataSeq, ACS::P##type##Seq, POA_ACS::CB##type##Seq, NS::Type>
#define MONITORPOINTSEQ(Type, type) MonitorPoint<const ACS::type##Seq&, TMCDB::type##SeqBlobDataSeq, ACS::P##type##Seq, POA_ACS::CB##type##Seq, Type>
#define CBTIENS(NS, Type, type) POA_ACS::CB##type##_tie<MONITORPOINTNS(NS, Type, type) >
#define CBTIE(Type, type) POA_ACS::CB##type##_tie<MONITORPOINT(Type, type) >
#define CBTIENSSEQ(NS, Type, type) POA_ACS::CB##type##Seq_tie<MONITORPOINTNSSEQ(NS, Type, type) >
#define CBTIESEQ(Type, type) POA_ACS::CB##type##Seq_tie<MONITORPOINTSEQ(Type, type) >
#define ALARMTIENS(NS, Type, type) POA_ACS::Alarm##type##_tie<ROMONITORPOINTNS(NS, Type, type) >
#define ALARMTIE(Type, type) POA_ACS::Alarm##type##_tie<ROMONITORPOINT(Type, type) >
#define ALARMTIENSSEQ(NS, Type, type) POA_ACS::Alarm##type##_tie<ROMONITORPOINTNSSEQ(NS, Type, type) >
#define ALARMTIESEQ(Type, type) POA_ACS::Alarm##type##_tie<ROMONITORPOINTSEQ(Type, type) >

namespace TMCDB
{



/** Class that represent a component/device whose properties are going to be monitored
 *
 */
class MonitorComponent
{
public:
	/// ctor where , MonitorDataBlock is also created.
	MonitorComponent(ACS::CharacteristicComponent_ptr comp, maci::ContainerServices *cs); //ctor

	virtual ~MonitorComponent(); //ctor

	void startMonitoring();

	void stopMonitoring();

	/// adds a property with certain name
	/// the method has to find the type and monitoring interval
	bool addProperty(const char *propName);

	bool addProperty(const char *propName,  const char *pType,  ACS::Property* propRef, ACS::TimeInterval &monitoringInterval);

	/// adds all component's properties
	void addAllProperties();

	void fillSeq();

	MonitorDataBlock& getMonitorDataBlock() { return monitorDataBlock_m; }

	void setDeviceSerialNumber(serialNumberType sn);

	void setPropertySerialNumber(char* propertyName, serialNumberTypeSeq sn);

	void set_archiving_interval(const char* propertyName, ACS::TimeInterval time);

	void suppress_archiving(const char* propertyName);

	void enable_archiving(const char* propertyName);

private:

	/**
	 * returns archiving interval for the property if the interval is 0 means that shall not be archived for archive
	 */
	ACS::TimeInterval propertyArchivingInterval(ACS::PropertyDesc *);

	/// list of properties - points that we have to monitor
	std::vector<MonitorPointBase*> monitorPoints_m;
	/// component reference
	ACS::CharacteristicComponent_var component_m;

	/// here we collect data for the device
	MonitorDataBlock monitorDataBlock_m;

	/// index of last added property in monitorPoints_m and monitorDataBlock_m.monitorBlobs
	unsigned int seqIndex_m;

	// we need container services for activate/deactivate callbacks for monitors
	maci::ContainerServices *containerServices_m;

	/// Description of the monitored characteristic component
	ACS::CharacteristicComponentDesc_var compDesc_m;

    /// number of properties that monitored characteristic component has
	CORBA::ULong numOfProp_m;

	bool monitoring_m; /// is monitoring turned on or off ?

	ACS::Time m_monitoringStartTime; /// time when the monitoring started

	ACE_Thread_Mutex m_proMutex;

};//MonitorComponent



};//namespace TMCDB


#endif /*!_H*/
