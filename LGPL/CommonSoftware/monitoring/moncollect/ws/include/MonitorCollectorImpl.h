#ifndef MONITOR_COLLECTOR_IMPL_H
#define MONITOR_COLLECTOR_IMPL_H
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
* "@(#) $Id: MonitorCollectorImpl.h,v 1.3 2012/01/21 22:48:11 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2009-02-11  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "MonitorComponent.h"
#include <ace/Synch.h>
#include <ace/Hash_Map_Manager_T.h>
#include <MonitorArchiverIFC.h>

namespace TMCDB
{

/**
 * Monitor collector implementation
 */
class MonitorCollectorImpl :
public acscomponent::ACSComponentImpl,
public POA_TMCDB::MonitorCollector
{
public:
	MonitorCollectorImpl(const ACE_CString& name,
			maci::ContainerServices * containerServices);

	~MonitorCollectorImpl();

	// componnet's life cycle
	void initialize();

	void cleanUp();


	// implementations of IDL's methods
	void registerMonitoredDevice (const char * componentName, const char* serialNumber);

	void registerMonitoredDeviceWithMultipleSerial(const char*componentName, const TMCDB::propertySerialNumberSeq& serialNumbers);

	void deregisterMonitoredDevice (const char * componentName);

	void startMonitoring (const char * componentName);

	void stopMonitoring (const char * componentName);

	TMCDB::MonitorDataBlocks * getMonitorData ();

	void set_archiving_interval(const char* componentName, const char* propertyName, ACS::TimeInterval time);

	void suppress_archiving(const char* componentName, const char* propertyName);

	void enable_archiving(const char* componentName, const char* propertyName);

private:

	MonitorComponent* registerMonitoredComponent (const char * componentName);

	ACE_Hash_Map_Manager <ACE_CString, MonitorComponent*, ACE_Recursive_Thread_Mutex> monitorComponents_m;

	ACE_Recursive_Thread_Mutex mcMutex_m; /// protection for monitorComponents_m and related stuff

	unsigned int numOfComponents_m; /// number of registered devices

	/// pointer to container services
	maci::ContainerServices *contServ_m;

	/// here we hold reference to Archive monitor controller
	MonitorArchiver::Controller_var archiveMonitorController_m;
};//class MonitorCollectorImpl



};//namespace TMCDB


#endif /*!_H*/
