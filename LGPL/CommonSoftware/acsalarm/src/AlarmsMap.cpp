/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2011 
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
* "@(#) $Id: AlarmsMap.cpp,v 1.5 2012/04/19 14:55:47 acaproni Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2011-06-19  created
*/


#include "vltPort.h"

static char *rcsId="@(#) $Id: AlarmsMap.cpp,v 1.5 2012/04/19 14:55:47 acaproni Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <ctime>

#include <ace/Time_Value.h>

#include "AlarmsMap.h"

//TODO: acstime cannot be used here due the acstime module is not build yet
//#include "acstimeTimeUtil.h"

using namespace acsalarm;

//////////////////////////////////////
// AlarmInfo
//////////////////////////////////////

AlarmInfo::AlarmInfo(const bool isActive):
	active_m(isActive)
{
	acsTime_m = time(NULL);
}

AlarmInfo::AlarmInfo(const AlarmInfo& ai)
{
	active_m=ai.active_m;
	acsTime_m=ai.acsTime_m;
}

//////////////////////////////////////
// AlarmsMap
//////////////////////////////////////


AlarmsMap::AlarmsMap():
		m_closed(false)
{
}

bool AlarmsMap::raise(std::string alarmID)
{
	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_mutex);
	if (m_closed) {
		return false;
	}
	return alarmSet(alarmID,true);
}

bool AlarmsMap::clear(std::string alarmID)
{
	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_mutex);
	if (m_closed) {
		return false;
	}
	return alarmSet(alarmID,false);
}

bool AlarmsMap::alarmSet(std::string alarmID, bool state)
{
	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_mutex);
	AlarmInfo* aInfo=new AlarmInfo(state);
	std::map<std::string,AlarmInfo*>::iterator it=alarmsMap.find(alarmID);
	if (it==alarmsMap.end()) {
		// No alarm in the map
		alarmsMap[alarmID]=aInfo;
		return true;
	}
	bool ret=(*it).second->active_m!=state; // The state changed
	alarmsMap[alarmID]=aInfo;
	return ret;
}

void AlarmsMap::start()
{
	alarmsMap.clear();
	m_closed=false;
}

void AlarmsMap::shutdown()
{
	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_mutex);
	m_closed=true;
	// Clean the map
	if (!alarmsMap.empty()) {
		std::map<std::string,AlarmInfo*>::iterator it;
		for (it=alarmsMap.begin(); it!=alarmsMap.end(); it++) {
			delete (*it).second;
		}
		alarmsMap.clear();
	}
}

void AlarmsMap::updateInternalDataStructs()
{
	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_mutex);
	time_t nowTime = time(NULL);
	// Clean the map
	if (!alarmsMap.empty()) {
		std::map<std::string,AlarmInfo*>::iterator it;
		for (it=alarmsMap.begin(); it!=alarmsMap.end() && !m_closed; it++) {
			if ((*it).second->acsTime_m + 30 < nowTime)
			{
				alarmsMap.erase(it);
			}
		}
	}
}

void AlarmsMap::getAllAlarms(std::vector<AlarmInfo> alarms) {
	if (alarmsMap.empty()) {
		return;
	}
	std::map<std::string,AlarmInfo*>::iterator it;
	for (it=alarmsMap.begin(); it!=alarmsMap.end(); it++) {
		const AlarmInfo aInfo((*it).second);
		alarms.push_back(aInfo);
	}
}

/*___oOo___*/
