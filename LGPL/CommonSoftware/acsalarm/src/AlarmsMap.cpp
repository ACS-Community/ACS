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
* "@(#) $Id: AlarmsMap.cpp,v 1.1 2011/06/20 19:06:26 acaproni Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2011-06-19  created
*/


#include "vltPort.h"

static char *rcsId="@(#) $Id: AlarmsMap.cpp,v 1.1 2011/06/20 19:06:26 acaproni Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <ctime>

#include <ace/Time_Value.h>

#include "AlarmsMap.h"

#include "acstimeTimeUtil.h"

using namespace acsalarm;

//////////////////////////////////////
// AlarmInfo
//////////////////////////////////////

AlarmInfo::AlarmInfo(const bool isActive):
	active_m(isActive)
{
	time_t nowTime=time(NULL);
	ACE_Time_Value now(nowTime);
	acstime::Epoch epoch = TimeUtil::ace2epoch(now);
	acsTime_m=epoch.value;
}

AlarmInfo::AlarmInfo(AlarmInfo& ai)
{
	active_m=ai.active_m;
	acsTime_m=ai.acsTime_m;
}

//////////////////////////////////////
// AlarmsMap
//////////////////////////////////////


AlarmsMap::AlarmsMap():
		ACS::Thread("AlarmsMap", 10000000, 10000000)
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
	// Start the thread
	resume();
}

void AlarmsMap::shutdown()
{
	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_mutex);
	m_closed=true;
	// Stop the thread
	terminate();
	// Clean the map
	if (!alarmsMap.empty()) {
		std::map<std::string,AlarmInfo*>::iterator it;
		for (it=alarmsMap.begin(); it!=alarmsMap.end(); it++) {
			delete (*it).second;
		}
		alarmsMap.clear();
	}
}

void AlarmsMap::runLoop()
{
	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_mutex);
	time_t nowTime_t=time(NULL);
	ACE_Time_Value now(nowTime_t);
	acstime::Epoch epoch = TimeUtil::ace2epoch(now);
	ACS::Time nowTime=epoch.value;
	// Clean the map
	if (!alarmsMap.empty()) {
		std::map<std::string,AlarmInfo*>::iterator it;
		for (it=alarmsMap.begin(); it!=alarmsMap.end() && !m_closed && check(); it++) {
			if ((*it).second->acsTime_m+30*10000000<nowTime)
			{
				alarmsMap.erase(it);
			}
		}
	}
}

/*___oOo___*/
