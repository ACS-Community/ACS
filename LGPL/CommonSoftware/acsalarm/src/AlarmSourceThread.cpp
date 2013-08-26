/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2012 
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
* "@(#) $Id: AlarmSourceThread.cpp,v 1.2 2012/04/27 09:10:28 acaproni Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2012-04-24  created 
*/

#include <ace/Guard_T.h>
#include <ace/Time_Value.h>
#include <ace/OS.h>

#include <ACSErrTypeCommon.h>
#include "AlarmSourceThread.h"

using namespace acsalarm;
using namespace acsthreadErrType;

AlarmSourceThread::AlarmSourceThread():
	ACS::Thread("AlarmSourceThread", 1000000),
	m_alarmSources()
{
	// Start the thread
	resume();
}

AlarmSourceThread::~AlarmSourceThread()
{
	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_mutex);
	m_alarmSources.clear();
	// Stop the thread
	terminate();
}

void AlarmSourceThread::runLoop()
{
	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_mutex);
	 std::set<AlarmSource*>::iterator it;
	 for (it=m_alarmSources.begin(); it!=m_alarmSources.end() && check(); ++it)
	 {
		 ACE_Time_Value nowAceTime=ACE_OS::gettimeofday();
		 try {
			 (*it)->update(nowAceTime.msec());
		 } catch (...) {
			 ACS_SHORT_LOG((LM_CRITICAL,"Exception caught callin AlarmSource->update()"));
			 ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__, "AlarmSourceThread::runLoop");
			 ExceptionInRunLoopExImpl ex(uex, __FILE__, __LINE__, "ACS::Thread::runLoop");
			 ex.setThreadName(getName());
			 throw ex;
		 }
	 }
}


bool AlarmSourceThread::registerForUpdating(AlarmSource* src)
{
	if (src==NULL) {
		return false;
	}
	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_mutex);
	std::pair<std::set<AlarmSource*>::iterator,bool> ret= m_alarmSources.insert(src);
	return ret.second;
}

bool AlarmSourceThread::unregisterFromUpdating(AlarmSource* src)
{
	if (src==NULL) {
		return false;
	}
	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_mutex);
	return m_alarmSources.erase(src)==1;
}

/*___oOo___*/
