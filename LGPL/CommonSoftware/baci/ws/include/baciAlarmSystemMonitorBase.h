#ifndef _ALARM_SYSTEM_MONITOR_BASE_H
#define _ALARM_SYSTEM_MONITOR_BASE_H
/*******************************************************************************
 * ALMA - Atacama Large Millimiter Array
 * (c) European Southern Observatory, 2006
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
 * "@(#) $Id: baciAlarmSystemMonitorBase.h,v 1.4 2011/09/02 11:39:00 bjeram Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * bjeram  2006-09-13  created
 */

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <AlarmSystemInterface.h>
#include <baciEvent.h>

namespace baci
{

/**
 *  Base class for the Alarm System Monitor that is not template class.
 *  The purpose of this class to have a not template base class for all Alarm System monitors.
 */
class baci_EXPORT AlarmSystemMonitorBase : public EventStrategy
{
public:

	AlarmSystemMonitorBase(EventDispatcher * eventDispatcher);

	virtual ~AlarmSystemMonitorBase();

	bool failed();
	void succeeded();

	virtual bool isSuspended() { return false; }

	// here we do not need recovery stuff
	virtual int getId(void){ return -1; }

	virtual const char* getName(void){ return ""; }

	virtual const char* getObjectState(void){ return ""; }

	virtual void setObjectState(const char * state){}
	// ... and also implementation of  POA_ACS::Subscription can be empty
	virtual void suspend () {}

	virtual void resume () {}

	virtual void destroy () {}

	/**
	 * set level of the alarm
	 * @parm alarm level
	 */
	void setLevel(int level){ alarmLevel_m = level; }

	/**
	 * (re)set Alarm's fault family
	 * @param fault family
	 */
	void setFaultFamily(const char *ff);

	/**
	 * (re)set Alarm's fault member
	 * @param fault member
	 */
	void setFaultMember(const char *fm);

protected:

	virtual void check(BACIValue &val,
			const ACSErr::Completion & c,
			const ACS::CBDescOut & desc )=0;

	/**
	 * Send an alarm with a certain alarm code to the AlarmSystem
	 * @parm code alarm code
	 * @parm active true-raise an alarm, false-clear an alarm
	 */
	void sendAlarm(int code, bool active);


	/**
	 * Clear the alarm that was previously set/raised. For the alarm code it is used alarm code used for sending/raising the alarm.
	 * Main goal of the method is to be called when fault family or fault member is changed to clear the alarm.
	 */
	virtual void clearAlarm();


	/**
	 * Check last value that caused an alarm for the alarm.
	 * The purpose of the method is to be used afer fault family or fault member is changed.
	 */
	virtual void recheckAlarm();

	/**
	 * sets property of the alarm.
	 * If the property was already set it will be set to a new value.
	 * @parm name - property name
	 * @parm prop value of the property
	 *
	 */
	void setProperty(const char *name, const char *prop);

	template <class VT>
	void setProperty(const char *name, VT value)
	{
		std::ostringstream ostr;
		std::string ts;

		ostr << value << std::ends;
		ts =  ostr.str();
		setProperty(name, ts.c_str());
	}//setProperty<>


private:

	/**
	 * ALMA C++ coding standards state assignment operators should be disabled.
	 */
	void operator=(const AlarmSystemMonitorBase&);

	/**
	 * ALMA C++ coding standards state copy constructors should be disabled.
	 */
	AlarmSystemMonitorBase(const AlarmSystemMonitorBase&);


protected:
	ACE_CString name_m;

	bool suspended_m;

	int failureCount_m;

	CBDescIn desc_mIn;

	ACS::TimeInterval interval_m;

	EventDispatcher * eventDispatcher_mp;

	// The alarm system source
	// The source is a singleton (there is no need to delete this pointer)
	acsalarm::AlarmSystemInterface* alarmSource_map;

	/// indicator (flag) if an alarm was raised.
	/// in case of pattern how many alarms were raised
	/// in case of continues types like long/dobule etc. -1 low alarm, 1 high alarm
	int alarmRaised_m;

	std::string faultMember_m;  /// alarm fault member that is used to send alarm
	std::string faultFamily_m;  /// alarm fault  family that is used to send alarm

	int alarmLevel_m;  /// alarm's level (priorty)


	int lastAlarmFaultCode_m; ///fault code of sent alarm
	BACIValue lastAlarmValue_m;	//value that caused (raised or cleared) the last alarm

	// here we can store alarm propertes
	std::map<std::string, std::string> alarmProperties_m;

	ACE_Recursive_Thread_Mutex faultStructMutex_m;
};//class AlarmSystemMonitorBase

}// namespace baci

#endif /*!_H*/

