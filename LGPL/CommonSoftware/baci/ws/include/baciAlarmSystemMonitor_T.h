#ifndef _ALARM_SYSTEM_MONITOR_H
#define _ALARM_SYSTEM_MONITOR_H
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
* "@(#) $Id: baciAlarmSystemMonitor_T.h,v 1.6 2006/10/20 07:45:05 gchiozzi Exp $"
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

namespace baci
{

/**
 *  Common class for the Alarm System Monitors.
 *  Implementation classes have only to implement check method.
 *  The purpose of this class or better its implementation is to send alarms to the ACS alarm system
 */
template<class TPROP>
class baci_EXPORT AlarmSystemMonitor : public EventStrategy
{
  public:
    
    AlarmSystemMonitor(TPROP * property, EventDispatcher * eventDispatcher);

    virtual ~AlarmSystemMonitor();
    
    bool failed();
    void succeeded();
    
    virtual bool isSuspended() { return false; }
    
// here we do not need recovery stuff
    virtual int getId(void){ return -1; }
    
    virtual const char* getName(void){ return ""; }
    
    virtual char* getObjectState(void){ return ""; }
    
    virtual void setObjectState(const char * state){}
// ... and also implementation of  POA_ACS::Subscription can be empty
    virtual void suspend () throw (CORBA::SystemException) {}
    
    virtual void resume () throw (CORBA::SystemException){}
    
    virtual void destroy () throw (CORBA::SystemException) {}

    virtual void check(BACIValue &val,
	       const ACSErr::Completion & c,
	       const ACS::CBDescOut & desc )=0;
	       
	/**
     * Send an alarm to the AlarmSystem
     */
    void sendAlarm(std::string family, std::string member, int code, bool active);
 
  private:
    
    ACE_CString name_m;
    
    bool suspended_m;
    
    int failureCount_m;
    
    CBDescIn desc_mIn;
    
    TimeInterval interval_m;
    
    EventDispatcher * eventDispatcher_mp;
    
    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const AlarmSystemMonitor&);
    
    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    AlarmSystemMonitor(const AlarmSystemMonitor&);
    
    
  protected:
    // The alarm system source
    auto_ptr<acsalarm::AlarmSystemInterface> alarmSource_map;

    TPROP *property_mp;
    
    int alarmRaised_m;
};//class AlarmSystemMonitor

}// namespace baci

#endif /*!_H*/

