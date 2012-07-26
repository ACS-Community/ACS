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
* "@(#) $Id: baciAlarmSystemMonitor_T.h,v 1.11 2012/07/26 12:55:35 gchiozzi Exp $"
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

#include <baciAlarmSystemMonitorBase.h>

namespace baci
{

/**
 *  Common class for the Alarm System Monitors.
 *  Implementation classes have only to implement check method.
 *  The purpose of this class or better its implementation is to send alarms to the ACS alarm system
 */
template<class TPROP>
class AlarmSystemMonitor : public AlarmSystemMonitorBase
{
  public:
    
    AlarmSystemMonitor(TPROP * property, EventDispatcher * eventDispatcher);

    virtual ~AlarmSystemMonitor();

  protected:
    
    void sendAlarm(int code, bool active);
    
  private:
    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const AlarmSystemMonitor&);
    
    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    AlarmSystemMonitor(const AlarmSystemMonitor&);
    
    
  protected:

    /// pointer to the baci property - owner
    TPROP *property_mp;
    
};//class AlarmSystemMonitor

}// namespace baci

#endif /*!_H*/

