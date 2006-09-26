#ifndef _ALARM_SYSTEM_MONITOR_DISC_H
#define _ALARM_SYSTEM_MONITOR_DISC_H
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
* "@(#) $Id: baciAlarmSystemMonitorDisc_T.h,v 1.3 2006/09/26 10:24:00 bjeram Exp $"
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

#include "baciAlarmSystemMonitor_T.h"

namespace baci
{

/**
 * implementation of the AlarmSystemMonitor for discret types (patter, ...) 
 */
template<class T, class TPROP>
class baci_EXPORT AlarmSystemMonitorDisc : public AlarmSystemMonitor<TPROP>
{
  public:
    
    AlarmSystemMonitorDisc(TPROP * property, EventDispatcher * eventDispatcher);

    virtual ~AlarmSystemMonitorDisc();
    
    virtual void check(BACIValue &val,
	       const ACSErr::Completion & c,
	       const ACS::CBDescOut & desc );
 
  private:
    
    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const AlarmSystemMonitorDisc&);
    
    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    AlarmSystemMonitorDisc(const AlarmSystemMonitorDisc&);
    
};//class AlarmSystemMonitorDisc

}//namespace baci

#endif /*!_H*/
