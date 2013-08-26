#ifndef _ENUMPROP_ALARM_SYSTEM_MONITOR_H
#define _ENUMPROP_ALARM_SYSTEM_MONITOR_H
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
* "@(#) $Id: enumpropAlarmSystemMonitorEnumProp.h,v 1.2 2012/01/24 01:00:04 tstaig Exp $"
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
#include <acserr.h>
#include "baciAlarmSystemMonitor_T.h"

namespace baci
{

/**
 * implementation of the AlarmSystemMonitor for discret types (patter, ...) 
 */
template<class T, class TPROP>
class AlarmSystemMonitorEnumProp : public AlarmSystemMonitor<TPROP>
{
  public:
    
    AlarmSystemMonitorEnumProp(TPROP * property, EventDispatcher * eventDispatcher);

    virtual ~AlarmSystemMonitorEnumProp();
    
    virtual void check(BACIValue &val,
	       const ACSErr::Completion & c,
	       const ACS::CBDescOut & desc );
 
  private:
    
    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const AlarmSystemMonitorEnumProp&);
    
    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    AlarmSystemMonitorEnumProp(const AlarmSystemMonitorEnumProp&);
    
};//class AlarmSystemMonitorEnumProp

};//baci

#include "enumpropAlarmSystemMonitorEnumProp.i"

#endif /*!_H*/
