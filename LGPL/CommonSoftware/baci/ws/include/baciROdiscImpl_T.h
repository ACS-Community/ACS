#ifndef _baciROdiscImpl_T_H_
#define _baciROdiscImpl_T_H_
/*******************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2003 
*
*This library is free software; you can redistribute it and/or
*modify it under the terms of the GNU Lesser General Public
*License as published by the Free Software Foundation; either
*version 2.1 of the License, or (at your option) any later version.
*
*This library is distributed in the hope that it will be useful,
*but WITHOUT ANY WARRANTY; without even the implied warranty of
*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*Lesser General Public License for more details.
*
*You should have received a copy of the GNU Lesser General Public
*License along with this library; if not, write to the Free Software
*Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: baciROdiscImpl_T"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram    10/02/2003  created

*/

/** 
 * @file 
 * Header file for BACI Read-only Disc. Property Template Class.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciROcommonImpl_T.h>
#include <baciAlarmSystemMonitorDisc_T.h>

namespace baci {

template <ACS_RO_C> 
class ROdiscImpl : public virtual POA_SK,
		   public ROcommonImpl<ACS_RO_TL> 
{
  public:
    ROdiscImpl(const ACE_CString& name, BACIComponent *component_p, DevIO<TM> *devIO=0, bool flagdeldevIO=false,  int initalize=0);

 ROdiscImpl(bool init, const ACE_CString& name, BACIComponent *component_p, DevIO<TM> *devIO=0, bool flagdeldevIO=false,  int initalize=0);
/* -------------------- [ RO interface ] -------------------- */
  
    virtual ACS::Subscription_ptr 
    new_subscription_Alarm (TAlarm *cb,
			    const ACS::CBDescIn & desc); 

    virtual ~ROdiscImpl();

  protected:
    /** @defgroup  PropTypeROdiscImplTemplate PropType Class (member from ROdiscImpl)
     * The  PropType class is a templated typedef so there is no actual inline doc generated for it per-se.
     *  @{
     * PropType is a templated class implementation. The templated parameter is not known
     * from the ROdiscImpl class (it is actually a templated parameter for ROdiscImpl) so we
     * really cannot say much more about PropType.
     */
    typedef ROdiscImpl<ACS_RO_TL> PropType;

    /** @} */


    /**
     * monitor which sends information (alarms) to the alarm system
     */
// for time being it is just in ROpatter
//    AlarmSystemMonitorDisc<TS, PropType> *alarmSystemMonitor_mp;

};

 }; 

#endif
