#ifndef _baciROSeqContImpl_T_H_
#define _baciROSeqContImpl_T_H_
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
* "@(#) $Id: baciROSeqContImpl_T"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram    18/02/2003  created
*/

/** 
 * @file 
 * Header file for BACI Read-only Sequence Cont. Class.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "baciROcontImpl_T.h"
#include "baciROSeqCommonImpl_T.h"
#include <baciAlarmSystemMonitorSeqCont_T.h>

namespace baci {

template <ACS_RO_C> 
class ROSeqContImpl : public virtual POA_SK,
		      public ROcontImpl<ACS_RO_TL>
{
  public:
    ROSeqContImpl(const ACE_CString& name, BACIComponent *component_p, DevIO<TM> *devIO=0, bool flagdeldevIO=false);

    ~ROSeqContImpl();

    virtual ACS::Subscription_ptr new_subscription_Alarm (
	TAlarm *cb,
	const ACS::CBDescIn & desc
	);

  protected:
    /** @defgroup  PropTypeROSeqContImplTemplate PropType Class (member from ROSeqContImpl)
     * The  PropType class is a templated typedef so there is no actual inline doc generated for it per-se.
     *  @{
     * PropType is a templated class implementation. The templated parameter is not known
     * from the ROSeqContImpl class (it is actually a templated parameter for ROSeqContImpl) so we
     * really cannot say much more about PropType.
     */
    typedef ROSeqContImpl<ACS_RO_TL> PropType;
    /** @} */

};

 }; 

#endif




