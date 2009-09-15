#ifndef _baciROSeqDiscImpl_H_
#define _baciROSeqDiscImpl_H_
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2004 
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
* "@(#) $Id: baciROSeqDiscImpl_T.h,v 1.8 2009/09/15 08:51:14 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2004-06-17  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

/** 
 * @file 
 * Header file for BACI Read-only Sequence Desc. Class.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "baciROdiscImpl_T.h"
#include "baciROSeqCommonImpl_T.h"
#include <baciAlarmSystemMonitorSeqDisc_T.h>

namespace baci
{
    template <ACS_RO_C>
    class ROSeqDiscImpl : public virtual POA_SK,
			  public ROdiscImpl<ACS_RO_TL> 
    {
      public:
	ROSeqDiscImpl(const ACE_CString& name, BACIComponent *component_p, DevIO<TM> *devIO=0, bool flagdeldevIO=false);
	
	~ROSeqDiscImpl();
	
	virtual ACS::Subscription_ptr new_subscription_Alarm (
	    TAlarm *cb,
	    const ACS::CBDescIn & desc
	    ) ;
	
      protected:
	/** @defgroup  PropTypeROSeqDiscImplTemplate PropType Class (member from ROSeqDiscImpl)
	 * The  PropType class is a templated typedef so there is no actual inline doc generated for it per-se.
	 *  @{
	 * PropType is a templated class implementation. The templated parameter is not known
	 * from the ROSeqDiscImpl class (it is actually a templated parameter for ROSeqDiscImpl) so we
	 * really cannot say much more about PropType.
	 */
	typedef ROSeqDiscImpl<ACS_RO_TL> PropType;
	/** @} */

	/**
	 * monitor which sends information (alarms) to the alarm system
	 */
    };//class ROSeqContImpl

}; //namespace baci

#endif /*!_H*/

