#ifndef _baciROpattern_H_
#define _baciROpattern_H_

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
 * "@(#) $Id: baciROpattern.h,v 1.111 2009/09/15 08:51:14 bjeram Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * bjeram    2003/02/22  ROpattern is using templates
 * bjeram    2002/11/29  changed to Monitorpattern 
 * bjeram    2002/02/25  added support for DevIO 
 * msekoran  2001/03/09  modified
 */

/** 
 * @file 
 * Header file for BACI Read-only Pattern Property.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciPpatternImpl.h>
#include <baciROdiscImpl_T.h>
#include "baciAlarmSystemMonitorPattern.h"

namespace baci {

    class ROpatternImpl;

/** @defgroup ROpatternTemplate ROpattern Class
 * The ROpattern class is a templated typedef so there is no actual inline doc generated for it per-se.
 *  @{
 * The ROpattern class is an implementation of the ACS::ROpattern IDL interface. 
 * See ROpatternImpl for the real descriptions.
 */
    typedef ROpatternImpl ROpattern;
/** @} */


    class baci_EXPORT ROpatternImpl : public virtual POA_ACS::ROpattern,
				      public ROdiscImpl<ACS_RO_T(pattern, ACS::pattern)>,
				      public PpatternImpl
				  
    {			 
      public:
	ROpatternImpl(const ACE_CString& name, BACIComponent *component_p, DevIO<ACS::pattern> *devIO=0, bool flagdeldevIO=false);

	~ROpatternImpl();

/* -------------------- [ RO interface ] -------------------- */
  
// for the pattern type only the value low_on and high_on for the alarm are implemented (no hysteresis)

	virtual ACS::pattern alarm_mask ();
	
	virtual ACS::pattern alarm_trigger ();

	virtual ACS::Subscription_ptr 
	    new_subscription_Alarm (ACS::Alarmpattern *cb,
				    const ACS::CBDescIn & desc); 


      protected:

	/**
	 * Read characteristics from CDB
	 * @return true on success, false on failure
	 */
	virtual bool readCharacteristics();


      private:
	///
	/// Characteristics
	///
	
	// RO
    /// bit mask: which bits can trigger an alarm
	ACS::pattern alarmMask_m;
	
	/// when a bit can trigger an alarm: if it is 0 or 1
	ACS::pattern alarmTrigger_m;			
    };

}; 

#endif  /* baciROpattern */

