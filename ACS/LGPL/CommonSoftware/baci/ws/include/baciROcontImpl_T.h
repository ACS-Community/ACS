#ifndef _baciROcontImpl_T_H_
#define _baciROcontImpl_T_H_

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
* "@(#) $Id: baciROcontImpl_T
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram    10/02/2003  created
*/

/** 
 * @file 
 * Header file for BACI Read-only Cont. Property Template Class.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciPcontImpl_T.h>
#include <baciROcommonImpl_T.h>
#include "baciAlarmSystemMonitorCont_T.h"

namespace baci {

/**
 * Implementation of ROcont property
 * @warning We have virtual inheritance from PortableServer::RefCountServantBase
 * because of a bug in gcc 2.95 (see SPR. ALMASW2001075)
 */
template<ACS_RO_C>
class baci_EXPORT ROcontImpl: public virtual POA_SK, 
			      public ROcommonImpl<ACS_RO_TL>,
			      public PcontImpl<ACS_P_TL>			     		 
{
public:
   
  /**
   * Constuctor
   * @param name property name (e.q. AMSMount:decliantion)
   * @param component_p parent of the property
   */
  ROcontImpl(const ACE_CString& name, BACIComponent *component_p, DevIO<TM> *devIO=0, bool flagdeldevIO=false);

 /**
   * Constuctor that has to be used from subclasses
   * @param name property name (e.q. AMSMount:decliantion)
   */
  ROcontImpl(bool init, const ACE_CString& name, BACIComponent *component_p, DevIO<TM> *devIO=0, bool flagdeldevIO=false );

  /**
   * Destructor
   */
  virtual ~ROcontImpl();

  /* ----------------------------------------------------------------- */
  /* ---------------------- [ CORBA interface ] ---------------------- */
  /* ----------------------------------------------------------------- */
	
  /* -------------------- [ RO interface ] -------------------- */
  
	virtual TS alarm_low_on ();
	
	virtual TS alarm_low_off ();
	
	virtual TS alarm_high_on ();
	
	virtual TS alarm_high_off ();
  
    virtual ACS::Subscription_ptr 
    new_subscription_Alarm (TAlarm *cb,
			    const ACS::CBDescIn & desc);

protected:

    /** @defgroup  PropTypeROcontImplTemplate PropType Class (member from ROcontImpl)
     * The  PropType class is a templated typedef so there is no actual inline doc generated for it per-se.
     *  @{
     * PropType is a templated class implementation. The templated parameter is not known
     * from the ROcontImpl class (it is actually a templated parameter for ROcontImpl) so we
     * really cannot say much more about PropType.
     */
    typedef ROcontImpl<ACS_RO_TL> PropType;
    /** @} */

    /**
     * monitor which sends information (alarms) to the alarm system
     */

  /**
   * Read characteristics from CDB
   * @param propertyName name of the property whose characteristics to read
   * @return true on success, false on failure
   */
  virtual bool readCharacteristics();
  
private:
  ///
  /// Characteristics
  ///
	
  // RO
  TSM	alarmLowOn_m; 			
  TSM	alarmLowOff_m;			
  TSM	alarmHighOn_m;			
  TSM	alarmHighOff_m;			
};


}; 

#endif













