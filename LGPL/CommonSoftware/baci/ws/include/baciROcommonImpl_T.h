#ifndef _baciROcommonImpl_T_H_
#define _baciROcommonImpl_T_H_

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
* "@(#) $Id: baciROcommonImpl_T.h,v 1.24 2006/09/24 18:43:34 bjeram Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram    07/02/2003  created
*/

/** 
 * @file 
 * Header file for BACI Read-only Common Property Template Class.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "baciAlarm_T.h"
#include <baciPcommonImpl_T.h>
#include <baciErrTypeDevIO.h>
#include <baciErrTypeProperty.h>
#include <ACSAlarmSystemInterface.h>

//#define ACS_RO_T(T, TC) ACS_P_TL(T, TC),  POA_ACS::RO##T, ACS::Alarm##T
/**
 * Helper macro for use with template parameters.
 */
#define ACS_RO_T(T, TC) TC, ACS::CB##T, ACS::T##Seq, ACS::T##Seq_out, ACS::Monitor##T, Monitor##T, TC, TC, TC, POA_ACS::RO##T, ACS::Alarm##T, POA_ACS::CB##T, TC

/**
 * Helper macro for use with template parameters.
 */
#define ACS_RO_TL ACS_P_TL, TAlarm, POA_CB, TIN
//T, TCB, TSeq, TSeq_out, TMonitor, TMonitorImpl, TAlarm, POA_SK, TM, TS, TSM

//#define ACS_RO_C class T, class TCB, class TSeq, class TSeq_out, class TMonitor, class TMonitorImpl, class POA_SK, class TM, class TS, class TSM, class TAlarm
/**
 * Helper macro for use with template parameters.
 */
#define ACS_RO_C ACS_P_C, class TAlarm, class POA_CB, class TIN

namespace baci {

/**
 * Implementation of ROcommon property
 * @warning We have virtual inheritance from PortableServer::RefCountServantBase
 * because of a bug in gcc 2.95 (see SPR. ALMASW2001075)
 */
template<ACS_RO_C>
class baci_EXPORT ROcommonImpl : public virtual POA_SK,
				 public PcommonImpl<ACS_P_TL>
{
public:
  /**
   * Constuctor (old one)
   * @param name property name (e.q. AMSMount:decliantion)
   * @param component_p parent of the property
   */
//  ROcommonImpl(const ACE_CString& name, BACIComponent *component_p, DevIO *devIO=0);
   
  /**
   * Constuctor
   * @param name property name (e.q. AMSMount:decliantion)
   * @param component_p parent of the property
   */
  ROcommonImpl(const ACE_CString& name, BACIComponent *component_p, DevIO<TM> *devIO, bool flagdeldevIO);

  /**
   * Constuctor that just calls readCharacterstics
   * @param name property name (e.q. AMSMount:decliantion)
   */
    ROcommonImpl(bool init, const ACE_CString& name, BACIComponent *component_p, DevIO<TM> *devIO, bool flagdeldevIO);

  /**
   * Destructor
   */
  virtual ~ROcommonImpl();

    /* -------------------- [ RO interface ] -------------------- */
  
/*    virtual ACS::Subscription_ptr new_subscription_Alarm (
	TAlarm *cb,
	const ACS::CBDescIn & desc,
	)
	throw (
	    CORBA::SystemException
	    );
*/    
  protected:

  /**
   * Read characteristics from CDB
   * @param propertyName name of the property whose characteristics to read
   * @return true on success, false on failure
   */
  virtual bool readCharacteristics();
  
protected:
 
  /// Event dispatcher;
  EventDispatcher * monitorEventDispatcher_mp;

  // RO
  TimeInterval	alarmTimerTrig_m;
  
  // The alarm system source
  auto_ptr<laserSource::ACSAlarmSystemInterface> alarmSource_map;

  private:
    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const ROcommonImpl&);

    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    ROcommonImpl(const ROcommonImpl&);
    
  public:
    laserSource::ACSAlarmSystemInterface* getAlarmSource() { return alarmSource_map.get(); }
};

// #include "baciROcommonImpl_T.i"

 }; 



#endif








