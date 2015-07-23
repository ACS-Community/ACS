#ifndef baciROboolean_H_
#define baciROboolean_H_

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
* "@(#) $Id: baciROboolean.h
*
* who       when        what
* --------  ----------  ----------------------------------------------
* pcolomer 2014/11/27   converted to a new class to handle alarm_on characteristic
* bjeram   2003/07/10   created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciMonitor_T.h>
#include <baciROcommonImpl_T.h>
#include <baciPcontImpl_T.h>
#include "baciAlarm_T.h"

namespace baci {

/**
 * @defgroup MonitorbooleanTemplate Monitorboolean Class
 * The Monitorboolean class is a templated typedef so there is no actual inline doc generated for it per-se.
 *  @{
 * The Monitorboolean class is an implementation of the ACS::Monitorboolean IDL interface.
 */
typedef  Monitor<ACS_MONITOR(boolean, CORBA::Boolean)> Monitorboolean;
/** @} */


#define ACS_RO_BOOL_TL \
	CORBA::Boolean/*T*/,             ACS::CBboolean/*TCB*/,           ACS::booleanSeq/*TSeq*/, \
	ACS::booleanSeq_out/*TSeq_out*/, ACS::Monitorboolean/*TMonitor*/, baci::Monitorboolean/*TMonitorImpl*/, \
	CORBA::Boolean/*TM*/,            CORBA::Boolean/*TS*/,            CORBA::Boolean/*TSM*/, \
	POA_ACS::ROboolean/*POA_SK*/,    ACS::Alarmboolean/*TAlarm*/,     POA_ACS::CBboolean/*POA_CB*/, \
	CORBA::Boolean/*TIN*/

#define ACS_RO_BOOL_P \
	CORBA::Boolean/*T*/, 			 ACS::CBboolean/*TCB*/, 		  ACS::booleanSeq/*TSeq*/, \
	ACS::booleanSeq_out/*TSeq_out*/, ACS::Monitorboolean/*TMonitor*/, baci::Monitorboolean/*TMonitorImpl*/, \
	CORBA::Boolean/*TM*/,            CORBA::Boolean/*TS*/,            CORBA::Boolean/*TSM*/, \
	POA_ACS::ROboolean/*POA_SK*/


/**
 * Implementation of ROboolean property
 * @warning We have virtual inheritance from PortableServer::RefCountServantBase
 * because of a bug in gcc 2.95 (see SPR. ALMASW2001075)
 */
class baci_EXPORT ROboolean:  public virtual PortableServer::RefCountServantBase,
			      public ROcommonImpl<ACS_RO_BOOL_TL>, //ACS_RO_TL
				  public PcontImpl<ACS_RO_BOOL_P>
{
public:

  /**
   * Constuctor
   * @param name property name (e.q. AMSMount:decliantion)
   * @param component_p parent of the property
   */
  ROboolean(const ACE_CString& name, BACIComponent *component_p, DevIO<CORBA::Boolean> *devIO=0, bool flagdeldevIO=false);

 /**
   * Constuctor that has to be used from subclasses
   * @param name property name (e.q. AMSMount:decliantion)
   */
  ROboolean(bool init, const ACE_CString& name, BACIComponent *component_p, DevIO<CORBA::Boolean> *devIO=0, bool flagdeldevIO=false );

  /**
   * Destructor
   */
  virtual ~ROboolean();

  /**
   * Get the value of the alarm_on attribute
   */
  virtual CORBA::Boolean alarm_on ();

  /**
   * Subscriber method to be notified when an alarm is raised or removed
   */
  virtual ACS::Subscription_ptr new_subscription_Alarm (
		  ACS::Alarmboolean *cb, const ACS::CBDescIn & desc);

protected:

  /**
   * Read characteristics from CDB
   * @return true on success, false on failure
   */
  virtual bool readCharacteristics();

private:

  /**
   * Value to be checked in order to raise an alarm
   */
  CORBA::Boolean alarmOn_m;
};


 }; 

#endif

// ************************************************************************
