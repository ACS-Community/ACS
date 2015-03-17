#ifndef _baciRObooleanSeq_H_
#define _baciRObooleanSeq_H_

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
* "@(#) $Id: baciRObooleanSeq.h,v 1.2 2012/10/09 14:22:58 bjeram Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* pcolomer  2014/11/27  converted to a new class to handle alarm_on characteristic
* bjeram    2003/02/18  removed everthing
* msekoran  2001/02/10  created
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
 * @defgroup MonitorbooleanSeqTemplate MonitorbooleanSeq Class
 * The MonitorbooleanSeq class is a templated typedef so there is no actual inline doc generated for it per-se.
 *  @{
 * The MonitorbooleanSeq class is an implementation of the ACS::MonitorbooleanSeq IDL interface.
 */
typedef  Monitor<ACS_MONITOR_SEQ(boolean, CORBA::Boolean)> MonitorbooleanSeq;
/** @} */


#define ACS_RO_BOOLSEQ_TL \
	ACS::booleanSeq*/*T*/,              ACS::CBbooleanSeq/*TCB*/,           ACS::booleanSeqSeq/*TSeq*/, \
	ACS::booleanSeqSeq_out/*TSeq_out*/, ACS::Monitorboolean/*TMonitor*/,    baci::MonitorbooleanSeq/*TMonitorImpl*/, \
	ACS::booleanSeq/*TM*/,              CORBA::Boolean/*TS*/,               CORBA::Boolean/*TSM*/, \
	POA_ACS::RObooleanSeq/*POA_SK*/,    ACS::Alarmboolean/*TAlarm*/,        POA_ACS::CBbooleanSeq/*POA_CB*/, \
	const ACS::booleanSeq&/*TIN*/


#define ACS_RO_BOOLSEQ_P ACS::booleanSeq*/*T*/, ACS::CBbooleanSeq/*TCB*/, ACS::booleanSeqSeq/*TSeq*/, \
		ACS::booleanSeqSeq_out/*TSeq_out*/, ACS::Monitorboolean/*TMonitor*/, baci::MonitorbooleanSeq/*TMonitorImpl*/, \
		ACS::booleanSeq/*TM*/, CORBA::Boolean/*TS*/, CORBA::Boolean/*TSM*/, POA_ACS::RObooleanSeq/*POA_SK*/


class RObooleanSeq : public virtual PortableServer::RefCountServantBase,
		public ROcommonImpl<ACS_RO_BOOLSEQ_TL/*ACS_RO_TL*/>,
		public PcontImpl<ACS_RO_BOOLSEQ_P>
{
  public:
	/**
	 * Constructor
	 */
    RObooleanSeq(const ACE_CString& name, BACIComponent *component_p, DevIO<ACS::booleanSeq> *devIO=0, bool flagdeldevIO=false);

    /**
      * Constuctor that has to be used from subclasses
      * @param name property name (e.q. AMSMount:decliantion)
      */
    RObooleanSeq(bool init, const ACE_CString& name, BACIComponent *component_p, DevIO<ACS::booleanSeq> *devIO=0, bool flagdeldevIO=false );

    /**
     * Destructor
     */
    virtual ~RObooleanSeq();

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
     * Read characteristics from the CDB
     * @return true on success, false on failure
     */
    virtual bool readCharacteristics();

  private:

    /**
     * Value to be checked in order to raise an alarm
     */
    CORBA::Boolean	alarmOn_m;
};



}

#endif  /* baciRObooleanSeq */

