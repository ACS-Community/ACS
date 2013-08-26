#ifndef baciMonitor_T_H
#define baciMonitor_T_H

/*******************************************************************************
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
* "@(#) $Id: baciMonitor_T.h,v 1.107 2012/01/04 12:51:41 rtobar Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* oat 2003-01-21 added templates for Monitors
*/

/** 
 * @file 
 * Header file for BACI Monitor Templates.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsutil.h>
#include <ace/SString.h>
#include <baci.h>
#include <baciS.h>
#include <logging.h>
#include <baciRecovery.h>
#include <baciRecoverableObject.h>
#include <Basic_Types.h> // for ACE_UINT64_FORMAT_SPECIFIER_ASCII


namespace baci {

#ifdef MAKE_VXWORKS
unsigned long long convString2LLU(char *);
char *printLLUasString(unsigned long long); 
#endif

#ifdef MAKE_VXWORKS
#define HEADER_PRINT_GET_OBJECT_STATE \
      ACE_OS::sprintf(buffer_p, "%s %s %lu %s %s %s %s %d %u %u", \
                      getName(), ior.in(), \
                      printLLUasString(tag), \
                      printLLUasString(bcb_p->getDescIn().normal_timeout), \
                      printLLUasString(monitor_mp->getTransmitTime()), \
                      monitor_mp->getTriggerTime(), \
                      valueTrigger.c_str(), \
                      monitor_mp->getUpdateMode(), monitor_mp->getTriggerOnValue(), \
                      monitor_mp->isSuspended());
#else
/**
 * Helper macro used to print the current state of a monitor to "buffer".
 */
#define HEADER_PRINT_GET_OBJECT_STATE \
      ACE_OS::sprintf(buffer_p, "%s %s %lu %llu %llu %llu %s %d %u %u", \
                      getName(), ior.in(), tag, bcb_p->getDescIn().normal_timeout, \
                      monitor_mp->getTransmitTime(), monitor_mp->getTriggerTime(), \
                      valueTrigger.c_str(), \
                      monitor_mp->getUpdateMode(), monitor_mp->getTriggerOnValue(), \
                      monitor_mp->isSuspended());
#endif
 
#ifdef MAKE_VXWORKS
#define IMPL_PRINT_GET_OBJECT_STATE \
      ACE_OS::sprintf(buffer_p, "%s %s %lu %s %s %s %d %u", \
                      getName(), ior.in(), \
                      printLLUasString(tag), \
                      printLLUasString(bcb_p->getDescIn().normal_timeout), \
                      monitor_mp->getTransmitTime(), \
                      printLLUasString(monitor_mp->getTriggerTime()), \
                      monitor_mp->getUpdateMode(), \
                      monitor_mp->isSuspended());
#else
/**
 * Helper macro used to print the current state of a monitor to "buffer".
 */
#define IMPL_PRINT_GET_OBJECT_STATE \
      ACE_OS::sprintf(buffer_p, "%s %s %lu %llu %llu %llu %d %u", \
                      getName(), ior.in(), tag, bcb_p->getDescIn().normal_timeout, \
                      monitor_mp->getTransmitTime(), \
                      monitor_mp->getTriggerTime(), \
                      monitor_mp->getUpdateMode(), \
                      monitor_mp->isSuspended());
#endif

#ifdef MAKE_VXWORKS
#define HEADER_SCAN_SET_OBJECT_STATE \
  char *tmpPtr1, *tmpPtr2, *tmpPtr3; \
  sscanf(state, "%s %s %lu %s %s %s %s %d %u %u", \
         cname, ior, &tag, \
         tmpPtr1, \
         tmpPtr2, \
         tmpPtr3, \
         valueTrigger, &mode, \
         &triggerOnValue, &isSuspended); \
  descIn.normal_timeout =  convString2LLU(tmpPtr1); \
  transmitTime = convString2LLU(tmpPtr2); \
  timeTrigger  = convString2LLU(tmpPtr3);
#else
/**
 * Helper macro used to extract info from "state" to various variables.
 */
#define HEADER_SCAN_SET_OBJECT_STATE \
  sscanf(state, "%s %s %lu "ACE_UINT64_FORMAT_SPECIFIER_ASCII" "ACE_UINT64_FORMAT_SPECIFIER_ASCII" "ACE_UINT64_FORMAT_SPECIFIER_ASCII" %s %d %u %u", \
         cname, ior, &tag, &descIn.normal_timeout, \
         &transmitTime, &timeTrigger, valueTrigger, &mode, \
         &triggerOnValue, &isSuspended);
#endif
 
#ifdef MAKE_VXWORKS
#define IMPL_SCAN_SET_OBJECT_STATE \
  char *tmpPtr1, *tmpPtr2, *tmpPtr3; \
  sscanf(state, "%s %s %lu %s %s %s %d %u", \
         cname, ior, &tag, \
         tmpPtr1, \
         tmpPtr2, \
         tmpPtr3, \
         &mode, \
         &isSuspended); \
  descIn.normal_timeout =  convString2LLU(tmpPtr1); \
  transmitTime = convString2LLU(tmpPtr2); \
  timeTrigger  = convString2LLU(tmpPtr3);
#else
/**
 * Helper macro used to extract info from "state" to various variables.
 */
#define IMPL_SCAN_SET_OBJECT_STATE \
  sscanf(state, "%s %s %lu %llu %llu %llu %d %u", \
         cname, ior, &tag, &descIn.normal_timeout, \
         &transmitTime, &timeTrigger, &mode, \
         &isSuspended);
#endif

/**
 * Maximum length of strigified value
 */
#define MAX_VALUE_LENGTH 500

/**
 * Helper macro used to define the templates passed to the Monitor class.
 */
#define ACS_MONITOR_C class TCORBA, class TCORBA_out, class TCB, class TPOA, baci::BACIValue::Type TBACIValuetype
/**
 * Helper macro  used to define the templates passed to the Monitor class.
 */
#define ACS_MONITOR_T TCORBA, TCORBA_out, TCB, TPOA, TBACIValuetype

/**
 * Helper macro  used to define the templates passed to the Monitor class for sequence properties.
 */
#define ACS_MONITOR_SEQ(T,TCORBA)  TCORBA, TCORBA##_out, ACS::CB##T##Seq, POA_ACS::Monitor##T, baci::BACIValue::type_##T##Seq
/**
 * Helper macro  used to define the templates passed to the Monitor class for non-sequence properties.
 */
#define ACS_MONITOR(T,TCORBA)  TCORBA, TCORBA##_out, ACS::CB##T, POA_ACS::Monitor##T, baci::BACIValue::type_##T

template <ACS_MONITOR_C>
class baci_EXPORT Monitor: public virtual PortableServer::RefCountServantBase,
                       public TPOA,
                       public RecoverableObject,
		       public MonitorImplementator
{
public:

  Monitor(ACE_CString name,
	  const ACS::TimeInterval& minTriggerTime,
	  const baci::BACIValue& minTriggerValue,
	  BACIProperty* property);

  Monitor(ACE_CString name,
	  Callback_ptr callback_p, const CBDescIn& inDesc,
	  const ACS::TimeInterval& triggerTime,
	  const baci::BACIValue& triggerValue,
	  const ACS::TimeInterval& minTriggerTime,
	  const baci::BACIValue& minTriggerValue,
	  BACIProperty* property,
	  const ACS::TimeInterval& transmitTime = 0,
	  const BACIMonitor::UpdateMode& updateMode=BACIMonitor::mumLast);

  virtual ~Monitor();

  virtual int initialization()
  {
	return initialization_m;
  }

  CORBA::Object_ptr getCORBAReference() const
  {
    return reference_mp;
  }

  BACIMonitor* getMonitor() const
  {
    return monitor_mp;
  }

  virtual int getId(void);
  virtual const char* getName(void);
  virtual const char* getObjectState(void);
  virtual void setObjectState(const char * state);

  virtual void setObjectState(const char * state,
		      const ACS::TimeInterval& minTriggerTime,
		      const baci::BACIValue& minTriggerValue,
		      BACIProperty * property);


  virtual void monitorDestroyed(void);

  virtual void monitorStateChanged(void);


  virtual void suspend ();

  virtual void resume ();

  virtual void destroy ();

  virtual void set_timer_trigger (ACS::TimeInterval timer);

  virtual void get_timer_trigger (ACS::TimeInterval_out timer);

  virtual void set_value_trigger (TCORBA delta, CORBA::Boolean enable);
  
  virtual void get_value_trigger (TCORBA_out delta, CORBA::Boolean_out enable);

  virtual void set_value_percent_trigger (CORBA::Double delta, CORBA::Boolean enable);
  
  virtual void get_value_percent_trigger (CORBA::Double_out delta, CORBA::Boolean_out enable);

  virtual ACS::Time start_time ();

private:
 
  int initialization_m;

  BACIMonitor* monitor_mp;

  CORBA::Object_ptr reference_mp;

    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const Monitor&);
    
    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    Monitor(const Monitor&);

};

/**
 * Helper macro  used to define the templates passed to the Monitor class for properties.
 */
#define ACS_MONITOR_BASIC_C class TCORBA, class TCORBA_out, class TCB, class TPOA, baci::BACIValue::Type TBACIValuetype
/**
 * Helper macro  used to define the templates passed to the Monitor class for properties.
 */
#define ACS_MONITOR_BASIC_T TCORBA, TCORBA_out, TCB, TPOA, TBACIValuetype

template<ACS_MONITOR_BASIC_C>
class baci_EXPORT MonitorBasic: public virtual PortableServer::RefCountServantBase,
		       public POA_ACS::Monitor,
		       public RecoverableObject,
		       public MonitorImplementator
{

public:

  MonitorBasic(ACE_CString name,
	  const ACS::TimeInterval& minTriggerTime,
	  const baci::BACIValue& minTriggerValue,
	  BACIProperty* property);

  MonitorBasic(ACE_CString name,
	  Callback_ptr callback_p, const CBDescIn& inDesc,
	  const ACS::TimeInterval& triggerTime,
	  const baci::BACIValue& triggerValue,
	  const ACS::TimeInterval& minTriggerTime,
	  const baci::BACIValue& minTriggerValue,
	  BACIProperty* property,
	  const ACS::TimeInterval& transmitTime = 0,
	  const BACIMonitor::UpdateMode& updateMode=BACIMonitor::mumLast);

  virtual ~MonitorBasic();

  virtual int initialization()
  {
	return initialization_m;
  }

  CORBA::Object_ptr getCORBAReference() const
  {
    return reference_mp;
  }

  BACIMonitor* getMonitor() const
  {
    return monitor_mp;
  }


  virtual int getId(void);
  virtual const char* getName(void);
  virtual const char* getObjectState(void);
  virtual void setObjectState(const char * state);

  virtual void setObjectState(const char * state,
		      const ACS::TimeInterval& minTriggerTime,
		      BACIProperty * property);


  virtual void monitorDestroyed(void);

  virtual void monitorStateChanged(void);


  virtual void suspend ();

  virtual void resume ();

  virtual void destroy ();

  virtual void set_timer_trigger (ACS::TimeInterval timer);

  virtual void get_timer_trigger (ACS::TimeInterval_out timer);

  virtual ACS::Time start_time ();

private:

  int initialization_m;

  BACIMonitor* monitor_mp;

  CORBA::Object_ptr reference_mp;

    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const MonitorBasic&);
    
    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    MonitorBasic(const MonitorBasic&);

};

 }; 

#endif   /* baciMonitorTempl_H */






