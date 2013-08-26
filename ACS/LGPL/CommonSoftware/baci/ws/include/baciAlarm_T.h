#ifndef _baci_alarmT_H_
#define _baci_alarmT_H_
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
* "@(#) $Id: baciAlarm_T.h,v 1.18 2011/09/02 11:39:00 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2003-06-11  created
*/

/** 
 * @file 
 * Header file BACI Alarms.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciEvent.h>
#include <ACSErrTypeAlarm.h>

namespace baci {

// forward
template<class T, class TCB, class POA_CB>
class EventCB;

/********************************************************************************************/

template<class T, class TCB, class POA_CB>
class  baci_EXPORT MonitorEventDispatcher :
    public EventDispatcher, public MonitorImplementator
{
  public:
    MonitorEventDispatcher(const CBDescIn& descIn,
			   const ACS::TimeInterval& interval,
			   BACIProperty * property);
    
    virtual ~MonitorEventDispatcher();
    
    virtual int subscribe(EventStrategy * event);
    
    virtual int unsubscribe(EventStrategy * event);
    
    virtual void dispatch(T value,
			  const ACSErr::Completion & c,
			  const ACS::CBDescOut & desc);
    
    virtual void suspend();
    
    virtual void resume();
    
    virtual void monitorDestroyed(void);
    
    virtual void monitorStateChanged(void);
    
  private:
    
    int callbackID_m;
    
    BACIMonitor * monitor_mp;
    
    TCB *monitorCallback_mp;
    
    EventCB<T, TCB, POA_CB>* callbackServant_mp;  
    
    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const MonitorEventDispatcher&);
    
    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    MonitorEventDispatcher(const MonitorEventDispatcher&);
};

/***********************************************************************************************/

template<class T, class TCB, class POA_CB>
class baci_EXPORT EventCB :  public virtual PortableServer::RefCountServantBase, public POA_CB
{
  public:
    
    EventCB(MonitorEventDispatcher<T, TCB, POA_CB>* dispatcher);
    
    virtual ~EventCB();
    
    virtual void disposeDispatcher();
    
    virtual void working (T value,
			  const ACSErr::Completion & c,
			  const ACS::CBDescOut & desc);
    
    virtual void done (T value,
		       const ACSErr::Completion & c,
		       const ACS::CBDescOut & desc);
    
    virtual CORBA::Boolean negotiate (ACS::TimeInterval time_to_transmit,
				      const ACS::CBDescOut & desc);
    
  private:
    
    MonitorEventDispatcher<T, TCB, POA_CB>* dispatcher_mp;
    
    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const EventCB&);

    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    EventCB(const EventCB&);
};

/************************************************************************************************/
/* Common class for AlarmEventStrategies */

template<class T, class TPROP, class TALARM>
class baci_EXPORT AlarmEventStrategy : public EventStrategy
{
  public:
    
    static const int maxFailureCount;
    
    AlarmEventStrategy(TPROP * property, EventDispatcher * eventDispatcher);
    
    AlarmEventStrategy(Callback_ptr callback_p,
		       const CBDescIn& descIn,
		       const ACS::TimeInterval& interval,
		       TPROP * property,
		       EventDispatcher * eventDispatcher);
    
    virtual ~AlarmEventStrategy();
    
    bool failed();
    void succeeded();
    
    CORBA::Object_ptr getCORBAReference() const 
	{ 
	    return reference_mp;
	}
    
    virtual bool isSuspended()
	{
	    return suspended_m;
	}
    
    virtual int getId(void);
    
    virtual const char* getName(void);
    
    virtual const char* getObjectState(void);
    
    virtual void setObjectState(const char * state);
    
    virtual void suspend ();
    
    virtual void resume ();
    
    virtual void destroy (); 
    
  private:
    
    ACE_CString name_m;
    
    bool suspended_m;
    
    int failureCount_m;
    
    CBDescIn desc_mIn;
    
    ACS::TimeInterval interval_m;
    
    EventDispatcher * eventDispatcher_mp;
    
    CORBA::Object_ptr reference_mp;
    
    
    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const AlarmEventStrategy&);
    
    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    AlarmEventStrategy(const AlarmEventStrategy&);
    
    
  protected:
    
    TPROP *property_mp;
    
    TALARM *callback_mp;
    
    int alarmRaised_m;
};

/************************************************************************************************/
/* AlarmEventStrategy implementation for discreet types */

template<class T, class TPROP, class TALARM>
class baci_EXPORT AlarmEventStrategyDisc : public AlarmEventStrategy<T, TPROP, TALARM>
{
  public:
    AlarmEventStrategyDisc(TPROP * property, EventDispatcher * eventDispatcher) :
	AlarmEventStrategy<T, TPROP, TALARM>(property, eventDispatcher)
	{};
    
    AlarmEventStrategyDisc(Callback_ptr callback_p,
			   const CBDescIn& descIn,
			   const ACS::TimeInterval& interval,
			   TPROP * property,
			   EventDispatcher * eventDispatcher) :
	AlarmEventStrategy<T, TPROP, TALARM>(callback_p, descIn, interval, property, eventDispatcher)
	{};
    
    virtual void check(BACIValue &value,
		       const ACSErr::Completion & c,
		       const ACS::CBDescOut & desc);
    
  private:
    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const AlarmEventStrategyDisc&);

    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    AlarmEventStrategyDisc(const AlarmEventStrategyDisc&);
};

/***********************************************************************************************/
/* AlarmEventStrategy implementation for continues types */

template<class T, class TPROP, class TALARM>
class baci_EXPORT AlarmEventStrategyCont : public AlarmEventStrategy<T, TPROP, TALARM>
{
  public:
    AlarmEventStrategyCont(TPROP * property, EventDispatcher * eventDispatcher) :
	AlarmEventStrategy<T, TPROP, TALARM>(property, eventDispatcher)
	{};
    
    AlarmEventStrategyCont(Callback_ptr callback_p,
			   const CBDescIn& descIn,
			   const ACS::TimeInterval& interval,
			   TPROP * property,
			   EventDispatcher * eventDispatcher) :
	AlarmEventStrategy<T, TPROP, TALARM>(callback_p, descIn, interval, property, eventDispatcher)
	{};
    
    virtual void check(BACIValue &value,
		       const ACSErr::Completion & c,
		       const ACS::CBDescOut & desc);
    
  private:
    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const AlarmEventStrategyCont&);
    
    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    AlarmEventStrategyCont(const AlarmEventStrategyCont&);
};


/***********************************************************************************************/
/* AlarmEventStrategy implementation for continues sequence types */

template<class T, class TPROP, class TALARM>
class baci_EXPORT AlarmEventStrategyContSeq : public AlarmEventStrategy<T, TPROP, TALARM>
{
  public:
    AlarmEventStrategyContSeq(TPROP * property, EventDispatcher * eventDispatcher) :
	AlarmEventStrategy<T, TPROP, TALARM>(property, eventDispatcher)
	{};
    
    AlarmEventStrategyContSeq(Callback_ptr callback_p,
			      const CBDescIn& descIn,
			      const ACS::TimeInterval& interval,
			      TPROP * property,
			      EventDispatcher * eventDispatcher) :
	AlarmEventStrategy<T, TPROP, TALARM>(callback_p, descIn, interval, property, eventDispatcher)
	{};
    
    virtual void check(BACIValue &value,
		       const ACSErr::Completion & c,
		       const ACS::CBDescOut & desc);
  protected:
    
    int * alarmsRaised_mp;
    int alarmsRaisedLength_m;
    
  private:
    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const AlarmEventStrategyContSeq&);
    
    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    AlarmEventStrategyContSeq(const AlarmEventStrategyContSeq&);
};

/***********************************************************************************************/
/* AlarmEventStrategy implementation for discrete sequence types */

template<class T, class TPROP, class TALARM>
class baci_EXPORT AlarmEventStrategyDiscSeq : public AlarmEventStrategy<T, TPROP, TALARM>
{
  public:
    AlarmEventStrategyDiscSeq(TPROP * property, EventDispatcher * eventDispatcher) :
	AlarmEventStrategy<T, TPROP, TALARM>(property, eventDispatcher)
	{};
    
    AlarmEventStrategyDiscSeq(Callback_ptr callback_p,
			      const CBDescIn& descIn,
			      const ACS::TimeInterval& interval,
			      TPROP * property,
			      EventDispatcher * eventDispatcher) :
	AlarmEventStrategy<T, TPROP, TALARM>(callback_p, descIn, interval, property, eventDispatcher)
	{};
    
    virtual void check(BACIValue &value,
		       const ACSErr::Completion & c,
		       const ACS::CBDescOut & desc);

  protected:
    
    int * alarmsRaised_mp;
    int alarmsRaisedLength_m;
    
  private:
    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const AlarmEventStrategyDiscSeq&);
    
    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    AlarmEventStrategyDiscSeq(const AlarmEventStrategyDiscSeq&);
};

}; //namespace baci

#endif /*!_H*/






















