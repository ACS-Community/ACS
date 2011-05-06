#ifndef ENUM_IMPL_H
#define ENUM_IMPL_H
/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: enumpropROImpl.h,v 1.51 2011/05/06 21:04:47 acaproni Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram 2004-01-12 changed m_value type from pattern to T
* bjeram 2002-11-18 change to create_monitor to return Monitorpattern (onchange)
* bjeram 2001-11-28 changed to virtual inheritence of PortableServer::RefCountServantBase
* bjeram 2001-10-24 added support for namespaces in enumpropMACRO.idl
* bjeram  19/04/01  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciDB.h>
#include <logging.h>
#include <baciROpattern.h>
#include <baciRecovery.h>
#include <enumpropAlarm.h>
#include <baciErrTypeProperty.h>
#include <baciCharacteristicModelImpl.h>
#include "enumpropAlarmSystemMonitorEnumProp.h"

#define HISTORY_SIZE 32


#define ACS_ENUM_T(T) T, T##CB, T##Seq, T##Seq_out, T##Alarm
#define ACS_ENUM_C class T, class TCB, class TSeq, class TSeq_out, class TAlarm, class SK
/**
 * Template implemantation of enum RO property
 */
template <ACS_ENUM_C>
class ROEnumImpl: public virtual PortableServer::RefCountServantBase,
		  public SK, 
		  public baci::CharacteristicModelImpl,
		  public baci::PropertyImplementator,
		  public baci::ActionImplementator 
{

public:


 /**
   * Constuctor
   * @param name property name (e.q. AMSMount:decliantion)
   * @param cob parent of the property
   */
  ROEnumImpl(const ACE_CString& name, baci::BACIComponent *cob, DevIO<T> *devIO=0, bool flagdeldevIO=false);
   
  /**
   * Destructor
   */
  virtual ~ROEnumImpl();

  /**
   * Property construction status method
   * After contruction of a property, this method will be called
   * to check if construction was successful; if not, monitor will be destroyed by parent
   * @return 0 on success or any other value (value can indicate cause of faulire) on faulure
   */
  virtual int initialization() { return initialization_m; }

  /**
   * Get CORBA referece of this object
   * @return reference to CORBa object
   */
  CORBA::Object_ptr getCORBAReference() const 
  { 
    return reference_mp;
  }

  /**
   * BACI Monitor instance accessor
   */
  baci::BACIProperty* getProperty() const 
  { 
    return property_mp;
  }

   /**
    * DevIO accessor
    */
    DevIO<T>* getDevIO() const
	{
	    return devIO_mp;
	}
  /**
   * Destructor method
   * This method must be called to destroy property,
   * POA will call destructor 
   * (direct call of destructor will most likely cause code to crash)
   */
   virtual void destroy();

  /***
   * Implementation of async. get_value method
   */
  virtual baci::ActionRequest getValueAction(baci::BACIComponent* cob, const int& callbackID,
							const CBDescIn& descIn, baci::BACIValue* value,
							Completion& completion, CBDescOut& descOut);

  /* --------------- [ Action implementator interface ] -------------- */

  /**
   * Action dispatcher function
   * @param function action funtion to be invoked
   * @param cob owner of the action
   * @param callbackID id of the callback to be notified
   * @param descIn callback descriptor (passed by client)
   * @param value action data (e.g. value to be set)
   * @param completion error handing structure
   * @param descOut callback descriptor which will be passed to client
   * @return request to be performed by BACI
   * <ul>
   *  <li><b><i>reqNone</b></i> - do nothing (action will be kept in queue)
   *  <li><b><i>reqInvokeWorking</b></i> - invoke <type>Callback::<i>working</i>
   *  <li><b><i>reqInvokeDone</b></i> - invoke <type>Callback::<i>done</i> and destroy callback
   *  <li><b><i>reqDestroy</b></i> - destroy callback (callback should has been called already by function)
   * </ul>
   */
  virtual baci::ActionRequest invokeAction(int function,
				     baci::BACIComponent* cob, const int& callbackID, 
				     const CBDescIn& descIn, baci::BACIValue* value, 
				     Completion& completion, CBDescOut& descOut);

  /* -------------- [ Property implementator interface ] -------------- */

  /**
   * Get value method (value accessor)
   * @param property property which requested value
   * @param value value to be returned
   * @param completion error handling structure
   * @param descOut callback descriptor
   */
  virtual void getValue(baci::BACIProperty* property,
			baci::BACIValue* value, 
			Completion &completion,
			CBDescOut& descOut);

  /* ----------------------------------------------------------------- */
  /* ---------------------- [ CORBA interface ] ---------------------- */
  /* ----------------------------------------------------------------- */

  /* ------------------ [ CharacteristicModel interface ] ------------------ */

	virtual char * name ();

    /*
	virtual CORBA::Any * get_characteristic_by_name (
	    const char * name
	    
	  );
	
	virtual ACS::stringSeq * find_characteristic (
	    const char * reg_exp
	    
	  );
    */

  /* --------------------- [ Property interface ] --------------------- */

	virtual char * characteristic_component_name ();

    /*
	virtual CosPropertyService::PropertySet_ptr get_all_characteristics (
	    
	  );
    */
  
  /* ----------------- [ TypelessProperty interface ] ----------------- */

	virtual char * description ();
	
	virtual char * format ();
	
	virtual char * units ();
	
	virtual ACS::pattern resolution ();
	
	virtual CORBA::Boolean initialize_devio (); 
  /* -------------------- [ P interface ] -------------------- */

    virtual ACS::TimeInterval default_timer_trigger ();
	
    virtual ACS::TimeInterval min_timer_trigger ();
	
    virtual T default_value ();
		
    virtual T get_sync (ACSErr::Completion_out c);
	
    virtual void get_async (
			  CBpattern* cb,
			  const ACS::CBDescIn & desc
		     );
	
    virtual CORBA::Long get_history (
	CORBA::Long n_last_values,
	TSeq_out vs,
	ACS::TimeSeq_out ts
    );
	
    virtual ACS::Monitorpattern* create_monitor (
	CBpattern* cb,
	const ACS::CBDescIn & desc
    );
	
    virtual ACS::Monitor* create_postponed_monitor (
	ACS::Time start_time,
	CBpattern* cb,
	const ACS::CBDescIn & desc
	);
    
    virtual ACS::stringSeq * statesDescription (
	); 
    
    virtual ACS::ConditionSeq * condition (
	);
    
    virtual TSeq * allStates ();

    /* -------------------- [ RO interface ] -------------------- */
  
    virtual TSeq* alarm_on ();
	
    virtual TSeq* alarm_off ();
    
    virtual ACS::Subscription_ptr new_subscription_AlarmEnum (
	ACS::Alarmpattern_ptr cb,
	const ACS::CBDescIn & desc
    ); 
    /**
     * Checks if the passed state correspond to an alarm on.
     * @param state the value for the enumeration to be checked.
     * @return true if this state is in the alarm_on list.
     */
    bool checkAlarm(T state);

    /**
     * Sets alarm fault family
     * @param fault family
     * @deprecated
     */
    void setAlarmFaultFamily(const char* ff);

    /**
     * Sets alarm fault member
     * @param fault member
     * @deprecated
     */
    void setAlarmFaultMember(const char* fm);

    /**
     * Accessor method for alarm fault family
     */
    const char* getAlarmFaultFamily() { return alarmFaultFamily_m.c_str(); }

    /**
     * Accessor method for alarm fault member
     */
    const char* getAlarmFaultMember() { return alarmFaultMember_m.c_str(); }

    /**
     * Accessor method for alarm level
     */
    int getAlarmLevel() { return this->alarmLevel_m;}


protected:
    
    /**
     * Read characteristics from CDB
     * @param propertyName name of the property whose characteristics to read
     * @return true on success, false on failure
     */
    virtual bool readCharacteristics();

    /* --------------------- [ History support ] ---------------------- */
    
    void addValueToHistory(ACS::Time time, ACS::pattern value);
    
  private:
    
    /**
     * Current value for the enumeration property
     */
    T state;

    /**
     * Definition of ActionFunction (member function of RO)
     */
    /*  typedef ActionRequest (ROdouble::*ActionFunction)(BACIComponent* cob, const int& callbackID,
	const CBDescIn& descIn, BACIValue* value,
	Completion& completion, CBDescOut& descOut);
    */
    /// Initialization status
    int initialization_m;
    
    /// Destroy status
    bool destroyed_m;
    
    /// CORBA reference
    CORBA::Object_ptr reference_mp;
    
    /// BACI property
    baci::BACIProperty* property_mp;
    
    /// Event dispatcher;
    baci::MonitorenumpropEventDispatcher *monitorEventDispatcher_mp;
    
    /// alarm system monitor
    baci::AlarmSystemMonitorEnumProp<T, ROEnumImpl<ACS_ENUM_T(T), SK> > *alarmSystemMonitorEnumProp_mp;

    /// history value buffer
    T historyValue_m[HISTORY_SIZE];
    
    /// history time buffer
    ACS::Time historyTime_m[HISTORY_SIZE];
    
    /// history start pointer
    int historyStart_m;
    
    /// history end pointer
    bool historyTurnaround_m;
    
    ///
    /// Characteristics
    ///
    
    // Typeless property
    ACE_CString  m_description; 			
    ACE_CString  format_m; 				
    ACE_CString  units_m; 				
    ACS::pattern m_resolution; 			
    bool initializeDevIO_m;   
 
    // P
    T	defaultValue_m; 			
    ACS::TimeInterval  defaultTimerTrig_m; 		
    ACS::TimeInterval  minTimerTrig_m; 			
    
    ACS::TimeInterval  m_alarm_timer_trig;
    
    ACS::stringSeq    m_statesDescription;
    ACS::ConditionSeq m_condition;	
    
    // RO
    /** A sequence of values that correspond to an alarm ON  */
    TSeq	m_alarm_on; 			

    /** A sequence of values that correspond to an alarm OFF  */
    TSeq	m_alarm_off;			

    /** True if the alarm is ON */
    bool  alarmRaised_m; 

    ACE_CString alarmFaultFamily_m;
    ACE_CString alarmFaultMember_m;
    int alarmLevel_m;

#ifdef MAKE_VXWORKS 
    baci::AlarmenumpropEventStrategy<T, ROEnumImpl<ACS_ENUM_T(T), SK>, ACS::Alarmpattern> a;
#endif
    
    DevIO<T> *devIO_mp;  
    bool deldevIO_m;
    T	m_value;
    int m_enumLength;
};

#include "enumpropROImpl.i"

#endif /*!ENUM_IMPL_H*/























