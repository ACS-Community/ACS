#ifndef ENUM_IMPL_H
#define ENUM_IMPL_H
/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: enumpropROImpl.h,v 1.41 2006/07/19 08:21:41 bjeram Exp $"
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

using namespace baci;
using namespace baciErrTypeProperty;

#define HISTORY_SIZE 32


#define ACS_ENUM_T(T) T, T##CB, T##Seq, T##Seq_out, T##Alarm
#define ACS_ENUM_C class T, class TCB, class TSeq, class TSeq_out, class TAlarm, class SK
/**
 * Template implemantation of enum RO property
 */
template <ACS_ENUM_C>
class ROEnumImpl: public virtual PortableServer::RefCountServantBase,
		  public SK, 
		  public CharacteristicModelImpl,
		  public PropertyImplementator,
		  public ActionImplementator 
{

public:


 /**
   * Constuctor
   * @param name property name (e.q. AMSMount:decliantion)
   * @param cob parent of the property
   */
  ROEnumImpl(const ACE_CString& name, BACIComponent *cob, DevIO<T> *devIO=0, bool flagdeldevIO=false);
   
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
  BACIProperty* getProperty() const 
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
  virtual ActionRequest getValueAction(BACIComponent* cob, const int& callbackID,
							const CBDescIn& descIn, BACIValue* value,
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
  virtual ActionRequest invokeAction(int function,
				     BACIComponent* cob, const int& callbackID, 
				     const CBDescIn& descIn, BACIValue* value, 
				     Completion& completion, CBDescOut& descOut);

  /* -------------- [ Property implementator interface ] -------------- */

  /**
   * Get value method (value accessor)
   * @param property property which requested value
   * @param value value to be returned
   * @param completion error handling structure
   * @param descOut callback descriptor
   */
  virtual void getValue(BACIProperty* property,
			BACIValue* value, 
			Completion &completion,
			CBDescOut& descOut);

  /* ----------------------------------------------------------------- */
  /* ---------------------- [ CORBA interface ] ---------------------- */
  /* ----------------------------------------------------------------- */

  /* ------------------ [ CharacteristicModel interface ] ------------------ */

	virtual char * name (
	      
	  )
	  throw (
	    CORBA::SystemException
	  );

    /*
	virtual CORBA::Any * get_characteristic_by_name (
	    const char * name
	    
	  )
	  throw (
	    CORBA::SystemException,
	    ACS::NoSuchCharacteristic
	  );
	
	virtual ACS::stringSeq * find_characteristic (
	    const char * reg_exp
	    
	  )
	  throw (
	    CORBA::SystemException
	  );
    */

  /* --------------------- [ Property interface ] --------------------- */

	virtual char * characteristic_component_name (
	      
	  )
	  throw (
	    CORBA::SystemException
	  );

    /*
	virtual CosPropertyService::PropertySet_ptr get_all_characteristics (
	    
	  )
	  throw (
	    CORBA::SystemException
	  );
    */
  
  /* ----------------- [ TypelessProperty interface ] ----------------- */

	virtual char * description (
	      
	  )
	  throw (
	    CORBA::SystemException
	  );
	
	virtual char * format (
	      
	  )
	  throw (
	    CORBA::SystemException
	  );
	
	virtual char * units (
	      
	  )
	  throw (
	    CORBA::SystemException
	  );
	
	virtual ACS::pattern resolution (
	      
	  )
	  throw (
	    CORBA::SystemException
	  );
	
  /* -------------------- [ P interface ] -------------------- */

    virtual ACS::TimeInterval default_timer_trigger (
	  
	)
	throw (
			    CORBA::SystemException
			    );
	
    virtual ACS::TimeInterval min_timer_trigger (
	  
	)
	throw (
			    CORBA::SystemException
			    );
	
    virtual T default_value (
	  
	)
	throw (
			    CORBA::SystemException
			    );
		
    virtual T get_sync (
	ACSErr::Completion_out c
	
	)
	throw (
			    CORBA::SystemException
			    );
	
    virtual void get_async (
			  CBpattern* cb,
			  const ACS::CBDescIn & desc
			    
			  )
    throw (
		     CORBA::SystemException
		     );
	
    virtual CORBA::Long get_history (
	CORBA::Long n_last_values,
	TSeq_out vs,
	ACS::TimeSeq_out ts
	
	)
	throw (
			    CORBA::SystemException
			    );
	
    virtual ACS::Monitorpattern* create_monitor (
	CBpattern* cb,
	const ACS::CBDescIn & desc
	
	)
	throw (
			    CORBA::SystemException
			    );
	
    virtual ACS::Monitor* create_postponed_monitor (
	ACS::Time start_time,
	CBpattern* cb,
	const ACS::CBDescIn & desc
	
	)
	throw (
			    CORBA::SystemException
			   );
    
    virtual ACS::stringSeq * statesDescription (
	  
	) 
	throw (
			    CORBA::SystemException
			    ); 
    
    virtual ACS::ConditionSeq * condition (
          
	)
	throw (
			    CORBA::SystemException
			    );
    
    virtual TSeq * allStates (
          
	)
	throw (
			    CORBA::SystemException
			    );

    /* -------------------- [ RO interface ] -------------------- */
  
    virtual TSeq* alarm_on (
	  
	)
	throw (
			    CORBA::SystemException
			    );
    
	
    virtual TSeq* alarm_off (
	  
	)
	throw (
			    CORBA::SystemException
			    );
    
    virtual ACS::Subscription_ptr new_subscription_AlarmEnum (
	ACS::Alarmpattern_ptr cb,
	const ACS::CBDescIn & desc
	  
	)
	throw (
			    CORBA::SystemException
			    );
    
    
//  ACSErr::Completion* set_sync (T value);
    
  protected:
    
    /**
     * Read characteristics from CDB
     * @param propertyName name of the property whose characteristics to read
     * @return true on success, false on failure
     */
    virtual bool readCharacteristics();

    /**
     * Checks if the passed state correspond to an alarm on.
     * @param state the value for the enumeration to be checked.
     * @return true if this state is in the alarm_on list.
     */
    bool checkAlarm(T state);

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
    BACIProperty* property_mp;
    
    /// Event dispatcher;
    MonitorenumpropEventDispatcher<T> * monitorEventDispatcher_mp;
    
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
    
    // P
    T	defaultValue_m; 			
    TimeInterval  defaultTimerTrig_m; 		
    TimeInterval  minTimerTrig_m; 			
    
    TimeInterval  m_alarm_timer_trig;
    
    ACS::stringSeq    m_statesDescription;
    ACS::ConditionSeq m_condition;	
    
    // RO
    /** A sequence of values that correspond to an alarm ON  */
    TSeq	m_alarm_on; 			

    /** A sequence of values that correspond to an alarm OFF  */
    TSeq	m_alarm_off;			

    /** True if the alarm is ON */
    bool  alarmRaised_m; 

#ifdef MAKE_VXWORKS 
    AlarmenumpropEventStrategy<T, ROEnumImpl<ACS_ENUM_T(T), SK>, ACS::Alarmpattern> a;
#endif
    
    DevIO<T> *devIO_mp;  
    bool deldevIO_m;
    T	m_value;
    int m_enumLength;
};

#include "enumpropROImpl.i"

#endif /*!ENUM_IMPL_H*/























