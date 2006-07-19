#ifndef RWENUM_IMPL_H
#define RWENUM_IMPL_H
/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: enumpropRWImpl.h,v 1.40 2006/07/19 08:21:41 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram 2004-01-12 changed m_value type from pattern to T
* bjeram 2003-03-14 changed defaultValue_m from ACS::pattern type to type T
* bjeram 2002-11-18 create_monitr returns Monitorpattern (onchange)
* bjeram 2001-11-28 RWEnumImpl derives virtaully out of  PortableServer::RefCountServantBase
* bjeram  05/11/01  created
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
 * Template implemantation of enum RW property
 */
template <ACS_ENUM_C>
class RWEnumImpl: public virtual PortableServer::RefCountServantBase,
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
  RWEnumImpl(const ACE_CString& name, BACIComponent *cob, DevIO<T> *devIO=0, bool flagdeldevIO=false);
   
  /**
   * Destructor
   */
  virtual ~RWEnumImpl();

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

  /***
   * Implementation of async. set_value method
   */
  virtual ActionRequest setValueAction(BACIComponent* cob, const int& callbackID,
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

 /* ----------------- [ Other interface ] ----------------- */

  /**
   * Set value method (value mutator)
   * To make RW property simetric to RO property
   * @param property property which requested value
   * @param value value to be returned
   * @param completion error handling structure
   * @param descOut callback descriptor
   */
  virtual void setValue(BACIProperty* property,
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

  /* -------------------- [ RW interface ] -------------------- */
  virtual ACSErr::Completion * set_sync ( T value )
	  throw (CORBA::SystemException );
	
  virtual void set_async ( T value,
			   ACS::CBvoid_ptr cb,
			   const ACS::CBDescIn & desc
			   
      ) throw (CORBA::SystemException);
	
  virtual void set_nonblocking ( T value)
      throw ( CORBA::SystemException );

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
  
  T state;
  /**
   * Definition of ActionFunction (member function of RW)
   */
  /*  typedef ActionRequest (RWdouble::*ActionFunction)(BACIComponent* cob, const int& callbackID,
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

  ACS::stringSeq    m_statesDescription;
  ACS::ConditionSeq m_condition;
	
    DevIO<T> *devIO_mp;  
    bool deldevIO_m;
    T m_value;
    int m_enumLength;
};

#include "enumpropRWImpl.i"

#endif /*!ENUM_IMPL_H*/






















