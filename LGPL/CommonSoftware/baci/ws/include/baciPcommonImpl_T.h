#ifndef _baciPcommonImpl_T_H_
#define _baciPcommonImpl_T_H_
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
* "@(#) $Id: baciPcommonImpl_T"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram    07/02/2003  created
*/

/** 
 * @file 
 * Header file for BACI Common Property Template Class.
 * Includes the basic and common functionality for all types of properties.
 * Also includes the basic characteristics of all properties.
 * This class also has the function to create the monitor.
 */


#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "baciDB.h"
#include <acsutil.h>
#include <baci.h>
#include <baciS.h>
#include <baciCORBA.h>
#include <baciCORBAMem.h>
#include <baciEvent.h>
#include <baciDevIOMem.h>
#include <baciCharacteristicModelImpl.h>
#include <ACSErrTypeCommon.h>
#include <ACSErrTypeDevIO.h>
#include <baciErrTypeProperty.h>
#include <baciErrTypeDevIO.h>

/**
 * This is the history size that is basically the maximum number of values for a
 * BACI property that will be saved by the Property. Used in conjunction with the
 * get_history IDL method I believe.
 */
#define HISTORY_SIZE 32

//!!! TM can be defined as default parameter: TM=T but than "class" macro will not work in implementation
//!!! POA_SK can be omited here ? try

/**
 * Helper macro for use with templated parameters.
 */
#define ACS_P_T(T, TC) TC, ACS::CB##T, ACS::T##Seq, ACS::T##Seq_out, ACS::Monitor##T, baci::Monitor##T,TC, TC, TC
//, POA_ACS::RW##T 

//#define ACS_P_TL(X) X T, X TCB, X TSeq, X TSeq_out, X TMonitor, X TMonitorImpl, X POA_SK, X TBACIValue 

/**
 * Helper macro for use with templated parameters.
 */
#define ACS_P_TL T, TCB, TSeq, TSeq_out, TMonitor, TMonitorImpl, TM, TS, TSM, POA_SK

/**
 * Helper macro for use with templated parameters.
 */
#define ACS_P_C class T, class TCB, class TSeq, class TSeq_out, class TMonitor, class TMonitorImpl, class TM, class TS, class TSM, class POA_SK


namespace baci {

/**
It helps converting value read from CDB to the right type. It was introduced due the problem with std library under VxWorks
 */
template <class T>
class CDBconverter
{
  public:
    static void convertValue(const char *str, T& v)
	{
	    std::istringstream is(str);
	    (istream&) is >> v;
	    if (!is)
		throw cdbErrType::WrongCDBDataTypeExImpl(__FILE__, __LINE__, "CDBconverter::converterValue");
	}
};

template<>
class CDBconverter<ACE_CString>
{
  public:
    static void convertValue(const char *s, ACE_CString& v)
	{
	    v = (const char*)s;
	}
};

/**
 * Implementation of P (common) property
 * @warning We have virtual inheritance from PortableServer::RefCountServantBase
 * because of a bug in gcc 2.95 (see SPR. ALMASW2001075)
 */
template<ACS_P_C>
class baci_EXPORT PcommonImpl:	public virtual POA_SK,  
				public CharacteristicModelImpl,
				public PropertyImplementator,
				public ActionImplementator
{


public:
  /**
   * Constuctor
   * @param name property name (e.q. AMSMount:decliantion)
   * @param component_p parent of the property
   */
    PcommonImpl(const ACE_CString& name, BACIComponent *component_p, DevIO<TM> *devIO, bool flagdeldevIO);

  /**
   * Destructor
   */
  virtual ~PcommonImpl();

  /*
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
  CORBA::Object_ptr getCORBAReference() const { return reference_mp;  }

  /**
   * BACI Monitor instance accessor
   */
  BACIProperty* getProperty() const { return property_mp; }

  virtual void publishNow();

  /**
   * Destructor method
   * This method must be called to destroy property,
   * POA will call destructor 
   * (direct call of destructor will most likely cause code to crash)
   */
   virtual void destroy();

/**
 * DevIO accessor
 */
    DevIO<TM>* getDevIO() const { return devIO_mp; }
    
  /***
   * Implementations of async. get_value method
   */
    virtual ActionRequest getValueAction(BACIComponent* component_p, 
					 int callbackID,
					 const CBDescIn& descIn, 
					 BACIValue* value,
					 Completion& completion, 
					 CBDescOut& descOut);


  /* --------------- [ Action implementator interface ] -------------- */

  /**
   * Action dispatcher function
   * @param function action funtion to be invoked
   * @param component_p owner of the action
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
				     BACIComponent* component_p, 
				     const int &callbackID, 
				     const CBDescIn& descIn, 
				     BACIValue* value, 
				     Completion& completion, 
				     CBDescOut& descOut);

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
  
  /* --------------------- [ Property interface ] --------------------- */
    
    virtual char * name ();
    
    virtual char * characteristic_component_name ();

    /*
      already defined bt CharacteristicModelImpl
    virtual CosPropertyService::PropertySet_ptr get_all_characteristics ()
    */
    
    /* ----------------- [ TypelessProperty interface ] ----------------- */
    
    virtual CORBA::Boolean initialize_devio ();
    
    virtual char * description ();
    
    virtual char * format ();
    
    virtual char * units ();
    
    virtual ACS::pattern resolution ();
    
    /* -------------------- [ P interface ] -------------------- */
    
    virtual ACS::TimeInterval default_timer_trigger ();
    
    virtual ACS::TimeInterval min_timer_trigger ();
    
    virtual TS default_value ();
    
    virtual T get_sync (ACSErr::Completion_out c);
    
    virtual void get_async (TCB *cb,
			    const ACS::CBDescIn & desc);
    
    virtual CORBA::Long get_history (CORBA::Long n_last_values,
				     TSeq_out vs,
				     ACS::TimeSeq_out ts);
	
    virtual TMonitor* create_monitor (TCB*cb,
				      const ACS::CBDescIn & desc);
	
    virtual TMonitor* create_postponed_monitor (ACS::Time start_time,
						TCB *cb,
						const ACS::CBDescIn & desc);
  
  protected:
    
    /**
     * Read characteristics from CDB
     * @param propertyName name of the property whose characteristics to read
     * @return true on success, false on failure
     */
    virtual bool readCharacteristics();
  
    /* --------------------- [ History support ] ---------------------- */
    
    void addValueToHistory(ACS::Time time, TM &value);
    
    /// BACI property
    BACIProperty* property_mp;
    
    DevIO<TM> *devIO_mp;  
    bool deldevIO_m;
    TM value_m;
    
    /// Initialization status
    int initialization_m;
    
    /** @defgroup ActionFunctionTemplate ActionFunction
     * The ActionFunction is a templated typedef so there is no actual inline doc generated for it per-se.
     *  @{
     * Definition of ActionFunction (member function of RO)
     * @param component_p A pointer to a BACIComponent
     * @param callbackID ID of a Callback object
     * @param descIn Callback input description
     * @param value Pointer to a BACIValue object
     * @cparam ompletion An error system completion describing how the action completed
     * @param descOut Callback output description
     * @return An ActionRequest
     */
    typedef ActionRequest (PcommonImpl<ACS_P_TL>::*ActionFunction)(BACIComponent* component_p, 
								   int callbackID,
								   const CBDescIn& descIn, 
								   BACIValue* value,
								   ACSErr::Completion& completion, 
								   CBDescOut& descOut);  
    /** @} */

  /// Destroy status
  bool destroyed_m;

  /// CORBA reference
  CORBA::Object_ptr reference_mp;

  /// history value buffer
  TM historyValue_m[HISTORY_SIZE];

  /// history time buffer
  ACS::Time historyTime_m[HISTORY_SIZE];

  /// history start pointer
  int historyStart_m;

  /// history end pointer
  bool historyTurnaround_m;

  ///
  /// Characteristics
  ///
 
  bool initializeDevIO_m; 
  // Typeless property
  ACE_CString  description_m; 			
  ACE_CString  format_m; 				
  ACE_CString  units_m; 				
  ACS::pattern resolution_m; 			

  // P	
  TSM	defaultValue_m; 			
  ACS::TimeInterval  defaultTimerTrig_m; 		
  ACS::TimeInterval  minTimerTrig_m;

  private:
    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const PcommonImpl&);

    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    PcommonImpl(const PcommonImpl&);
};

//#include "baciPcommonImpl_T.i"

 }; 

#endif























