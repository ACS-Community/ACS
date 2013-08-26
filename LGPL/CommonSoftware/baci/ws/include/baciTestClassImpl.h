#ifndef baciTestClassImpl_h
#define baciTestClassImpl_h

/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2004 
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
* "@(#) $Id: baciTestClassImpl.h,v 1.113 2008/10/01 02:26:45 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran 2002-02-10 Added new properties for test
* msekoran 2002-01-03 Added new properties for test
* gchiozzi 2001-10-18 Added new properties for test
* gchiozzi 2001-10-17 Added warning on virtual inheritance from PortableServer::RefCountServantBase
* gchiozzi 2001-02-15 Added declaration of get_interface() method for Object Explorer
* gchiozzi 2001-02-15 created created standard header
* msekoran 2001-03-10 integrated with new BACI; ALMA coding convention used; doc.
*/

/** 
 * @file 
 * Header file for BACI Test Class.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsutil.h>

#include <baci.h>
#include <baciTestS.h>

#include <baciROdouble.h>
#include <baciRWdouble.h>
#include <baciROfloat.h>
#include <baciRWfloat.h>
#include <baciROlong.h>
#include <baciRWlong.h>
#include <baciROpattern.h>
#include <baciRWpattern.h>
#include <baciROstring.h>
#include <baciRWstring.h>

#include <baciROdoubleSeq.h>
#include <baciRWdoubleSeq.h>
#include <baciROfloatSeq.h>
#include <baciRWfloatSeq.h>

#include <baciROlongSeq.h>
#include <baciRWlongSeq.h>

#include <baciCharacteristicComponentImpl.h>

#include <baciSmartPropertyPointer.h>

/**
 * This test class provides 4 methods: shutdown, on, off and reset.
 * It also provides one property per each type supported
 */

class BaciTestClassImpl: public baci::CharacteristicComponentImpl,
			 public POA_BACI_TEST::BaciTestClass,
			 public baci::ActionImplementator
{
  
public:
  // Constructors & Destructors
  BaciTestClassImpl(
		    const ACE_CString& name,
		    maci::ContainerServices *,
		    bool monitoring=true);

  /**
   * Destructor
   */
  virtual ~BaciTestClassImpl();

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
  virtual baci::ActionRequest invokeAction(int function,
				     baci::BACIComponent* component_p, const int &callbackID, 
				     const CBDescIn& descIn, baci::BACIValue* value, 
				     Completion& completion, CBDescOut& descOut);

  /***
   * Implementation of async. on() method
   */
  virtual baci::ActionRequest onAction(baci::BACIComponent* component_p, int callbackID,
			 const CBDescIn& descIn, baci::BACIValue* value,
			 Completion& completion, CBDescOut& descOut);

  /***
   * Implementation of async. off() method
   */
  virtual baci::ActionRequest offAction(baci::BACIComponent* component_p, int callbackID,
			  const CBDescIn& descIn, baci::BACIValue* value,
			  Completion& completion, CBDescOut& descOut);

  /***
   * Implementation of async. reset() method
   */
  virtual baci::ActionRequest resetAction(baci::BACIComponent* component_p, int callbackID,
			    const CBDescIn& descIn, baci::BACIValue* value,
			    Completion& completion, CBDescOut& descOut);

  /* ----------------------------------------------------------------*/
  /* --------------------- [ CORBA interface ] ----------------------*/
  /* ----------------------------------------------------------------*/

  /** 
   * Servant shutdown request function
   */
   virtual void shutdown ();
   
  /**
   * Switches on the power supply.
   * A callback is used to inform the caller when the action is
   * performed.
   *
   * @param callBack  callback when action has finished
   */     
  virtual void on (
		   ACS::CBvoid_ptr cb,
		   const ACS::CBDescIn & desc
		   );
  
  /**
   * Switches off the power supply.
   * A callback is used to inform the caller when the action is
   * performed.
   *
   * @param callBack  callback when action has finished
   */ 
  virtual void off (
		    ACS::CBvoid_ptr cb,
		    const ACS::CBDescIn & desc
		    );
  
  /**
   * Resets the power supply.
   * A callback is used to inform the caller when the action is
   * performed.
   *
   * @param callBack  callback when action has finished
   */ 
    virtual void reset (
		      ACS::CBvoid_ptr cb,
		      const ACS::CBDescIn & desc
		      );
  
    /**
     * Method to turn monitoring on from a remote client
     */
    virtual void turnOnMonitoring() 
	{
	    try
		{
		startPropertiesMonitoring();
		}
	    catch(ACSErr::ACSbaseExImpl &ex)
		{
		ex.log();
		}
	    
	}//turnOnMonitoring

    /**
     * Method to turn monitoring off from a remote client
     */
    virtual void turnOffMonitoring()
	{
            try
		{
		stopPropertiesMonitoring();
		}
	    catch(ACSErr::ACSbaseExImpl &ex)
		{
		ex.log();
		}
	}//turnOffMonitoring

    /**
     * Check the status of the monitoring thread and logs it
     */
    virtual CORBA::Boolean isPropertiesMonitoringActive();

    /**
     * Property RWdoubleProps contains the actual RWdoubleWithErrorDevIOProp of the 
     * power supply.
     */ 
    virtual ACS::RWdouble_ptr RWdoubleWithErrorDevIOProp ();

  /**
   * Property RWdoubleProps contains the actual RWdoubleWithDevIOProp of the 
   * power supply.
   */ 
  virtual ACS::RWdouble_ptr RWdoubleWithDevIOProp ();

  /**
   * Property RWdoubleProps contains the actual RWdoubleProp of the 
   * power supply.
   */ 
  virtual ACS::RWdouble_ptr RWdoubleProp ();
  
  /**
   * Property ROdoubleProp is the ROdoubleProp of the actual setting
   * of the power supply.
   */ 
  virtual ACS::ROdouble_ptr ROdoubleProp ();
  
  /**
   * Property RWfloatProps contains the actual RWfloatProp of the 
   * power supply.
   */ 
  virtual ACS::RWfloat_ptr RWfloatProp ();
  
  /**
   * Property ROfloatProp is the ROfloatProp of the actual setting
   * of the power supply.
   */ 
  virtual ACS::ROfloat_ptr ROfloatProp ();
  
  /**
   * Property RWlongProp contains the actual "RWlongProp" of the 
   * power supply.
   */ 
  virtual ACS::RWlong_ptr RWlongProp ();
  
  /**
   * Property ROlongProp is the ROdoubleProp of the actual "RWlongProp"
   * of the power supply.
   */ 
  virtual ACS::ROlong_ptr ROlongProp ();
  
  /**
   * Property ROpatternProp contains the actual ROpatternProp
   * of the power supply.
   */
  virtual ACS::ROpattern_ptr ROpatternProp ();

  /**
   * Property RWpatternProp contains the actual RWpatternProp
   * of the power supply.
   */
  virtual ACS::RWpattern_ptr RWpatternProp ();
  
  /**
   * Property ROstringProps contains a test ROstringProp string
   */ 
  virtual ACS::ROstring_ptr ROstringProp ();

  /**
   * Property RWstringProps contains a test RWstringProp string
   */ 
  virtual ACS::RWstring_ptr RWstringProp ();


  /**
   * Property RWdoubleSeqProps contains the actual RWdoubleSeqProp of the 
   * power supply.
   */ 
  virtual ACS::RWdoubleSeq_ptr RWdoubleSeqProp ();

  /**
   * Property ROdoubleSeqProp is the ROdoubleSeqProp of the actual setting
   * of the power supply.
   */ 
  virtual ACS::ROdoubleSeq_ptr ROdoubleSeqProp ();

  /**
   * Property RWfloatSeqProps contains the actual RWfloatSeqProp of the 
   * power supply.
   */ 
  virtual ACS::RWfloatSeq_ptr RWfloatSeqProp ();

  /**
   * Property ROfloatSeqProp is the ROfloatSeqProp of the actual setting
   * of the power supply.
   */ 
  virtual ACS::ROfloatSeq_ptr ROfloatSeqProp ();

  /**
   * Property RWlongSeqProp contains the actual "RWlongSeqProp" of the 
   * power supply.
   */ 
  virtual ACS::RWlongSeq_ptr RWlongSeqProp ();
  
  /**
   * Property ROlongSeqProp is the ROlongSeqProp of the actual "RWlongSeqProp"
   * of the power supply.
   */ 
  virtual ACS::ROlongSeq_ptr ROlongSeqProp ();

  
  
private:
  
  /**
   * Definition of ActionFunction (member function of PowerSupply class)
   */
  typedef baci::ActionRequest (BaciTestClassImpl::*ActionFunction)(baci::BACIComponent* component_p, int callbackID,
						       const CBDescIn& descIn, baci::BACIValue* value,
						       Completion& completion, CBDescOut& descOut);

  /// Is manager shutting down?
  bool m_shutdown;

  /// Array of actions
  ActionFunction m_actions[3];

  /// The smart pointers for (other) properties
  baci::SmartPropertyPointer<baci::RWdouble>  m_RWdoubleWithErrorDevIOProp_sp;
  baci::SmartPropertyPointer<baci::RWdouble>  m_RWdoubleWithDevIOProp_sp;
  baci::SmartPropertyPointer<baci::ROdouble>  m_ROdoubleProp_sp;
  baci::SmartPropertyPointer<baci::RWdouble>  m_RWdoubleProp_sp;
  baci::SmartPropertyPointer<baci::ROfloat>   m_ROfloatProp_sp;
  baci::SmartPropertyPointer<baci::RWfloat>   m_RWfloatProp_sp;
  baci::SmartPropertyPointer<baci::ROlong>    m_ROlongProp_sp;
  baci::SmartPropertyPointer<baci::RWlong>    m_RWlongProp_sp;
  baci::SmartPropertyPointer<baci::ROpattern> m_ROpatternProp_sp;
  baci::SmartPropertyPointer<baci::RWpattern> m_RWpatternProp_sp;
  baci::SmartPropertyPointer<baci::ROstring>  m_ROstringProp_sp;
  baci::SmartPropertyPointer<baci::RWstring>  m_RWstringProp_sp;

  baci::SmartPropertyPointer<baci::ROdoubleSeq>  m_ROdoubleSeqProp_sp;
  baci::SmartPropertyPointer<baci::RWdoubleSeq>  m_RWdoubleSeqProp_sp;
  baci::SmartPropertyPointer<baci::ROfloatSeq>  m_ROfloatSeqProp_sp;
  baci::SmartPropertyPointer<baci::RWfloatSeq>  m_RWfloatSeqProp_sp;
  baci::SmartPropertyPointer<baci::ROlongSeq>    m_ROlongSeqProp_sp;
  baci::SmartPropertyPointer<baci::RWlongSeq>    m_RWlongSeqProp_sp;

};

#endif   /* baciTestClassImpl_h */






