#ifndef baciTestAlarmClassImpl_h
#define baciTestAlarmClassImpl_h

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
 * "@(#) $Id: baciTestAlarmClassImpl.h,v 1.1 2008/03/03 14:54:23 rcirami Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * oat      2008-02-28 created
 */

/** 
 * @file 
 * Header file for BACI Test Alarm Class.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsutil.h>

#include <baci.h>
#include <baciTestS.h>

#include <baciROpattern.h>
#include <baciRWpattern.h>

#include <baciCharacteristicComponentImpl.h>

#include <baciSmartPropertyPointer.h>

using namespace baci;


class MyROPatternProperty : public RWpattern
{
  public:   
    /**
     * Constuctor
     * @param name property name (e.q. rwDoubleProperty)
     * This is also the name that will be used to find the
     * configuration data for the property in the Configuration Database.
     * @param component_p parent of the property
     * @param roPatternProperty_p reference to the associated pattern property
     */
    MyROPatternProperty(const ACE_CString &name, 
			BACIComponent *component_p, 
			ROpattern *roPatternProperty_p);
    
    /**
     * Set value method (value mutator)
     * We add this to override the setValue method inherited from RWdouble.  We do 
     * this to make the RW property symetric to the RO property passed to the constructor.
     * @param property_p property which requested value
     * @param value_p value to be returned
     * @param completion error handling structure
     * @param descOut callback descriptor
     * @htmlonly
     <br><hr>
     @endhtmlonly
    */
    virtual void 
    setValue (BACIProperty *property_p,
	      BACIValue *value_p, 
	      Completion &completion,
	      CBDescOut &descOut);
    
  private:
    /** 
     * m_roPatternProperty_p is never created in this class.
     * It just points to the ROpattern passed to this class's constructor.  This is done so 
     * that we can synchronize this class and the ROpattern's value. 
     */
    ROpattern *m_roPatternProperty_p;

    void operator=(const MyROPatternProperty&);
};




/**
 * This test class provides 4 methods: shutdown, on, off and reset.
 * It also provides one property per each type supported
 */

class BaciTestAlarmClassImpl: public baci::CharacteristicComponentImpl,
			      public POA_BACI_TEST::BaciTestAlarmClass
{
  
  public:
    // Constructors & Destructors
    BaciTestAlarmClassImpl(const ACE_CString& name,
			   maci::ContainerServices* containerServices,
			   bool monitoring=true);

    /**
     * Destructor
     */
    virtual ~BaciTestAlarmClassImpl();


    /* ----------------------------------------------------------------*/
    /* --------------------- [ CORBA interface ] ----------------------*/
    /* ----------------------------------------------------------------*/
    
    /** 
     * Servant shutdown request function
     */
    virtual void shutdown ()
	throw (CORBA::SystemException);

    /**
     * Check the status of the monitoring thread and logs it
     */
    virtual CORBA::Boolean isPropertiesMonitoringActive() 
	throw (CORBA::SystemException);

    virtual ACS::ROpattern_ptr roPatternProperty()
	throw (CORBA::SystemException);
        
    virtual ACS::RWpattern_ptr rwPatternProperty()
	throw (CORBA::SystemException);


  private:
  
    /// Is manager shutting down?
    bool m_shutdown;

    /// The smart pointers for the properties
    SmartPropertyPointer<ROpattern> m_roPatternProperty_sp;    
    SmartPropertyPointer<MyROPatternProperty> m_rwPatternProperty_sp;
    
    /**
     * ALMA C++ coding standards state copy operators should be disabled.
     */
    void operator=(const BaciTestAlarmClassImpl&);
};

#endif   /* baciTestAlarmClassImpl_h */
