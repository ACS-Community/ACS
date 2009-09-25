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
 * "@(#) $Id: baciTestAlarmClassImpl.h,v 1.4 2009/09/25 13:50:08 bjeram Exp $"
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


class MyROPatternProperty : public baci::RWpattern
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
			baci::BACIComponent *component_p, 
			baci::ROpattern *roPatternProperty_p);
    
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
    setValue (baci::BACIProperty *property_p,
	      baci::BACIValue *value_p, 
	      Completion &completion,
	      CBDescOut &descOut);
    

  private:
    /** 
     * m_roPatternProperty_p is never created in this class.
     * It just points to the ROpattern passed to this class's constructor.  This is done so 
     * that we can synchronize this class and the ROpattern's value. 
     */
    baci::ROpattern *m_roPatternProperty_p;

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
    virtual void shutdown ();

    /**
     * we can change FF and FM from a client so that we can test this functionalty
     * @param FF
     * @param FM
     */
    void changeAlarmFFFM(const char*, const char*);

    /**
     * Check the status of the monitoring thread and logs it
     */
    virtual CORBA::Boolean isPropertiesMonitoringActive(); 

    virtual ACS::ROpattern_ptr roPatternProperty();
        
    virtual ACS::RWpattern_ptr rwPatternProperty();


  private:
  
    /// Is manager shutting down?
    bool m_shutdown;

    /// The smart pointers for the properties
    baci::SmartPropertyPointer<baci::ROpattern> m_roPatternProperty_sp;    
    baci::SmartPropertyPointer<MyROPatternProperty> m_rwPatternProperty_sp;
    
    /**
     * ALMA C++ coding standards state copy operators should be disabled.
     */
    void operator=(const BaciTestAlarmClassImpl&);
};

#endif   /* baciTestAlarmClassImpl_h */
