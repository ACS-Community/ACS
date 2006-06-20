#ifndef CHARACTERISTIC_COMPONENT_IMPL_H
#define CHARACTERISTIC_COMPONENT_IMPL_H
/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration)
*    and Cosylab 2002, All rights reserved
*
*    This library is free software; you can redistribute it and/or
*    modify it under the terms of the GNU Lesser General Public
*    License as published by the Free Software Foundation; either
*    version 2.1 of the License, or (at your option) any later version.
*
*    This library is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*    Lesser General Public License for more details.
*
*    You should have received a copy of the GNU Lesser General Public
*    License along with this library; if not, write to the Free Software
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: baciCharacteristicComponentImpl.h,v 1.29 2006/06/20 15:24:48 bjeram Exp $"
*
*/

/** 
 * @file 
 * Header file BACI Characteristic Component.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <vector>
#include <baci.h>
#include <baciCORBA.h>
#include <acsutil.h>
#include <acscomponentImpl.h>
#include <baciCharacteristicModelImpl.h>
#include <acsErrTypeLifeCycle.h>

/**
 * The ACE NAMESPACE_USE() macro is equivalent to the use C++ directive,
 * but allows namespace support for compilers that do not have it
 * natively (i.e., GCC 2.95 for VxWorks).
 */
NAMESPACE_USE(baci)

/** @file baciCharacteristicComponentImpl.h
 *  This is the include file for CharacteristicComponent which is the 
 *  base class for all DO's within ALMA.
 */

////////////////////////////////////////////////////////////////////////
/** @def CHARACTERISTIC_COMPONENT_PROPERTY(IdlAccessor, CppImplVar)
 *  This macro checks to see if a property object has been created and initialized correctly.  
 *  It should normally be put in the distributed object's constructor after creating each 
 *  new property.
 *
 *  Also adds property information to a vector that is then used to define the descriptor. 
 *  Use this macro for each of the properties.
 *  @param IdlAccessor Name of the property located in the IDL file.  This is also the name 
 *  of the C++ method that must be implemented to acccess the IDL property.
 *  @param CppImplVariable Name of the C++ variable used in the C++ method for each IDL 
 *  property.  CORBA poses no restrictions on the name of this variable although the "C++ 
 *  Coding Standards" document does.
 *  @htmlonly
 *  <br><hr>
 *  @endhtmlonly
 */
#define CHARACTERISTIC_COMPONENT_PROPERTY(IdlAccessor, CppImplVar) \
    if (CppImplVar == 0) return; \
    else if (CppImplVar->initialization() == 1) \
        return; \
    { \
     \
    desc_m->properties.length(desc_m->properties.length()+1); \
    desc_m->properties[desc_m->properties.length()-1].property_ref = this->IdlAccessor(); \
    desc_m->properties[desc_m->properties.length()-1].name = CppImplVar->name(); \
    desc_m->properties[desc_m->properties.length()-1].characteristics = CppImplVar->get_all_characteristics(); \
    }

NAMESPACE_BEGIN(baci);

/**
 * This class implements the ACS DO.  All the standard methods and 
 * macros necessary for the CORBA interface implementation and the 
 * MACI-DLL support are in this class.
 */
class CharacteristicComponentImpl : public acscomponent::ACSComponentImpl,
				    public baci::CharacteristicModelImpl,
                    public virtual POA_ACS::CharacteristicComponent
{

  public:

    /**
     * Constructor.
     * The CharacteristicComponent shall be considered an abstract class
     * and at the end of the constructor an CharacteristicComponent
     * is not initilized (i.e. initilization() would return 1 == incomplete).
     * @param Tha name of the Component
     * @param containerServices  pointer to services provided by the container
     * @param monitoringProperties flag that indicates if monitoring of properties has to be turned on or not. Default is monitroing turned on.
     */
    CharacteristicComponentImpl(
        const ACE_CString& name,
        maci::ContainerServices *containerServices,
	bool monitoringProperties=true);

    /**
     * Destructor
     */
    virtual ~CharacteristicComponentImpl();

    /**
     * Get BACI Component instance of DO
     * This function is used to return component_mp because inherited classes would not 
     * have access to it otherwise.
     * @return BACI Component instance
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */
    BACIComponent* getComponent() const { return component_mp; }

    /**
     * Descriptor of the CharacteristicComponent
     * This method returns a pointer to this CharacteristicComponent's descriptor.  The descriptor contains
     * data (i.e., DO reference, name, UID, etc) used by typical clients retrieved in 
     * only one network call.  However, the descriptor ONLY contains named members that 
     * are declared by BACI and not a specific control system.
     
     * @return A pointer to this DO's descriptor.
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */
    virtual ACS::CharacteristicComponentDesc * 
    descriptor ()
	throw (CORBA::SystemException);
	
	/**
	 * Add a property to the descriptor desc_m
	 * 
	 * @param fun The idl accessor of the property
	 * @param name The name of the property
	 * @param propSet The property set of the property
	 * 
	 * @htmlonly
     * <br><hr>
     * @endhtmlonly
	 */
	void addPropertyToDesc(ACS::Property_ptr prop);

    /*************** Override Life Cycle functions ***************/

    /**
     * The function creates and starts the thread before calling  execute
     * 
     * @return void
     */
    virtual void __execute()
        throw (ACSErr::ACSbaseExImpl);
    
    /**
     * The function stops the threads before calling  aboutToAbort()
     * 
     * @return void
     */
    virtual void __aboutToAbort();
    
    /**
     * The function stops the threads before calling  cleanUp()
     * 
     * @return void
     */
    virtual void __cleanUp();
    
   /**
     * Method to resume (enable) monitoring of the properties.
     * 
     * @return void
     */
    void resumePropertiesMonitoring();

   /**
     * Method to suspend (disable) monitoring of the properties.
     * 
     * @return void
     */
    void suspendPropertiesMonitoring();

  protected:

    /** ComponentDesc returned by the descriptor method.
     */
    ACS::CharacteristicComponentDesc_var desc_m;
    
  private:

    /**
       Signal if monitoring of properties is turned on or not.
     */
    bool monitoringProperties_mp;


    /** BACI Component instance
     *  This is a reference to the CORBA object
     */
    BACIComponent *component_mp;

    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const CharacteristicComponentImpl&);

    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    CharacteristicComponentImpl(const CharacteristicComponentImpl&);

};

NAMESPACE_END(baci);

#endif /* CHARACTERISTIC_COMPONENT_IMPL_H */














