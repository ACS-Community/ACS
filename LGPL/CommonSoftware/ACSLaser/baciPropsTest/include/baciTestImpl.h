#ifndef baciTestImpl_h
#define baciTestImpl_h
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
*
* "@(#) $Id: baciTestImpl.h,v 1.12 2009/10/13 10:27:03 acaproni Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni 2008-03-17 created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

///Contains the defintion of the standard superclass for C++ components
#include <baciCharacteristicComponentImpl.h>

///CORBA generated servant stub
#include <testComponentS.h>

///Includes for each BACI property used in this example
#include <baciROdouble.h>
#include <baciROboolean.h>
#include <baciROpattern.h>
#include <enumpropROImpl.h>

///Include the smart pointer for properties
#include <baciSmartPropertyPointer.h>


/**
 * A component for testing if baci sends alarms when a property goes out of range.
 *
 */
class BaciPropTest: public baci::CharacteristicComponentImpl,     //Standard component superclass
			     public virtual POA_alarmsystemPropTest::BaciPropTest   //CORBA servant stub
{
  public:
     /**
     * Constructor
     *
     * @param name component's name. This is also the name that will be used to find the
     * configuration data for the component in the Configuration Database.
     * @param containerService The pointer to the container services
     */
	  BaciPropTest(ACE_CString name, maci::ContainerServices * containerServices);

     /**
      * @throw ACSErr::ACSbaseExImpl
      */
	  virtual void execute();


    /**
     * Destructor
     */
    virtual ~BaciPropTest();

    /* --------------------- [ CORBA interface ] ----------------------*/
    /**
     *
     */
    virtual void setDoubleVar(CORBA::Float);
    virtual void setPatternVar(CORBA::Long);
    virtual void setBooleanVar(CORBA::Boolean);
    virtual void setEnumVar(alarmsystemPropTest::AlarmEnum);
    virtual void setDoubleVarComplete(CORBA::Float val, const char* faultFamily, const char* faultMember);

    /**
     * Returns a reference to the double property
     */
    virtual ACS::ROdouble_ptr testDoubleVar();

    /**
     * Returns a reference to the pattern property
     */
     virtual ACS::ROpattern_ptr testPatternVar();

    /**
     * Returns a reference to the enum property
     */
    virtual ::alarmsystemPropTest::ROAlarmEnum_ptr testEnumVar();

    /**
	 * Returns a reference to the boolean property
	 */
	virtual ACS::ROboolean_ptr testBooleanVar();

  private:

    /**
     *  The test properties
     */
    baci::SmartPropertyPointer<baci::ROdouble>  m_testDoubleVar_sp;

    baci::SmartPropertyPointer<baci::ROboolean>m_testBooleanVar_sp;

    baci::SmartPropertyPointer<baci::ROpattern>  m_testPatternVar_sp;

    baci::SmartPropertyPointer<ROEnumImpl<ACS_ENUM_T(alarmsystemPropTest::AlarmEnum),  POA_alarmsystemPropTest::ROAlarmEnum>
        > m_testEnumVar_sp;
};
/*\@}*/
/*\@}*/

#endif /*!baciTestImpl_H*/
