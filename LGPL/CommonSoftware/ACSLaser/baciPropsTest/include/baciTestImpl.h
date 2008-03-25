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
* "@(#) $Id: baciTestImpl.h,v 1.4 2008/03/25 17:30:03 acaproni Exp $"
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
#include <baciROpattern.h>
#include <enumpropROImpl.h>

///Include the smart pointer for properties
#include <baciSmartPropertyPointer.h>

using namespace baci;


/**
 * A component for testing if baci sends alarms when a property goes out of range.
 * 
 */
class BaciPropTest: public CharacteristicComponentImpl,     //Standard component superclass
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
	  
	  virtual void execute() throw (ACSErr::ACSbaseExImpl);

    
    /**
     * Destructor
     */
    virtual ~BaciPropTest();
    
    /* --------------------- [ CORBA interface ] ----------------------*/   
    /**
     * 
     */   
    virtual void setDoubleVar(CORBA::Float) throw (CORBA::SystemException); 
    
    /**
     * Returns a reference to the double property
     */
    virtual ACS::ROdouble_ptr testDoubleVar() throw (CORBA::SystemException);
    
    /**
     * Returns a reference to the pattern property
     */
    virtual ::alarmsystemPropTest::ROAlarmEnum_ptr testPatternVar() throw (CORBA::SystemException);
    
  private:
    
    /**
     *  The test properties
     */
    SmartPropertyPointer<ROdouble>  m_testDoubleVar_sp;
    
    
    SmartPropertyPointer<ROEnumImpl<ACS_ENUM_T(alarmsystemPropTest::AlarmEnum),  POA_alarmsystemPropTest::ROAlarmEnum>
        > m_testPatternVar_sp;
    
};
/*\@}*/
/*\@}*/

#endif /*!baciTestImpl_H*/
