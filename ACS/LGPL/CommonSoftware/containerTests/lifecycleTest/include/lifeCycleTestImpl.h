#ifndef _LIFE_CYCLE_TEST_H
#define _LIFE_CYCLE_TEST_H
/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) Associated Universities Inc., 2002 
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
* "@(#) $Id: lifeCycleTestImpl.h,v 1.2 2008/07/25 07:45:52 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* eallaert  2007-11-05  initial version
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

///superclass, ACSComponent.
#include <acscomponentImpl.h>

///This is the CORBA stub client header for ACSErrTypeCommon.idl where the 
///definition of the CORBA exception is found.
#include <ACSErrTypeCommon.h>

/**
 *  The empty CORBA servant interface, POA_contLogTest::TestLifeCycleComp,
 *  is obtained from this header file and is automatically generated from 
 *  contLogTest's Interface Definition File (i.e., contLogTest.idl) 
 *  by CORBA.
 */
#include <lifecycleTest_IFS.h>
 
class TestLifeCycleComp: public virtual acscomponent::ACSComponentImpl,     //Component superclass
		  public POA_lifecycleTest::TestLifeCycleComp    //CORBA servant stub
{    
  public:
    /**
     * Constructor
     * @param poa Poa which will activate this and also all other components. Developers need
     * not be concerned with what a PortableServer does...just pass it to the superclass's
     * constructor.
     * @param name component's name. All components have a name associated with them so other 
     * components and clients can access them.
     */
    TestLifeCycleComp(
	       const ACE_CString& name,
	       maci::ContainerServices * containerServices);
    
    /**
     * Destructor
     */
    virtual ~TestLifeCycleComp();
    
    /* --------------------- [ CORBA interface ] ----------------------*/    
    /**
     * Implementation of IDL getLevels().
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */     

    virtual void dummyInterface ();
    
  private:
};
/*\@}*/
/*\@}*/

#endif /*!_LIFE_CYCLE_TEST_H*/



