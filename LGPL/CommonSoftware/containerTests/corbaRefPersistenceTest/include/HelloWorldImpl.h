#ifndef _HELLO_WORLD_IMPL_H
#define _HELLO_WORLD_IMPL_H
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
* "@(#) $Id: HelloWorldImpl.h,v 1.4 2008/10/07 09:47:17 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni 2008-04-01 created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

///This example is unique because it is derived from CharacteristicComponent's
///superclass, ACSComponent.
#include <acscomponentImpl.h>

///This is the CORBA stub client header for ACSErrTypeCommon.idl where the 
///definition of the CORBA exception is found.
#include <ACSErrTypeCommon.h>

/**
 *  The empty CORBA servant interface, POA_acsexmplHelloWorld::HelloWorld,
 *  is obtained from this header file and is automatically generated from 
 *  HelloWorld's Interface Definition File (i.e., acsexmplHelloWorld.idl) 
 *  by CORBA.
 */
#include <HelloWorldS.h>
 
/**
 * This class define an IDL module for ttesting persistence
 * of CORMA references
 */
class HelloWorld: public virtual acscomponent::ACSComponentImpl,     //Component superclass
		  public virtual POA_CorbaRefTest::HelloWorld    //CORBA servant stub
{    
  public:
    /**
     * Constructor
     */
	  HelloWorld(const ACE_CString& name, maci::ContainerServices * containerServices);
    
    /**
     * Destructor
     */
    virtual ~HelloWorld();
    
    /* --------------------- [ CORBA interface ] ----------------------*/    
    /**
     * Displays "Hello World" to the console.
     */     
    virtual void displayMessage () ;

};
/*\@}*/
/*\@}*/

#endif /*!_HELLO_WORLD_IMPL_H*/
