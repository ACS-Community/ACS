#ifndef acstestcompErrorExplorerImpl_h
#define acstestcompErrorExplorerImpl_h

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
* "@(#) $Id: acstestcompErrorExplorerImpl.h,v 1.3 2008/10/01 05:33:43 cparedes Exp $"
*
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

///Contains the defintion of the standard superclass for C++ components
#include <baciCharacteristicComponentImpl.h>

///CORBA generated servant stub
#include <acstestcompS.h>

///Includes for each BACI property used in this example
#include <baciRWdouble.h>

///Import the template for the smart pointer
#include <baciSmartPropertyPointer.h>

/** @file acstestcompErrorExplorerImpl.h
 */

/**
 * The class ErrorExplorer simulates the behaviour of a power supply.
 * It provides three methods: on, off and reset.
 * It also provides the properties current, readback and status.
 * Asynchronous calls are implemented using the ...... pattern and the ..... support classes.
 * For each xxx action defined in the IDL interface two methods are provided:
 *  - xxx() just registers the action and installs the callback
 *  - xxxAction() performs (asyncronously) the action and invokes the callback when finished.
 * The ErrorExplorer::invokeAction method is called by the asynchronous dispatcher whenever there is an
 * xxx pending action and it calls the xxxAction corresponding method.
 * 
 * @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 * Jozef Stefan Institute, Slovenia<br>
 * @version "@(#) $Id: acstestcompErrorExplorerImpl.h,v 1.3 2008/10/01 05:33:43 cparedes Exp $"
 */

class ErrorExplorerImpl: public baci::CharacteristicComponentImpl,     //Standard component superclass
			 public virtual POA_acstestcomp::ErrorExplorer    //CORBA servant stub
{
  public:
    /**
     * Constructor
     * 
     * @param name component name
     * @param containerService A pointer to the container services
     */
    ErrorExplorerImpl(
		const ACE_CString &name,
		maci::ContainerServices * containerServices);
  
    /**
     * Destructor
     */
    virtual ~ErrorExplorerImpl();  
    
    /**  
     * Returns a reference to the current_p property (commanded current).
     * Implementation of IDL interface for the property.
     * @return a pointer to the property
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */ 
    virtual ACS::RWdouble_ptr 
    explorerDoubleProperty();
    
    
  protected:
    
  private:
    /**
     *  m_readback_sp is the actual value of ErrorExplorer's current.
     */
    baci::SmartPropertyPointer<baci::RWdouble> m_explorerDoubleProperty_sp;
    
    /**
     * ALMA C++ coding standards state copy operators should be disabled.
     */
    void operator=(const ErrorExplorerImpl&);
};

#endif   /* acstestcompErrorExplorerImpl_h */



