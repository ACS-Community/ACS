#ifndef acsComponentStateManger_h
#define acsComponentStateManger_h
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
 * "@(#) $Id: acsComponentStateManager.h,v 1.4 2008/10/09 06:34:01 cparedes Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * acaproni  2005-04-15  created
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acscomponentC.h>
#include <acsErrTypeLifeCycle.h>

namespace maci {
  
  /**
   * ComponentStateManager is an interface with the methods to manage
   * the state of a component
   */
  class ComponentStateManager {
    
    
    public:

      virtual ~ComponentStateManager(){}
    
        /**
         * Returns the current state of the component
         * 
         * @return the state of the component
         */
        virtual ACS::ComponentStates getCurrentState()=0;
        
        /**
         * Change the state of the component
         * 
         * @param newState The new state of the component
         * @throw LifeCycleException if the transition from the current
         *                           state to the new state is not allowed
         */
        virtual void setState(ACS::ComponentStates newState)=0;
            
        /**
         * Return the state in a human readable format
         * It could be useful for logging or printing the state
         * 
         * @return A string with the name of the state
         */
         virtual ACE_CString getName()=0;
  };
}; // namespace maci

#endif // acsComponentStateManger_h
