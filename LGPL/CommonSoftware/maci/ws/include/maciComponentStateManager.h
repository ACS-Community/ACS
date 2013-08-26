#ifndef maciComponentStateManager_h
#define maciComponentStateManager_h
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
 * "@(#) $Id: maciComponentStateManager.h,v 1.5 2012/01/21 22:48:11 tstaig Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * acaproni  2005-04-06  created
 */
 
 #include <acsComponentStateManager.h>
 #include <ace/Functor_String.h>
 
 #include <acsErrTypeLifeCycle.h>
 
 namespace maci {
     
     /** 
      * The default implementation of the ComponentStateManager
      * abstract class.
      */
     class MACIComponentStateManager: public ComponentStateManager
     {
        public:
        
            /**
             * The constructor
             */
            MACIComponentStateManager(ACE_CString& compName);
            
            /**
             * The destructor
             */
            virtual ~MACIComponentStateManager() { }
    
            /**
             * Returns the current state of the component
             * 
             * @return the state of the component
             */
            virtual ACS::ComponentStates getCurrentState();
            
            /**
             * Change the state of the component
             * 
             * @param newState The new state of the component
             * @throw LifeCycleException if the transition from the current
             *                           state to the new state is not allowed
             */
            virtual void setState(ACS::ComponentStates newState);
                
            /**
             * Return the state in a human readable format
             * It could be useful for logging or printing the state
             * 
             * @return A string with the name of the state
             */
             virtual ACE_CString getName();
        
        private:
        
            /**
             * Check if the transition from the actual state
             * to the requested state is consistent
             * 
             * At the present this function does nothing: it checks the consistency
             * and in case of error send a message to the log. But it always
             * returns true.
             * 
             * @param candidateState The state to check against the current state
             * @return true if the proposed change is consistent (always in this version)
             */
            bool checkTransition(ACS::ComponentStates candidateState);
            
            
            /**
             * Convert a state to a string
             * 
             * @param state The state to convert
             * @return The string representing the state
             */
             ACE_CString stateToString(ACS::ComponentStates state);
        
        private:
            /// The current state of the component
            ACS::ComponentStates componentState_m;
            
            /// The name of the component (to beautify log messages)
            ACE_CString componentName_m;
     };
 };
 
 #endif // maciComponentStateManager_h
