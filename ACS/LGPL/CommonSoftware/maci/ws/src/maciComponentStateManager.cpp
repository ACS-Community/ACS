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
 * "@(#) $Id: maciComponentStateManager.cpp,v 1.5 2008/10/09 07:05:37 cparedes Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * rcirami   27/11/03  created 
 */
 
#include <maciComponentStateManager.h>
#include <logging.h>

using namespace maci;
 
MACIComponentStateManager::MACIComponentStateManager(ACE_CString& compName):
    componentState_m(ACS::COMPSTATE_NEW),
    componentName_m(compName)
{
}
 
ACS::ComponentStates MACIComponentStateManager::getCurrentState()
{
    return componentState_m;
}

void MACIComponentStateManager::setState(ACS::ComponentStates newState)
{
    if (newState==componentState_m)
    {
        // trying to change the state to the currrent state: nothing to do
        ACS_SHORT_LOG((LM_DEBUG,"Ignoring no-op transition to and from %s",getName().c_str()));
    }
    else if (checkTransition(newState))
    {
        // The requested transition is valid
        ACE_CString tempStr(getName()); // The current state
        componentState_m=newState;
        ACS_SHORT_LOG((
            LM_INFO,
            "Switched state of component %s: %s -> %s",
            componentName_m.c_str(),
            tempStr.c_str(),
            getName().c_str()));
    } else 
    {
    }
}

ACE_CString MACIComponentStateManager::getName()
{
    return stateToString(componentState_m);
}

ACE_CString MACIComponentStateManager::stateToString(ACS::ComponentStates state)
{
    switch (state)
    {
        case ACS::COMPSTATE_NEW: return "NEW";
        case ACS::COMPSTATE_INITIALIZING : return "INITIALIZING";
        case ACS::COMPSTATE_INITIALIZED : return "INITIALIZED";
        case ACS::COMPSTATE_OPERATIONAL : return "OPERATIONAL";
        case ACS::COMPSTATE_ERROR : return "ERROR";
        case ACS::COMPSTATE_DESTROYING : return "DESTROYING";
        case ACS::COMPSTATE_ABORTING : return "ABORTING";
        case ACS::COMPSTATE_DEFUNCT : return "DEFUNCT";
        default : 
            ACS_SHORT_LOG((LM_WARNING,"Unknown component state value"));
            return "UNKNOWN";
    }
}

bool MACIComponentStateManager::checkTransition(ACS::ComponentStates candidateState)
{
    // The variable is set to true if we need to log the message
    bool inconsistentRequest=false;
    if (componentState_m==candidateState) {
        // Actual state ==> same state
        inconsistentRequest=true;
    }
    else if (candidateState==ACS::COMPSTATE_NEW)
    {
        // Any ==> NEW
        // This is set in the constructor and should not be selected
        inconsistentRequest=true;
    }
    else if (candidateState==ACS::COMPSTATE_INITIALIZING &&  componentState_m!=ACS::COMPSTATE_NEW)
    {
        // !NEW ==> INITIALIZING
        inconsistentRequest=true;
    }
    else if (candidateState==ACS::COMPSTATE_INITIALIZED &&  componentState_m!=ACS::COMPSTATE_INITIALIZING)
    {
        // !INITIALIZING ==> INITIALIZED
        inconsistentRequest=true;
    }
    else if (candidateState==ACS::COMPSTATE_OPERATIONAL 
        && !(componentState_m==ACS::COMPSTATE_INITIALIZED || componentState_m==ACS::COMPSTATE_ERROR) )
    {
        // !(INITIALIZED || ERROR ) ==> OPERATIONAL
        inconsistentRequest=true;
    }
    else if (candidateState==ACS::COMPSTATE_DESTROYING
        && !(componentState_m==ACS::COMPSTATE_OPERATIONAL || componentState_m==ACS::COMPSTATE_ERROR) )
    {
        // !(OPERATONAL || ERROR ) ==> DESTROYING
        inconsistentRequest=true;
    }
    else if (candidateState==ACS::COMPSTATE_DESTROYING
        && !(componentState_m==ACS::COMPSTATE_OPERATIONAL || componentState_m==ACS::COMPSTATE_ERROR) )
    {
        // !(OPERATONAL || ERROR ) ==> DESTROYING
        inconsistentRequest=true;
    }
    else if (candidateState==ACS::COMPSTATE_DEFUNCT
        && !(
            componentState_m==ACS::COMPSTATE_DESTROYING || 
            componentState_m==ACS::COMPSTATE_ERROR || 
            componentState_m==ACS::COMPSTATE_ABORTING) )
    {
        // !(DESTROYING || ERROR || ABORTING ) ==> DEFUNCT
        inconsistentRequest=true;
    }
    
    if (inconsistentRequest)
    {
        // Log a message
        ACS_SHORT_LOG((LM_WARNING,
            "Inconsistent state transition in %s: %s --> %s",
            componentName_m.c_str(),
            getName().c_str(),
            stateToString(candidateState).c_str()));
    }
    
    // Returns always true regardless of the real consistency of the transion
    return true;
}

