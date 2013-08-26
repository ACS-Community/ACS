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
 * "@(#) $Id: maciBlockingComponentListener.cpp,v 1.1 2007/09/03 06:07:12 cparedes Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 */

#include "maciBlockingComponentListener.h"


using namespace maci;

BlockingComponentListener::BlockingComponentListener(){notificationArrived=false;}
BlockingComponentListener::~BlockingComponentListener(){}

ACE_CString_Vector BlockingComponentListener::getAllCompsAvailable() { return allCompsAvailable;} 

ACE_CString_Vector BlockingComponentListener::getAllCompsUnavailable() { return allCompsUnavailable;} 
    
void BlockingComponentListener::componentsUnavailable(ACE_CString_Vector& compNames){
    ACS_SHORT_LOG((LM_INFO, "Informing components Unavailable n=%d",(int)compNames.size()));
    for(int i = 0; i< (int)compNames.size(); i++){
        ACS_SHORT_LOG((LM_INFO, "unavailable: %s", compNames[i].c_str()));
        allCompsUnavailable.push_back(compNames[i]);
    }
    notificationArrived = true;
}
void BlockingComponentListener::componentsAvailable(ACE_CString_Vector& compNames){
    ACS_SHORT_LOG((LM_INFO, "Informing components Available n=%d",(int)compNames.size()));
    for(int i = 0; i< (int)compNames.size(); i++){
        ACS_SHORT_LOG((LM_INFO, "available: %s", compNames[i].c_str()));
        allCompsAvailable.push_back(compNames[i]);
    }
    notificationArrived = true;
}

bool BlockingComponentListener::awaitNotifications(int sec){
    if(notificationArrived) return true;
    else return false;
}
void BlockingComponentListener::clearAndExpect(int nCalls) {
    notificationArrived = false;
    allCompsAvailable.clear();
    allCompsUnavailable.clear();
}

