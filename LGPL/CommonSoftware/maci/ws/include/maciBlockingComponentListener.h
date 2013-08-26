#ifndef maciBlockingComponentListener_h
#define maciBlockingComponentListener_h
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
 * "@(#) $Id: maciBlockingComponentListener.h,v 1.1 2007/09/03 06:07:12 cparedes Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * acaproni  2005-04-06  created
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "acsComponentListener.h"

namespace maci {
   
   class BlockingComponentListener : public ComponentListener { 
    public:
        BlockingComponentListener();
        ~BlockingComponentListener();
        ACE_CString_Vector getAllCompsAvailable();
        ACE_CString_Vector getAllCompsUnavailable();
            
        void componentsUnavailable(ACE_CString_Vector& compNames);
        void componentsAvailable(ACE_CString_Vector& compNames);
        bool awaitNotifications(int milisec);
        bool notificationArrived; 
        ACE_CString_Vector allCompsAvailable;
        ACE_CString_Vector allCompsUnavailable;
        void clearAndExpect(int nCalls);
   };
   
}; // namespace maci

#endif // maciBlockingComponentListener_h
