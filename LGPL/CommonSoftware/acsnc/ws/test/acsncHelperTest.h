#ifndef _acsnc_helper_test_h_
#define _acsnc_helper_test_h_
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
* "@(#) $Id: acsncHelperTest.h,v 1.1 2008/11/13 01:57:44 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-09-26 added more comments
* david  25/09/02  created
*/
#include "acsncHelper.h"
namespace nc{

class HelperTest: public Helper{

    public:
    static ACE_Condition_Thread_Mutex m_tester_condition;
    static ACE_Thread_Mutex m_tester_mutex;
    static bool m_useMutex;

    //bool resolveInternalNotificationChannel();

    HelperTest(const char*, int, char**);
    virtual void createNotificationChannel();
   // void useMutex(bool useMutex);

    CosNotifyChannelAdmin::EventChannel_var getNotifyChannel();
    ~HelperTest(){}

};
};
#endif
