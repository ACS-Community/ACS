#ifndef ACSUTIL_ORB_HELPER_H
#define ACSUTIL_ORB_HELPER_H
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
* "@(#) $Id: acsutilORBHelper.h,v 1.2 2006/01/09 18:52:17 dfugate Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david  20/09/02  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

/** @file acsutilORBHelper.h
 *  ORBHelper
 */

#include <lokiSingleton.h>
#include <ace/Thread_Manager.h>
#include <tao/ORB.h>
    
/**
 * The class ORBHelper is used to create and start an Object Request Broker.
 * This is useful because the ORB will be spawned in a separate thread.
 * Primarily this class is used internally by ACS and most likely is not 
 * particularly useful elsewhere.
 * 
 * TODO:
 * - move this class to the acsutil module
 * - check for memory leaks
 * - set the static ORB from maci/baci/etc
 */
class ORBHelper
{
  public:
    /**
     * getORB()
     * This method merely returns the ORB. If no other object within the 
     * system has set the ORB, this method will implicitly create an ORB
     * @return A pointer to the orb.
     */
    static CORBA::ORB_ptr 
    getORB();

    /**
     * setORB()
     * This method sets the ORB. If the ORB has been set before, the call 
     * to setORB is ignored. Otherwise, a private member variable is set to
     * the parameter (without using CORBA::ORB::_duplicate()).
     * @return 
     */
    static void
    setORB(CORBA::ORB_ptr);
    
  protected:
    /**
     * Typedef defining a singleton ORBHelper.
     */
    typedef Loki::SingletonHolder<ORBHelper, 
				  Loki::CreateUsingNew, 
				  Loki::PhoenixSingleton, 
				  Loki::SingleThreaded> ORBHelperSingleton;

    /**
     * Default Constructor
     * This constructor uses default parameters (NameService and 
     * NotifyFactory dynamically generated using the hostname).  Should be used
     * only when this object will be instantiated on the same host as manager.
     */
    ORBHelper();

    /**
     * Destructor
     * Destroys orb_mp.
     */
    ~ORBHelper();

    /////////////////////////////////////////////////////////////////////////////
  private:
    /** 
     * Run the orb in a thread because orb_mp->run() is a blocking call.
     * @param pThis a pointer to "this".
     */
    static void
    runOrbThread(void *pThis);

    /**
     * The ORB set by some other library or created by this class.
     */
    static CORBA::ORB_ptr orb_mp;
    
    /**
     * Used to spawn a thread for running orb_mp.
     */
    ACE_Thread_Manager *threadManager_mp;

    /**
     * Used to see if the thread has started running the ORB yet.
     */
    bool orbRunYet_m;

    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const ORBHelper&);

    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    ORBHelper(const ORBHelper&);

    /**
     * As a result of this class's constructor being made protected,
     * it is necessary to friend the appropriate Loki function.
     */
    friend ORBHelper* Loki::CreateUsingNew<ORBHelper>::Create();

    /**
     * As a result of this class's destructor being made protected,
     * it is necessary to friend the appropriate Loki function.
     */
    friend void Loki::CreateUsingNew<ORBHelper>::Destroy(ORBHelper*);
};

#endif
