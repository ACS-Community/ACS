#ifndef ACSNC_ORB_H
#define ACSNC_ORB_H
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
* "@(#) $Id: acsncORBHelper.h,v 1.49 2008/10/09 07:57:41 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-09-23 added doxygen comments
* david  20/09/02  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baci.h>
#include <baciDB.h>
#include <maciHelper.h>
#include <ACSErrTypeCommon.h>

// using namespace baci;
// using namespace maci;

// using namespace ACSErrTypeCommon;


/** @file acsncORBHelper.h
 *  ORBHelper
 */

namespace nc {

    
/**
 * The class ORBHelper is used to create and start an Object Request Broker.
 * This is useful because the ORB will be spawned in a separate thread.  Two 
 * different initialization options are available: one using a default setup
 * and another passing parameters to the orb (as argc and argv respectively).
 * 
 * TODO:
 * - move this class to the acsutil module
 * - check for memory leaks
 */
class ORBHelper
{
  public:
    
    /**
     * Default Constructor
     * This constructor uses default parameters (NameService and 
     * NotifyFactory dynamically generated using the hostname).  Should be used
     * only when this object will be instantiated on the same host as manager.
     */
    ORBHelper();

    /**
     * Constructor
     * This constructor assumes all parameters needed by the ORB are passed 
     * directly in argc and argv.  This constructor is much more powerful than
     * the default and should normally be used.  A sample usage would be:
     * argc=4
     * argv={"", 
     *       "-ORBInitRef NameService=corbaloc::hostname:xxxx/NameService",
     *       "-ORBDottedDecimalAddresses=1",
     *       "-ORBInitRef NotifyEventChannelFactory=corbaloc::hostname:xxxx/NotifyEventChannelFactory"};
     */
    ORBHelper(int argc, char *argv[]);

    /**
     * Destructor
     * Destroys orb_mp.
     */
    virtual ~ORBHelper();

    /**
     *  getORB()
     *  This method merely returns the ORB.
     *  @return A pointer to the orb spawned by this class.
     *  @htmlonly
        <br><hr>
        @endhtmlonly
     */
    CORBA::ORB_ptr 
    getORB() const { return orb_mp; }

    /**
     * Runs the orb in a separate thread.
     * This has to be called manually!
     * @return void
     * @throw ACSErrTypeCommon::CouldntCreateThreadEx
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void 
    runOrb();
    
    /////////////////////////////////////////////////////////////////////////////
  private:
    /**
     * Called by ORBHelper()
     * It also initializes CORBA references, gets POA & POA manager, activates POA mgr.
     * @return void
     * @throw ACSErrTypeCommon::CORBAProblemEx
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void 
    init_ORB();

    /**
     * Called by ORBHelper(int argc, char *argv[])
     * It also initializes CORBA references, gets POA & POA manager, activates POA mgr.
     * @param argc Number of arguments to CORBA::ORB_init(...)
     * @param argv Arguments to CORBA::ORB_init(...)
     * @throw ACSErrTypeCommon::CORBAProblemEx
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void 
    init_ORB(int argc, 
	     char *argv[]);

    /** 
     * Run the orb in a BACI thread because orb_mp->run() is a blocking call.
     * Called by runOrb.
     * @param pThis a pointer to "this".
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    static void *
    runOrbThread(void *pThis);

    /**
     * The orb created by this class
     */
    CORBA::ORB_ptr orb_mp;
    
    /**
     * Used to spawn a thread for running orb_mp.
     */
    baci::BACIThreadManager *threadManager_mp;

    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const ORBHelper&);

    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    ORBHelper(const ORBHelper&);

};
 }; 

#endif /*!ACSNC_ORB_H*/



