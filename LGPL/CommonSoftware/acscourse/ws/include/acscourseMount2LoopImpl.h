#ifndef acscourseMount2LoopImpl_h
#define acscourseMount2LoopImpl_h
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
* "@(#) $Id: acscourseMount2LoopImpl.h,v 1.5 2006/03/24 12:54:38 vwang Exp $"
*
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acscourseMount2Impl.h>

/** @file acscourseMount2Impl.h
 */

/** @defgroup ACSCOURSEMOUNTDOC Mount2
 *  @{
 * @htmlonly
<hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
The class Mount2Loop simulates the behaviour of an antenna interface
implementing a control loop in a thread.
The Component is implementing by inheriting from the previous 
Mount2 exercise and adding the thread on top of that implementation.
<br>
<h2>What can I gain from this example?</h2>
<ul>
  <li>an example derived from the ACS::Component IDL interface.</li>
  <li>understanding of synchronous method implementation.</li>
  <li>usage of ACS Thread classes.</li>
</ul>
<br>
<br>
<h2>Links</h2>
<ul>
  <li><a href="classMount2LoopImpl.html">Mount2 Class Reference</a></li>
  <li><a href="interfaceMOUNT__ACS_1_1Mount2.html">Mount2 IDL Documentation</a></li>
  <li>Mount2 CDB XML Schema</li>
</ul>
</div>
 * @endhtmlonly
 * @}
 */

class Mount2LoopImpl;

/** @class Mount2
 * The class PositionControlThread is a basic example of 
 * a thread implementation class.
 * The thread is logically owned by the Mount2LoopImpl
 * ACS Component that contains it.
 * The thread class overrides the runLoop() method to implement a control loop
 * function that is periodically executed.
 * In order to get access to the Mount2LoopImpl to read and set
 * the values of the properties in the loop, it
 * takes a pointer to it in the constructor and it is a friend class.
 * @version "@(#) $Id: acscourseMount2LoopImpl.h,v 1.5 2006/03/24 12:54:38 vwang Exp $"
 */
class PositionControlThread :public ACS::Thread
{
  public:
    /**
     * Destructor
     * @param name threads's name.
     * @param mount_p pointer to the containing Component
     * @param suspended see ACS::Thread documentation
     * @param responseTime see ACS::Thread documentation
     * @param sleepTime see ACS::Thread documentation
     */
    PositionControlThread(const ACE_CString& name, 
		  Mount2LoopImpl *mount_p, 
		  const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime, 
			  const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime);
    /**
     * Destructor
     */
    ~PositionControlThread();

    /**
     * Thread method invoqued at any iteration of the thread loop
     * Is the real thread performer.
     */
    virtual void runLoop();

  protected:
    /**
     * Data member used to store the pointer to the Mount2LoopImpl
     * component implentation that owns the thread.
     * Received from the constructor and used in the runLoop()
     */
    Mount2LoopImpl *mount_p;
};

/** @class Mount2
 * The class Mount2 is a basic example of a component and simulates the behaviour of an antenna interface.
 * It provides one asynchronous methods: objfix.  The methods only writes the data into 
 * virtual properties.
 * @version "@(#) $Id: acscourseMount2LoopImpl.h,v 1.5 2006/03/24 12:54:38 vwang Exp $"
 */
class Mount2LoopImpl: public Mount2Impl
{
  public:
     /**
     * Constructor
     * @param name component's name. This is also the name that will be used to find the
     * @param containerServices the container services object for this component
     */
    Mount2LoopImpl(const ACE_CString &name, maci::ContainerServices *containerServices);
    
    /**
     * Destructor
     */
    virtual ~Mount2LoopImpl();
    
    

  private:
    
    /**
     * ALMA C++ coding standards state copy operators should be disabled.
     */
    void operator=(const Mount2Impl&);

    /**
     * The PositionControlThread class needs to access
     * the private and protected  members of this class, 
     * therefore it is given friend status
     */
    friend class PositionControlThread;
};

#endif /*!acscourseMount2Impl_H*/



