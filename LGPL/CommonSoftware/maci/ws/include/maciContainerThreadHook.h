#ifndef _maci_container_thread_hook_H_
#define _maci_container_thread_hook_H_
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2005
*
*This library is free software; you can redistribute it and/or
*modify it under the terms of the GNU Lesser General Public
*License as published by the Free Software Foundation; either
*version 2.1 of the License, or (at your option) any later version.
*
*This library is distributed in the hope that it will be useful,
*but WITHOUT ANY WARRANTY; without even the implied warranty of
*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*Lesser General Public License for more details.
*
*You should have received a copy of the GNU Lesser General Public
*License along with this library; if not, write to the Free Software
*Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: maciContainerThreadHook.h,v 1.3 2012/01/21 22:48:11 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2005-01-03  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <ace/Thread_Hook.h>

namespace maci
{

/**
 * @class ContainerThreadHook
 *
 * @brief Container Thread Start-up hook.
 *
 * ContainerThreadHook class provides the default thread initialization
 * for all threads that are going to be created inside the container.
 * It implements the start() method from ACE_Thread_Hook, which is called before the thread is executed.
 * The default thread initialization initializes the logging system.
 * If someone wants to change the default thread initialization, she/he has to override the initThread() method and instatiate the object. After this all threads will be initialized using new functionally.
 * It is recommended that ContainerThreadHook::initThread() method is called from the overridden method to get logging system initialized.
 * This hook is set by creating ContainerThreadHook object at the container instantiation.
 */
class ContainerThreadHook : public ACE_Thread_Hook
{
  public:

    /**
       Constructor of ContainerThreadHook which registers the hook
     */

    ContainerThreadHook() { ACE_Thread_Hook::thread_hook(this); }

    /**
     * virtual destructor of ContainerThreadHook
     */
    virtual ~ContainerThreadHook(){}

    /**
     * Actual implementation of thread initialization, which provides the logging system initialization.
     * This function is called from the start() method.
     * If someone wants to change default threads' start-up functionallty he/she has to override this method.
     * It is recommended that overriden method calls this one, i.e. ContainerThreadHook::initThread();
     * @param arg parameter that is passed to the thread
     */
    virtual void initThread(void *arg);

    /**
     * Implementation of virtual start method from ACE_Thread_Hook, which is called before the thread is executed.
     * The implemntation just calls initThread.
     * @param func thread function
     * @param arg thread parameter
     */
    virtual ACE_THR_FUNC_RETURN start(ACE_THR_FUNC func, void* arg)
	{
	    initThread(arg); // call actual thread initalization
	    return (*func)(arg); // call thread function
	}
};

}; //namespace maci

#endif /*!_H*/
