#ifndef _STATIC_CONTAINER_H
#define _STATIC_CONTAINER_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2004 
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
* "@(#) $Id: taskStaticContainer.h,v 1.10 2012/05/02 16:09:52 acaproni Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2004-09-27  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <memory>

#include <maciContainerImpl.h>
#include <maciLibraryManager.h>
#include "AlarmSourceThread.h"
#include <acserr.h>
#include <ace/ARGV.h>

class StaticContainer
{
  public:
    StaticContainer();

    void init(int argc, char **argv, const char *containerName=0 );
    void done();

    CORBA::Object_ptr createComponentWithName(const char *name);

    CORBA::Object_ptr createComponent(const char *libname);

    CORBA::Object_ptr createComponent(const char* name, const char *libname);

    void destroyComponent(CORBA::Object_ptr obj);

    void executeRunCmd(const char* param);  // this should not be part of generic static container
  protected:

    void initCORBA(int &argc, char **argv);
    void doneCORBA();

    maci::ContainerImpl container_m;
    maci::LibraryManager dllmgr_m;

    LoggingProxy *m_logger;
    bool services_m;
    
    CORBA::ORB_var orb_m;
    PortableServer::POAManager_var poaManager_m;
    PortableServer::POA_var poaRoot_m;
    PortableServer::POA_var componentPOA_m;

//    ACE_CString libName;
    ACE_CString containerName_m;
    ACE_CString componentName_m;

    ACE_ARGV containerArgv;

    // The thread for the alarm source
    std::auto_ptr<acsalarm::AlarmSourceThread> m_alarmThread_ap;

};//class StaticContainer


#endif /*!_H*/

