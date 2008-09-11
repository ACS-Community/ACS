#ifndef _ACS_SERVICES_HANDLER_IMPL_H_
#define _ACS_SERVICES_HANDLER_IMPL_H_

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
* "@(#) $Id: acsServicesHandlerImpl.h,v 1.6 2008/09/11 09:29:56 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran 2006-06-21 created 
* agrimstr 2007-11-07 refactored Services interface into separate
*                     class for use in template pattern implementation
*                     of the acsdaemon
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "acsdaemonS.h"
#include "logging.h"
#include "acsDaemonImpl.h"
#include <acserr.h>
#include <ACSErrTypeOK.h>
#include <acsdaemonErrType.h>
#include <ACSErrTypeCommon.h>
#include <acsutilPorts.h>
#include <acsThread.h>
#include "acsThreadManager.h"
#include <ace/Synch.h>
#include <queue>

struct Request
{
    acsdaemon::DaemonCallback_ptr cb;
    const char *cmd;
    Request(acsdaemon::DaemonCallback_ptr icbp, const char *icmd) :
	cb(icbp), cmd(icmd) {}
};

class CommandProcessorThread : public ACS::Thread
{
  public:
    CommandProcessorThread(const ACE_CString &name,
	       const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime, 
	       const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime) :
	ACS::Thread(name, responseTime, sleepTime, false, THR_DETACHED) 
	{
	    ACS_TRACE("ACSServicesHandlerImpl::CommandProcessorThread"); 
	    m_mutex = new ACE_Thread_Mutex();
	    m_wait = new ACE_Condition<ACE_Thread_Mutex>(*m_mutex);
	}
    
    ~CommandProcessorThread() 
	{ 
	    ACS_TRACE("ACSServicesHandlerImpl::~CommandProcessorThread"); 
	}
    
    void onStart();

    void stop();

    void exit() { ACS::Thread::exit(); stop(); }

    void runLoop()
      ACE_THROW_SPEC ((
        CORBA::SystemException,
	::ACSErrTypeCommon::BadParameterEx
      ));

    void addRequest(Request* r);

  private:
    ACE_Thread_Mutex *m_mutex;
    ACE_Condition<ACE_Thread_Mutex> *m_wait;
    std::queue<Request*> pending;
    volatile bool running;
};


class ACSServicesHandlerImpl : public POA_acsdaemon::ServicesDaemon {

  public:
    
   /**
    * Constructor
    */
    ACSServicesHandlerImpl();
  
    /**
     * Destructor
     */
    virtual ~ACSServicesHandlerImpl();

    /**
     * Sets ACS Daemon service
     */
    void setService(ACSDaemonServiceImpl<ACSServicesHandlerImpl> *service)
    {
	h_service = service;
    }

    /**
     * Initialize handler
     */
    void initialize(CORBA::ORB_ptr orb);

    /**
     * Dispose handler
     */
    void dispose(CORBA::ORB_ptr orb);

    /**
     * Get the name of this container handler
     */
    std::string getName();

    /**
     * Get the type string of this container handler
     */
    std::string getType();
    
    /**
     * Return the port where this services handler listens for connections
     */
    std::string getPort();
    
    /*************************** CORBA interface *****************************/

    void start_acs (
	acsdaemon::DaemonCallback_ptr callback,
        ::CORBA::Short instance_number,
        const char * additional_command_line
      )
      ACE_THROW_SPEC ((
        CORBA::SystemException,
	::ACSErrTypeCommon::BadParameterEx
      ));
    
    void stop_acs (
	acsdaemon::DaemonCallback_ptr callback,
        ::CORBA::Short instance_number,
        const char * additional_command_line
      )
      ACE_THROW_SPEC ((
        CORBA::SystemException,
	::ACSErrTypeCommon::BadParameterEx
      ));

     virtual char * status_acs ( 
	 ::CORBA::Short instance_number
	 )
      ACE_THROW_SPEC ((
        CORBA::SystemException,
        ::acsdaemonErrType::FailedToGetAcsStatusEx
      ));

    virtual void shutdown ()
      ACE_THROW_SPEC ((
        CORBA::SystemException,
        ::maciErrType::NoPermissionEx
      ));

  private:
    std::string h_name; // Name of services handler (used for logging purposes
    std::string h_type; // CORBA-type for this services handler
    ACSDaemonServiceImpl<ACSServicesHandlerImpl> *h_service; // ACS daemon service
    CommandProcessorThread *cmdproc;
    ACS::ThreadManager tm;
};



#endif
