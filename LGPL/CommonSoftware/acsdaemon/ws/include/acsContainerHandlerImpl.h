#ifndef _ACS_CONTAINER_HANDLER_IMPL_H_
#define _ACS_CONTAINER_HANDLER_IMPL_H_

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
* "@(#) $Id: acsContainerHandlerImpl.h,v 1.7 2008/10/27 21:11:23 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran 2006-06-21 created 
* agrimstr 2007-11-07 refactored Container interface into separate
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
#include <acsdaemonErrType.h>
#include <ACSErrTypeCommon.h>
#include <acsutilPorts.h>

class ACSContainerHandlerImpl : public POA_acsdaemon::ContainerDaemon {

  public:
    
   /**
    * Constructor
    */
    ACSContainerHandlerImpl();
  
    /**
     * Destructor
     */
    virtual ~ACSContainerHandlerImpl();

    /**
     * Sets ACS Daemon service
     */
    void setService(ACSDaemonServiceImpl<ACSContainerHandlerImpl> *service)
    {
	h_service = service;
    }

    /**
     * Initialize handler
     */
    void initialize(CORBA::ORB_ptr orb) {}

    /**
     * Dispose handler
     */
    void dispose(CORBA::ORB_ptr orb) {}

    /**
     * Get the name of this container handler
     */
    std::string getName();

    /**
     * Get the type string of this container handler
     */
    std::string getType();
    
    /**
     * Return the port where this container handler listens for connections
     */
    std::string getPort();

    /*************************** CORBA interface *****************************/

    virtual void start_container (
        const char * container_type,
        const char * container_name,
        ::CORBA::Short instance_number,
        const ::ACS::stringSeq & type_modifiers,
        const char * additional_command_line
      )
      ACE_THROW_SPEC ((
        CORBA::SystemException,
        ::acsdaemonErrType::FailedToStartContainerEx,
	::ACSErrTypeCommon::BadParameterEx
      ));
    virtual void stop_container (
        const char * container_name,
        ::CORBA::Short instance_number,
        const char * additional_command_line
      )
      ACE_THROW_SPEC ((
        CORBA::SystemException,
        ::acsdaemonErrType::FailedToStopContainerEx,
	::ACSErrTypeCommon::BadParameterEx
      ));
    virtual void shutdown ()
      ACE_THROW_SPEC ((
        CORBA::SystemException,
        ::maciErrType::NoPermissionEx
      ));

  private:
    std::string h_name; // Name of container handler (used for logging purposes
    std::string h_type; // CORBA-type for this container handler
    ACSDaemonServiceImpl<ACSContainerHandlerImpl> *h_service; // ACS daemon service

};



#endif
