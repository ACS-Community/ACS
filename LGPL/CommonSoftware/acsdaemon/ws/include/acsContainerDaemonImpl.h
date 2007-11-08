#ifndef _ACS_CONTAINER_DAEMON_IMPL_H_
#define _ACS_CONTAINER_DAEMON_IMPL_H_

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
* "@(#) $Id: acsContainerDaemonImpl.h,v 1.2 2007/11/08 19:18:44 agrimstrup Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran 2006-06-21 created 
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "acsdaemonS.h"
#include "logging.h"

class ACSContainerDaemonImpl : public POA_acsdaemon::ContainerDaemon {

  public:
    
   /**
    * Constructor
    */
    ACSContainerDaemonImpl(LoggingProxy &logProxy);
  
    /**
     * Destructor
     */
    virtual ~ACSContainerDaemonImpl();
    
    /**
     * Initalization status
     */
    bool 
    isInitialized() { return m_isInitialized; }
    
    /**
     * Initializes the daemon.
     */
    int
    startup (int argc, char *argv[]);

    /**
     * Run the daemon.
     * @return Returns 0 on success, -1 on error.
     */
    int 
    run ();

    /**
     * Shutdown the daemon.
     */
    void shutdown (); 

    /**
     * Get CORBA IOR.
     */
    const char* getIOR() const { return m_ior.in(); };
    
    /*************************** CORBA interface *****************************/

    virtual void start_container (
        const char * container_type,
        const char * container_name,
        ::CORBA::Short instance_number,
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

  protected:

    /**
     *  initialize the ORB.
     */
    int 
    init_ORB (int& argc, char *argv []);

    //--Common data members-------------------------------------

    /** Initialization status */
    bool m_isInitialized;

    /** The ORB that we use. */
    CORBA::ORB_var m_orb;

    /** Logging. proxy **/
    LoggingProxy &m_logProxy;

    /** CORBA IOR **/
    CORBA::String_var m_ior;


};

#endif
