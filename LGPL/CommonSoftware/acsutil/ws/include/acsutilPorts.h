#ifndef ACSUTIL_PORTS_H
#define ACSUTIL_PORTS_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) Associated Universities Inc., 2003 
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
* "@(#) $Id: acsutilPorts.h,v 1.8 2009/09/28 08:40:13 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david  2003-07-04  created
*/
////////////////////////////////////////////////////////////////////////
#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif
////////////////////////////////////////////////////////////////////////
#include "acsutil.h"
#include <sstream>
////////////////////////////////////////////////////////////////////////
/** @file acsutilPorts.h
 *  Header file ACSPorts class.
 */

/**
 * @class ACSPorts
 * ACSPorts is a utility class providing static methods to access the port
 * numbers for various CORBA and ACS Services.
 */
class ACSPorts
{
  public:
    ////////////////////////////////////////////////////////////////////////
    /** Constructor
     */
    ACSPorts(){};
    /** Destructor
     */
    virtual ~ACSPorts(){};

    /**
     * @return the "ACS Base Port".  This is just an integer ranging from 0-9.
     * @htmlonly
     <br><hr>
     @endhtmlonly
     */
    static const unsigned long
    getBasePort();
	
    /**
     * @return the port manager is running on.
     * @htmlonly
     <br><hr>
     @endhtmlonly
     */
    static std::string
    getManagerPort(int baseport);

    static std::string
    getManagerPort();
    
    /**
     * @return the port the CORBA Naming Service is running on.
     * @htmlonly
     <br><hr>
     @endhtmlonly
     */
    static std::string
    getNamingServicePort(int baseport);
    
    static std::string
    getNamingServicePort();
    
    /**
     * @return the port the CORBA Notification Service is running on.
     * @htmlonly
     <br><hr>
     @endhtmlonly
     */
    static std::string
    getNotifyServicePort(int baseport, const char *name = NULL);
    
    static std::string
    getNotifyServicePort();
    
    /**
     * @return the port the CORBA Logging Service is running on.
     * @htmlonly
     <br><hr>
     @endhtmlonly
     */
    static std::string
    getLoggingServicePort(int baseport);
    
    static std::string
    getLoggingServicePort();
    
    /**
     * @return the port the CORBA Interface Repository is running on.
     * @htmlonly
     <br><hr>
     @endhtmlonly
     */
    static std::string
    getIRPort(int baseport);
    
    static std::string
    getIRPort();
    
    /**
     * @return the port the ACS Logging Service is running on.
     * @htmlonly
     <br><hr>
     @endhtmlonly
     */
    static std::string
    getLogPort(int baseport);
    
    static std::string
    getLogPort();
    
    /**
     * @return the port the ACS CDB is running on.
     * @htmlonly
     <br><hr>
     @endhtmlonly
     */
    static std::string
    getCDBPort(int baseport);

    static std::string
    getCDBPort();

    /**
     * @return the port the ACS Alarm Service is running on.
     * @htmlonly
     <br><hr>
     @endhtmlonly
     */
    static std::string
    getAlarmServicePort(int baseport);

    static std::string
    getAlarmServicePort();

    /**
     * @return the port the ACS Container Daemon is running on.
     * @htmlonly
     <br><hr>
     @endhtmlonly
     */
    static std::string
    getContainerDaemonPort();

    /**
     * @return the port the ACS Services Daemon is running on.
     * @htmlonly
     <br><hr>
     @endhtmlonly
     */
    static std::string
    getServicesDaemonPort();

    /**
     * @return the stringified IP address of this host.
     * @htmlonly
     <br><hr>
     @endhtmlonly
     */
    static const char*
    getIP();
    
  private:
    ////////////////////////////////////////////////////////////////////////
    /** Copy not allowed.
     */
    ACSPorts(const ACSPorts&);

    /** Assignment not allowed.
     */
    void operator= (const ACSPorts&);
};

#endif
