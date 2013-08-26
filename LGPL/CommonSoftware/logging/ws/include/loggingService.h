#ifndef loggingService_H
#define loggingService_H

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
* "@(#) $Id: loggingService.h,v 1.43 2007/05/28 06:23:39 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram    2003-03-10  DsLogAdmin::LogFullAction -> DsLogAdmin::LogFullActionType, LogMgr_i -> TAO_LogMgr_i, BasicLog_i to TAO_BasicLog_i (TAO x.3)
* msekoran  2001-07-08  added implementation of sending logs to Notify Service
* msekoran  2001-06-17  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsutil.h>
#include <orbsvcs/CosNamingC.h>
#include <orbsvcs/DsLogAdminC.h>
#include <orbsvcs/CosNotifyChannelAdminS.h>

#include "loggingACSStructuredPushSupplier.h"
#include "loggingACSLogFactory_i.h"
#define LOG_BIN_TYPE 0
#define LOG_XML_TYPE 1

/**
 * Implementation of the Telecom Log Service
 */
class LoggingService
{   
  public:
    
    //--Initialization and termination methods-----------------
    /**
     * Constructor
     */
    LoggingService();
    
    /**
     * Destructor
     */
    virtual ~LoggingService();
    
    /**
     * Initalization status
     */
    bool 
    isInitialized() { return m_isInitialized; }
    
    /**
     * Parses the command line arguments.
     * @return  Returns 0 on success, -1 on error.
     */
    int 
    parse_args (int argc, char *argv []);

    /**
     * Initializes the Telecom Log Service.
     */
    void 
    startup (int argc, char *argv[]);

    /**
     * run the Telecom Log Service.
     * @return Returns 0 on success, -1 on error.
     */
    int 
    run ();

    /**
     * Shutdown the Telecom Log Service.
     */
    void shutdown (); 
    

  protected:

    /**
     *  initialize the ORB.
     */
    void 
    init_ORB (int& argc, char *argv []);

    /**
     * Resolve the naming service.
     */
    void 
    resolve_naming_service ();
    
    /**
     * Try to resolve the Notify factory from the Naming service.
     */
    void resolve_notify_factory ();

    /**
     * Create an EC.
     */
    void create_EC ();

    /**
     * Create the Supplier Admin.
     */
    void 
    create_supplieradmin();

    /**
     * create and initialize the suppliers.
     */
    void 
    create_suppliers(); 

    /**
     * Create the Basic Log Factory
     */
    void 
    create_basic_log_factory();

    /**
     * Create the Basic Log
     */
    void 
    create_basic_log();


    //--Notify Service data members-------------------------------

    /** Channel factory */
    CosNotifyChannelAdmin::EventChannelFactory_var m_notify_factory;
    
    /** The logging channel that we create using the factory. */
    CosNotifyChannelAdmin::EventChannel_var m_logging_ec;

    /** Initial qos specified to the factory when creating the EC. */
    CosNotification::QoSProperties m_initial_qos;

    /** Initial admin props specified to the factory when creating the EC. */
    CosNotification::AdminProperties m_initial_admin;

    /** The group operator between admin-proxy's. */
    CosNotifyChannelAdmin::InterFilterGroupOperator m_ifgop;

    /** The logging supplier admin used by suppliers. */
    CosNotifyChannelAdmin::SupplierAdmin_var m_logging_supplier_admin;

    /** The loggingg supplier */
    ACSStructuredPushSupplier* m_logging_supplier;
    

    //--Logging Service data members----------------------------

    /** The Log Factory name. */
    const char* m_basic_log_factory_name;

    /** The Log name. */
    const char* m_basic_log_name;
    
    /** The Basic Log Factory. */
    ACSLogFactory_i m_basic_log_factory;

    /** The Basic Log  */
    DsLogAdmin::BasicLog_var m_basic_log;


    //--Common data members-------------------------------------

    bool m_logBin;

    /** Initialization status */
    bool m_isInitialized;

    /** The ORB that we use. */
    CORBA::ORB_var m_orb;

    /**  Reference to the root poa. */
    PortableServer::POA_var m_poa;

    /** A naming context. */
  CosNaming::NamingContext_var m_naming_context;
};
#endif /* loggingService_H */



// ************************************************************************
//
// REVISION HISTORY:
//
// $Log: loggingService.h,v $
// Revision 1.43  2007/05/28 06:23:39  cparedes
// Adding the new alternate method to log binaries
//
// Revision 1.42.14.2  2007/04/03 07:46:03  cparedes
// Changing from ACS_LOG_TYPE to ACS_LOG_BIN
//
// Revision 1.42.14.1  2007/03/05 06:16:24  cparedes
// First attempt, work well with old things, but seg fault with the new things. To debug
//
// Revision 1.42  2006/01/28 00:03:51  dfugate
// The LoggingChannel is now created using the LoggingNotifyEventChannelFactory instead of NotifyEventChannelFactory.
//
// Revision 1.41  2005/09/12 19:02:15  dfugate
// Stripped out all code dealing with the archiving channel EXCEPT that used to
// create the notification channel.
//
// Revision 1.40  2005/09/12 17:57:06  dfugate
// Converted plain C++ comments to Doxygen-style.
// Split loggingService.h into four headers (three new) as it's an ALMA C++
// coding violation to have more than one class declaration per header.
//
// Revision 1.39  2005/09/09 21:33:45  dfugate
// Decoupled a generic event supplier from loggingService.cpp.
//
// Revision 1.38  2003/10/24 19:27:07  dfugate
// Fixed a few serious bugs and now use native exceptions.
//
// Revision 1.37  2003/10/23 07:39:09  acaproni
// True native exception handling. No more extra parameters
//
// Revision 1.36  2003/07/28 09:46:57  bjeram
// modification for native exception
//
// Revision 1.35  2003/03/14 10:24:37  rgeorgie
// LGPL
//
// Revision 1.34  2003/03/10 14:29:29  bjeram
// changes according to the changes in TAO x.3
//
// Revision 1.33  2002/09/23 12:43:04  vltsccm
// msekoran: loggingXMLParser fixed, memory leak removed and tat test added.
// ************************************************************************




