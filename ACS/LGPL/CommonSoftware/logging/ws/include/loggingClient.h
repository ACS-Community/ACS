#ifndef loggingClient_H
#define loggingClient_H

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
* "@(#) $Id: loggingClient.h,v 1.44 2008/09/29 08:36:42 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2001-07-12  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <string>

#include <acsutil.h>

#include <orbsvcs/CosNamingC.h>

#include <orbsvcs/CosNotifyChannelAdminS.h>
#include <orbsvcs/CosNotifyCommC.h>

#include "logging_idlC.h"

#define LOG_BIN_TYPE 0
#define LOG_XML_TYPE 1
// The syslog facility to use while logging messages throwgh the
// kernel logger
// Its value depends on the running ACS_INSTANCE
// For instance 0-4 we use LOG_LOCAL0 to LOG_LOCAL4
// Messages for all the other instances are written in LOG_LOCAL5
//
// All the messages are logged at level LOG_INFO
int syslogFacility;
  	
// Return the syslog facility depending on the running ACS instance
int getSyslogFacility();

// If true the output is redirected to the syslog
bool toSyslog;

// The name of the channel to connect to
std::string channelName;

FILE * outputFile;
std::string fileName;
bool toFile=false;

// Read the command line params into local variables
void getParams(int argc, char *argv []);

// Prints the usage message on the stdout
void printUsage(const char* prgName);

// Write a message to the kernel log
void writeSyslogMsg(const char* msg);

class ACSStructuredPushConsumer;

class Subscribe
{
    // = TITLE
    //   Subscribe
    // = DESCRIPTION
    //   Shows how consumers subscribe for events.
    
  public:
    // = Initialization and Termination
    Subscribe (void);
    ~Subscribe ();
    
    void init (int argc, char *argv [], std::string channel);
    // Init the Client.
    
    void run ();
    // Run the demo.
    
    void shutdown ();
    // Shutdown the logging client.
    
  protected:
    void init_ORB (int argc, char *argv []);
    // Initializes the ORB.
    
    void resolve_naming_service ();
    // Try to get hold of a running naming service.
    
    void resolve_notify_channel (const char * channel_name);
    // Try to resolve the Notify Channel from the Naming service.
    
    void create_consumeradmin ();
    // Create the Consumer Admin.
    
    void create_consumers ();
    // Create and initialize the consumers.
    
    void setup_events ();
    // Setup events

    void teardown_events();
    // Tear down (remove) events
    
    // = Data Members
    PortableServer::POA_var root_poa_;
    // Reference to the root poa.
    
    CORBA::ORB_var orb_;
    // The ORB that we use.
    
    CosNaming::NamingContext_var naming_context_;
    // Handle to the name service.
    
    CosNotifyChannelAdmin::EventChannel_var ec_;
    // The one channel that we create using the factory.
    
    CosNotifyChannelAdmin::InterFilterGroupOperator ifgop_;
    // The group operator between admin-proxy's.
    
    CosNotification::QoSProperties initial_qos_;
    // Initial qos specified to the factory when creating the EC.
    
    CosNotification::AdminProperties initial_admin_;
    // Initial admin props specified to the factory when creating the EC.
    
    CosNotifyChannelAdmin::ConsumerAdmin_var consumer_admin_;
    // The consumer admin used by consumers.
    
    ACSStructuredPushConsumer* consumer_;
    // The consumer
};

/*****************************************************************/

class ACSStructuredPushConsumer : public POA_CosNotifyComm::StructuredPushConsumer,
				  public PortableServer::RefCountServantBase
{
    // = TITLE
    //   ACSStructuredPushConsumer
    //
    // = DESCRIPTION
    //   Structured Push Consumer
    //
    
  public:
    // = Initialization and Termination code
    ACSStructuredPushConsumer (Subscribe* subscribe);
    // Constructor.
    
    void connect (CosNotifyChannelAdmin::ConsumerAdmin_ptr consumer_admin);
    // Connect the Consumer to the EventChannel.
    // Creates a new proxy supplier and connects to it.
    
    virtual void disconnect ();
    // Disconnect from the supplier.
    
    CosNotifyChannelAdmin::StructuredProxyPushSupplier_ptr get_proxy_supplier (void);
    // Accessor for the Proxy that we're connected to.
    
  protected:
    // = Data members
    CosNotifyChannelAdmin::StructuredProxyPushSupplier_var proxy_supplier_;
    // The proxy that we are connected to.
    
    CosNotifyChannelAdmin::ProxyID proxy_supplier_id_;
    // The proxy_supplier id.
    
    Subscribe* subscribe_;
    // callback <done>
    
    
    // = Methods
    virtual ~ACSStructuredPushConsumer (void);
    // Destructor
    
    /*
    * = NotifyPublish method
    * \throw CosNotifyComm::InvalidEventType
    */
    virtual void offer_change(const CosNotification::EventTypeSeq & added,
			      const CosNotification::EventTypeSeq & removed);
    
    /*
    * = StructuredPushSupplier methods
    * \throw CosEventComm::Disconnected
    */
    virtual void push_structured_event (const CosNotification::StructuredEvent & notification);
    
    virtual void disconnect_structured_push_consumer ();

    bool m_logBin;
	static ACE_TCHAR* m_LogEntryTypeName[];	
};

#endif /* loggingClient_H */

