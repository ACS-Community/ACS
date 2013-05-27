#ifndef SUPPLIER_H
#define SUPPLIER_H

/* @(#) $Id: acsncSupplier.h,v 1.67 2011/11/17 23:31:54 javarias Exp $
 *
 *    Supplier Abstract base class for notification channel push structured event
 *    supplier.
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
 */

/** @file acsncSupplier.h
 *  Header file for event channel Supplier.
 */

#include "acsncHelper.h"
#include <acscomponentImpl.h>
#include "RepeatGuardLogger.h"
#include "acsncCircularQueue.h"

namespace nc {
/** 
 * Supplier provides an implementation of the structured event push supplier interface 
 * from the notification service (NC). It is designed to assist the developer in publishing
 * CORBA structured events to a given channel. Instances of Supplier can contact the notification
 * service using the Container, a SimpleClient, and various other means using the numerous
 * constructors provided. 
 * 
 * For those who "know what they're doing", Supplier can be used 
 * directly in lieu of the SimpleSupplier class to publish <b>entire</b> CORBA structured events 
 * (see the publishEvent() methods). This functionality should be used with extreme caution
 * if used at all.
 *
 * TODO:
 * - check for local and remote memory leaks
 * - clean-up the usage of typeName_mp somehow
 */
class Supplier : 
    protected Helper, 
    public POA_CosNotifyComm::StructuredPushSupplier,
    protected virtual PortableServer::RefCountServantBase
{
  public:
    /** 
     * Standard constructor - used within a component.
     * If this constructor is used, we use container's ORB to access Manager and then
     * retrieve a reference to the Naming Service.
     * @param channlName The name of the channel events will be published to.
     * @param component A reference to a component is needed for the Event 
     */
    Supplier(const char* channelName, 
	     acscomponent::ACSComponentImpl* component);

    /** 
     * Generic constructor.
     * This constructor is provided for API users who create their own ORB that has 
     * a reference to the Naming Service. SimpleClient provides such access to an ORB.
     * @param channlName The name of the channel events will be published to.
     * @param orb_mp ORB that <b>has</b> a valid reference to the Naming Service.
     * @param component A reference to a component is needed for the Event 
     */
    Supplier(const char* channelName, 
	     CORBA::ORB_ptr orb_mp, 
	     acscomponent::ACSComponentImpl* component);
    
    /** 
     * Optional constructor - used outside of ACS.
     * This constructor is very resource intensive (it spawns it's own ORB) and should 
     * only to be utilized when there is a reason not to use Manager to get at the 
     * Naming Service.  If argc==0, default parameters (i.e., environment variables) 
     * specify how to get to the Naming Service.
     * @param channlName The name of the channel events will be published to.
     * @param argc Number of ORB parameters in argv or 0
     * @param argv ORB params.  Typically something like:<br>
     *	orbArg[0] = ""<br>
     *  orbArg[1] = "-ORBDottedDecimalAddresses=1"<br>
     *  orbArg[2] = "-ORBInitRef NameService=corbaloc::host:xxxx/NameService"<br>
     *  @htmlonly
       Sample Usage:<br>
       <ul>
         <li><b><i>Supplier(names,0,(char **)0);</b></i> - Generates ORB arguments on the fly from environment variables. 
         <li><b><i>Supplier(names,argc,argv);</b></i> - Uses passed ORB arguments.
       </ul>
       @endhtmlonly
     * @param component A reference to a component is needed for the Event 
     */
    Supplier(const char* channelName, 
	     int argc, 
	     char *argv[],
	     acscomponent::ACSComponentImpl* component);

    /**
     * Called to disconnect the Supplier from the channel. Must be used instead of
     * deleting the object.
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual void 
    disconnect();

    /**
     * Call this to publish an entire CORBA structured event. This is a blocking call
     * which does not return control until the event has arrived whereever the 
     * Notification Service is actually running.
     * Makes a call to the push_structured_event() method of consumers.
	  * @throw CORBA::TRANSIENT In case that the Notification Service is down
	  * @throw nc::EventDroppedException In the case that que event buffer queue
	  * discard a meesage.
     * @throw acsncErrType::PublishEventFailureExImpl In case there's any other error
     * while trying to publish the event
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void 
    publishEvent(const CosNotification::StructuredEvent &event)
	;

    /**
     * Call this to publish a CORBA Any and implicitly have the structured event
     * "filled-out". It actually makes a call to the push_structured_event() 
     * method of consumers.
	  * @throw CORBA::TRANSIENT In case that the Notification Service is down
	  * @throw nc::EventDroppedException In the case that que event buffer queue
	  * discard a meesage.
     * @throw acsncErrType::PublishEventFailureExImpl In case there's any other error
     * while trying to publish the event
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual void 
    publishEvent(const CORBA::Any &eventData)
	;

    /**
     * <b>Do not under any circumstances invoke this method from your code!</b>
     * ACE/TAO (implementor of the Notification Service) is notorious about changing
     * the behaviour of the underlying NC CORBA objects with each release. As of 
     * ACS 3.1, this method disconnects all consumers from the channel!
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual void 
    disconnect_structured_push_supplier();
    
    /** 
     * Called by supplier's consumer proxy object to inform us of changes
     * to the subscription information by the notification channel's consumers.
     * Essentially, this is called each time a consumer subscribes to the NC or
     * disconnects. <b>Do not call it from your code!</b>
     * @param eventsAdded Event types that consumers want to see.
     * @param eventsRemoved Event types consumers no longer care about.   
     * @throw CosNotifyComm::InvalidEventType
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual void 
    subscription_change(const CosNotification::EventTypeSeq &eventsAdded,
			const CosNotification::EventTypeSeq &eventsRemoved);

    /**
      * Override of reconnect method
      * @see acsnc::Helper::reconnect
      */
    void reconnect(::NotifyMonitoringExt::EventChannelFactory *ecf);

    void setAntennaName(std::string antennaName);

  protected:
    /**
     * Destructor is protected.
     */
    virtual ~Supplier();
    
    /** 
     *  Fills the fixed header with Event Description information.
     *  @param event The structured event which holds the header & data.
     * @throw ACSErrTypeCommon::CORBAProblemEx
     *  @htmlonly
        <br><hr>
        @endhtmlonly
     */
    void
    populateHeader(CosNotification::StructuredEvent &event)
	;

    /** 
     * Extracts the event type name from the any parameter and encodes that
     * into the current structured event to be published. So if any contains
     * a FRIDGE::temperatureDataBlockEvent structure, "temperatureDataBlock" is 
     * encoded into the type_name field of event_m.
     * @throw ACSErrTypeCommon::CORBAProblemEx
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual void
    populateHeader(const CORBA::Any &any)
	;

    /**
     * Sets the type_name field in the structured event. Method should 
     * probably be removed sometime in the near future but what impact
     * will this have?
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void 
    setEventType(const char *);



    /**
     * Utility method.
     * Create the Supplier Admin; create & init the supplier.
     * @return void
     * @throw ACSErrTypeCommon::CORBAProblemEx
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void
    createSupplier()
	;
    
    /**
     * Utility method.
     * Destroys a notification channel.  <b>ONLY USE THIS METHOD IF YOU KNOW FOR CERTAIN
     * THERE IS ONLY ONE SUPPLIER FOR THE CHANNEL!!! Use with extreme caution! This method
     * will most likely become deprecated in future releases of ACS!</b>     
     * @throw ACSErrTypeCommon::CORBAProblemEx
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void 
    destroyNotificationChannel()
	;

    /**
     * Utility method.
     * Initialization method where code common to all constructors is kept.
     * @param orb ORB which has a reference to the Naming Service.
     * @throw ACSErrTypeCommon::CORBAProblemEx
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void
    init(CORBA::ORB_ptr orb)
	;


    
    /** 
     *  Supplier Admin object is responsible for creating & managing proxy consumers
     *  w/ a common set of QoS property settings & filter objects. 
     */
    CosNotifyChannelAdmin::SupplierAdmin_var SupplierAdmin_m;
    
    /**
     *  The proxy consumer object used by supplier to push events onto the NC
     */
    CosNotifyChannelAdmin::StructuredProxyPushConsumer_var proxyConsumer_m;

    /**
     * Handle destruction cleanly using our own CORBA reference.
     */
    CosNotifyComm::StructuredPushSupplier_var reference_m;

    

    /**
     * This is a reference to the component this supplier is contained within.
     * The Executive subsystem has requested that the component's name be packed
     * into structured events and that is the reason we need this.
     */
    acscomponent::ACSComponentImpl* component_mp;

    /**
     * The so-called "type" of the event being sent.
     */
    char* typeName_mp;

    /**
     * The total number of events sent from this particular supplier.
     */
    unsigned long long count_m;
    Logging::RepeatGuardLogger<Logging::BaseLog> guardbl;
    /**
     * A single structured event. This was taken out of the publishEvent() method
     * for a small performance gain.
     */
    CosNotification::StructuredEvent event_m;
    
    std::string antennaName;

  private:
    

    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const Supplier&);

    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    Supplier(const Supplier&);

    CosNotifyChannelAdmin::AdminID adminid;
    CosNotifyChannelAdmin::ProxyID proxyConsumerID;

    CircularQueue eventBuff;

};
 }; 
#endif

