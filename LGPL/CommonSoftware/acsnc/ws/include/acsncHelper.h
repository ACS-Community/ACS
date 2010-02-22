#ifndef ACSNC_HELPER_H
#define ACSNC_HELPER_H
/*******************************************************************************
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
*
* "@(#) $Id: acsncHelper.h,v 1.74 2010/02/22 18:31:17 javarias Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david  20/09/02  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <orbsvcs/CosNamingC.h>
#include <orbsvcs/CosNotifyChannelAdminS.h>
#include <orbsvcs/CosNotifyCommC.h>
#include <orbsvcs/CosNotifyFilterC.h>
#include <orbsvcs/CosNotificationC.h>
#include <orbsvcs/Notify/MonitorControlExt/NotifyMonitoringExtC.h>

#include "acsncS.h"
#include "acsncC.h"
#include "acsncORBHelper.h"
#include "acsncCDBProperties.h"
#include "acsncReconnectionCallback.h"

#include <basencHelper.h>

#include <ACSErrTypeCommon.h>


/** @file acsncHelper.h
 *  Header file for acsncHelper.cpp
 */

/**
 * @define ACSNC_STRING_MACRO
 * Macro which changes `something` to `"something"`. Should be removed sometime
 * in the near future.
 */
#define ACSNC_STRING_MACRO(something) #something

namespace nc {

   class ReconnectionCallback;

/**
 *  Class Helper is a base class used to provide common functionality between the
 *  Consumer and Supplier classes. That is, it hides much of the ACE/TAO
 *  code used to access the Naming Service and channels from these classes.
 *
 *  TODO:
 *  - elminate use of the ORBHelper class
 *  - check for memory leaks
 */
class Helper
{
  public:
    /**
     *  Constructor
     *  @param channelName Name of the channel
     *  @param notifyServiceDomainName Name of the notification service domain name used to determine notification service.
     *  @htmlonly
        <br><hr>
        @endhtmlonly
     */
    Helper(const char* channelName, const char* notifyServiceDomainName = 0);
    
    /**
     *  Resolve the TAO naming service.
     *  In reality, this is the only CORBA service we need to create a notification channel.  The
     *  naming service can be used to get references to anything registered with it.
     *  @param orb_mp orb_mp is what we use to get a reference to the naming service.  This method
     *  <b>assumes that orb_mp was started with a correct reference to the Naming Service</b>.  It should
     *  be noted that by passing <b><i>(CORBA::ORB *) 0</i></b>, this method will default to Container's
     *  ORB (which is preferred).
     *  @throw ACSErrTypeCommon::CORBAProblemEx
     *  @throw ACSErrTypeCommon::CouldntCreateThreadEx
     *  @return void
     *  @htmlonly
        <br><hr>
        @endhtmlonly
     */ 
    void 
    resolveNamingService(CORBA::ORB_ptr orb_mp);
    
    /**  
     *  This is used to determine if the notification channel we want to use has been registered
     *  with the Naming Service. If it has, we use that directly.
     *  @return true if named channel exists. False otherwise
     *  @throw ACSErrTypeCommon::CORBAProblemEx
     *  @htmlonly
        <br><hr>
        @endhtmlonly
     */
    bool 
    resolveNotifyChannel();

    /**
     * This is used by Comsumers and Suppliers to get the notification channel.
     * It first tries to resolved and if it fails, it tries to create it.
     * If it fails to create it, it waits 2 seconds and tries again, 20 times.
     * If it still fails, returns false
     *   
     **/
    bool resolveInternalNotificationChannel();

    /**  
     *  Helper method designed to eliminate "xxx::" from a passed string where "xxx" is some
     *  namespace.  Designed to access the stringified name of an "ICD event".
     *  @param idlStruct a string that may or may not contain "xxx::"
     *  @return the string without "xxx::"
     *  @htmlonly
        <br><hr>
        @endhtmlonly
     */
    static char *
    extractStructName(const char* idlStruct);

    /** 
      * This method allow to the Helper reconnect to the Notification Channel
      * in case of Notify Service crashes. It is used by the the Persistance 
      * Notify Service extension and must be
      * called only from the Reconnection Callback.
      * 
      *  @see acsnc::ReconnectionCallback
      *  @param ecf Pointer to the EventChannelFactory
      */
    virtual void reconnect(::NotifyMonitoringExt::EventChannelFactory *ecf);

   
    
  protected:
    /**
     * This method returns a constant character pointer to the "kind" of notification channel
     * as registered with the naming service (i.e., the kind field of a CosNaming::Name) which
     * is normally equivalent to acscommon::NC_KIND. The sole reason this method is provided is to 
     * accomodate subclasses which subscribe/publish non-ICD style events (ACS archiving channel 
     * for example).In that case, the developer would override this method.
     * @return pointer to a constant string.
     *  @htmlonly
        <br><hr>
        @endhtmlonly
     */
    virtual const char* 
    getChannelKind();

    /**
     * This method returns a constant character pointer to the domain of notification channel
     * which is normally equivalent to acscommon::ALMADOMAIN. The sole reason this method is provided is to 
     * accomodate subclasses which subscribe/publish non-ICD style events (ACS archiving channel 
     * for example).In that case, the developer would override this method.
     * @return pointer to a constant string.
     *  @htmlonly
        <br><hr>
        @endhtmlonly
     */
    virtual const char*
    getChannelDomain();

    /**
     * This method returns a constant character pointer to the name of 
     * the notification service as registered with the CORBA Naming Service.
     * @return pointer to a constant string. Normally acscommon::NOTIFICATION_FACTORY_NAME
     */
    virtual const char*
    getNotificationFactoryName()
	{
		if (!notificationServiceName_mp)
		{
			CDB::DAL_var dal = CDBProperties::getCDB();
			notificationServiceName_mp = BaseHelper::getNotificationFactoryNameForChannel(dal.in(), channelName_mp, notifyServiceDomainName_mp);
			if (!notificationServiceName_mp)
				notificationServiceName_mp = CORBA::string_dup(acscommon::NOTIFICATION_FACTORY_NAME);
		}
		
		return notificationServiceName_mp;
	}



    /**
     * Utility method.
     * Try to resolve the Notify factory from the Naming service.
     * Only used by Supplier to create NC.
     * @throw ACSErrTypeCommon::CORBAProblemEx
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual void 
    resolveNotificationFactory();
    
    /** 
     * Utility method.
     * Create notification channel. 
     * Only used by Supplier to create NC.
     * @throw ACSErrTypeCommon::CORBAProblemEx
     * @throw NotifyMonitoringExt::NameAlreadyUsed when the factory tries to create a notification channel with the same name
     * @throw NotifyMonitoringExt::NameMapError
     * @htmlonly
       <br><hr>
       @endhtmlonly
    */
    virtual void 
    createNotificationChannel();

    /**
     * Destructor is protected.
     */
    virtual ~Helper();

    /**
     * Utility method.
     * Returns quality of service properties used to create a notification channel.
     * Override this method if the defaults are insufficient.
     * @return quality of service properties used to create a notification channel
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual const CosNotification::QoSProperties
    getQoSProps();
    
    /**
     * Utility method.
     * Returns admin properties used to create a notification channel.
     * Developers should override this method if the defaults are insufficient.
     * @return quality of service properties used to create a notification channel
     * @htmlonly
     <br><hr>
     @endhtmlonly
    */
    virtual const CosNotification::AdminProperties 
    getAdminProps();    
    

    /**
     *  A naming context (i.e., Naming Service reference)
     */
    CosNaming::NamingContext_var namingContext_m;
    
    /** 
     *  The notification channel used to send/receive events. This is 
     *  created by the notification channel factory. This channel is bound to 
     *  the naming service using channelName_mp.
     */
    CosNotifyChannelAdmin::EventChannel_var notifyChannel_m;

    /**
     *  The group operator between admin-proxy's.
     *  The purpose of this variable is to define how multiple Filters are 
     *  considered in a proxy admin.  Basically this variable does not 
     *  matter since filtering at the admin level is not allowed!
     */
    CosNotifyChannelAdmin::InterFilterGroupOperator ifgop_m;

    /**
     *  Name of the notification channel.
     */
    char *channelName_mp;

    
    /**
     *  Name of the nofitication service domain.
     */
    char *notifyServiceDomainName_mp;
    
    /**
     * Name of "resovled" notification service.
     */
    char *notificationServiceName_mp;

    /**
     *  In case of standalone mode, this must be used!
     */
    ORBHelper *orbHelper_mp;


    /**
     * Channel factory. Used to create new channels.
     * The extended mode of TAO is used to prevent the creation
     * of a channel multiple times
     */
    NotifyMonitoringExt::EventChannelFactory_var notifyFactory_m;
    
    /**
     * Channel factory. Used to create new channels.
     * In the case of a non TAO implementation or the initialization
     * of the service without the extended mode, the standard
     * mode is used
     */
    CosNotifyChannelAdmin::EventChannelFactory_var notifyFactoryOld_m;

    /**
     * Keep a reference to the channel. Should this be removed in future
     * releases of ACS???
     */
    CosNotifyChannelAdmin::ChannelID channelID_m;


    /**
     * The following was requested by Heiko Sommer and is needed for integrations.
     * It should be removed at some later date.
     */
    void
    integrationLog(const std::string& log);

    ReconnectionCallback *callback_m;

  private:
    
    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const Helper&);

    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    Helper(const Helper&);

    /**
     * The following was requested by Heiko Sommer and is needed for integrations.
     * It should be removed at some later date.
     */
    bool okToLog_m;
};
 }; 

#endif /*!_H*/


