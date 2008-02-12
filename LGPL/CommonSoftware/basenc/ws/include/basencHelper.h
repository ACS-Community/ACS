#ifndef basenc_helper_H
#define basenc_helper_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) National Research Council of Canada, 2005 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: basencHelper.h,v 1.6 2008/02/12 01:06:22 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-11-15  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <orbsvcs/CosNotifyChannelAdminS.h>
#include <orbsvcs/CosNamingC.h>
#include <acsncC.h>
#include <cdbDALC.h>

/**
 * Baseclass for all NC objects. Provides common functionality
 * such as access to the Notification Service and channel creation.
 */
class BaseHelper
{
  public:
  
	/**
	 * Get notification channel factory name for given channel/domain.
	 * @param channelName	name of the channel.
	 * @param domainName	name of the domain, <code>0</code> if undefined.
	 * @return notification channel factory name.
	 */
	static char*  getNotificationFactoryNameForChannel(CDB::DAL_ptr dal, const char* channelName, const char* domainName = 0);
  
  protected:
    
    /**
     * Standard constructor.
     * @param channelName Name of the channel
     * @param notifyServiceDomainName Name of the notification service domain name used to determine notification service.
     */
    BaseHelper(const char* channelName, const char* notifyServiceDomainName = 0);
    
    /**
     * Initialization method. In short, no method of this class can be
     * successfully invoked until this method has been called.
     * @param nc_p CORBA Naming Service pointer
     */
    virtual void
    init(CosNaming::NamingContext_ptr nc_p);

    /**
     * Destructor
     */
    virtual ~BaseHelper();
    
    /**
     * Disconnects from the channel. This method should be used
     * instead of the destructor.
     */ 
    virtual void 
    disconnect();

    /**
     * This method returns a constant character pointer to the "kind" of notification channel
     * as registered with the naming service (i.e., the kind field of a CosNaming::Name) which
     * is normally equivalent to acscommon::NC_KIND. The sole reason this method is provided is to 
     * accomodate subclasses which subscribe/publish non-ICD style events (ACS archiving channel 
     * for example).In that case, the developer would override this method.
     * @return pointer to a constant string.
     */
    virtual const char* 
    getChannelKind() = 0;

    /**
     * This method returns a constant character pointer to the domain of notification channel
     * which is normally equivalent to acscommon::ALMADOMAIN. The sole reason this method is provided is to 
     * accomodate subclasses which subscribe/publish non-ICD style events (ACS archiving channel 
     * for example).In that case, the developer would override this method.
     * @return pointer to a constant string.
     */
    virtual const char*
    getChannelDomain() 
	{return acscommon::ALMADOMAIN;}

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
			// @todo temporary implementation, containerServices to be used
			CDB::DAL_var cdb;
			
		    //Common name sequence. This little object defines the name of 
		    //channel as registered with the CORBA Naming Service.
		    CosNaming::Name name(1);
		    name.length(1);
		    
		    //name of the channel
		    name[0].id   = CORBA::string_dup("CDB");
		    //channel kind
		    name[0].kind = CORBA::string_dup("");
		
		    try 
			{
				//use the naming service to get our object
				CORBA::Object_var dal_obj =  namingContext_m->resolve(name);
				
				//narrow it
				cdb = CDB::DAL::_narrow(dal_obj.in());
			}
		    catch(CosNaming::NamingContext::NotFound ex)
			{
			// noop
			}

			notificationServiceName_mp = getNotificationFactoryNameForChannel(cdb.in(), channelName_mp, notifyServiceDomainName_mp);
			if (!notificationServiceName_mp)
				notificationServiceName_mp = CORBA::string_dup(acscommon::NOTIFICATION_FACTORY_NAME);
		}
		
		return notificationServiceName_mp;
	}

    /**
     * Utility method.
     * Returns quality of service properties used to create a notification channel.
     * Override this method if the defaults are insufficient.
     * @return quality of service properties used to create a notification channel
     */
    virtual const CosNotification::QoSProperties
    getQoSProps();
    
    /**
     * Utility method.
     * Returns admin properties used to create a notification channel.
     * Developers should override this method if the defaults are insufficient.
     * @return quality of service properties used to create a notification channel
     */
    virtual const CosNotification::AdminProperties 
    getAdminProps();

    /** 
     * Utility method only used by Supplier and should not
     * be called by your code directly. Provided just in case
     * someone wants to do something extra after creating a 
     * channel.
     * 
     * Creates notification channel.
     */
    virtual void 
    createNotificationChannel();

    /**
     * Utility method only used by Supplier and should not
     * be called by your code directly. Provided just in case
     * someone wants to get at the notification service using
     * something other than the CORBA Naming Service.
     *
     * Gets a reference to the notification service and sets member
     * values accordingly.
     */
    virtual void
    getNotifyService();

    /**
     * Utility method only used by Supplier and should not
     * be called by your code directly. Provided just in case
     * someone wants to do something extra after adding a reference
     * to the newly created channel within the CORBA Naming Service.
     *
     * Adds a reference to the channel to the CORBA Naming Service.
     */
    virtual void
    attachChannelToNS();

    /**
     * Utility method.
     * Destroys a notification channel.  <b>ONLY USE THIS METHOD IF YOU KNOW FOR CERTAIN
     * THERE IS ONLY ONE SUPPLIER FOR THE CHANNEL!!! Use with extreme caution! This method
     * will most likely become deprecated in future releases of ACS!</b>
     * @return void
     */
    virtual void 
    destroyNotificationChannel();

    //--members-------------------------------------------------------------------------
    /**
     *  A naming context (i.e., Naming Service reference)
     */
    CosNaming::NamingContext_var namingContext_m;

    /**
     * Channel factory. Used to create new channels.
     */
    CosNotifyChannelAdmin::EventChannelFactory_var notifyFactory_m;

    /**
     * Keep a reference to the channel. Should this be removed in future
     * releases of ACS???
     */
    CosNotifyChannelAdmin::ChannelID channelID_m;
    
    /**
     *  The group operator between admin-proxy's.
     *  The purpose of this variable is to define how multiple Filters are 
     *  considered in a proxy admin.  Basically this variable does not 
     *  matter since filtering at the admin level is not allowed!
     */
    CosNotifyChannelAdmin::InterFilterGroupOperator ifgop_m;

    /** 
     *  The notification channel used to send/receive events. This is 
     *  created by the notification channel factory. This channel is bound to 
     *  the naming service using channelName_mp.
     */
    CosNotifyChannelAdmin::EventChannel_var notifyChannel_m;

    /**
     * Name of the channel.
     */
    char* channelName_mp;

    /**
     *  Name of the nofitication service domain.
     */
    char *notifyServiceDomainName_mp;
    
    /**
     * Name of "resovled" notification service.
     */
    char *notificationServiceName_mp;

    /**
     * Shows whether the init method has been called yet.
     */
    bool initCalled_m;
};

#endif /*!_H*/
