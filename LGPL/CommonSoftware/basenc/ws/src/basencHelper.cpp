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
* "@(#) $Id: basencHelper.cpp,v 1.2 2005/11/18 00:17:58 dfugate Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-11-15  created 
*/

/************************************************************************
*   NAME
*   
* 
*   SYNOPSIS
*
*   
*   PARENT CLASS
*
* 
*   DESCRIPTION
*
*
*   PUBLIC METHODS
*
*
*   PUBLIC DATA MEMBERS
*
*
*   PROTECTED METHODS
*
*
*   PROTECTED DATA MEMBERS
*
*
*   PRIVATE METHODS
*
*
*   PRIVATE DATA MEMBERS
*
*
*   FILES
*
*   ENVIRONMENT
*
*   COMMANDS
*
*   RETURN VALUES
*
*   CAUTIONS 
*
*   EXAMPLES
*
*   SEE ALSO
*
*   BUGS   
* 
*------------------------------------------------------------------------
*/

#include "basencHelper.h"
//-----------------------------------------------------------------------------
static char *rcsId="@(#) $Id: basencHelper.cpp,v 1.2 2005/11/18 00:17:58 dfugate Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);
//-----------------------------------------------------------------------------
BaseHelper::BaseHelper(const char* channelName) :
    namingContext_m(CosNaming::NamingContext::_nil()),
    notifyFactory_m(CosNotifyChannelAdmin::EventChannelFactory::_nil()),
    channelID_m(0),
    ifgop_m(CosNotifyChannelAdmin::AND_OP),
    notifyChannel_m(CosNotifyChannelAdmin::EventChannel::_nil()),
    channelName_mp(channelName),
    initCalled_m(false)
{
    //no-op
}
//-----------------------------------------------------------------------------
BaseHelper::~BaseHelper()
{
    //no-op
    //because all members of this class are _var's, we don't even have to call
    //the disconnect method.
}
//-----------------------------------------------------------------------------
void
BaseHelper::disconnect()
{
    //DO NOT REMOVE THE NAMING SERVICE REFERENCE!
    
    //REMOVE NOTIFICATION SERVICE REFERENCE
    CosNotifyChannelAdmin::EventChannelFactory_var notifyFactory = notifyFactory_m;
    notifyFactory_m = CosNotifyChannelAdmin::EventChannelFactory::_nil();
    
    //0 Channel ID
    channelID_m = 0;

    //DO NOT TOUCH THE INTERFILTER GROUP OPERATOR
    
    //REMOVE THE NOTIFICATION CHANNEL REFERENCE
    CosNotifyChannelAdmin::EventChannel_var notifyChannel = notifyChannel_m;
    notifyChannel_m = CosNotifyChannelAdmin::EventChannel::_nil();

    //DO NOT TOUCH THE CHANNEL NAME

    //unset initCalled_m
    initCalled_m = false;
}

//-----------------------------------------------------------------------------
void
BaseHelper::destroyNotificationChannel()
{
    //DWF-todo!!!
}
//-----------------------------------------------------------------------------
void
BaseHelper::init(CosNaming::NamingContext_ptr nc_p)
{
    //in this method, we take a look at the Naming Service to 
    //see if the channel has been created before. if not,
    //we go ahead and create it and then register it with the CORBA
    //naming service
    
    namingContext_m = CosNaming::NamingContext::_duplicate(nc_p);
    ACE_ASSERT(!CORBA::is_nil(namingContext_m.in()));
    
    //Common name sequence. This little object defines the name of 
    //channel as registered with the CORBA Naming Service.
    CosNaming::Name name(1);
    name.length(1);
    
    //name of the channel
    name[0].id   = CORBA::string_dup(channelName_mp);
    //channel kind
    name[0].kind = CORBA::string_dup(getChannelKind());

    try 
	{
	//use the naming service to get our object
	CORBA::Object_var ec_obj =  namingContext_m->resolve(name);
	ACE_ASSERT(!CORBA::is_nil(ec_obj.in()));
	
	//narrow it
	notifyChannel_m = CosNotifyChannelAdmin::EventChannel::_narrow(ec_obj.in());
	ACE_ASSERT(!CORBA::is_nil(notifyChannel_m.in()));
	}
    catch(CosNaming::NamingContext::NotFound ex)
	{
	//create it
	createNotificationChannel();
	}
    
    initCalled_m = true;
}
//-----------------------------------------------------------------------------
void
BaseHelper::getNotifyService()
{
    //sanity check. If we cannot access the naming service, there is
    //no point whatsoever in continuing.
    ACE_ASSERT(!CORBA::is_nil(namingContext_m));
    
    //first get the notification factory
    CosNaming::Name name(1);
    name.length(1);
    name[0].id = CORBA::string_dup(getNotificationFactoryName());
    
    //get the generic object for the notify service
    CORBA::Object_var corbaObj = namingContext_m->resolve(name);
    ACE_ASSERT(!CORBA::is_nil(corbaObj));

    //narrow the generic object
    notifyFactory_m = CosNotifyChannelAdmin::EventChannelFactory::_narrow(corbaObj.in());
    ACE_ASSERT(!CORBA::is_nil(notifyFactory_m.in()));
}
//-----------------------------------------------------------------------------
void
BaseHelper::createNotificationChannel()
{
    //sanity check. If we cannot access the naming service, there is
    //no point whatsoever in continuing.
    ACE_ASSERT(!CORBA::is_nil(namingContext_m));

    //get access to the notification service
    getNotifyService();
    
    //here is where the channel is actually created
    notifyChannel_m = notifyFactory_m->create_channel(getQoSProps(), 
						      getAdminProps(), 
						      channelID_m);
    //ensure it's a valid reference
    ACE_ASSERT(!CORBA::is_nil(notifyChannel_m.in()));
    
    // Bind notification channel to Naming service
    attachChannelToNS();
}
//-----------------------------------------------------------------------------
void
BaseHelper::attachChannelToNS()
{
    //sanity check. If we cannot access the naming service, there is
    //no point whatsoever in continuing.
    ACE_ASSERT(!CORBA::is_nil(namingContext_m));

    //name for our channel
    CosNaming::Name name(1);
    name.length(1);
    name[0].id = CORBA::string_dup(channelName_mp);
    name[0].kind = CORBA::string_dup(getChannelKind());
    
    //really bind the reference here
    namingContext_m->rebind(name, notifyChannel_m.in());
}
//-----------------------------------------------------------------------------
const CosNotification::AdminProperties
BaseHelper::getAdminProps()
{
    CosNotification::AdminProperties retVal;
    retVal.length(0);

    return retVal;
}
//-----------------------------------------------------------------------------
const CosNotification::QoSProperties
BaseHelper::getQoSProps()
{
    CosNotification::QoSProperties retVal;
    retVal.length(0);

    return retVal;
}
//-----------------------------------------------------------------------------


/*___oOo___*/
