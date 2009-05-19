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
* "@(#) $Id: basencHelper.cpp,v 1.7 2009/05/19 17:35:42 javarias Exp $"
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
#include <loggingACEMACROS.h>
#include <acsutilWildcard.h>
#include <ACSErrTypeCORBA.h>

//-----------------------------------------------------------------------------
static const char *rcsId="@(#) $Id: basencHelper.cpp,v 1.7 2009/05/19 17:35:42 javarias Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);
//-----------------------------------------------------------------------------
BaseHelper::BaseHelper(const char* channelName, const char* notifyServiceDomainName) :
    namingContext_m(CosNaming::NamingContext::_nil()),
    notifyFactory_m(CosNotifyChannelAdmin::EventChannelFactory::_nil()),
    channelID_m(0),
    ifgop_m(CosNotifyChannelAdmin::AND_OP),
    notifyChannel_m(CosNotifyChannelAdmin::EventChannel::_nil()),
    notifyServiceDomainName_mp(0),
    notificationServiceName_mp(0),
    initCalled_m(false)
{
    channelName_mp = CORBA::string_dup(channelName);
    
    // make a copy of the NS domain name (if given)
    if (notifyServiceDomainName)
        notifyServiceDomainName_mp = CORBA::string_dup(notifyServiceDomainName);
}
//-----------------------------------------------------------------------------
BaseHelper::~BaseHelper()
{
    //most of all members of this class are _var's, we don't even have to call
    //the disconnect method.
	// set them free...	

	if (channelName_mp != 0)
		CORBA::string_free(channelName_mp);
	if (notifyServiceDomainName_mp != 0)
		CORBA::string_free(notifyServiceDomainName_mp);
	if (notificationServiceName_mp != 0)
		CORBA::string_free(notificationServiceName_mp);
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
	if(CORBA::is_nil(namingContext_m.in())){
		ACSErrTypeCORBA::CORBAReferenceNilExImpl ex(__FILE__, __LINE__, "BaseHelper::init");
		ex.setVariable("namingContext_m");
		throw ex;
	}

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
		if(CORBA::is_nil(ec_obj.in())){
			ACSErrTypeCORBA::FailedToResolveServiceExImpl 
				ex(__FILE__, __LINE__, "BaseHelper::init");
			throw ex;
		}

		//narrow it
		notifyChannel_m = CosNotifyChannelAdmin::EventChannel::_narrow(ec_obj.in());
		if(CORBA::is_nil(notifyChannel_m.in())){
			ACSErrTypeCORBA::NarrowFailedExImpl ex(__FILE__, __LINE__, "BaseHelper::init");
			ex.setNarrowType("CosNotifyChannelAdmin::EventChannel");
			throw ex;
		}
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
	if(CORBA::is_nil(namingContext_m)){
		ACSErrTypeCORBA::CORBAReferenceNilExImpl 
			ex(__FILE__, __LINE__, "BaseHelper::getNotifyService");
		ex.setVariable("namingContext_m");
		throw ex;
	}
	//first get the notification factory
	CosNaming::Name name(1);
	name.length(1);
	name[0].id = CORBA::string_dup(getNotificationFactoryName());

	//get the generic object for the notify service
	CORBA::Object_var corbaObj = namingContext_m->resolve(name);
	if(CORBA::is_nil(corbaObj.in())){
		ACSErrTypeCORBA::CORBAReferenceNilExImpl 
			ex(__FILE__, __LINE__, "BaseHelper::getNotifyService");
		ex.setVariable("corbaObj");
		throw ex;
	}

	//narrow the generic object
	notifyFactory_m = CosNotifyChannelAdmin::EventChannelFactory::_narrow(corbaObj.in());
	if(CORBA::is_nil(notifyFactory_m.in())){
		ACSErrTypeCORBA::NarrowFailedExImpl ex(__FILE__, __LINE__, "BaseHelper::getNotifyService");
		ex.setNarrowType("CosNotifyChannelAdmin::EventChannelFactory");
		throw ex;
	}
}
//-----------------------------------------------------------------------------
void
BaseHelper::createNotificationChannel()
{
	//sanity check. If we cannot access the naming service, there is
	//no point whatsoever in continuing.
	if(CORBA::is_nil(namingContext_m)){
		ACSErrTypeCORBA::CORBAReferenceNilExImpl 
			ex(__FILE__, __LINE__, "BaseHelper::createNotificationChannel");
		ex.setVariable("namingContext_m");
		throw ex;
	}

	//get access to the notification service
	getNotifyService();

	//here is where the channel is actually created
	notifyChannel_m = notifyFactory_m->create_channel(getQoSProps(), 
			getAdminProps(), 
			channelID_m);
	//ensure it's a valid reference
	if(CORBA::is_nil(notifyChannel_m.in())){
		ACSErrTypeCORBA::CORBAReferenceNilExImpl 
			ex(__FILE__, __LINE__, "BaseHelper::createNotificationChannel");
		ex.setVariable("notifyChannel_m");
		throw ex;
	}

	// Bind notification channel to Naming service
	attachChannelToNS();
}
//-----------------------------------------------------------------------------
void
BaseHelper::attachChannelToNS()
{
	//sanity check. If we cannot access the naming service, there is
	//no point whatsoever in continuing.
	if(CORBA::is_nil(namingContext_m)){
		ACSErrTypeCORBA::CORBAReferenceNilExImpl 
			ex(__FILE__, __LINE__, "BaseHelper::attachChannelToNS");
		ex.setVariable("namingContext_m");
		throw ex;
	}

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
char*
BaseHelper::getNotificationFactoryNameForChannel(CDB::DAL_ptr cdbRef, const char* channelName, const char* domainName)
{
//sanity check
if(CORBA::is_nil(cdbRef))
    {
    ACS_STATIC_SHORT_LOG((LM_DEBUG,
			  "BaseHelper::getNotificationFactoryNameForChannel",
			  "CDB ref null."));
    return 0;
    }

std::string cdbChannelsName = "MACI/Channels";
CDB::DAO_var tempDAO;
//try reading the entry from the CDB
try
    {
		tempDAO = cdbRef->get_DAO_Servant(cdbChannelsName.c_str());
		//sanity check, should not happen
	    if (tempDAO.in()==0)
	      return 0;
//DAL throws exceptions if the entry being searched for does not
//exist
    }
catch(...)
    {
    ACS_STATIC_SHORT_LOG((LM_DEBUG,
			  "BaseHelper::getNotificationFactoryNameForChannel",
			  "No CDB entry found for '%s' channel. OK.",
			  cdbChannelsName.c_str()));
    return 0;
    }

// if channel mapping exists take it, wildchars are also supported
try
    {
    CDB::stringSeq_var channelNameList = tempDAO->get_string_seq("NotificationServiceMapping/Channels_");
    for (CORBA::ULong n = 0; n < channelNameList->length(); n++)
        if (Wildcard::wildcardfit(channelNameList[n], channelName))
            return CORBA::string_dup(tempDAO->get_string((std::string("NotificationServiceMapping/Channels_/") + channelNameList[n].in() + "/NotificationService").c_str())); 
    }
catch(...)
    {
    ACS_STATIC_SHORT_LOG((LM_DEBUG,
			  "BaseHelper::getNotificationFactoryNameForChannel",
			  "No Channel to NotificationService mapping found for channel'%s' channel.",
			  channelName));
    }
    
// if domain mapping, if given
if (domainName)
	try
    {
    return CORBA::string_dup(tempDAO->get_string((std::string("NotificationServiceMapping/Domains/") + domainName + "/NotificationService").c_str())); 
    }
catch(...)
    {
    ACS_STATIC_SHORT_LOG((LM_DEBUG,
			  "BaseHelper::getNotificationFactoryNameForChannel",
			  "No Domain to NotificationService mapping found for domain/channel'%s/%s' channel. OK.",
			  domainName, channelName));
    }
    
// if default
	try
    {
    return CORBA::string_dup(tempDAO->get_string("NotificationServiceMapping/DefaultNotificationService")); 
    }
catch(...)
    {
    ACS_STATIC_SHORT_LOG((LM_DEBUG,
			  "BaseHelper::getNotificationFactoryNameForChannel",
			  "No NotificationServiceMapping/DefaultNotificationService attribute found. OK.",
			  domainName, channelName));
    }

	// not found
    return 0;
}
//------------------------------------------------------


/*___oOo___*/
