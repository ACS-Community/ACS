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
* "@(#) $Id: acsncHelperImpl.cpp,v 1.84 2011/08/26 22:04:59 javarias Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david  20/09/02  created
*/
//-----------------------------------------------------------------------------
#include "acsncHelper.h"
#include <maciContainerImpl.h>
#include <baciCORBA.h>
#include <acscommonC.h>
#include <AcsNCTraceLog.h>
#include "acsncCDBProperties.h"
//-----------------------------------------------------------------------------
 using namespace baci;
 using namespace maci;
 using namespace ACSErrTypeCommon;
//-----------------------------------------------------------------------------
namespace nc {
//-----------------------------------------------------------------------------
Helper::Helper(const char* channelName, const char* notifyServiceDomainName):
    namingContext_m(CosNaming::NamingContext::_nil()),
    notifyChannel_m(CosNotifyChannelAdmin::EventChannel::_nil()),
    channelName_mp(0),
    notifyServiceDomainName_mp(0),
    notificationServiceName_mp(0),
    orbHelper_mp(0),
    notifyFactory_m(0),
    notifyFactoryOld_m(0),
    channelID_m(0),
    okToLog_m(false)
{
    ACS_TRACE("Helper::Helper");
    //make a copy of the channel's name
    channelName_mp = CORBA::string_dup(channelName);
    // make a copy of the NS domain name (if given)
    if (notifyServiceDomainName)
        notifyServiceDomainName_mp = CORBA::string_dup(notifyServiceDomainName);
    //this is common to both suppliers and consumers, but what does it really
    //do?
    ifgop_m = CosNotifyChannelAdmin::AND_OP;

    //if this doesn't work
    if((BACI_CORBA::getInstance()==0) && (BACI_CORBA::InitCORBA(0, 0) == false))
	{
	ACS_SHORT_LOG((LM_ERROR,"Helper::Helper unable to gain access to BACI_CORBA!",
		       channelName_mp));
	CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Helper::Helper");
	throw err.getCORBAProblemEx();
	}

    //check the CDB to see if we use integration logs
    if(nc::CDBProperties::getIntegrationLogs(channelName_mp)==false)
	    {
	    okToLog_m = false;
	    }
	else
	    {
	    okToLog_m = true;
	    }
    callback_m = new ReconnectionCallback(this);
}
//-----------------------------------------------------------------------------
Helper::~Helper()
{
    ACS_TRACE("Helper::~Helper");
    //since everything is essentially created using _var types...just delete
    //the ORB helper if it exists
    if (callback_m !=0){
        delete callback_m;
        callback_m = 0;
    }

    if (orbHelper_mp != 0)
	{
	delete orbHelper_mp;
	orbHelper_mp=0;
	}

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
Helper::resolveNamingService(CORBA::ORB_ptr orb_mp)
{
    ACS_TRACE("Helper::resolveNamingService");
    try
	{
	//Here we try a couple of different methods to get at the naming service.
	if(orb_mp == 0)    //We've been passed a fake ORB.
	    {
	    //Try to get at the Naming Service using the activator singleton first.
	    if ((ContainerImpl::getContainer() != 0) &&
		(ContainerImpl::getContainer()->getContainerCORBAProxy() != maci::Container::_nil()))
		{
		namingContext_m = ContainerImpl::getContainer()->getService<CosNaming::NamingContext>(acscommon::NAMING_SERVICE_NAME, 0, true);
		}
	    //DWF - Ideally there would be a SimpleClient singleton that we would try next (this would
	    // be especially useful in Consumers), but instead we will just create our own ORB
	    // and hope this is running on the same host as the Naming Service =(
	    else    //This is basically just a fail-safe mechanism.
		{
		ACS_SHORT_LOG((LM_INFO,
			       "Helper::resolveNameService wrong constructor - attempting recovery for the '%s' channel!",
			       channelName_mp));

		if (orbHelper_mp == 0)
		    {
		    //should never be the case but if it does happen...
		    orbHelper_mp = new ORBHelper();
		    orbHelper_mp->runOrb();
		    }
		// Get the naming context
		namingContext_m=MACIHelper::resolveNameService(orbHelper_mp->getORB());
		}
	    }

	//Passed a valid orb so we try to resolve the naming service using
	//the "normal" method
	else
	    {
	    namingContext_m=MACIHelper::resolveNameService(orb_mp);
	    }
	}
    catch(...)
	{
        ACS_SHORT_LOG((LM_ERROR, "Helper::resolveNameService CORBA exception caught for the '%s' channel!",
		       channelName_mp));
	CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Helper::resolveNamingService");
	throw err.getCORBAProblemEx();
	}

	//one last check to make sure we have the correct reference to the name service
	if(CORBA::is_nil(namingContext_m))
	    {
	    ACS_SHORT_LOG((LM_ERROR,"Helper::resolveNameService unable to resolve name service for the '%s' channel!",
			   channelName_mp));
	    CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Helper::resolveNamingService");
	    throw err.getCORBAProblemEx();
	    }
}
//-----------------------------------------------------------------------------
void
Helper::resolveNotificationFactory()
{
    ACS_TRACE("Helper::resolveNotificationFactory");

    CosNaming::Name name(1);
    name.length(1);
    name[0].id = getNotificationFactoryName();

    //first a simple sanity check to ensure the naming service is up and running
    if(CORBA::is_nil(namingContext_m.in()) == true)
	{
	ACS_SHORT_LOG((LM_ERROR, "Helper::resolveNotificationFactory error occured for the '%s' channel, Naming Context is nil!",
		       channelName_mp));
	CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Helper::resolveNotificationFactory");
	throw err.getCORBAProblemEx();
	}
    try
	{
        //try to resolve the object with the naming service.  a few exceptions can be
        //thrown by this
        CORBA::Object_var corbaObj = namingContext_m->resolve(name);
        //double-check to ensure it's not a nil reference
        if(CORBA::is_nil(corbaObj.in()) == true)
        {
            ACS_SHORT_LOG((LM_ERROR, "Helper::resolveNotificationFactory error occured for the '%s' channel, Corba Object of the resolve of Naming Context is nil!",
                   channelName_mp));
            CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Helper::resolveNotificationFactory");
            throw err.getCORBAProblemEx();
        }
        //now try to narrow the notification service reference
        notifyFactory_m = NotifyMonitoringExt::EventChannelFactory::_narrow(corbaObj.in());
        //double-check to ensure it's not a nil reference
        if(CORBA::is_nil(notifyFactory_m.in()) == true)
        {
            ACS_SHORT_LOG((LM_ERROR, "Helper::resolveNotificationFactory error occured for the '%s' channel using NotifyMonitoringExt!",
                   channelName_mp));
            notifyFactoryOld_m = CosNotifyChannelAdmin::EventChannelFactory::_narrow(corbaObj.in());
            //double-check to ensure it's not a nil reference again
            if(CORBA::is_nil(notifyFactoryOld_m.in()) == true)
            {
                ACS_SHORT_LOG((LM_ERROR, "Helper::resolveNotificationFactory error occured for the '%s' channel using CosNotifyChannelAdmin!",
                       channelName_mp));
                CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Helper::resolveNotificationFactory");
                throw err.getCORBAProblemEx();
            }
        }
	}
    catch(CORBAProblemEx)
	{
        ACS_SHORT_LOG((LM_TRACE, "Helper::resolveNotificationFactory failed for the '%s' channel with a nil pointer!",
                   channelName_mp));
        //thrown by this method and OK to rethrow
        throw;
	}
    catch(...)
	{
        //most likely some exception like the notification service is not registered
        //with the naming service.  nothing can be done
        ACS_SHORT_LOG((LM_ERROR, "Helper::resolveNotificationFactory() error occured for the '%s' channel!, unknown error on resolveNotificationFactory",
                   channelName_mp));
        CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Helper::resolveNotificationFactory");
        throw err.getCORBAProblemEx();
	}
}
//-----------------------------------------------------------------------------
void Helper::createNotificationChannel() {
    ACS_TRACE("Helper::resolveNotificationChannel");
    ACE_Time_Value start_time = ACE_OS::gettimeofday();
    ACE_Time_Value end_time;
    unsigned long msec = 0;
    try {
        //double-check the notification service reference
        if (CORBA::is_nil(notifyFactory_m.in()) == true) {
            //it means that the extended notify factory failed to be created.
            //we will try with the standard implementation
            if (CORBA::is_nil(notifyFactoryOld_m.in()) == true) {
                ACS_SHORT_LOG(
                        (LM_ERROR, "Helper::createNotificationChannel() error occured for the '%s' channel, Default Notify factory is nil!", channelName_mp));
                CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__, __LINE__,
                        "nc::Helper::createNotificationChannel");
                throw err.getCORBAProblemEx();
            }

            //here is where the channel is actually created
            notifyChannel_m = notifyFactoryOld_m->create_channel(getQoSProps(),
                    getAdminProps(), channelID_m);
            end_time = ACE_OS::gettimeofday();
        } else {
            //here is where the channel is actually created
            notifyChannel_m = notifyFactory_m->create_named_channel(
                    getQoSProps(), getAdminProps(), channelID_m,
                    channelName_mp);
            end_time = ACE_OS::gettimeofday();
        }
        msec = (end_time.sec() - start_time.sec()) * 1000 + (end_time.usec() - start_time.usec()) / 1000;
        AcsNCTraceLog::LOG_NC_ChannelCreatedRaw_OK TS_RawOK_Log(__FILE__, __LINE__, __PRETTY_FUNCTION__);
        TS_RawOK_Log.setTimeMillis(msec);
        TS_RawOK_Log.setChannelName(channelName_mp);
        TS_RawOK_Log.setChannelId(channelID_m);
        TS_RawOK_Log.log();
        //ensure it's a valid reference
        if (CORBA::is_nil(notifyChannel_m.in()) == true) {
            ACS_SHORT_LOG(
                    (LM_ERROR, "Helper::createNotificationChannel() error occured for the '%s' channel, Notify Channel is nil!", channelName_mp));
            CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__, __LINE__,
                    "nc::Helper::createNotificationChannel");
            throw err.getCORBAProblemEx();
        }

        // Bind notification channel to Naming service
        CosNaming::Name name(1);
        name.length(1);
        name[0].id = CORBA::string_dup(channelName_mp);
        name[0].kind = acscommon::NC_KIND;
        //sanity check to make sure the naming service is really there
        if (CORBA::is_nil(namingContext_m.in()) == true) {
            ACS_SHORT_LOG(
                    (LM_ERROR, "Helper::createNotificationChannel() error occured for the '%s' channel, Naming Context is nil!", channelName_mp));
            CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__, __LINE__,
                    "nc::Helper::createNotificationChannel");
            throw err.getCORBAProblemEx();
        }
        //really bind the reference here
        namingContext_m->rebind(name, notifyChannel_m.in());
    } catch (NotifyMonitoringExt::NameAlreadyUsed e) {
        ACS_SHORT_LOG(
                (LM_TRACE, "Helper::createNotificationChannel() failed for the '%s' channel, the name is already used!", channelName_mp));
        throw e;
    } catch (NotifyMonitoringExt::NameMapError e) {
        ACS_SHORT_LOG(
                (LM_TRACE, "Helper::createNotificationChannel() failed for the '%s' channel, Name Map Error!", channelName_mp));
        throw e;
    } catch (CORBAProblemEx) {
        //exception thrown by us...OK to rethrow
        ACS_SHORT_LOG(
                (LM_TRACE, "Helper::createNotificationChannel() failed for the '%s' channel with a nil pointer!", channelName_mp));
        throw;
    } catch (...) {
        //lots of things could have caused this (bad QoS props, admin props, etc.)
        ACS_SHORT_LOG(
                (LM_ERROR, "Helper::createNotificationChannel() error occured for the '%s' channel, unknown error on createNotificationChannel!", channelName_mp));
        CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__, __LINE__,
                "nc::Helper::createNotificationChannel");
        throw err.getCORBAProblemEx();
    }
}
//-----------------------------------------------------------------------------
const char*
Helper::getChannelKind()
{
    //return a constant defined in acscommon.idl to be portable in the other
    //programming languages supported by ACS.
    return acscommon::NC_KIND;
}
//-----------------------------------------------------------------------------
const char*
Helper::getChannelDomain()
{
    //return a constant defined in acscommon.idl to be portable in the other
    //programming languages supported by ACS.
    return acscommon::ALMADOMAIN;
}
//-----------------------------------------------------------------------------
const CosNotification::QoSProperties
Helper::getQoSProps()
{
    ACS_TRACE("Helper::getQoSProps");
    return nc::CDBProperties::getCDBQoSProps(channelName_mp);
}
//-----------------------------------------------------------------------------
const CosNotification::AdminProperties
Helper::getAdminProps()
{
    ACS_TRACE("Helper::getAdminProps");
    return nc::CDBProperties::getCDBAdminProps(channelName_mp);
}

bool Helper::resolveInternalNotificationChannel(){

    ACS_TRACE("Helper::resolveInternalNotificationChannel");
    AcsNCTraceLog::LOG_NC_ChannelCreated_ATTEMPT TS_NC_Attempt(__FILE__, __LINE__, __PRETTY_FUNCTION__);
    TS_NC_Attempt.log();
    ACE_Time_Value start_time = ACE_OS::gettimeofday();
    ACE_Time_Value end_time;
    unsigned long msec = 0;
    int retryNumberAttempts = 20;
    int retrySleepSec = 2;
    bool existNotifyChannel = resolveNotifyChannel();
    if(!existNotifyChannel)
        resolveNotificationFactory();

    while(!existNotifyChannel && retryNumberAttempts >= 0){
        try{
            createNotificationChannel();
            end_time = ACE_OS::gettimeofday();
            msec = (end_time.sec() - start_time.sec()) * 1000 + (end_time.usec() - start_time.usec()) / 1000;
            AcsNCTraceLog::LOG_NC_ChannelCreated_OK TS_NC_OK(__FILE__, __LINE__, __PRETTY_FUNCTION__);
            TS_NC_OK.setChannelId(channelID_m);
            TS_NC_OK.setChannelName(channelName_mp);
            TS_NC_OK.setTimeMillis(msec);
        }catch(NotifyMonitoringExt::NameAlreadyUsed){
            ACS_SHORT_LOG((LM_INFO,"NC '%s' seems to be getting created. Will wait and try again in %d seconds.", channelName_mp, retrySleepSec));
        }catch(NotifyMonitoringExt::NameMapError){
            ACS_SHORT_LOG((LM_WARNING,"*** TODO check what this NameMapError means!!!"));
        }catch(...){
            ACS_SHORT_LOG((LM_WARNING,""));
        }
        retryNumberAttempts--;
        sleep(retrySleepSec);
        existNotifyChannel = resolveNotifyChannel();
    }
    return existNotifyChannel;
}

//-----------------------------------------------------------------------------
bool
Helper::resolveNotifyChannel()
{
    //commented out as per Rodrigo Amestica's request.
    //should be uncommented once a couple of ACS logging system SPRs get completed.
    //ACS_TRACE("Helper::resolveNotifyChannel");

    CosNaming::Name name(1);
    name.length(1);
    name[0].id = CORBA::string_dup(channelName_mp);
    name[0].kind = getChannelKind();

    try
	{
	if(CORBA::is_nil(namingContext_m))
	    {
	    ACS_SHORT_LOG((LM_ERROR,"Helper::resolveNotifyChannel Naming Context bad for the '%s' channel!",
			   channelName_mp));
	    //This error is bad enough that we can thrown an exception
	    CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Helper::resolveNotifyChannel");
	    throw err.getCORBAProblemEx();
	    return false;
	    }

	CORBA::Object_var obj = namingContext_m->resolve(name);
	if(CORBA::is_nil(obj.in()) == true)
	    {
	    ACS_SHORT_LOG((LM_ERROR,"Helper::resolveNotifyChannel Notify Channel object bad for the '%s' channel!",
			   channelName_mp));
	    //Hmm...can this really happen? In theory this would end up being a NotFound exception
	    CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Helper::resolveNotifyChannel");
	    throw err.getCORBAProblemEx();
	    return false;
	    }
	notifyChannel_m = CosNotifyChannelAdmin::EventChannel::_narrow(obj.in());
	}
    catch(CosNaming::NamingContext::NotFound ex)
	{
	ACS_SHORT_LOG((LM_TRACE,"Helper::resolveNotifyChannel - this is expected when a channel is being created."));
	//This is actually expected when creating a new channel, but still return false.
	return false;
	}
    catch(const CORBA::SystemException &ex)
    	{
    	ACS_SHORT_LOG((LM_ERROR, "Helper::resolveNotifyChannel CORBA System exception caught for the '%s' channel!",
    		       channelName_mp));
    	CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Helper::resolveNotifyChannel");
    	err.setMinor(ex.minor());
    	err.setCompletionStatus(ex.completed());
    	err.setInfo(ex._info().c_str());

    	throw err.getCORBAProblemEx();
    	return false;
    	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR, "Helper::resolveNotifyChannel Unknown exception caught for the '%s' channel!",
		       channelName_mp));
	CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Helper::resolveNotifyChannel");
	throw err.getCORBAProblemEx();
	return false;
	}

    if (CORBA::is_nil(notifyChannel_m))
	{
	ACS_SHORT_LOG((LM_ERROR,"Helper::resolveNotifyChannel Notify channel bad for the '%s' channel!",
		       channelName_mp));
	CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::Helper::resolveNotifyChannel");
	throw err.getCORBAProblemEx();
	return false;
	}

    //All went OK, so return true.
    return true;
}

//-----------------------------------------------------------------------------
char *
Helper::extractStructName(const char* idlStruct)
{
    //position within the char* parameter passed in that is considered to meet
    //the requirements of this method
    int goodPos=0;

    //sanity check
    if (idlStruct==0)
	{
	return static_cast<char *>(0);
	}

    //find the position just past the final "::" if it exists
    for (unsigned int i=1U; i<strlen(idlStruct); i++)
	{
	if ((idlStruct[i]==':') && (idlStruct[i-1]==':'))
	    {
	    goodPos = i + 1;
	    }
	}

    //set the retValue
    return CORBA::string_dup(const_cast<char *>(idlStruct + goodPos));
}

void Helper::reconnect(::NotifyMonitoringExt::EventChannelFactory *ecf)
{
   if (::CORBA::is_nil(notifyChannel_m))
      resolveNotifyChannel();
	notifyChannel_m->set_qos(getQoSProps());
}

//-----------------------------------------------------------------------------
//The following was requested by Heiko Sommer and is needed for integrations.
void
Helper::integrationLog(const std::string& log)
{
    if (okToLog_m==true)
	{
	//fine, send the log
	getNamedLogger("IntegrationLogger")->log(Logging::BaseLog::LM_NOTICE,
						 log,
						 __FILE__, __LINE__, "Helper::integrationLog");
	}
}
//-----------------------------------------------------------------------------

 };
/*___oOo___*/
