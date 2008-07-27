/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) UNSPECIFIED - FILL IN, 2005 
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
* "@(#) $Id: acsncCDBProperties.cpp,v 1.12 2008/07/27 15:09:30 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* almadave  2005-04-24  created 
*/

#include "acsncCDBProperties.h"
#include <maciContainerImpl.h>

static char *rcsId="@(#) $Id: acsncCDBProperties.cpp,v 1.12 2008/07/27 15:09:30 cparedes Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

namespace nc {
    //------------------------------------------------------
    CDB::DAL_ptr
    CDBProperties::getCDB()
    {
	//initialize return value to 0
	CDB::DAL_var retVal = 0;
	
	//use a nice little trick to get at the CDB reference
	if ((maci::ContainerImpl::getContainer() != 0) && 
	    (maci::ContainerImpl::getContainer()->getContainerCORBAProxy() != maci::Container::_nil()))
	    {
	    retVal = maci::ContainerImpl::getContainer()->getService<CDB::DAL>("CDB", 
									 0, 
									 true);
	    }
	else
	    {
	    ACS_STATIC_SHORT_LOG((LM_ERROR,
				 "CDBProperties::getCDB",
				 "Container ref null."));
	    }
	return retVal._retn();
    }
    //------------------------------------------------------
    bool 
    CDBProperties::cdbChannelConfigExists(const std::string& channelName)
    {
	//complete name of the channel within the CDB
	std::string cdbChannelName = "MACI/Channels/" + channelName;

	//delegate obtaining a reference to the CDB
	CDB::DAL_var cdbRef = getCDB();

	//sanity check
	if(cdbRef.in()==0)
	    {
	    ACS_STATIC_SHORT_LOG((LM_ERROR,
				  "CDBProperties::cdbChannelConfigExists",
				  "CDB ref null."));
	    return false;
	    }
	
	//try reading the entry from the CDB
	try
	    {
	    char *joe = 0;
	    joe = cdbRef->get_DAO(cdbChannelName.c_str());
	    //if the pointer is not null
	    if (joe!=0)
		{
		//it's probably OK
		return true;
		}
	    else
		{
		//something went wrong!
		return false;
		}
	    }
	//DAL throws exceptions if the entry being searched for does not
	//exist
	catch(...)
	    {
	    ACS_STATIC_SHORT_LOG((LM_INFO,
				  "CDBProperties::cdbChannelConfigExists",
				  "No CDB entry found for '%s' channel. OK.",
				  cdbChannelName.c_str()));
	    return false;
	    }
    }
    //------------------------------------------------------
    CosNotification::AdminProperties
    CDBProperties::getCDBAdminProps(const std::string& channelName)
    {
	CosNotification::AdminProperties retVal;
	retVal.length(0);
	
	//sanity check
	if (cdbChannelConfigExists(channelName)==false)
	    {
	    ACS_STATIC_SHORT_LOG((LM_INFO,
				  "CDBProperties::getCDBAdminProps",
				  "Channel does not exist."));
	    return retVal;
	    }
	
	//CDB
	//complete name of the channel within the CDB
	std::string cdbChannelName = "MACI/Channels/" + channelName;
	CDB::DAL_var cdbRef = getCDB();
	CDB::DAO_var tempDAO = cdbRef->get_DAO_Servant(cdbChannelName.c_str());

	//temporary pointer points to the name of the CosNotification
	//property
	const char *name_p;
	//temporary counter
	unsigned int i = 0U;
	
	//MaxQueueLength - TAO 1.1 had a queque of ~5 events.  TAO 1.3 allows an
	//                 infinite amount of events to be stored.  20 seems like
	//                 a reasonable default.
	{
	name_p = CosNotification::MaxQueueLength;
	//allocate one extra element
	i++;
	retVal.length(i);
	
	retVal[i-1].name = CORBA::string_dup(name_p);
	retVal[i-1].value <<= tempDAO->get_long(name_p);
	}

	//Max consumers///////////////////////////////////////////////////
	{
	name_p = CosNotification::MaxConsumers;
	//allocate one extra element
	i++;
	retVal.length(i);
	retVal[i-1].name = CORBA::string_dup(name_p);
	retVal[i-1].value <<= tempDAO->get_long(name_p);
	}
	//Max suppliers///////////////////////////////////////////////////
	{
	name_p = CosNotification::MaxSuppliers;
	//allocate one extra element
	i++;
	retVal.length(i);
	retVal[i-1].name = CORBA::string_dup(name_p);
	retVal[i-1].value <<= tempDAO->get_long(name_p);
	}
	//Reject new events///////////////////////////////////////////////
	{
	name_p = CosNotification::RejectNewEvents;
	//allocate one extra element
	i++;
	retVal.length(i);
	retVal[i-1].name = CORBA::string_dup(name_p);
	std::string tString = tempDAO->get_string(name_p);
	if(tString=="false")
	    {
	    retVal[i-1].value <<= FALSE; 
	    }
	else
	    {
	    retVal[i-1].value <<= TRUE;
	    }
	}

	//for debugging purposes only
	std::string debugMessage = "Length=" + retVal.length();
	STATIC_LOG(Logging::BaseLog::LM_DEBUG, 
		   "nc::CDBProperties::getCDBAdminProps",
		   debugMessage);
	
	return retVal;
    }
    //------------------------------------------------------
    CosNotification::QoSProperties
    CDBProperties::getCDBQoSProps(const std::string& channelName)
    {
	CosNotification::QoSProperties retVal;
	retVal.length(0);
	
	//sanity check
	if (cdbChannelConfigExists(channelName)==false)
	    {
	    return retVal;
	    }
	
	//CDB
	//complete name of the channel within the CDB
	std::string cdbChannelName = "MACI/Channels/" + channelName;
	CDB::DAL_var cdbRef = getCDB();
	CDB::DAO_var tempDAO = cdbRef->get_DAO_Servant(cdbChannelName.c_str());

	//temporary pointer points to the name of the CosNotification
	//property
	const char *name_p;
	//temporary counter
	unsigned int i = 0U;
	
	//commented out because TAO does not support these props
	/*
	//EventReliability///////////////////////////////////////////////
	{
	name_p = CosNotification::EventReliability;
	//allocate one extra element
	i++;
	retVal.length(i);
	retVal[i-1].name = CORBA::string_dup(name_p);
	std::string tStringER = tempDAO->get_string(name_p);
	if(tStringER=="BestEffort")
	    {
	    retVal[i-1].value <<= CosNotification::BestEffort; 
	    }
	else
	    {
	    retVal[i-1].value <<= CosNotification::Persistent;
	    }
	}
	//ConnectionReliability///////////////////////////////////////////////
	{
	name_p = CosNotification::ConnectionReliability;
	//allocate one extra element
	i++;
	retVal.length(i);
	retVal[i-1].name = CORBA::string_dup(name_p);
	std::string tStringCR = tempDAO->get_string(name_p);
	if(tStringCR=="BestEffort")
	    {
	    retVal[i-1].value <<= CosNotification::BestEffort; 
	    }
	else
	    {
	    retVal[i-1].value <<= CosNotification::Persistent;
	    }
	}
	*/
	//Priority/////////////////////////////////////////////////////
	{
	name_p = CosNotification::Priority;
	//allocate one extra element
	i++;
	retVal.length(i);
	retVal[i-1].name = CORBA::string_dup(name_p);
	retVal[i-1].value <<= static_cast<CORBA::Short>(tempDAO->get_long(name_p));
	}
	//Timeout//////////////////////////////////////////////////////
	{
	name_p = CosNotification::Timeout;
	//allocate one extra element
	i++;
	retVal.length(i);
	retVal[i-1].name = CORBA::string_dup(name_p);
	retVal[i-1].value <<= static_cast<TimeBase::TimeT>(tempDAO->get_long(name_p));
	}
	//OrderPolicy///////////////////////////////////////////////
	{
	name_p = CosNotification::OrderPolicy;
	//allocate one extra element
	i++;
	retVal.length(i);
	retVal[i-1].name = CORBA::string_dup(name_p);
	std::string tStringOP = tempDAO->get_string(name_p);
	if(tStringOP=="AnyOrder")
	    {
	    retVal[i-1].value <<= CosNotification::AnyOrder; 
	    }
	else if(tStringOP=="FifoOrder")
	    {
	    retVal[i-1].value <<= CosNotification::FifoOrder; 
	    }
	else if(tStringOP=="PriorityOrder")
	    {
	    retVal[i-1].value <<= CosNotification::PriorityOrder; 
	    }
	else if(tStringOP=="DeadlineOrder")
	    {
	    retVal[i-1].value <<= CosNotification::DeadlineOrder; 
	    }
	else
	    {
	    //DWF-throw exception
	    }
	}
	//DiscardPolicy///////////////////////////////////////////////
	{
	name_p = CosNotification::DiscardPolicy;
	//allocate one extra element
	i++;
	retVal.length(i);
	retVal[i-1].name = CORBA::string_dup(name_p);
	std::string tStringDP = tempDAO->get_string(name_p);
	if(tStringDP=="AnyOrder")
	    {
	    retVal[i-1].value <<= CosNotification::AnyOrder; 
	    }
	else if(tStringDP=="FifoOrder")
	    {
	    retVal[i-1].value <<= CosNotification::FifoOrder; 
	    }
	else if(tStringDP=="PriorityOrder")
	    {
	    retVal[i-1].value <<= CosNotification::PriorityOrder; 
	    }
	else if(tStringDP=="DeadlineOrder")
	    {
	    retVal[i-1].value <<= CosNotification::DeadlineOrder; 
	    }
	else if(tStringDP=="LifoOrder")
	    {
	    retVal[i-1].value <<= CosNotification::LifoOrder; 
	    }
	else
	    {
	    //DWF-throw exception
	    }
	}
	//commented out because TAO does not support these props
	/*
	//Start Time Supported/////////////////////////////////////////
	{
	name_p = CosNotification::StartTimeSupported;
	//allocate one extra element
	i++;
	retVal.length(i);
	retVal[i-1].name = CORBA::string_dup(name_p);
	std::string tStringStartTS = tempDAO->get_string(name_p);
	if(tStringStartTS=="false")
	    {
	    retVal[i-1].value <<= false; 
	    }
	else
	    {
	    retVal[i-1].value <<= true;
	    }
	}
	//Stop Time Supported/////////////////////////////////////////
	{
	name_p = CosNotification::StopTimeSupported;
	//allocate one extra element
	i++;
	retVal.length(i);
	retVal[i-1].name = CORBA::string_dup(name_p);
	std::string tStringStopTS = tempDAO->get_string(name_p);
	if(tStringStopTS=="false")
	    {
	    retVal[i-1].value <<= false; 
	    }
	else
	    {
	    retVal[i-1].value <<= true;
	    }
	}
	*/
	//Max Events per Consumer/////////////////////////////////////
	{
	name_p = CosNotification::MaxEventsPerConsumer;
	//allocate one extra element
	i++;
	retVal.length(i);
	retVal[i-1].name = CORBA::string_dup(name_p);
	retVal[i-1].value <<= static_cast<CORBA::Long>(tempDAO->get_long(name_p));
	}

	//for debugging purposes only
	std::string debugMessage = "Length=" + retVal.length();
	STATIC_LOG(Logging::BaseLog::LM_DEBUG, 
		   "nc::CDBProperties::getCDBQoSProps",
		   debugMessage);
	
	return retVal;
    }
    //------------------------------------------------------
    bool
    CDBProperties::getIntegrationLogs(const std::string& channelName)
    {
	//sanity check
	if (cdbChannelConfigExists(channelName)==false)
	    {
	    return false;
	    }
	
	//CDB
	//complete name of the channel within the CDB
	std::string cdbChannelName = "MACI/Channels/" + channelName;
	CDB::DAL_var cdbRef = getCDB();
	CDB::DAO_var tempDAO = cdbRef->get_DAO_Servant(cdbChannelName.c_str());
	
	//get the raw string
	std::string tString = tempDAO->get_string("IntegrationLogs");

	//just convert the string to real boolean types
	if(tString=="false")
	    {
	    return false;
	    }
	else
	    {
	    return true;
	    }
    }	

//------------------------------------------------------
    CDBProperties::EventHandlerTimeoutMap
    CDBProperties::getEventHandlerTimeoutMap(const std::string& channelName)
    {
	EventHandlerTimeoutMap retVal;

	//sanity check
	if (cdbChannelConfigExists(channelName)==false)
	    {
	    return retVal;
	    }

	//CDB
	//complete name of the channel within the CDB
	std::string cdbChannelName = "MACI/Channels/" + channelName;
	CDB::DAL_var cdbRef = getCDB();
	CDB::DAO_var tempDAO = cdbRef->get_DAO_Servant(cdbChannelName.c_str());
	CDB::stringSeq_var keys;
	//names of all the events
	try {
	keys = tempDAO->get_string_seq("Events");
	}
	catch(...){
	return retVal;
	}
	    
	//another sanity check
	if (keys.ptr()==0)
	    {
	    return retVal;
	    }
	
	//populate the map
	for (CORBA::ULong i=0; i<keys->length(); i++) 
	    {
	    //get the key's value
	    std::string timeoutLocation = "Events/";
	    timeoutLocation =  timeoutLocation + (const char*)keys[i] + "/MaxProcessTime";
	    double value = tempDAO->get_double(timeoutLocation.c_str());
	    
	    //set the map's contents
	    retVal[(const char*)keys[i]] = value;
	    }
	return retVal;
    }

//------------------------------------------------------
};
/*___oOo___*/
