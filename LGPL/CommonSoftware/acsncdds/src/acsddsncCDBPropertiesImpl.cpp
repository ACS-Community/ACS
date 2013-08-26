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
* "@(#) $Id: acsddsncCDBPropertiesImpl.cpp,v 1.5 2010/06/15 09:23:02 hsommer Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* cmaureir  2010-02-02  created 
*/

#include "acsddsncCDBProperties.h"
#include <maciHelper.h>
#include <acsutilPorts.h>
#include <acsutil.h> 

static char *rcsId="@(#) $Id: acsddsncCDBPropertiesImpl.cpp,v 1.5 2010/06/15 09:23:02 hsommer Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

namespace ddsnc {
    //------------------------------------------------------
    CDB::DAL_ptr
    CDBProperties::getCDB()
    {
	ACE_CString nameService;
	ACE_CString managerName;

	managerName = maci::MACIHelper::getManagerHostname(1,NULL);
	//get NameService Reference
	nameService += acscommon::NAMING_SERVICE_NAME;
	nameService +="=corbaloc::";
	nameService += managerName;
	nameService += ":";
	nameService += ACSPorts::getNamingServicePort().c_str();
	nameService += "/";
	nameService += acscommon::NAMING_SERVICE_NAME;

	// ORB
	int argc = 5;
	const char* orbArgs[] = { "",
				"-ORBInitRef",
				nameService.c_str(),
				"-ORBDottedDecimalAddresses",
				"1"};

	CORBA::ORB_var m_orb;
	m_orb = CORBA::ORB_init(argc, const_cast<char**>(orbArgs), "");

	ACE_CString cdbLoc;
	cdbLoc += "corbaloc::";
	cdbLoc += managerName;
	cdbLoc += ":";
	cdbLoc += ACSPorts::getCDBPort().c_str();
	cdbLoc += "/CDB";

	CDB::DAL_var dalObj = CDB::DAL::_nil();
	CORBA::Object_var obj = m_orb->string_to_object(cdbLoc.rep());
	    
	if (!CORBA::is_nil(obj.in()))
	   {
		dalObj = CDB::DAL::_narrow(obj.in());
		if (CORBA::is_nil(dalObj.in())) 
		    {
		    ACS_STATIC_LOG(LM_FULL_INFO, " CDBProperties::getCDB", (LM_ERROR,
				         "Failed to narrow CDB."));
		    }
	   }

        return dalObj._retn();

    }
    //------------------------------------------------------
    bool 
    CDBProperties::cdbChannelConfigExists(CORBA::String_var channelName)
    {
	//complete name of the channel within the CDB
	std::string tmpChannelName(channelName._retn());
	std::string cdbChannelName = "MACI/Channels/" + tmpChannelName;
	 
	//delegate obtaining a reference to the CDB
	CDB::DAL_var cdbRef = getCDB();


	//sanity check
	if(cdbRef.in()==0)
	    {
	    ACS_STATIC_LOG(LM_FULL_INFO, "CDBProperties::cdbChannelConfigExists", (LM_ERROR,
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
	    ACS_STATIC_LOG(LM_FULL_INFO, "CDBProperties::cdbChannelConfigExists", (LM_INFO,
				  "No CDB entry found for '%s' channel. OK.",
				  cdbChannelName.c_str()));
	    return false;
	    }
    }
    //------------------------------------------------------
    DDS::QosPolicyCountSeq
    CDBProperties::getCDBQoSProps(CORBA::String_var channelName)
    {
	DDS::QosPolicyCountSeq retVal;
	retVal.length(0);

	//sanity check
	if (cdbChannelConfigExists(channelName)==false)
	    {
	    return retVal;
	    }
	//CDB
	//complete name of the channel within the CDB
	std::string tmpChannelName(channelName._retn());
	std::string cdbChannelName = "MACI/Channels/" + tmpChannelName;
	CDB::DAL_var cdbRef = getCDB();
	CDB::DAO_var tempDAO = cdbRef->get_DAO_Servant(cdbChannelName.c_str());

	//property
	const char *name_p;
	//temporary counter
	unsigned int i = 0U;

	
	//Transport Priority/////////////////////////////////////////////////////
	{
	//name_p = DDS::TRANSPORTPRIORITY_QOS_POLICY_NAME;
	name_p = "Priority";
	//allocate one extra element
	i++;
	retVal.length(i);
	retVal[i-1].policy_id = DDS::TRANSPORTPRIORITY_QOS_POLICY_ID;
	retVal[i-1].count <<= static_cast<CORBA::Short>(tempDAO->get_long(name_p));
	}
	//Timeout//////////////////////////////////////////////////////
	{
	//name_p = CosNotification::Timeout;
	name_p = "Timeout";
	//This is a simpleTCP !
	//allocate one extra element
	i++;
	retVal.length(i);
	retVal[i-1].policy_id = 30; 
	retVal[i-1].count <<= static_cast<TimeBase::TimeT>(tempDAO->get_long(name_p));
	}

	//OrderPolicy///////////////////////////////////////////////
	{
	name_p = "OrderPolicy";
	//allocate one extra element
	i++;
	retVal.length(i);
	retVal[i-1].policy_id = DDS::DESTINATIONORDER_QOS_POLICY_ID;
	std::string tStringOP = tempDAO->get_string(name_p);
	if(tStringOP=="AnyOrder")
	    {
	    retVal[i-1].count <<= 0;
	    }
	else if(tStringOP=="FifoOrder")
	    {
	    retVal[i-1].count <<= 0;
	    }
	else if(tStringOP=="PriorityOrder")
	    {
	    retVal[i-1].count <<= 1;
	    }
	else if(tStringOP=="DeadlineOrder")
	    {
 	    retVal[i-1].count <<= 1;
	    }
	else
	    {
	    //DWF-throw exception
	    }
	}
	//MaxQueueLength - TAO 1.1 had a queque of ~5 events.  TAO 1.3 allows an
	//                 infinite amount of events to be stored.  20 seems like
	//                 a reasonable default.
	{
	name_p = "MaxQueueLength";
	//allocate one extra element
	i++;
	retVal.length(i);
	retVal[i-1].policy_id = DDS::HISTORY_QOS_POLICY_ID; 
	retVal[i-1].count <<= tempDAO->get_long(name_p);
	// 0 is not valid, the integer must be > 1
	if( retVal[i-1].count == 0)
		retVal[i-1].count += 2;
	if( retVal[i-1].count == 1)
		retVal[i-1].count += 1;
	}

	//for debugging purposes only
	std::string debugMessage = "Length=" + retVal.length();
	STATIC_LOG(Logging::BaseLog::LM_DEBUG, 
		   "ddsnc::CDBProperties::getCDBQoSProps",
		   debugMessage);

	return retVal;

    }
}
