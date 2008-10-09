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
* "@(#) $Id: testCDBProps.cpp,v 1.7 2008/10/09 07:57:41 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-04-25  created
*/

/************************************************************************
*   NAME
*   
* 
*   SYNOPSIS
*   
* 
*   DESCRIPTION
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


#include <baci.h>
#include <acscomponentImpl.h>
#include "EventComponentS.h"
#include "acsncCDBProperties.h"

#include <basencHelper.h>

 using namespace baci;

/**
 * This class is for testing the notification channel under an activator
 */
class CDBPropsCompImpl: public virtual acscomponent::ACSComponentImpl,
			public virtual POA_demo::ConsumerComp
{
  public:
    /* ----------------------------------------------------------------*/
    /**
     * Constructor
     * @param poa Poa which will activate this and also all other COBs. 
     * @param name DO's name. This is also the name that will be used to find the
     * configuration data for the DO in the Configuration Database.
     */
    CDBPropsCompImpl(const ACE_CString& name,
		     maci::ContainerServices *cs) :
	acscomponent::ACSComponentImpl(name, cs)
	{
	    ;
	}
    /**
     * Destructor
     */
    virtual ~CDBPropsCompImpl() {;}
    
    
    virtual void initialize()
	{

	    //first try on one that exists in the CDB
	    CosNotification::AdminProperties joe;
	    {
	    joe = nc::CDBProperties::getCDBAdminProps("blar");
	    }
	    std::cout << "getCDBAdminProps(blar):";
	    for (unsigned int i=0; i <joe.length(); i++)
		{
		std::cout << " " << joe[i].name;
		}
	    std::cout << std::endl;

	    //next try on one that is not in the CDB
	    {
	    joe = nc::CDBProperties::getCDBAdminProps("NotInCDB");
	    }
	    std::cout << "getCDBAdminProps(NotInCDB):";
	    for (unsigned int i=0; i <joe.length(); i++)
		{
		std::cout << " " << joe[i].name;
		}
	    std::cout << std::endl;
	    
	    
	    //first try on one that exists in the CDB
	    CosNotification::QoSProperties joe2;
	    {
	    joe2 = nc::CDBProperties::getCDBQoSProps("blar");
	    }
	    std::cout << "getCDBQoSProps(blar):";
	    for (unsigned int i=0; i <joe2.length(); i++)
		{
		std::cout << " " << joe2[i].name;
		}
	    std::cout << std::endl;

	    //next try on one that is not in the CDB
	    {
	    joe2 = nc::CDBProperties::getCDBQoSProps("NotInCDB");
	    }
	    std::cout << "getCDBQoSProps(NotInCDB):";
	    for (unsigned int i=0; i <joe2.length(); i++)
		{
		std::cout << " " << joe2[i].name;
		}
	    std::cout << std::endl;

	    //-----------------------------------------------
	    {
	    nc::CDBProperties::EventHandlerTimeoutMap ncMap = nc::CDBProperties::getEventHandlerTimeoutMap("blarIL");
	    std::cout << "getEventHandlerTimeoutMap(blarIL):" << ncMap.count("EventDescription") << std::endl;
	    }
	    {
	    nc::CDBProperties::EventHandlerTimeoutMap ncMap = nc::CDBProperties::getEventHandlerTimeoutMap("blar");
	    std::cout << "getEventHandlerTimeoutMap(blar):" << ncMap.count("EventDescription") << std::endl;
	    }
	    {
	    nc::CDBProperties::EventHandlerTimeoutMap ncMap = nc::CDBProperties::getEventHandlerTimeoutMap("nah");
	    std::cout << "getEventHandlerTimeoutMap(nah):" << ncMap.count("EventDescription") << std::endl;
	    }

	    //-----------------------------------------------
		
		CDB::DAL_var cdb = nc::CDBProperties::getCDB();
		
		// default expected
		CORBA::String_var res = BaseHelper::getNotificationFactoryNameForChannel(cdb.in(), "any");
		std::cout << "default: " << (res.in() ? res.in() : "(null)") << std::endl;
		
		// channel mapping 
		res = BaseHelper::getNotificationFactoryNameForChannel(cdb.in(), "PARTICULAR");
		std::cout << "particular: " << (res.in() ? res.in() : "(null)") << std::endl;
		
		// wildchars channel mapping 
		res = BaseHelper::getNotificationFactoryNameForChannel(cdb.in(), "CONTROL_CHANNEL");
		std::cout << "wildcard: " << (res.in() ? res.in() : "(null)") << std::endl;
		
		// domain mapping
		res = BaseHelper::getNotificationFactoryNameForChannel(cdb.in(), "anyOnLaser", "ALARMSYSTEM");
		std::cout << "domain: " << (res.in() ? res.in() : "(null)") << std::endl;
		
		// fallback to default
		res = BaseHelper::getNotificationFactoryNameForChannel(cdb.in(), "anyOnNonExistingDomain", "NONEXISTING_DOMAIN");
		std::cout << "non-existing domain: " << (res.in() ? res.in() : "(null)") << std::endl;
	}
    
  private:
    
};

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(CDBPropsCompImpl)
/* ----------------------------------------------------------------*/
