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
*
* "@(#) $Id: acsexmplClientTower.cpp,v 1.8 2005/09/21 13:14:49 vwang Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* oat 2002-12-17 templatization of the client.get_object method
* david 2002-06-17 changed client.init(argc,argv) for improved error checking
* gchiozzi 2002-02-13 cleane up
* msekoran  2001/07/13  created 
*/

/** @file acsexmplClientTower.cpp
 *  The only purpose of this file is to test a known bug involving the Constructable 
 *  interface.  The Building example derives from this interface.
 *  @htmlonly
 *  <br><br>
 *  @endhtmlonly

 *  @param "TOWER COB in the CDB" Use this required parameter to specify which TOWER object
 *  should be activated.
 *  @htmlonly
 *  <br><hr>
 *  @endhtmlonly

 *  @param "-ORBEndpoint iiop://yyy:xxxx" Use this optional parameter to specify which host/port SimpleClient
 *  should run on.
 *  @htmlonly
 *  <br><hr>
 *  @endhtmlonly

 *  @param "-ORBInitRef NameService=corbaloc::yyy:xxxx/NameService" Use this optional parameter to specify which 
 *  host/port SimpleClient should get a reference to the naming service from.
 *  @htmlonly
 *  <br><hr>
 *  @endhtmlonly

 *  @param "-ORBInitRef NotifyEventChannelFactory=corbaloc::yyy:xxxx/NotifyEventChannelFactory" Use this optional 
 *  parameter to specify which host/port SimpleClient should get a reference to the notification service from.
 *  @htmlonly
 *  <br><hr>
 *  @endhtmlonly
 */  

#include <vltPort.h>
#include <acsutil.h>

#include <maciSimpleClient.h>
#include <acsexmplBuildingC.h>
#include <baciS.h>

using namespace maci;
ACE_RCSID(acsexmplClientTower, acsexmpClientTower, "$Id: acsexmplClientTower.cpp,v 1.8 2005/09/21 13:14:49 vwang Exp $")    
/*******************************************************************************/
int main(int argc, char *argv[])
{
    if (argc < 2)
	return -1;
    
    SimpleClient client;
    
    if (client.init(argc,argv) == 0)
	{
	ACE_DEBUG((LM_DEBUG,"Cannot init client"));
	return -1;
	}
    client.login();
    
    
    try
	{
	ACS_SHORT_LOG((LM_INFO, "Getting COB: %s", argv[1]));
	acsexmplBuilding::Building_var tower = client.get_object<acsexmplBuilding::Building>(argv[1], 0, true);
	
	
	if (!CORBA::is_nil(tower.in()))
	    {	  
	    ACS_SHORT_LOG((LM_INFO, "Got COB: %s", argv[1]));
	    }
	else
	    {
	    ACS_SHORT_LOG((LM_INFO, "Unable to access COB: %s", argv[1]));
	    }
	}
    catch( CORBA::Exception &ex )
	{
	ACE_PRINT_EXCEPTION (ex, "main");
	}
    
    try
	{
	ACS_SHORT_LOG((LM_INFO,"Releasing..."));
	client.manager()->release_component(client.handle(), argv[1]);
	client.logout();
	}
    catch( CORBA::Exception &_ex )
	{
	ACE_PRINT_EXCEPTION (_ex, "main");
	}
    
    ACE_OS::sleep(3);
    return 0;
}
/*___oOo___*/




