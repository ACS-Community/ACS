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
 *
 * "@(#) $Id: componentsBulkDataTest.cpp,v 1.5 2005/05/27 19:07:10 dfugate Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * oat       2003-07-18 created
 */


#include <maciSimpleClient.h>

#include <perftestC.h>
#include <perftestBDC.h>
#include "bulkDataReceiverC.h"

#include <baci.h>

#include "orbsvcs/AV/AVStreams_i.h"
#include "ACSBulkDataError.h"


using namespace maci;
using namespace ACSBulkDataError;

int main(int argc, char *argv[])
{
    CORBA::ULong invocations = static_cast<CORBA::ULong>(std::atol(argv[3]));
    unsigned long long size = static_cast<unsigned long long>(std::atol(argv[4]));
    
    // Creates and initializes the SimpleClient object
    SimpleClient client;
    
    if (client.init(argc,argv) == 0)
	{
	return -1;
	}
    else
	{
	client.login();
	}
    
    bulkdata::BulkDataReceiver_var receiver = client.get_object<bulkdata::BulkDataReceiver>(argv[1], 0, true);
    perftestBD::BulkDataSenderEx1_var sender = client.get_object<perftestBD::BulkDataSenderEx1>(argv[2], 0, true);

    
    sender->connect(receiver.in());
    sender->setSize(size);
    
    for (CORBA::ULong i=0; i < invocations; i++)
	{
	sender->startSend();
	sender->stopSend();
	ACE_OS::sleep(3);
	}
    
    sender->disconnect();
    ACE_OS::sleep(3);
    receiver->closeReceiver();
    ACE_OS::sleep(3);

    //We release our component and logout from manager
    client.manager()->release_component(client.handle(), argv[2]);
    ACE_OS::sleep(3);
    client.manager()->release_component(client.handle(), argv[1]);
    ACE_OS::sleep(3);
    client.logout();
    
    ACS_SHORT_LOG((LM_INFO,"Sleeping 3 sec to allow everything to cleanup and stabilize"));  
    ACE_OS::sleep(3);
    return 0;
}


/*___oOo___*/
