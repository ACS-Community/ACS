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
* "@(#) $Id: stressManager.cpp,v 1.1 2004/12/15 16:26:27 dfugate Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-06-17 fixed client.init(argc,argv)
* gchiozzi 2002-02-13 Cleaned up
* msekoran  17/02/01  created 
*/


#include <maciSimpleClient.h>

ACE_RCSID(acsexmpl, acsexmplListCOBS, "$Id: stressManager.cpp,v 1.1 2004/12/15 16:26:27 dfugate Exp $")
using namespace maci;

/*---------------------------------------------------------------------------------------*/

int main(int argc, char *argv[]) 
{
    ACS_SHORT_LOG((LM_INFO, "Welcome to %s!", argv[0]));

    long numLogins = std::atol(argv[1]);
    long sleepTime = std::atol(argv[2]);

    //Creates and initializes the SimpleClient object
    SimpleClient client;
    if (client.init(argc,argv) == 0)
	{
	ACE_DEBUG((LM_DEBUG,"Cannot init client"));
	return -1;
	}

    for (long i=0; i< numLogins; i++)
	{
	client.login();

	ACE_OS::sleep(sleepTime);
	client.logout();
	}

    ACS_SHORT_LOG ((LM_INFO, "The end!"));
    return 0;
}



