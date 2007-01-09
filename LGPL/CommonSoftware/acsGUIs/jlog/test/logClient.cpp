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
* "@(#) $Id: logClient.cpp,v 1.4 2007/01/09 13:57:49 acaproni Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni 2006-10-15 created
*/

#include <maciSimpleClient.h>

/**
 * This process is a SimpleClient that sends logs
 * 
 * Usage:
 *    - logClient 0
 *    - logClient 1 nIter sendDone
 * 
 * If first param is 0 then the process sends 100 logs with a defined 
 * message reporting the number of the log.
 * This is used to check if all the messages are published and received
 * by the LCEngine
 *  
 * If the first param is 1 then the process executes nIter iteration
 * and in each iteration it sends one message of each possible
 * log type.
 * If sendDone==0 the process sends a log containing the Done message
 * (that is used as a termination message fro the client listening 
 * for logs)
 * This second format is used as a stress test to based on the number
 * of logs received. The difference between this case and the former 
 * is that the test is now performed on a great numer of messages
 * produced by several clients (copies of logClient) publishing messages
 * in parallel.
 *  
 */
using namespace maci;

int main(int argc, char *argv[])
{
    // Creates and initializes the SimpleClient object
    SimpleClient client;
    if (client.init(argc,argv) == 0)
        {
        return -1;
        }
    else
        {
        //Must log into manager before we can really do anything
        client.login();
        }
        
    int mode = atol(argv[1]);
	
	if (mode==1) {
	    for (int t=0; t<100; t++) 
		{
		ACS_SHORT_LOG ((LM_INFO, "Test message %d",t));
		}
	} else if (mode==2) {
		for (int t=0; t<atol(argv[2]); t++) 
		{
			ACS_SHORT_LOG ((LM_TRACE, "Test message"));
			usleep(10);
			ACS_SHORT_LOG ((LM_DEBUG, "Test message %d",t));
			usleep(10);
			ACS_SHORT_LOG ((LM_INFO, "Test message %d",t));
			usleep(10);
			ACS_SHORT_LOG ((LM_NOTICE, "Test message %d",t));
			usleep(10);
			ACS_SHORT_LOG ((LM_WARNING, "Test message %d",t));
			usleep(10);
			ACS_SHORT_LOG ((LM_ERROR, "Test message %d",t));
			usleep(10);
			ACS_SHORT_LOG ((LM_CRITICAL, "Test message %d",t));
			usleep(10);
			ACS_SHORT_LOG ((LM_ALERT, "Test message %d",t));
			usleep(10);
			ACS_SHORT_LOG ((LM_EMERGENCY, "Test message %d",t));
			usleep(250);
		}
		if (atol(argv[3])!=0) 
			{
			ACS_SHORT_LOG ((LM_INFO, "Done"));
			}
		sleep(10);
	}

	ACE_OS::sleep(5);
    client.logout();
	ACE_OS::sleep(60);
    return 0;
}
