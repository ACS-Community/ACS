/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2011
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
* "@(#) $Id: bdNTSenderMLTest.cpp,v 1.1 2012/07/10 14:46:41 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/
#include "bulkDataNTSenderFlow.h"
#include <iostream>
#include <ace/Get_Opt.h>


using namespace AcsBulkdata;

unsigned int sleepPeriod=0;
unsigned int dataSize=65000;


/*
 * In this function we create a stream and two flows.
 * If we want to test it deletion of streams/flows and thier recreation we run it severla (2) times
 */
void OneSenderFlowGo(int iterations)
{

	try{
		double send_time;

		ACE_Time_Value start_time, elapsed_time;

		SenderFlowConfiguration sndFlowCfg;
		sndFlowCfg.setACKsTimeout(5.0);


		while(iterations > 0)
		{
		BulkDataNTSenderStream senderStream1("DefaultStream");

		BulkDataNTSenderFlow* flow0 = senderStream1.createFlow("00", sndFlowCfg);
		BulkDataNTSenderFlow* flow1 = senderStream1.createFlow("01", sndFlowCfg);
		std::string flow0Name= flow0->getName();
		std::string flow1Name= flow1->getName();
		ACS_SHORT_LOG((LM_INFO, "Flows: %s and %s have been created.", flow0Name.c_str(), flow1Name.c_str()));


		unsigned char parm[]="123";

		ACS_SHORT_LOG((LM_INFO, "Going to send parameter (invoking startSend) %s on flow: %s to %d receivers", parm, flow0Name.c_str(), flow0->getNumberOfReceivers()));
		flow0->startSend(parm, 3);

		ACS_SHORT_LOG((LM_INFO, "Going to send stop  (invoking stopSend) on flow: %s to %d receivers", flow0Name.c_str(), flow0->getNumberOfReceivers()));
		flow0->stopSend();

		ACS_SHORT_LOG((LM_INFO, "Let's try to delete the flow: %s ... ", flow0Name.c_str()));
		delete flow0;

		ACS_SHORT_LOG((LM_INFO, "... and then (re)create it (flow: %s) again ...", flow0Name.c_str()));
		flow0 = senderStream1.createFlow("00");//, sndFlowCfg);
		parm[0]='4';
		ACS_SHORT_LOG((LM_INFO, "... and send parameter %s to (re)created flow: %s", parm, flow0Name.c_str()));
		flow0->startSend(parm, 3);


		ACS_SHORT_LOG((LM_INFO, "flow: %s will be deleted when 'DefaultStream' is deleted - when the stream object goes out of scope", flow1Name.c_str()));

		/*
		printf("A key for next iteration. e for exit\n");
		int c = getchar();
		if (c=='e')	break;
*/
		system("top -b -p `pgrep -d, bdNTSenderML` -n 1 >>  ./mem_prof.txt");
		iterations--;
		}
	}
	catch(ACSErr::ACSbaseExImpl &ex)
	{
		ex.log();
	}

}//OneSenderFlowGo


void print_usage(char *argv[]) {
	cout << "Usage: " << argv[0] << "[-i # of iterations] [-s data size in bytes] [-w end_test_wait_period]" << endl;
	exit(1);
}

int main(int argc, char *argv[])
{
	char c;
	int iterations=1;

	LoggingProxy m_logger(0, 0, 31, 0);

	LoggingProxy::init (&m_logger);
    ACS_CHECK_LOGGER;


	// Parse the args
    ACE_Get_Opt get_opts (argc, argv, "w:s:i:");
    while(( c = get_opts()) != -1 ) {
    	switch(c) {
    	case 'i':
    		iterations = atoi(get_opts.opt_arg());
    		break;
    	case 'w':
    		sleepPeriod = atoi(get_opts.opt_arg());
    		break;
    	case 's':
    		dataSize = atoi(get_opts.opt_arg());
    		break;
    	default:
    		print_usage(argv);
    		break;
    	}
    }//while

    ACS_SHORT_LOG((LM_INFO, "Is new bulk data enabled (ENABLE_BULKDATA_NT) %d", isBulkDataNTEnabled()));

    OneSenderFlowGo(iterations);

    if (sleepPeriod>0)
    {
    	sleep(sleepPeriod);
    }
    else
    {
    	std::cout << "press a key to exit.." << std::endl;
    	getchar();
    }

    m_logger.flush();
};//main

