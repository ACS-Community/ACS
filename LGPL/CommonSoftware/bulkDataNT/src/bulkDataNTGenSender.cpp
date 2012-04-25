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
* "@(#) $Id: bulkDataNTGenSender.cpp,v 1.1 2012/04/25 12:55:29 bjeram Exp $"
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


void executeF()
{
	double send_time;

	ACE_Time_Value start_time, elapsed_time;

    try{



    	SenderFlowConfiguration sndFlowCfg;
   		sndFlowCfg.setACKsTimeout(5.0);

    	BulkDataNTSenderStream senderStream1("DefaultStream");

    	BulkDataNTSenderFlow* flow0 = senderStream1.createFlow("00", sndFlowCfg);
    	BulkDataNTSenderFlow* flow1 = senderStream1.createFlow("01", sndFlowCfg);

    	sleep(1); //here we should wait for at least one receiver
    	//std::cout << "press a key to start.." << std::endl;

    	//getchar();

    	unsigned char parm[]="123";

    	flow0->startSend(parm, 3);
    	strcpy(parm, "abc");
  		flow1->startSend(parm, 3);

    	//unsigned char data[]="Hello wrold !!!!";
    	unsigned char *data= new unsigned char[dataSize];
    	for (unsigned int i=0; i<dataSize; i++)
    		data[i]=i;

    	ACS_SHORT_LOG((LM_INFO, "Going to send: %d Bytes to %d receivers", dataSize, flow0->getNumberOfReceivers()));
    	start_time = ACE_OS::gettimeofday();
    	flow0->sendData(data, dataSize);
    	elapsed_time = ACE_OS::gettimeofday() - start_time;
    	send_time = (elapsed_time.sec()+( elapsed_time.usec() / 1000000. ));
    	ACS_SHORT_LOG((LM_INFO, "Transfer rate: %f", (dataSize/(1024.0*1024.0))/send_time));

    	try
    	{
    		flow1->sendData(data, dataSize);
    	}
    	catch(ACSErr::ACSbaseExImpl &ex)
    	{
    		ex.log();
    	}

    	for (unsigned int i=0; i<dataSize; i++)
    		data[i]=i%10;

    	ACS_SHORT_LOG((LM_INFO, "Going to send: %d Bytes to %d receivers", dataSize, flow0->getNumberOfReceivers()));
    	start_time = ACE_OS::gettimeofday();
    	flow0->sendData(data, dataSize);
    	elapsed_time = ACE_OS::gettimeofday() - start_time;
    	send_time = (elapsed_time.sec()+( elapsed_time.usec() / 1000000. ));
    	ACS_SHORT_LOG((LM_INFO, "Transfer rate: %f", (dataSize/(1024.0*1024.0))/send_time));




    	try
    	{
    		ACS_SHORT_LOG((LM_INFO, "Going to send: %d Bytes to %d receivers", dataSize, flow0->getNumberOfReceivers()));
    		start_time = ACE_OS::gettimeofday();
    		flow1->sendData(data, dataSize);
    		elapsed_time = ACE_OS::gettimeofday() - start_time;
    		send_time = (elapsed_time.sec()+( elapsed_time.usec() / 1000000. ));
    		ACS_SHORT_LOG((LM_INFO, "Transfer rate: %f", (dataSize/(1024.0*1024.0))/send_time));
    	}
    	catch(ACSErr::ACSbaseExImpl &ex)
    	{
    		ex.log();
    	}


    	sleep(2);

    	flow0->stopSend();
    	flow1->stopSend();




    	delete flow0;
    }
    catch(ACSErr::ACSbaseExImpl &ex)
    {
    	ex.log();
    }


// flow1 will be deleted when senderStream1 is deleted
}//executeF


int main(int argc, char *argv[])
{
	char c;

	LoggingProxy m_logger(0, 0, 31, 0);

	LoggingProxy::init (&m_logger);
    ACS_CHECK_LOGGER;


	// Parse the args
    ACE_Get_Opt get_opts (argc, argv, "w:s:");
    while(( c = get_opts()) != -1 ) {
    	switch(c) {
    	case 'w':
    		sleepPeriod = atoi(get_opts.opt_arg());
    		break;
    	case 's':
    		dataSize = atoi(get_opts.opt_arg());
    		break;
    	}
    }//while

    ACS_SHORT_LOG((LM_INFO, "Is new bulk data enabled (ENABLE_BULKDATA_NT) %d", isBulkDataNTEnabled()));
    executeF();

    std::cout << "press a key to exit.." << std::endl;
    getchar();


    m_logger.flush();
};//main
