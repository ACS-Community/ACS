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
 * "@(#) $Id: bdNT32SendersTest.cpp,v 1.3 2012/01/04 10:43:44 bjeram Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * bjeram  2011-04-19  created
 */
#include "bulkDataNTSenderFlow.h"
#include <iostream>
#include <ace/Get_Opt.h>


using namespace AcsBulkdata;

int main(int argc, char *argv[])
{
  char c;
  double send_time;
  unsigned int sleepPeriod=0;
  unsigned int dataSize=65000;
  ACE_Time_Value start_time, elapsed_time;

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


  SenderStreamConfiguration rcvCfg;
  //rcvCfg.DDSLogVerbosity= NDDS_CONFIG_LOG_VERBOSITY_STATUS_ALL;
  BulkDataNTSenderStream  senderStream("TestSenderSTream");


  char buf[]="00";
  BulkDataNTSenderStream* senderStreams[32];
  for (int i=0; i<32; i++)
    {
      sprintf(buf, "%d", i);
      std::string streamName("Stream");
      streamName += buf;
//      std::cout << "Going to create stream: " << streamName << std::endl;
      //    senderStreams[i] = new BulkDataNTSenderStream(streamName.c_str(), rcvCfg);
      //     std::cout << "Stream: " << streamName << " has been created. Going to create a flow inside the stream" << std::endl;
      senderStream.createFlow(streamName.c_str());
    }

  getchar();
  return 0;
  for (int i=0; i<32; i++)
      {
        std::cout << "Going to destroy stream: " << senderStreams[i]->getName() << std::endl;
        delete senderStreams[i];
      }


  return 0;
}
