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
* "@(#) $Id: bdNTReceiverImplTest.cpp,v 1.6 2011/11/09 12:01:36 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/
#include "bulkDataNTReceiverImpl.h"
#include "bulkDataNTCallback.h"
#include <iostream>

class  TestCB:  public BulkDataNTCallback
{
public:
	int cbStart(unsigned char* userParam_p, unsigned  int size)
	{
		std::cout << "=>cbStart[" << recvName_m << "/" << flowName_m << "]: got " << size << " :";
		for(unsigned int i=0; i<size; i++)
		{
			std::cout <<  *(char*)(userParam_p+i);
		}
		std::cout << std::endl;
		return 0;
	}

	int cbReceive(unsigned char* userParam_p, unsigned  int size)
	{
		std::cout << "=>cbReceive[" << recvName_m << "/" << flowName_m << "]: got " << size << " :";
		/*		for(unsigned int i=0; i<frame_p->length(); i++)
		{
			std::cout <<  *(char*)(frame_p->base()+i);
		}
		 */	std::cout << std::endl;

	/*	 if (sleep_period!=0)
		 {
			 cout << listName << " Rest:" << message.restDataLength << " Going sleep for: " << sleep_period << endl;
			 //cout <<  message.data.length() << endl;
			 //cout <<  message.restDataLength << endl;
			 usleep(sleep_period);
		 }
		*/
		 /* simulate seg fault
    			if (data_length>100000) {
    			char *tt=0;
    			printf("XXX %s\n", tt);
    			printf("crash\n");
    			ACE_Time_Value *t=0;
    			t->sec();
    			DDS::DataReaderQos *ddr_qos=0;
    			ddr_qos->reliability.kind = 0;
    			printf("after crash\n");
    			} */

		 return 0;
	}

	int cbStop()
	{
		std::cout << "=>cbStop[" << recvName_m << "/" << flowName_m << "]" << std::endl;
		return 0;
	}

};


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(BulkDataNTReceiverImpl<TestCB>)
/* ----------------------------------------------------------------*/
