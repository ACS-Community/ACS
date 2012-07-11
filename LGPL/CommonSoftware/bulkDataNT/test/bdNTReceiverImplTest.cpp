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
* "@(#) $Id: bdNTReceiverImplTest.cpp,v 1.7 2012/07/11 08:46:26 bjeram Exp $"
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
		ACS_SHORT_LOG((LM_INFO, "=>cbStart[%s/%s]: got %d: %s", recvName_m.c_str(), flowName_m.c_str(), size, userParam_p));
		//std::cout << "=>cbStart[" << recvName_m << "/" << flowName_m << "]: got " << size << " :";

		return 0;
	}

	int cbReceive(unsigned char* userParam_p, unsigned  int size)
	{
		//std::cout << "=>cbReceive[" << recvName_m << "/" << flowName_m << "]: got " << size << " :";
		ACS_SHORT_LOG((LM_INFO, "=>cbStart[%s/%s]: got %d.", recvName_m.c_str(), flowName_m.c_str(), size));

		 return 0;
	}

	int cbStop()
	{
		//std::cout << "=>cbStop[" << recvName_m << "/" << flowName_m << "]" << std::endl;
		ACS_SHORT_LOG((LM_INFO, "=>cbStop[%s/%s]", recvName_m.c_str(), flowName_m.c_str()));
		return 0;
	}

};


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(BulkDataNTReceiverImpl<TestCB>)
/* ----------------------------------------------------------------*/
