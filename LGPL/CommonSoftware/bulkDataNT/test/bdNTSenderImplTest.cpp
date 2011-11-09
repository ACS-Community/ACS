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
* "@(#) $Id: bdNTSenderImplTest.cpp,v 1.3 2011/11/09 12:01:36 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/
#include "bdNTSenderImplTest.h"

bdNTSenderImplTest::bdNTSenderImplTest(const ACE_CString& name,maci::ContainerServices* containerServices) :
BulkDataNTSenderImpl(name, containerServices)
{
    ACS_TRACE("bdNTSenderImplTest::bdNTSenderImplTest");

}

bdNTSenderImplTest::~bdNTSenderImplTest()
{
    ACS_TRACE("bdNTSenderImplTest::~bdNTSenderImplTest");
}

void bdNTSenderImplTest::startSend()
{
    ACS_TRACE("bdNTSenderImplTest::startSend");
    unsigned char parm[]="Test Parameter";

    getSenderStream()->getFlow("0")->startSend(parm, 14);
    getSenderStream()->getFlow("1")->startSend(parm, 14);
}

void bdNTSenderImplTest::paceData ()
{
	ACS_TRACE("bdNTSenderImplTest::paceData");
	unsigned char *data= new unsigned char[65000];
	for (unsigned int i=0; i<65000; i++)
		data[i]=i;
	getSenderStream()->getFlow("0")->sendData(data, 65000);

	for (unsigned int i=0; i<65000; i++)
		data[i]=i%10;
	getSenderStream()->getFlow("1")->sendData(data, 65000);

}

void bdNTSenderImplTest::stopSend()
{
    ACS_TRACE("bdNTSenderImplTest::stopSend");
    getSenderStream()->getFlow("0")->stopSend();
    getSenderStream()->getFlow("1")->stopSend();
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(bdNTSenderImplTest)
/* ----------------------------------------------------------------*/
