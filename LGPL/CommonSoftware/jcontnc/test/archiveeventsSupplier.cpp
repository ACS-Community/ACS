/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) National Research Council of Canada, 2005 
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
* "@(#) $Id$"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-09-14  created
*/

/************************************************************************
*   NAME
*   
* 
*   SYNOPSIS
*   
* 
*   DESCRIPTION
*
*   FILES
*
*   ENVIRONMENT
*
*   COMMANDS
*
*   RETURN VALUES
*
*   CAUTIONS 
*
*   EXAMPLES
*
*   SEE ALSO
*
*   BUGS   
* 
*------------------------------------------------------------------------
*/

// Uncomment this if you are using the VLT environment
// #include "vltPort.h"

#include <baci.h>
#include <maciSimpleClient.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

using namespace baci;
using namespace maci;

static char *rcsId="@(#) $Id$"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

//helper function which sends out an event of some BACIValue type
template<class T> 
void
sendEvent(const T value)
{
    //force it to the correct type just in case.
    BACIValue bVal(static_cast<T>(value));

    //send it out
    ACS_ARCHIVE_PRIORITY("no_device",
			 "no_param",
			 "no_type",
			 bVal,
			 0);
}

int main(int argc, char *argv[])
{
    //setup the archive event supplier
    SimpleClient client;    
    if (client.init(argc,argv) == 0)
	{
	ACE_DEBUG((LM_DEBUG,"Cannot init client"));
	return -1;
	}
    else
	{
	//Must log into manager before we can really do anything
	client.login();
	}
    
    //get the naming service to use with archive supplier's init method
    CORBA::Object_var nc_obj = client.manager()->get_service(client.handle(), "NameService", true);
    CosNaming::NamingContext_var nc = CosNaming::NamingContext::_narrow(nc_obj.in());
    
    //initialize the singleton
    ArchiveSupplierSingleton::Instance().init(nc.in());
    
    //double
    sendEvent<BACIdouble>((BACIdouble)3.14);
    
    //flaot
    sendEvent<BACIfloat>((BACIfloat)3.1);
    
    //long
    sendEvent<BACIlong>((BACIlong)1);

    //pattern
    sendEvent<BACIpattern>((BACIpattern)2);
    
    //long long
    sendEvent<BACIlongLong>((BACIlongLong)3);
    
    //unsigned long long
    sendEvent<BACIuLongLong>((BACIuLongLong)4);

    //string
    sendEvent<const char*>((const char*)"a string");

    //double seqence
    {
    BACIdoubleSeq mySeq(1);
    mySeq.length(1);

    mySeq[0] = (BACIdouble)3.14;
    sendEvent<BACIdoubleSeq>((BACIdoubleSeq)mySeq);
    }

    //float seqence
    {
    BACIfloatSeq mySeq(1);
    mySeq.length(1);

    mySeq[0] = (BACIfloat)3.1;
    sendEvent<BACIfloatSeq>((BACIfloatSeq)mySeq);
    }

    //long seqence
    {
    BACIlongSeq mySeq(1);
    mySeq.length(1);

    mySeq[0] = (BACIlong)1;
    sendEvent<BACIlongSeq>((BACIlongSeq)mySeq);
    }

    //string seqence
    {
    BACIstringSeq mySeq(1);
    mySeq.length(1);

    mySeq[0] = (BACIstring)"a string";
    sendEvent<BACIstringSeq>((BACIstringSeq)mySeq);
    }

    client.logout();
    return 0;
}
