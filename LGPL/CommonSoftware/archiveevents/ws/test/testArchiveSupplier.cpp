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
* "@(#) $Id: testArchiveSupplier.cpp,v 1.3 2006/10/09 06:06:00 gchiozzi Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-09-08  created
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


#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "archiveeventsArchiveSupplier.h"

#include <maciSimpleClient.h>

#include <baciC.h>

using namespace maci;

static char *rcsId="@(#) $Id: testArchiveSupplier.cpp,v 1.3 2006/10/09 06:06:00 gchiozzi Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

template<class T> 
void
sendEvent(const T value)
{
    ACS_SHORT_LOG((LM_INFO, "Sending an event."));
    ArchiveSupplierSingleton::Instance().sendEvent<T>(0,
						      0ULL, 
						      "no component", 
						      "no property", 
						      (T)value);
}


int main(int argc, char *argv[])
{
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
    
    //send events of supported types only for now
    //double
    sendEvent<CORBA::Double>(3.14);
    
    //flaot
    sendEvent<CORBA::Float>(3.1);
    
    //long
    sendEvent<CORBA::Long>(1);
    
    //pattern
    sendEvent<ACS::pattern>(2);
    
    //long long
    sendEvent<ACS::longLong>(3);
    
    //unsigned long long
    sendEvent<ACS::uLongLong>(4);
    
    //string
    sendEvent<const char*>((const char*)"a string");
    
    //double seqence
    {
    ACS::doubleSeq mySeq(1);
    mySeq.length(1);
    
    mySeq[0] = (CORBA::Double)3.14;
    sendEvent<ACS::doubleSeq>(mySeq);
    }
    
    //float seqence
    {
    ACS::floatSeq mySeq(1);
    mySeq.length(1);
    
    mySeq[0] = (CORBA::Float)3.1;
    sendEvent<ACS::floatSeq>(mySeq);
    }
    
    //long seqence
    {
    ACS::longSeq mySeq(1);
    mySeq.length(1);
    
    mySeq[0] = (CORBA::Long)1;
    sendEvent<ACS::longSeq>(mySeq);
    }
    
    //string seqence
    {
    ACS::stringSeq mySeq(1);
    mySeq.length(1);
    
    mySeq[0] = CORBA::string_dup("a string");
    sendEvent<ACS::stringSeq>(mySeq);
    }

    client.logout();
    return 0;
}

