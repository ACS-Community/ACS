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
* "@(#) $Id: IdlCompilationTimeClient.cpp,v 1.2 2007/03/20 17:55:42 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-10-31 created
*/


#include <maciSimpleClient.h>
#include <IdlCompilationTimeC.h>

ACE_RCSID(acsexmpl, acsexmplHelloWorldClient, "$Id: IdlCompilationTimeClient.cpp,v 1.2 2007/03/20 17:55:42 sharring Exp $")
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
   
    IdlCompilationTime::IdlCompilationTimeComponent_var foo = client.get_object<IdlCompilationTime::IdlCompilationTimeComponent>(argv[1], 0, true);
    
    foo->method();
    
    client.manager()->release_component(client.handle(), argv[1]);
    client.logout();
    
    ACE_OS::sleep(3);
    return 0;
}
