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
* "@(#) $Id: componentsGenericTest.cpp,v 1.2 2004/10/21 16:55:57 dfugate Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-10-31 created
*/

//For the time being this exec is not too useful...a similarly named Python script
//does exactly the same thing.

#include <maciSimpleClient.h>
#include <perftestC.h>
#include <stdlib.h>

ACE_RCSID(acsexmpl, acsexmplHelloWorldClient, "$Id: componentsGenericTest.cpp,v 1.2 2004/10/21 16:55:57 dfugate Exp $")
using namespace maci;

/*******************************************************************************/
    
int main(int argc, char *argv[])
{
    SimpleClient client;
    if (client.init(argc,argv) == 0)
	{
	return -1;
	}
    else
	{
	client.login();
	}
   
    CORBA::ULong count = static_cast<CORBA::ULong>(std::atol(argv[2]));
    CORBA::ULong size  = static_cast<CORBA::ULong>(std::atol(argv[3]));
    ACS::TimeInterval waitTime = static_cast<CORBA::LongLong>(std::atol(argv[4]));
    perftest::BasePerfComp_var foo = client.get_object<perftest::BasePerfComp>(argv[1], 0, true);
    
    foo->setup(count, size, waitTime);
    foo->method();
    
    client.manager()->release_component(client.handle(), argv[1]);
    client.logout();
    ACE_OS::sleep(2);
    return 0;
}

/*___oOo___*/



