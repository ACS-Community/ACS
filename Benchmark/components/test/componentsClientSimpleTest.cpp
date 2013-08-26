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
* "@(#) $Id: componentsClientSimpleTest.cpp,v 1.1 2004/09/27 21:38:04 dfugate Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-10-31 created
*/

#include <maciSimpleClient.h>
#include <perftestC.h>
#include <stdlib.h>
#include <acstimeProfiler.h>
#include <sstream>

ACE_RCSID(acsexmpl, acsexmplHelloWorldClient, "$Id: componentsClientSimpleTest.cpp,v 1.1 2004/09/27 21:38:04 dfugate Exp $")
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
   
    Profiler *profiler = new Profiler();

    CORBA::ULong invocations = static_cast<CORBA::ULong>(std::atol(argv[2]));

    std::ostringstream msg;
    for (int i=3; i < argc; i++)
	{
	msg << argv[i];
	if (i!=(argc-1))
	    {
	    msg << " ";
	    }
	}    
    msg << std::ends;

    perftest::BasePerfComp_var foo = client.get_object<perftest::BasePerfComp>(argv[1], 0, true);
    
    for (unsigned int i=0; i < invocations; i++)
	{
	profiler->start();
	foo->method();
	profiler->stop();
	}

    profiler->fullDescription(const_cast<char *>(msg.str().c_str()));

    client.manager()->release_component(client.handle(), argv[1]);
    client.logout();
    delete profiler;
    ACE_OS::sleep(2);
    return 0;
}

/*___oOo___*/
