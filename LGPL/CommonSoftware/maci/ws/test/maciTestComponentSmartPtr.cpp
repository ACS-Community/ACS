/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) National Research Council of Canada, 2007 
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
* "@(#) $Id: maciTestComponentSmartPtr.cpp,v 1.1 2007/03/06 08:17:24 agrimstrup Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* agrimstr  2007-03-01  created 
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

#define _POSIX_SOURCE 1
#include "vltPort.h"

static char *rcsId="@(#) $Id: maciTestComponentSmartPtr.cpp,v 1.1 2007/03/06 08:17:24 agrimstrup Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <maciTestC.h>
#define private public
#include <maciSimpleClient.h>
#undef private
#include <logging.h>
#include <string>
#include <iostream>
#include <cassert>

using namespace maci;

/**
 * The test checks the methods of the ComponentSmartPtr
 * 
 */
int main (int argc, char **argv)
{
	/// The simple client to connect to the component to test
	SimpleClient client;
	
	if (client.init(argc,argv) == 0)
	    {
	    return -1;
	    } 
	else
	    {
	    // Log into the manager before doing anything
	    client.login();
	    }

	// Check the default constructor
	try
	    {
	    ComponentSmartPtr<MACI_TEST::MaciTestClass> foo;
	    assert(foo.component_name == 0);
	    assert(foo.client == 0);
	    assert(foo.pointee_ == 0);
	    assert(*foo.pCount_ == 1);
	    ACS_SHORT_LOG((LM_INFO,"Default constructor... [OK]"));
	    }
	catch(...)
	    {
	    ACS_SHORT_LOG((LM_INFO,"Default constructor... [Failed]"));
	    }
	
	// Check the parameterized constructor
	try
	    {
	    ComponentSmartPtr<MACI_TEST::MaciTestClass> foo("MACI04", &client, true,
							    client.getComponent<MACI_TEST::MaciTestClass>(
								"MACI04", 0, true));
	    assert(foo.component_name == "MACI04");
	    assert(foo.client == &client);
	    assert(foo.pointee_ != 0);
	    assert(*foo.pCount_ == 1);
	    ACS_SHORT_LOG((LM_INFO,"Parameterized constructor... [OK]"));
	    }
	catch(...)
	    {
	    ACS_SHORT_LOG((LM_INFO,"Parameterized constructor... [Failed]"));
	    }
	
	// Check the copy constructor
	try
	    {
	    ComponentSmartPtr<MACI_TEST::MaciTestClass> foo("MACI04", &client, true,
							    client.getComponent<MACI_TEST::MaciTestClass>(
								"MACI04", 0, true));
	    ComponentSmartPtr<MACI_TEST::MaciTestClass> bar(foo);

	    assert(foo.component_name == bar.component_name);
	    assert(foo.client == bar.client);
	    assert(foo.pointee_ == bar.pointee_);
	    assert(*foo.pCount_ == 2);
	    ACS_SHORT_LOG((LM_INFO,"Copy constructor... [OK]"));
	    }
	catch(...)
	    {
	    ACS_SHORT_LOG((LM_INFO,"Copy constructor... [Failed]"));
	    }
	
	// Check the assignment operator
	try
	    {
	    ComponentSmartPtr<MACI_TEST::MaciTestClass> foo("MACI04", &client, true,
							    client.getComponent<MACI_TEST::MaciTestClass>(
								"MACI04", 0, true));
	    ComponentSmartPtr<MACI_TEST::MaciTestClass> bar;

	    bar = foo;
	    assert(foo.component_name == bar.component_name);
	    assert(foo.client == bar.client);
	    assert(foo.pointee_ == bar.pointee_);
	    assert(*foo.pCount_ == 2);
	    ACS_SHORT_LOG((LM_INFO,"Assignment operator... [OK]"));
	    }
	catch(...)
	    {
	    ACS_SHORT_LOG((LM_INFO,"Assignment operator... [Failed]"));
	    }

	try
	    {
	    ComponentSmartPtr<MACI_TEST::MaciTestClass> bar = 
		client.getComponentSmartPtr<MACI_TEST::MaciTestClass>("MACI04", 0, true);
	    ACS_SHORT_LOG((LM_INFO,"getComponentSmartPtr... [OK]"));
	    }
	catch(...)
	    {
	    ACS_SHORT_LOG((LM_INFO,"getComponentSmartPtr... [Failed]"));
	    }

	try
	    {
	    ComponentSmartPtr<MACI_TEST::MaciTestClass> foo = 
		client.getComponentSmartPtr<MACI_TEST::MaciTestClass>("MACI04", 0, true);
	    ComponentSmartPtr<MACI_TEST::MaciTestClass> bar = 
		client.getComponentNonStickySmartPtr<MACI_TEST::MaciTestClass>("MACI04");
	    //Sleep for 3 sec to allow everytihng to cleanup and stablize
	    ACE_OS::sleep(3);
	    ACS_SHORT_LOG((LM_INFO,"getComponentNonStickySmartPtr... [OK]"));
	    }
	catch(...)
	    {
	    ACS_SHORT_LOG((LM_INFO,"getComponentNonStickySmartPtr... [Failed]"));
	    }
	
	try
	    {
	    ComponentSmartPtr<MACI_TEST::MaciTestClass> foo;

	    {
	    ComponentSmartPtr<MACI_TEST::MaciTestClass> bar = 
		client.getComponentSmartPtr<MACI_TEST::MaciTestClass>("MACI04", 0, true);

	    foo = bar;
	    assert(*foo.pCount_ == 2);
	    }
	    assert(*foo.pCount_ == 1);
	    ACS_SHORT_LOG((LM_INFO,"Nested scope assignment... [OK]"));
	    }
	catch(...)
	    {
	    ACS_SHORT_LOG((LM_INFO,"Nested scope assignment... [Failed]"));
	    }

	try
	    {
	    ComponentSmartPtr<MACI_TEST::MaciTestClass> bar = 
		client.getComponentSmartPtr<MACI_TEST::MaciTestClass>("MACI04", 0, true);
	    assert(bar->test());
	    ACS_SHORT_LOG((LM_INFO,"Sticky SmartPtr method invocation... [OK]"));
	    }
	catch(...)
	    {
	    ACS_SHORT_LOG((LM_INFO,"Sticky SmartPtr method invocation... [Failed]"));
	    }

	try
	    {
	    ComponentSmartPtr<MACI_TEST::MaciTestClass> foo = 
		client.getComponentSmartPtr<MACI_TEST::MaciTestClass>("MACI04", 0, true);
	    ComponentSmartPtr<MACI_TEST::MaciTestClass> bar = 
		client.getComponentNonStickySmartPtr<MACI_TEST::MaciTestClass>("MACI04");
	    assert(bar->test());
	    ACS_SHORT_LOG((LM_INFO,"NonSticky SmartPtr method invocation... [OK]"));
	    }
	catch(...)
	    {
	    ACS_SHORT_LOG((LM_INFO,"NonSticky SmartPtr method invocation... [Failed]"));
	    }

	//TODO: Write some threaded tests to ensure thread-safety.

	client.logout();
}//main
