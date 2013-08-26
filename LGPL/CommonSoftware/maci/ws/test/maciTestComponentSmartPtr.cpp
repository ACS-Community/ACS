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
* "@(#) $Id: maciTestComponentSmartPtr.cpp,v 1.5 2009/10/29 01:56:29 agrimstrup Exp $"
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

static char *rcsId="@(#) $Id: maciTestComponentSmartPtr.cpp,v 1.5 2009/10/29 01:56:29 agrimstrup Exp $"; 
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
	    assert(foo.handle == 0);
	    assert(foo.pointee_ == 0);
	    assert(*foo.pCount_ == 1);
	    assert(foo.isNil() == true);
	    ACS_SHORT_LOG((LM_INFO,"Default constructor... [OK]"));
	    }
	catch(...)
	    {
	    ACS_SHORT_LOG((LM_INFO,"Default constructor... [Failed]"));
	    }
	
	// Check the parameterized constructor
	try
	    {
	    ComponentSmartPtr<MACI_TEST::MaciTestClass> foo(&client, true,
							    client.getComponent<MACI_TEST::MaciTestClass>("MACI04", 0, true));
	    assert(foo.handle == &client);
	    assert(foo.pointee_ != 0);
	    assert(*foo.pCount_ == 1);
	    assert(foo.isNil() == false);
	    ACS_SHORT_LOG((LM_INFO,"Parameterized constructor... [OK]"));
	    }
	catch(...)
	    {
	    ACS_SHORT_LOG((LM_INFO,"Parameterized constructor... [Failed]"));
	    }
	
	// Check the copy constructor
	try
	    {
	    ComponentSmartPtr<MACI_TEST::MaciTestClass> foo(&client, true,
							    client.getComponent<MACI_TEST::MaciTestClass>("MACI04", 0, true));
	    ComponentSmartPtr<MACI_TEST::MaciTestClass> bar(foo);

	    assert(foo.handle == bar.handle);
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
	    ComponentSmartPtr<MACI_TEST::MaciTestClass> foo(&client, true,
							    client.getComponent<MACI_TEST::MaciTestClass>("MACI04", 0, true));
	    ComponentSmartPtr<MACI_TEST::MaciTestClass> bar;

	    bar = foo;
	    assert(foo.handle == bar.handle);
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

	// Check name out-of-scope error handling
	try
	    {
	    ComponentSmartPtr<MACI_TEST::MaciTestClass> foo;
	    {
	    std::string tname("MACI04");
	    foo = client.getComponentSmartPtr<MACI_TEST::MaciTestClass>(tname.c_str(), 0, true);
	    }
	    ACS_SHORT_LOG((LM_INFO,"Name out of scope... [OK]"));
	    }
	catch(...)
	    {
	    ACS_SHORT_LOG((LM_INFO,"Name out of scope... [Failed]"));
	    }
	
	// Check double release handling
	try
	    {
	    ComponentSmartPtr<MACI_TEST::MaciTestClass> bonzo;
	    {
	    std::string tname("MACI04");
	    bonzo = client.getComponentSmartPtr<MACI_TEST::MaciTestClass>(tname.c_str(), 0, true);
	    bonzo.release();
	    bonzo.release();
	    }
	    ACS_SHORT_LOG((LM_INFO,"Double release... [OK]"));
	    }
	catch(...)
	    {
	    ACS_SHORT_LOG((LM_INFO,"Double release... [Failed]"));
	    }
	
	// Check release and reassignment
 	try
 	    {
	    ComponentSmartPtr<MACI_TEST::MaciTestClass> bonzo;
	    {
	    std::string tname("MACI04");
	    bonzo = client.getComponentSmartPtr<MACI_TEST::MaciTestClass>(tname.c_str(), 0, true);
	    bonzo.release();

	    bonzo = client.getComponentSmartPtr<MACI_TEST::MaciTestClass>(tname.c_str(), 0, true);
	    }
	    ACS_SHORT_LOG((LM_INFO,"Release and re-assignment... [OK]"));
	    }
 	catch(...)
 	    {
 	    ACS_SHORT_LOG((LM_INFO,"Release and re-assignment... [Failed]"));
 	    }

	//TODO: Write some threaded tests to ensure thread-safety.

	client.logout();
}//main
