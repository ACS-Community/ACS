/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) National Research Council of Canada, 2008 
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
* "@(#) $Id: cppClient.cpp,v 1.1 2008/06/19 19:15:45 agrimstrup Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* arne  2008-06-17  created
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


#include <maciSimpleClient.h>
#include <HelloWorldC.h>
#include <ACSErrTypeCommon.h>
#include <acsutilTimeStamp.h>

ACE_RCSID(corbaRefPersistenceTest, cppClient, "$Id: cppClient.cpp,v 1.1 2008/06/19 19:15:45 agrimstrup Exp $")
using namespace maci;

typedef std::map<std::string,CorbaRefTest::HelloWorld_ptr> CompMap;

/*******************************************************************************/
/** @cond
*/    
int main(int argc, char *argv[])
{
    SimpleClient client;
    CompMap foo;
    int done = 0;
    std::string cmdln;

// Creates and initializes the SimpleClient object

    if (client.init(argc,argv) == 0)
	{
	return -1;
	}
    else
	{
	//Must log into manager before we can really do anything
	client.login();
	}

    std::cout << "Ready" << std::endl;

    std::string word;
    while (!done)
	{
	std::getline(cin,cmdln);
	std::istringstream sstream(cmdln);
	sstream >> word;
	std::cout << word << endl;
	if (word == "Done")
	    {
	    done = 1;
	    }
	else if (word == "Load")
	    {
	    std::string lgood;
	    std::string lbad;

	    while (1)
		{
		if (!(sstream >> word)) break;
		try
		    {
		    foo[word] = client.getComponent<CorbaRefTest::HelloWorld>(word.c_str(), 0, true);
		    lgood += word;
		    lgood += " ";
		    }
		catch(maciErrType::CannotGetComponentExImpl &_ex)
		    {
		    lbad += word;
		    lbad += " ";
		    }
		}
	    std::cout << "Load Complete: " << lgood << " Failed with Exceptions: " << lbad << std::endl;
	    }
	else if (word == "Call")
	    {
	    std::string cgood;
	    std::string cbad;

	    for (CompMap::iterator pos = foo.begin(); pos != foo.end(); ++pos)
		{
		try
		    {
		    pos->second->displayMessage();
		    cgood += pos->first;
		    cgood += " ";
		    }
		catch(CORBA::SystemException &_ex)
		    {
		    cbad += pos->first;
		    cbad += " ";
		    }
		}
	    std::cout << "Call Success: " << cgood << " Failed with Exceptions: " << cbad << std::endl;
	    }
	}
   
    //We release our component and logout from manager
    for (CompMap::iterator pos = foo.begin(); pos != foo.end(); ++pos)
        {
	    try
		{
		client.releaseComponent((pos->first).c_str());
		}
	    catch(maciErrType::CannotReleaseComponentExImpl &_ex)
		{
		}
	}
    
    client.logout();
    
    //Sleep for 3 sec to allow everytihng to cleanup and stablize
    ACE_OS::sleep(3);

    std::cout << "Bye" << endl;
    return 0;
}
/** @endcond
*/
/*___oOo___*/













