/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2004 
*
*This library is free software; you can redistribute it and/or
*modify it under the terms of the GNU Lesser General Public
*License as published by the Free Software Foundation; either
*version 2.1 of the License, or (at your option) any later version.
*
*This library is distributed in the hope that it will be useful,
*but WITHOUT ANY WARRANTY; without even the implied warranty of
*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*Lesser General Public License for more details.
*
*You should have received a copy of the GNU Lesser General Public
*License along with this library; if not, write to the Free Software
*Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: acserrTestHandlers.cpp,v 1.1 2004/05/05 15:51:05 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2004-05-05  created
*/

#include "acserr.h"
#include "ACSErrTypeTest.h"
#include "acserrTest.h"

static char *rcsId="@(#) $Id: acserrTestHandlers.cpp,v 1.1 2004/05/05 15:51:05 bjeram Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

using namespace ACSErrTypeTest;

CORBA::ORB_var orb;

class TestExHandlers 
{
  public:
    void testUnspecified() throw (ACSErrTest0ExImpl, int)
	{
	    throw ACSErrTest1ExImpl(__FILE__, __LINE__, "TestExHandlers::testUnspecified");
	}
    
    void testUncaught()	{
	    throw ACSErrTest2ExImpl(__FILE__, __LINE__, "TestExHandlers::testUncaught");
	}



void testFunct ()
{
    testUnspecified();
}

};
int main(int argc, char *argv[])
{
    // creating ORB
    ACS_TEST_INIT_CORBA;

    ACSError::init (orb.ptr());
  	TestExHandlers th;
  
//th.testUncaught();
    try
	{
	
	
	th.testFunct();
	} 
    catch (ACSErr::ACSbaseExImpl &ex)
	{
	printf("in main \n");
	ex.log();
	}
    catch (int i )
	{
	printf("number: %d \n", i);
	}
    catch (...)
	{
	    ACE_OS::printf ("catch ...\n");
	}
    ACE_OS::printf ("return\n");
    return 0;
}








