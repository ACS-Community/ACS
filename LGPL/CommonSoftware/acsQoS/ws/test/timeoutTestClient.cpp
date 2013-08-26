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
* "@(#) $Id: timeoutTestClient.cpp,v 1.9 2006/03/20 22:28:47 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2004-08-24  created
*/

static char *rcsId="@(#) $Id: timeoutTestClient.cpp,v 1.9 2006/03/20 22:28:47 sharring Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "timeoutTestC.h"
#include "acsQoS.h"
#include <logging.h>

const char *ior = "file://s1.ior";

int main(int argc, char *argv[])
{
	CORBA::ORB_var orb = CORBA::ORB_init (argc, argv, "");
	acsQoS::init(orb.in());

	CORBA::Object_var object = orb->string_to_object(ior);

	TimeOutTest_var timeoutTest = TimeOutTest::_narrow(object.in ());

	if (CORBA::is_nil (timeoutTest.in()))
	{
		ACS_SHORT_LOG((LM_ERROR, "Object reference <%s> is nil", ior));
		return -1;
	}
      
	// test invoking a method w/o a timeout
	try
	{
		timeoutTest->echo(0, 1000);
		ACS_SHORT_LOG((LM_INFO,"SUCCESS 1: Method w/o timeout executed successfully."));
	}
	catch (CORBA::TIMEOUT timeout)
	{
		ACS_SHORT_LOG((LM_ERROR, "ERROR 0: timeout exception caught on method invocation when not expected."));
		return -1;
	}

	// test setting of the timeout and also test invoking a method (with a timeout set) that doesn't exceed the time out
	try
	{
		acsQoS::Timeout timeout(400);

		{
			// test if it resets to the previous timeout when timeout object is deleted
			acsQoS::Timeout timeout1(5000);
		}

		timeoutTest->echo(0, 1000);
		ACS_SHORT_LOG((LM_INFO,"ERROR 1: timeout exception was expected but not thrown."));
	}
	catch (CORBA::TIMEOUT timeout)
	{
		ACS_SHORT_LOG((LM_INFO, "SUCCESS 2: Timeout exception caught, this is expected."));
	}

	// test if setting timeout on the object level works
	try
	{
		TimeOutTest_var timed_timeoutTest =  acsQoS::Timeout::setObjectTimeout(900, timeoutTest.in());

		timed_timeoutTest->echo(0, 950);
		ACS_SHORT_LOG((LM_INFO,"ERROR 2:  timeout exception was expected but not thrown."));
	}
	catch (CORBA::TIMEOUT timeout)
	{
		ACS_SHORT_LOG((LM_INFO, "SUCCESS 3: Timeout exception caught, this is expected."));
	}

	// check if deleting timeout object resets timeout value
	try
	{
		timeoutTest->echo(0, 450);
		ACS_SHORT_LOG((LM_INFO,"SUCCESS 4: Method w/o timeout executed successfully."));
	}
	catch (CORBA::TIMEOUT timeout)
	{
		ACS_SHORT_LOG((LM_ERROR, "ERROR 3: timeout exception caught on method invocation when not expected."));
		return -1;
	}

	acsQoS::done();

	timeoutTest->shutdownOrb();
	orb->shutdown(0);
	return 0;
}
