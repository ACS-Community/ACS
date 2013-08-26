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
* "@(#) $Id: timeoutTestServer.cpp,v 1.5 2006/02/28 19:25:12 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2004-08-24  created
*/

static char *rcsId="@(#) $Id: timeoutTestServer.cpp,v 1.5 2006/02/28 19:25:12 sharring Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "timeoutTestImpl.h"
#include "logging.h"

const char *ior_output_file = "s1.ior";

int main(int argc, char *argv[])
{
	CORBA::ORB_var orb;
	try
	{
		orb = CORBA::ORB_init(argc, argv, "");

		CORBA::Object_var poa_object = orb->resolve_initial_references("RootPOA");

		if (CORBA::is_nil (poa_object.in()))
		{
			ACS_SHORT_LOG((LM_ERROR, " (%P|%t) Unable to initialize the POA."));
			return -1;
		}

		PortableServer::POA_var root_poa = PortableServer::POA::_narrow(poa_object.in ());

		PortableServer::POAManager_var poa_manager = root_poa->the_POAManager();
		poa_manager->activate();

		TimeOutTestImpl timeOutTestImpl(orb.in ());

		TimeOutTest_var timeOutTest = timeOutTestImpl._this();

		CORBA::String_var ior = orb->object_to_string(timeOutTest.in ());

		//ACS_SHORT_LOG((LM_DEBUG, "Activated as <%s>", ior.in ()));

		// If the ior_output_file exists, output the ior to it
		if (ior_output_file != 0)
		{
			FILE *output_file= ACE_OS::fopen (ior_output_file, "w");
			if (output_file == 0)
			{
				ACS_SHORT_LOG ((LM_ERROR, "Cannot open output file for writing IOR: %s", ior_output_file));
				return -1;
			}
			ACE_OS::fprintf (output_file, "%s", ior.in ());
			ACE_OS::fclose (output_file);
		}

		poa_manager->activate();

		orb->run();

		// ACS_SHORT_LOG ((LM_DEBUG, "event loop finished"));
	}
	catch (CORBA::Exception &ex)
	{
		ACE_PRINT_EXCEPTION (ex, "Caught CORBA exception:");
		if(NULL != orb) 
		{
			orb->shutdown(0);
		}
		return 1;
	}
	catch(...)
	{
		ACS_SHORT_LOG((LM_DEBUG, "Caught an unknown exception"));
		if(NULL != orb) 
		{
			orb->shutdown(0);
		}
		return 2;
	}

	return 0;
}








