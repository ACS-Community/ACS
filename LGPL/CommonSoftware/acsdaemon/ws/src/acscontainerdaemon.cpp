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
* "@$Id: acscontainerdaemon.cpp,v 1.1 2007/10/10 16:21:35 ntroncos Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran 2006-06-21 created
*/

static char *rcsId="@ $Id: acscontainerdaemon.cpp,v 1.1 2007/10/10 16:21:35 ntroncos Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <acsContainerDaemonImpl.h>
#include <acsutilPorts.h>

// globals
volatile bool g_blockTermination = false;
ACSContainerDaemonImpl * g_service = 0;

void TerminationSignalHandler(int)
{
    if (g_blockTermination) return;
    g_blockTermination = true;

    ACS_SHORT_LOG ((LM_INFO, "Stopping the ACS Container Daemon..."));

    if (g_service)
	{
	g_service->shutdown ();
	}
}

int
main (int argc, char *argv[])
{
    if (argc >= 2 &&
	(ACE_OS_String::strcmp(argv[1], "-?") == 0 ||
	ACE_OS_String::strcmp(argv[1], "-h") == 0))
	{
	ACE_OS::printf ("\n\tusage: %s [-ORBEndpoint iiop://ip:port] [-o iorfile]\n\n", argv[0]);
	return -1;
	}

    const char* hostName = ACSPorts::getIP();

    // create logging proxy
    LoggingProxy::ProcessName(argv[0]);
    LoggingProxy::ThreadName("main");
    ACE_Log_Msg::instance()->local_host(hostName);

    LoggingProxy m_logger (0, 0, 31, 0);
    LoggingProxy::init (&m_logger);  


    int nargc = 0;
    char** nargv = 0;
    ACE_CString iorFile;

    ACE_CString argStr;
    for(int i=1; i<argc; i++)
	{
	argStr += argv[i];
	argStr += " ";
	
	if (!ACE_OS_String::strcmp(argv[i], "-o") && (i+1)<argc)
	    {
	    iorFile = argv[i+1];
	    i++; // skip filename
	    }
	}
    
    // add endpoint if not already specified
    if (argStr.find ("-ORBEndpoint")==ACE_CString::npos)
      {
      argStr = argStr + "-ORBEndpoint iiop://" + hostName + ":" + ACSPorts::getDaemonPort().c_str();
      }

    // create new argv
    ACS_SHORT_LOG((LM_INFO, "Command line is: %s", argStr.c_str()));
    ACE_OS::string_to_argv ((ACE_TCHAR*)argStr.c_str(), nargc, nargv);


    ACSContainerDaemonImpl service(m_logger);
    if (!service.isInitialized())
	{
	return -1;
	}
    g_service = &service;

    ACE_OS::signal(SIGINT, TerminationSignalHandler);  // Ctrl+C
    ACE_OS::signal(SIGTERM, TerminationSignalHandler); // termination request
  
    try
	{
	if (service.startup (nargc, nargv) != 0)
	    {
	    return -1;
	    }

	// write IOR to file, if necessary
	if (iorFile.length() > 0)
	    {
	    FILE *output_file = ACE_OS::fopen (iorFile.c_str(), "w");
	    if (output_file == 0) 
		{
		ACS_SHORT_LOG ((LM_ERROR, "Cannot open output file '%s' to write IOR.", iorFile.c_str()));
		return  -1;
		}

	    int result = ACE_OS::fprintf (output_file, "%s", service.getIOR());
	    if (result < 0) 
		{
		ACS_SHORT_LOG ((LM_ERROR, "ACE_OS::fprintf failed to write IOR."));
		return  -1;
		}

	    ACE_OS::fclose (output_file);
	    ACS_SHORT_LOG((LM_INFO, "ACS Container Daemon IOR has been written into file '%s'.", iorFile.c_str()));
	    }

	// run, run, run...
	if (service.run () == -1)
	    {
	    service.shutdown ();
	    ACS_SHORT_LOG ((LM_ERROR, "Failed to run the ACS Container Daemon."));
	    return  1;
	    }
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR, "Failed to start the ACS Container Daemon."));
	return 1;
	}
  
  
    if (!g_blockTermination)
	{
	g_blockTermination=true;
	service.shutdown ();
	}
  
  
    ACS_SHORT_LOG ((LM_INFO, "ACS Container Daemon stopped."));

    LoggingProxy::done();
  
    return 0;
}
