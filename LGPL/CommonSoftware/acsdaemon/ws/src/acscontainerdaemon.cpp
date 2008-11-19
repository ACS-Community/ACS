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
* "@$Id: acscontainerdaemon.cpp,v 1.7 2008/11/19 11:02:44 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran 2006-06-21 created
* agrimstr 2007-11-07 refactored to use daemon implementation template class
*/

static char *rcsId="@ $Id: acscontainerdaemon.cpp,v 1.7 2008/11/19 11:02:44 msekoran Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <acsDaemonImpl.h>
#include <acsContainerHandlerImpl.h>


acsDaemonImpl<ACSContainerHandlerImpl>* g_daemon = 0;  // Reference used by signal handler

void TerminationSignalHandler(int)
{
    LoggingProxy * logger = new LoggingProxy(0, 0, 31);
    LoggingProxy::init(logger);
    LoggingProxy::ThreadName("termination");
    if (g_daemon)
	g_daemon->shutdown();
}

int
main (int argc, char *argv[])
{
    acsDaemonImpl<ACSContainerHandlerImpl> daemon(argc,argv);

    // Daemon is to be shutdown on receipt of SIGINT or SIGTERM
    g_daemon = &daemon;
    ACE_OS::signal(SIGINT, TerminationSignalHandler);  // Ctrl+C
    ACE_OS::signal(SIGTERM, TerminationSignalHandler); // termination request

    return daemon.run();
}
