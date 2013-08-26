/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) Associated Universities Inc., 2004 
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
* "@(#) $Id: testPorts.cpp,v 1.4 2006/06/21 16:30:30 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2004-03-24  created
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


#include <stdlib.h>
#include <stdio.h>
//#include <stream.h>
#include <iostream>
#include <string.h>
#include <tao/corba.h>
#include "acsutil.h"
#include "acsutilPorts.h"

static char *rcsId="@(#) $Id: testPorts.cpp,v 1.4 2006/06/21 16:30:30 msekoran Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

using namespace std;

int main(int argc, char *argv[])
{
    cout << "Baseport is: "             << ACSPorts::getBasePort()            << endl;
    cout << "Manager port is: "         << ACSPorts::getManagerPort()         << endl;
    //cout << "Naming Service port is: "  << ACSPorts::getNamingServicePort()   << endl;
    //cout << "Notify Service port is: "  << ACSPorts::getNotifyServicePort()   << endl;
    //cout << "Logging Service port is: " << ACSPorts::getLoggingServicePort()  << endl;
    //cout << "IR port is: "              << ACSPorts::getIRPort()              << endl;
    //cout << "ACS log port is: "         << ACSPorts::getLogPort()             << endl;
    //cout << "CDB port is: "             << ACSPorts::getCDBPort()             << endl;
    //cout << "Daemon port is: "          << ACSPorts::getDaemonPort()          << endl;
    //cout << "IP address is: "           << ACSPorts::getIP()                  << endl;
    
    
    return 0;
}
