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
* "@(#) $Id: memTestLog.cpp,v 1.35 2003/03/21 08:13:55 rgeorgie Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  21/01/02  created
*/


static char *rcsId="@(#) $Id: memTestLog.cpp,v 1.35 2003/03/21 08:13:55 rgeorgie Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "logging.h"

CORBA::ORB_var orb;

void ft(int n){
    ACE_OS::printf("%d\n", n);
    if (n >0 )
	{
	ACS_DEBUG ("befor memTestLog", "deep");
	ft(--n);
	ACS_DEBUG ("after memTestLog", "deep");
	}
    else
	{
	ACS_DEBUG ("memTestLog", "end");
	}
}

int main(int argc, char *argv[])
{
    int u = (argc > 1) ? atoi(argv[1]) : 100;
    
    LoggingProxy *m_logger = new LoggingProxy (0, 0, 31, 0);
    LoggingProxy::init (m_logger); 

    ACE_OS::printf ("press enter to start test ...\n");
    getchar();
    for (int i=0; i<u; i++)
	{
	ACS_DEBUG ("befor memTestLog", "msg1");
	ACS_DEBUG ("after memTestLog", "msg2");
	}
//    ft(u); //recursive
    getchar();
    m_logger->flush();
    LoggingProxy::done();
    delete m_logger;
    getchar(); 
    ACE_OS::printf ("Exit\n");
    
    return 0;
}








