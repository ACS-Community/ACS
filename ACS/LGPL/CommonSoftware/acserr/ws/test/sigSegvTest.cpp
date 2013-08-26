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
* "@(#) $Id: sigSegvTest.cpp,v 1.1 2010/03/15 11:57:26 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram 2002-06-03 created
*/

// #include "acserr.h"
#include "logging.h"
#include "acserrTest.h"
#include "acserr.h"


static char *rcsId="@(#) $Id: sigSegvTest.cpp,v 1.1 2010/03/15 11:57:26 bjeram Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

CORBA::ORB_var orb;

#ifndef MAKE_VXWORKS
int main(int argc, char *argv[]) {
#else
#	include "rebootLib.h"
#	include "acsutilArgUnpack.h"
    int sigSegvTestaddDataTest(char *szCmdLn) {
    int  argc;
    char *argv[100];

    argc = argUnpack(szCmdLn, argv);
    argv[0] = "sigSegvTest";
#endif
    
    LoggingProxy *m_logger = new LoggingProxy (0, 0, 31, 0);
    LoggingProxy::init (m_logger);

    ACS_TEST_INIT_CORBA;

    ACSError::init (orb.ptr());
   
    char* ptr = 0;
      std::cout << "This should not be printed but create a SIGSEGV."
          << *ptr
          << "\n";

      std::cout << "Signalhandler should have executed.\n";

    return 0;
}







