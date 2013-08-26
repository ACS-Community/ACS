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
* "@(#) $Id: addDataTest.cpp,v 1.47 2010/04/27 15:30:02 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram 2002-06-03 created
*/

// #include "acserr.h"
#include "logging.h"
#include "acserrTest.h"
#include "ACSErrTypeTest.h"

using namespace ACSErrTypeTest;

static char *rcsId="@(#) $Id: addDataTest.cpp,v 1.47 2010/04/27 15:30:02 bjeram Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

CORBA::ORB_var orb;

#ifndef MAKE_VXWORKS
int main(int argc, char *argv[]) {
#else
#	include "rebootLib.h"
#	include "acsutilArgUnpack.h"
    int addDataTest(char *szCmdLn) {
    int  argc;
    char *argv[100];

    argc = argUnpack(szCmdLn, argv);
    argv[0] = "addDataTest";
#endif
    
    LoggingProxy *m_logger = new LoggingProxy (0, 0, 31, 0);
    LoggingProxy::init (m_logger);

    ACS_TEST_INIT_CORBA;

    ACSError::init (orb.ptr());
   

    ACSErrTest2ExImpl e(__FILE__, __LINE__, "main");

    e.addData("one", 0);  // test duplcation
    e.setMemberValue("one", 1);
    e.addData("two", 2);
    e.addData("three", "xcvcx"); // test duplcation
    e.setMemberValue("three", "three");
    
    // test maximum length
    char longName[ADD_DATA_NAME_MAX+10];
    char longValue[ADD_DATA_VALUE_MAX+10];
    memset(longName, 'A', ADD_DATA_NAME_MAX+10);
    memset(longValue, 'B', ADD_DATA_VALUE_MAX+10);
    longName[ADD_DATA_NAME_MAX-2]='#';
    longName[ADD_DATA_NAME_MAX+9]=0;
    longValue[ADD_DATA_VALUE_MAX-2]='#';
    longValue[ADD_DATA_VALUE_MAX+9]=0;
    e.addData(longName, longValue);

    // test NULL string passing
    e.addData(NULL, (const char*)NULL);

    ACE_DEBUG ((LM_DEBUG, "getData(\"one\") %d", e.getMemberValue<int>("one")));
    ACE_DEBUG ((LM_DEBUG, "getData(\"two\") %s", e.getData("two").c_str()));
    char *buf;
    buf = e.getMemberValue<char*>("three");
    ACE_DEBUG ((LM_DEBUG, "getData(\"three\") %s", buf));
    delete[] buf;
    e.addData("four", 4);
    ACE_DEBUG ((LM_DEBUG, "getData(\"four\") %s", e.getData("four").c_str()));

    // since longName is truncate in addData getData should return empty string
    ACE_DEBUG ((LM_DEBUG, "getData(longName) %s", e.getData(longName).c_str()));

    longName[255]=0;
    ACE_DEBUG ((LM_DEBUG, "getData(longName) %s", e.getData(longName).c_str()));
    longName[255]='A';

    ACE_DEBUG ((LM_DEBUG, "getData(NULL) %s", e.getData(NULL).c_str()));

    e.log();
    // here we test also logging with priorty different than default (LM_ERROR)
    // the detail of this can be seen in log file in tmp/
    e.log(LM_DEBUG);
    e.log(LM_INFO);

    LoggingProxy::done();
    delete m_logger;
    
    return 0;
}







