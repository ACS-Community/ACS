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
* "@(#) $Id: testTmp.cpp,v 1.17 2004/03/25 01:51:17 dfugate Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2002/03/18  created
*/

#include <ace/OS.h>
#include <stdio.h>
#include <stdlib.h>
#include <ace/SString.h>
#include <acsutilTempFile.h>

#define TEST_ENV_VAR "ACSUTIL_TMP_TEST"

void printFileName(const char * fileName)
{
    if (fileName && *fileName)
	ACE_OS::printf("FileName: '%s'.\n", fileName);
    else
	ACE_OS::printf("Empty result returned.\n");
}

#define TEST(message) ACE_OS::printf("\nTEST: %s\n", message);

/*
 * GCH - 2003-09-03
 * On Linux I can use the setenv() and unsetenv() calls,
 * but they are not available on Sun, therefore
 * I switch to putenv() and getenv(), but
 * then I cannot "unset" really and environment variable.
 * This is a problem in particular for VxWorks
 * where the environment is shared among all processes.
 * But in the modular test should not be important, since
 * we are every time rebooting the LCUs
 */

#ifndef MAKE_VXWORKS
 int main(int argc, char *argv[])
#else
 int testTmp (char *szCmdLn)
#endif
{
    /*
     * To be sure I should call
     *    unsetenv(TEST_ENV_VAR);
     * but this call is not available on Sun
     * I assume this variable is NOT set
     */
    TEST("Empty string should be returned.");
    ACE_CString fileName = getTempFileName(0, 0);
    printFileName(fileName.c_str());

    TEST("'/tmp/fileName' should be returned.");
    putenv(TEST_ENV_VAR  "=/tmp/fileName");
    fileName = getTempFileName(TEST_ENV_VAR, 0);
    printFileName(fileName.c_str());

    TEST("'/tmp/fileName' should be returned.");
    fileName = getTempFileName(TEST_ENV_VAR, "anotherFileName");
    printFileName(fileName.c_str());

    /*
     * This I can safely comment out.
     * Environment is modified only inside the process
     *
     *   //   unsetenv(TEST_ENV_VAR);
     */
    ACE_CString tmpFileName(getenv("ACSDATA"));
    tmpFileName += "/tmp/someFileName";
    fileName = getTempFileName(0, "someFileName");
    if(fileName.compare(tmpFileName) == 0)
	{
	TEST("Comparison of getTmpFileName(someFileName) with $ACSDATA/tmp/filename is OK");
	}
    else
	{
	TEST("Comparison of getTmpFileName(someFileName) with $ACSDATA/tmp/filename failed");
        printFileName(fileName.c_str());
        printFileName(tmpFileName.c_str());
	}

    /*
     * This would be important for VxWorks
     *   //    unsetenv("ACSDATA");
     * but I just replace it with an empty string
     */
    putenv("ACSDATA=");
     

    TEST("'/tmp/someFileName' should be returned.");
    fileName = getTempFileName(0, "someFileName");
    printFileName(fileName.c_str());
    
    putenv("ACSDATA=/ignoredPath");
    putenv("ACS_TMP=/acs/tmp");

    TEST("'/acs/tmp/someFileName' should be returned.");
    fileName = getTempFileName(0, "someFileName");
    printFileName(fileName.c_str());

    return 0;
}
