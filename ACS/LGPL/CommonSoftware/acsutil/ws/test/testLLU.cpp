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
* "@(#) $Id: testLLU.cpp,v 1.16 2003/09/09 13:38:46 gchiozzi Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  08/01/02  created
*/

#include <ace/OS.h>
#include <stdio.h>
#include <acsutilLL.h>

static char *rcsId="@(#) $Id: testLLU.cpp,v 1.16 2003/09/09 13:38:46 gchiozzi Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#ifndef MAKE_VXWORKS

 int main(int argc, char *argv[])

#else

 int testLLU (char *szCmdLn)

#endif

{
    unsigned long long  a = 123456781L;
    unsigned long long  b = 2L;
   
    ACE_OS::printf ("\nTest printing unsigned long long  " ACS_LLU ". Module:" ACS_LLU " \n", 
		    ACS_LLU_PRINTF(a),
		    ACS_LLU_PRINTF(ACS_LLU_MOD(a,b)));

    char buf[]="Number: 12345678";
    unsigned long long num=0;
    
#ifndef MAKE_VXWORKS
    sscanf (buf, "Number: %llu",  &num);
#else
    char tst[32];
    sscanf (buf, "Number: %s",  tst);
    num = string2LLU(tst);
#endif

    ACE_OS::printf ("String: %s \t Unsigned long long number: " ACS_LLU "\n", buf, ACS_LLU_PRINTF(num));
    
    
    return 0;

}








