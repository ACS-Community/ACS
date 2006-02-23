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
* "@(#) $Id: memTest.cpp,v 1.37 2003/10/23 07:41:14 acaproni Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  21/01/02  created
*/



static char *rcsId="@(#) $Id: memTest.cpp,v 1.37 2003/10/23 07:41:14 acaproni Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "acserrTestC.h"
#include "acserr.h"
#include "acserrTest.h"

CORBA::ORB_var orb;

int main(int argc, char *argv[])
{
    
    int u = (argc > 1) ? atoi(argv[1]) : 100;
 // creating ORB
    ACS_TEST_INIT_CORBA;

    // init ACS error system (and inside also logging)
    ACSError::init (orb.ptr());

#ifdef MAKE_VXWORKS      
      ACSError::processName (szCmdLn);
#else 
      char *buf;
      ACE_OS::argv_to_string (argv, buf);
      ACSError::processName (buf);
      delete[] buf;
#endif    

    ACSError *e, *er;

    ACE_OS::printf ("Create a error stack of length of: %d\n", u);
    e = new ACS_ERROR(ACSErr::ACSErrTypeTest, ACSErr::ACSErrTest2, "acserrTestImpl::memTest");
    for (int i=0; i < u; i++)
	{
	er =  new ACS_ERROR(e, ACSErr::ACSErrTypeTest, ACSErr::ACSErrTest2, "acserrTestImpl::memTest", DEFAULT_SEVERITY);
	e =er;
	}//for
    getchar();
    ACE_OS::printf ("Size of one item in error stack: %d   completion: %d \n", sizeof (*er), sizeof( ACSErr::ErrorTrace));
    
    getchar();
    e->log();

    getchar();
    delete e;

    getchar();
    ACE_OS::printf ("Exit\n");
    
    return 0;
}










