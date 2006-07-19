/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) National Research Council of Canada, 2005 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: testSupplier.cpp,v 1.4 2006/07/19 16:57:28 dfugate Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-11-15  created
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

#include "testSupplier.h"
#include "acsncORBHelper.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static char *rcsId="@(#) $Id: testSupplier.cpp,v 1.4 2006/07/19 16:57:28 dfugate Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);


int main(int argc, char *argv[])
{
    nc::ORBHelper *oh = new nc::ORBHelper();
    oh->runOrb();
    CORBA::ORB_ptr orb = oh->getORB();
    CORBA::Object_var naming_obj = orb->resolve_initial_references ("NameService");
    CosNaming::NamingContext_var naming_context = CosNaming::NamingContext::_narrow(naming_obj.in());
 
    TestSupplier* joe = new TestSupplier("blah");
    joe->init(naming_context.in());

    for(int i=0; i<5; i++)
	{
	joe->publishEvent("some event");
	}

    joe->disconnect();

    delete oh;

    //G. Chiozzi complained about missing logs during various runs
    //of this test. They come from logging which will not print logs
    //to stdout if everything does not have a chance to shutdown
    //properly. This sleep should be considered a temporary solution.
    ACE_OS::sleep(3);
    return 0;

}
