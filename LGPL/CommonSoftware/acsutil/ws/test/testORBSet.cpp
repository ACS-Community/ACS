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
* "@(#) $Id: testAnyAide.cpp,v 1.8 2008/08/21 15:35:49 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-09-20  created
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <baciC.h>

static char *rcsId="@(#) $Id: testAnyAide.cpp,v 1.8 2008/08/21 15:35:49 bjeram Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "acsutilORBHelper.h"

int main(int argc, char *argv[])
{
    if(true == ORBHelper::setORB(NULL))
    {
        printf("ORB can be set to NULL\n");
    }

    if(ORBHelper::isORBSet() == false)
    {
        printf("ORB is not set\n");
    }

    CORBA::ORB_var orb = CORBA::ORB_init(argc, argv);
    if(true == ORBHelper::setORB(orb.in()))
    {
        printf("ORB can be set to orb because ORB in ORBHelper is still NULL\n");
    }
    if(false == ORBHelper::setORB(orb.in()))
    {
        printf("ORB cannot be set to orb because ORB in ORBHelper has already been set\n");
    }

    if(ORBHelper::isORBSet() == true)
    {
        printf("ORB is set\n");
    }
    
    CORBA::ORB_ptr orb_p = ORBHelper::getORB();
    if(orb_p == orb.in())
    {
	printf("Returned ORB from ORBHelper is equal to orb\n");
    }
    if(false == ORBHelper::setORB(NULL))
    {
        printf("ORB cannot be set to NULL because has already been set\n");
    }
    if(false == ORBHelper::setORB(orb.in()))
    {
        printf("ORB cannot be set to orb because has already been set\n");
    }

    if(ORBHelper::isORBSet() == true)
    {
        printf("ORB is set\n");
    }
 
    orb->destroy();
    return 0;
}
