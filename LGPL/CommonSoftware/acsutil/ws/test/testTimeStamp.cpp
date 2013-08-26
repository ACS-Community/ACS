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
* "@(#) $Id: testTimeStamp.cpp,v 1.1 2005/12/12 19:12:29 dfugate Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-12-12  created
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

#include "acsutilTimeStamp.h"

static char *rcsId="@(#) $Id: testTimeStamp.cpp,v 1.1 2005/12/12 19:12:29 dfugate Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);


int main(int argc, char *argv[])
{
    
    printf("Results of 'getTime()': %lld\n", getTime());
    printf("Results of 'getTimeStamp()': %lld\n", getTimeStamp());
    printf("Results of 'getStringifiedTimeStamp()': %s\n", getStringifiedTimeStamp().c_str());
    printf("Results of 'getStringifiedUTC()': %s\n", getStringifiedUTC(132922080005000000ULL).c_str()); //2004-01-01T00:00:00


    printf("Results of 'getStringifiedUTC(getTime())': %s\n", getStringifiedUTC(getTime()).c_str());
    printf("Results of 'getStringifiedUTC(getTimeStamp())': %s\n", getStringifiedUTC(getTimeStamp()).c_str());
    return 0;
}








