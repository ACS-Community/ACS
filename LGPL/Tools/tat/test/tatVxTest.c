/******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: tatVxTest.c,v 1.76 2003/01/09 17:51:31 vltsccm Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* ssandroc  13/10/95  created
*/

/************************************************************************
*   NAME
*	tatVxTest - tat VxWorks test program
* 
*   SYNOPSIS
*	ld < tatVxTest
*	tatVxTest
* 
*   DESCRIPTION
*	tatVxTest can be started from the VxWorks shell and prints hello.
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

#define _POSIX_SOURCE 1
#include "vltPort.h"

static char *rcsId="@(#) $Id: tatVxTest.c,v 1.76 2003/01/09 17:51:31 vltsccm Exp $"; 
static void *use_rcsId = (&use_rcsId,(void *) &rcsId);

/* 
 * System Headers 
 */
#include "vxWorks.h"
#include "stdio.h"


int tatVxTest(void)
{
    (void)printf("hello, world\n");
    return OK;
}

/*___oOo___*/
