/******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: tat101.c,v 1.76 2003/01/09 17:51:22 vltsccm Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* pforstma  09/10/95  created
*/

/************************************************************************
*   NAME
*	tat101 - a simple binary for TestList
* 
*   SYNOPSIS
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

#define _POSIX_SOURCE 1
#include "vltPort.h"

static char *rcsId="@(#) $Id: tat101.c,v 1.76 2003/01/09 17:51:22 vltsccm Exp $"; 
static void *use_rcsId = (&use_rcsId,(void *) &rcsId);

/* 
 * System Headers 
 */
#include "stdio.h"


int main(int argc, char **argv)

{
    (void)printf("hello, tat101 \n");
    exit(0);
}



/*___oOo___*/
