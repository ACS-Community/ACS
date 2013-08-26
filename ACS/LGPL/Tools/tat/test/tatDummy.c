/******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: tatDummy.c,v 1.76 2003/01/09 17:51:31 vltsccm Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* pforstma  10/04/96  created
*/

/************************************************************************
*   NAME
*	tatDummy - a simple binary for tat11
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

static char *rcsId="@(#) $Id: tatDummy.c,v 1.76 2003/01/09 17:51:31 vltsccm Exp $"; 
static void *use_rcsId = (&use_rcsId,(void *) &rcsId);

/* 
 * System Headers 
 */
#include "stdio.h"


int main(int argc, char **argv)

{
    (void)fprintf(stderr,"tatDummy\n");
    exit(0);
}



/*___oOo___*/
