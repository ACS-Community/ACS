/***************************************************************************
* E.S.O. - VLT project - 1994
*
* vltMakeTestClibrary.c
*
* HISTORY
*
* who       when      what
* --------  --------  --------------------------------------------
* gfilippi  12/10/94  created
*
*/
/***************************************************************************
*
*  NAME 
* 
*   vltMakeTestClibrary - test C library	
* 
* SYNOPSIS 
*
*   vltMakeTestClibrary 
*  
*
* DESCRIPTION 
*
*   This program is part of the vltMake modular test package
*
*
*   RETURN VALUES 
*    SUCCESS if the file has been compiled with the MYFLAG on. 
*    FAILURE if not
*
*   ENVIRONMENT
*
*   SEE ALSO
*
*************************************************************************/

#define _POSIX_SOURCE 1
/*
 * System Headers
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* 
 * application headers
 */
#include "vltMakeTest.h"


/*
 * MAIN
 */
int main(int argc, char ** argv)

{
char testId[80] = "vltMakeTestClibrary";

/*
 * check input parameter
 */
if ( argc != 1) 
    {
    printf("\n\tusage: vltMakeTestClibrary \n\n");
    TEST_FAILED(testId);
    }

/*
 * Call procedure 1
 */
vltMakeTestProcedure1();

/*
 * Call procedure 2
 */
vltMakeTestProcedure2();
  
/*
 * go out
 */
TEST_SUCCEEDED(testId);
return 0;

}  /* end of main */

/*___oOo___*/

