/***************************************************************************
* E.S.O. - VLT project - 1994
*
* vltMakeTestUSER_CFLAG.c
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
*   vltMakeTestUSER_CFLAG - test possibility of user definable C-compilation flags	
* 
* SYNOPSIS 
*
*   vltMakeTestUSER_CFLAG 
*  
*
* DESCRIPTION 
*
*   This program is part of the vltMake modular test package
*
*   test possibility of user definable C-compilation flags
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

char testId[80] = "vltMakeTestUSER_CFLAG";

/*
 * check input parameter
 */
if ( argc != 1) 
    {
    printf("\n\tusage: vltMakeTestUSER_CFLAG \n\n");
    TEST_FAILED(testId);
    }
    
/*
 * use MYFLAG to have different code.
 */ 
#ifdef MYFLAG
    TEST_SUCCEEDED(testId);
#else
    TEST_FAILED(testId);
#endif 

}  /* end of main */

/*___oOo___*/

