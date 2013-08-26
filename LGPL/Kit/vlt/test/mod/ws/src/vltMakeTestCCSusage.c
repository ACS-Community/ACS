/***************************************************************************
* E.S.O. - VLT project - 1994
* 
* testCCSSusage.c
*
* HISTORY
*
* who       when      what
* --------  --------  ----------------------------------------------
* gfilippi  20/10/94  created
*
*/
/***************************************************************************
*
*  NAME 
* 
*   testCCSusage - test access to CCS package
* 
* SYNOPSIS 
*
*   testCCSusage
* 
*
* DESCRIPTION 
*
*   This program is part of the vltMake modular test package
*
*
*   The program opens the local db, reads a point attribute and closes the 
*   environment.
*
*   The atrribute to be displayed is: @PARAMS:SCALARS.scalar_int32
*
*   RETURN VALUES 
*   EXIT_SUCCES if everything is ok             
*   EXIT_FAILURE on any error
*
*   ENVIRONMENT
*   RTAPENV   	READ    the WS local environment.
*
*   SEE ALSO
*
*************************************************************************/
                 
#define _POSIX_SOURCE 1
#include <vltPort.h>

/*
 * System Headers
 */
#include <signal.h>                  /* tty_*    */
#include <termios.h>                 /* tty_*    */
#include <time.h>                    /* strftime */

/* 
 * application headers
 */
#include "vltMakeTest.h"


/*
 * MAIN
 */
int main (int argc, char **argv)
{
/* 
 * some local declarations
 */ 
dbTYPE          dataType[200];
dbSYMADDRESS    pointName;
vltINT32        actual;
dbITEM	    	attrNameP;
dbATTRTYPE      attrType;
vltUINT8        fieldCnt;
vltUINT16       recCnt;
vltUINT16	recsUsed;
vltUINT32	recSize;
char		pBuf[256];
dbDIRADDRESS	attr_dirAdd;
rtNumericDeType	readValue;
char            testId[80] = "vltMakeTestCCSusage";

/*
 * declarations for ccs in common s/w
 */
ccsENVNAME		envName;
ccsPROCNUM		procNum;
ccsPROCNAME		procName;
ccsERROR		error;


/*
 * check input parameter
 */
if ( argc > 1) 
    {
    printf("\n\tusage: %s \n\n", argv[0]);
    TEST_FAILED(testId);
    }

strcpy ((char *)pointName, "PARAMS:SCALARS.scalar_int32");
attrNameP = dbEMPTY;

/*
 * connect with the data base
 */
if (ccsInit (argv[0], 0, NULL, NULL, &error) == FAILURE)
    ABORT("ccsInit");

if (ccsGetMyProcId (envName, &procNum, procName, &error) == FAILURE)
    CLEAN("ccsGetMyProcId");

/*
 * Get Direct address as well as some more info on the point to be read
 */
if (dbGetDirAddr(pointName,attrNameP,&attr_dirAdd,&error) == FAILURE)
    CLEAN("dbGetDirAddr");
 
if (dbGetAttrInfo(pointName,attrNameP,&attrType,&fieldCnt,
                      &recCnt,&recSize,&recsUsed,
	 	      dataType,&error) == FAILURE)
    CLEAN("dbGetAttrInfo");

/*
 * Read the current value
 */
if (dbReadDirect( &attr_dirAdd, dataType, pBuf, recSize, &actual, &recCnt,
                      &attrType, &error) == FAILURE)
    CLEAN("dbReadDirect");

memcpy (&readValue, pBuf, rtDataElemSize[(rtInt)dataType[0]]);

/*
 * normal termination: close environment and exit
 */
if (ccsExit(&error) != SUCCESS) 
    ABORT("ccsExit");

TEST_SUCCEEDED(testId);

/*
 * error handlers
 */
clean:
    /*
     * try to close the connection
     */
    if (ccsExit (&error) == FAILURE)
        {
        printf("CLEAN: Unable to close db.\n"); 
        }

abort:
    /*
     * just exit with error code
     */
    TEST_FAILED(testId);

}  /* end of main */

/*___oOo___*/



