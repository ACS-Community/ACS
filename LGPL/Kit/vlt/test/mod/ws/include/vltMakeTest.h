#ifndef VLTMAKETEST_H
#define VLTMAKETEST_H
/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: vltMakeTest.h,v 1.1.1.1 2003/02/20 10:44:07 mzampare Exp $" 
*
* vltMakeTest.h
*
* who       when      what
* --------  --------  ----------------------------------------------
* gfilippi  20/10/94  created
*/

/*
 *   This file is part of the vltMake modular test package
 */

/*
 * System Headers required by all applications in this module
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>


#ifndef MAKE_VXWORKS
/*
 * VLT Software for WS only
 */
#include "rtapClasses/rtTypes.h"    /* RTAP */

#include "db.h"                     /* ccs */
#include "msg.h"
#include "tims.h"
#include "err.h"
#include "log.h"
#endif  /* MAKE_VXWORKS */

/* 
 * Macros 
 */

/* 
 * to make the code more readable the following macros for error treatment 
 * are defined:
 *
 *    - CLEAN used when ccsInit has been performed
 *    - ABORT used when ccsInit has not yet been performed
 *
 * For each "XXXXX" macro a "xxxxx" label defines in the code the
 * appropriate actions needed for that specific error condition.
 * 
 * The macro PRINT_ERROR produces debugging information.
 */

#define PRINT_ERROR(reason) \
    {\
    printf("File %s line %d\n", __FILE__, __LINE__);     \
    printf("Failure: %s\n", reason);             \
    printf("Error structure\n");                         \
    printf("   EnvName    : %s\n", error.envName);       \
    printf("   StackId    : %x\n", error.stackId.id);    \
    printf("   SequenceNbr: %d\n", error.sequenceNumber);\
    printf("   ModuleId   : %s\n", error.moduleId);      \
    printf("   LocationId : %s\n", error.location);      \
    printf("   ErrorNumber: %d\n", error.errorNumber);   \
    printf("   Parameters : %s\n", error.runTimePar);    \
    errCloseStack(&error); \
    }

#define CLEAN(reason) { PRINT_ERROR(reason); goto clean; }
#define ABORT(reason) { PRINT_ERROR(reason); goto abort; }

/*
 * test output
 */

#define TEST_FAILED(testName)    { printf("%s - FAIL\n", testName); exit(EXIT_FAILURE); }
#define TEST_SUCCEEDED(testName) { printf("%s - PASS\n", testName); exit(EXIT_SUCCESS); }


/*
 * function prototypes
 */
void vltMakeTestProcedure1();
void vltMakeTestProcedure2();


#endif /*!VLTMAKETEST_H*/
