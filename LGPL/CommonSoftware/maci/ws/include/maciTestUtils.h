#ifndef maciTestUtils_h
#define maciTestUtils_h

/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciTestUtils.h,v 1.78 2003/01/16 12:14:16 vltsccm Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran 2002-05-17 ASSERT_EQUALS_STR macro fixed
* kzager   2002-02-15 Created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <logging.h>

#include <tao/corba.h>
#include <ace/Read_Buffer.h>

#include <string>
#include <baci.h>

#define TEST_INIT(name)                                      \
  char *testSuiteName = name;                                \
  int   nTestCases  = 0;                                     \
  int   nFailures   = 0;                                     \
  ACS_SHORT_LOG((LM_INFO, "Test suite '%s'", testSuiteName))

#define TEST_DONE                                                              \
  ACS_SHORT_LOG((LM_INFO, "Test suite '%s' complete. %d out of %d test cases " \
                 "have failed (%.2f%%)", testSuiteName, nFailures, nTestCases, \
                 100.0*nFailures/nTestCases));

#define ASSERT_EQUALS_STR(actual, expected)                                   \
  {                                                                           \
    ++nTestCases;                                                             \
    if (expected==0)                                                          \
     {                                                                        \
        if (actual!=0)                                                        \
        {                                                                     \
          ACS_SHORT_LOG((LM_ERROR, "Test case in file '%s' at line %d failed: " \
                         "Expected 0 but got '%s'!", __FILE__, __LINE__,      \
                         actual));                                            \
          ++nFailures;                                                        \
        }                                                                     \
     }                                                                        \
    else if (strcmp(actual, expected) != 0)                                   \
      {                                                                       \
        ACS_SHORT_LOG((LM_ERROR, "Test case in file '%s' at line %d failed: " \
                       "Expected '%s' but got '%s'!", __FILE__, __LINE__,     \
                       expected, actual));                                    \
        ++nFailures;                                                          \
      }                                                                       \
  }

#define ASSERT_EQUALS_INT(actual, expected)                                   \
  {                                                                           \
    ++nTestCases;                                                             \
    if (actual != expected)                                                   \
      {                                                                       \
        ACS_SHORT_LOG((LM_ERROR, "Test case in file '%s' at line %d failed: " \
                       "Expected '%d' but got '%d'!", __FILE__, __LINE__,     \
                       expected, actual));                                    \
        ++nFailures;                                                          \
      }                                                                       \
  }

#endif   /* maciTestUtils_h */

