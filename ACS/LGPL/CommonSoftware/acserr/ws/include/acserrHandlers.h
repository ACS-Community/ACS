#ifndef _ACSERR_HANDLERS_H
#define _ACSERR_HANDLERS_H
/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: acserrHandlers.h,v 1.3 2010/03/15 11:58:05 bjeram Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram  yyyy-mm-dd  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "acserrHandlersErr.h"

void acserrUnspecifiedExHandler();

void acserrUncaughtExHandler();

void acserrSigSegvHandler(int signal, siginfo_t* info, void* data);

#endif /*!_H*/
