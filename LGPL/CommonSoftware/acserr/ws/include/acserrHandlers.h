#ifndef _ACSERR_HANDLERS_H
#define _ACSERR_HANDLERS_H
/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: acserrHandlers.h,v 1.2 2005/06/16 08:50:27 bjeram Exp $"
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

#endif /*!_H*/
