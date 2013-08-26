#ifndef baciError_H
#define baciError_H

/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2003 
*
*This library is free software; you can redistribute it and/or
*modify it under the terms of the GNU Lesser General Public
*License as published by the Free Software Foundation; either
*version 2.1 of the License, or (at your option) any later version.
*
*This library is distributed in the hope that it will be useful,
*but WITHOUT ANY WARRANTY; without even the implied warranty of
*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*Lesser General Public License for more details.
*
*You should have received a copy of the GNU Lesser General Public
*License along with this library; if not, write to the Free Software
*Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: baciError.h,v 1.97 2008/06/03 09:14:11 bjeram Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram    2003-03-12  added ACSErr prefix
* bjeram    2002-06-06  renamed  ACS_ERROR_STACK* ACS_COMPLETION*
* bjeram    2002-06-05  added ACSErrHelper::ACSErrStack( const ACE_TCHAR * file, int line,  ACSErr::Completion &completion, ACSError& error);
* msekoran  2002-05-23  created
*/

/** 
 * @file 
 * Header file BACI Error.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baci.h>
#include <baciC.h>
#include <acserr.h>
#include <ACSErrTypeOKC.h>

/**
 * ACS_COMPLETION is a macro which updates the singleton Completion. 
 * Takes in a variable number of arguments and passes them
 * to ACSErrHelper::ACSErrStack while prepending the current file name and line number
 * to the list of arguments.
 * @param arg List of arguments used by ACSErrStack
 */
#define ACS_COMPLETION(arg...) ACSErrHelper::ACSErrStack(__FILE__, __LINE__,## arg) 

/**
 * ACS_COMPLETION_LOG is a macro which updates the singleton Completion and logs it
 * if necessary. Takes in variable number of arguments and passes them
 * to ACSErrHelper::ACSErrStackLog while prepending the current file name and line number
 * to the list of arguments.
 * @param arg List of arguments used by ACSErrStack
 */
#define ACS_COMPLETION_LOG(arg...) ACSErrHelper::ACSErrStackLog(__FILE__, __LINE__,## arg) 

class ACSErrHelper
{
  public:

    /// Log error stack.
    static const unsigned int LOG_FLAG_M;

    /// Update ACSErr::Completion.
    static const unsigned int COMPLETION_FLAG_M;

    /// Update ACSErr::Completion with ACSError
    static void ACSErrStack(const ACE_TCHAR * file, int line, /* there just that the same macro can be used*/
					  ACSErr::Completion &completion, ACSError& error);

    /// Updates ACSErr::Completion with stack if necessary.
    static void ACSErrStack(const ACE_TCHAR * file, int line, 
			    ACSErr::Completion &completion, const ACE_TCHAR * routine, 
//			    ACSErrType errtype = ACSErrTypeCommon, ACSErr::ErrorCode errcode = ACSErrUnknown,
			    ACSErr::ACSErrType errtype = ACSErr::ACSErrTypeOK, ACSErr::ErrorCode errcode = ACSErrTypeOK::ACSErrOK,
			    const ACE_TCHAR * msgname = 0, const ACE_TCHAR * msg = 0,
			    int flag = COMPLETION_FLAG_M);

    /// Updates ACSErr::Completion with stack and logs it if necessary.
    static void ACSErrStackLog(const ACE_TCHAR * file, int line, 
			    ACSErr::Completion &completion, const ACE_TCHAR * routine, 
//			    ACSErrType errtype = ACSErrTypeCommon, ACSErr::ErrorCode errcode = ACSErrUnknown,
			    ACSErr::ACSErrType errtype = ACSErr::ACSErrTypeOK, ACSErr::ErrorCode errcode = ACSErrTypeOK::ACSErrOK,
			    const ACE_TCHAR * msgname = 0, const ACE_TCHAR * msg = 0,
			    int flag = COMPLETION_FLAG_M | LOG_FLAG_M);
};

#endif /*! baciError_H*/



