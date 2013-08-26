/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration),
*    All rights reserved
*
*    This library is free software; you can redistribute it and/or
*    modify it under the terms of the GNU Lesser General Public
*    License as published by the Free Software Foundation; either
*    version 2.1 of the License, or (at your option) any later version.
*
*    This library is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*    Lesser General Public License for more details.
*
*    You should have received a copy of the GNU Lesser General Public
*    License along with this library; if not, write to the Free Software
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: baciError.cpp,v 1.96 2005/02/04 00:34:00 dfugate Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram    2003/03012  added ACSErr prefix
* msekoran  2002/05/26  created 
*/

#include <vltPort.h>
#include "baciError.h" 

const unsigned int ACSErrHelper::LOG_FLAG_M = 0x01;
const unsigned int ACSErrHelper::COMPLETION_FLAG_M = 0x02;

void ACSErrHelper::ACSErrStack(const ACE_TCHAR * file, int line,
			       ACSErr::Completion &completion, ACSError& error)
{
    ACE_UNUSED_ARG(file);
    ACE_UNUSED_ARG(line);
    completion.previousError.length(1);
    completion.previousError[0] = error.getErrorTrace();
    completion.type = error.getErrorType();
    completion.code = error.getErrorCode();
    completion.timeStamp = error.getTimeStamp();
}

void ACSErrHelper::ACSErrStack(const ACE_TCHAR * file, int line,
			  ACSErr::Completion &completion, const ACE_TCHAR * routine, 
			  ACSErr::ACSErrType errtype, ACSErr::ErrorCode errcode,
			  const ACE_TCHAR * msgname, const ACE_TCHAR * msg, int flags)
{
    bool firstErrorOnStack = completion.previousError.length()==0;

    if (errtype == ACSErr::ACSErrTypeOK && errcode == ACSErrTypeOK::ACSErrOK)
      {
	// everything is OK, do not add stack
	if (firstErrorOnStack==true)
	    {
	    return;
	    }
        // set previous error
        else
	  {
	    errtype = completion.previousError[0].errorType;
	    errcode = completion.previousError[0].errorCode;
	  }
      }

    // add stack
    ACSError * errorStack_p;
    if (firstErrorOnStack==true)
	{
	errorStack_p = new ACSError(file, line, errtype, errcode, routine);
	}
    else
	{
	errorStack_p = new ACSError(file, line, completion.previousError[0], errtype, errcode, routine);
	}

    // add additional data
    if ((msgname!=0) && (msg!=0))
	{
	errorStack_p->addData(msgname, msg);
	}

    // log if flag is set
    if ((flags & LOG_FLAG_M)!=0)
	{
	errorStack_p->log();
	}

    // update completion if flag is set
    if ((flags & COMPLETION_FLAG_M)!=0)
      {
	// update completion stack 
	ACSErr::ErrorTrace * errc_p = errorStack_p->returnErrorTrace();
	completion.previousError.length(1);
	completion.previousError[0] = *errc_p;
	delete errc_p;
	
	// update completion codes
	if (firstErrorOnStack==true)
	  {
	    completion.timeStamp = completion.previousError[0].timeStamp;
	    completion.type = completion.previousError[0].errorType;
	    completion.code = completion.previousError[0].errorCode;
	  }
      }
    else
	{
	delete errorStack_p;
	}
}

void
ACSErrHelper::ACSErrStackLog(const ACE_TCHAR * file, int line,
			     ACSErr::Completion &completion, const ACE_TCHAR * routine, 
			     ACSErr::ACSErrType errtype, ACSErr::ErrorCode errcode,
			     const ACE_TCHAR * msgname, const ACE_TCHAR * msg, int flags)
{
    ACSErrStack(file, line, completion, routine, 
		errtype, errcode, msgname, msg, flags);
}




