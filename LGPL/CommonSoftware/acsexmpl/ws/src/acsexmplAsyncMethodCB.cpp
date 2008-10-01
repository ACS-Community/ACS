/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2004 
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
* "@(#) $Id: acsexmplAsyncMethodCB.cpp,v 1.2 2008/10/01 04:30:47 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2004-08-16  created 
*/

/************************************************************************
*   NAME
*   
* 
*   SYNOPSIS
*
*   
*   PARENT CLASS
*
* 
*   DESCRIPTION
*
*
*   PUBLIC METHODS
*
*
*   PUBLIC DATA MEMBERS
*
*
*   PROTECTED METHODS
*
*
*   PROTECTED DATA MEMBERS
*
*
*   PRIVATE METHODS
*
*
*   PRIVATE DATA MEMBERS
*
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

#include "vltPort.h"

static char *rcsId="@(#) $Id: acsexmplAsyncMethodCB.cpp,v 1.2 2008/10/01 04:30:47 cparedes Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "acsexmplAsyncMethodCB.h"
#include <logging.h>

void AsyncMethodCBvoid::working (const ACSErr::Completion &c, const ACS::CBDescOut &desc)
	 {
		char logStr[128];
		sprintf(logStr,"AsyncMethodCBvoid::working for method %s",methodName.c_str());
		ACS_SHORT_LOG((LM_INFO,logStr));
}

void AsyncMethodCBvoid::done (const ACSErr::Completion &c, const ACS::CBDescOut &desc)
	 {
		char logStr[128];
		sprintf(logStr,"AsyncMethodCBvoid::done for method %s",methodName.c_str());
		ACS_SHORT_LOG((LM_INFO,logStr));
}

// Time negotiation is not yet implemented
CORBA::Boolean AsyncMethodCBvoid::negotiate (ACS::TimeInterval time_to_transmit, const ACS::CBDescOut &desc) 
	 {
		char logStr[128];
		sprintf(logStr,"AsyncMethodCBvoid::negotiate for method %s",methodName.c_str());
		ACS_SHORT_LOG((LM_INFO,logStr));
		return true;
}

/*___oOo___*/
