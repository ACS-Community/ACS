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
* "@(#) $Id: acsexmplAsyncCallbacks.cpp,v 1.2 2008/10/01 04:30:47 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2004-08-10  created 
*/

/************************************************************************
*   NAME
*   
* 
*   SYNOPSIS
*   
* 
*   DESCRIPTION
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

//#include "vltPort.h"

static char *rcsId="@(#) $Id: acsexmplAsyncCallbacks.cpp,v 1.2 2008/10/01 04:30:47 cparedes Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "acsexmplAsyncCallbacks.h"
#include <logging.h>

void AsyncCBdouble::working (CORBA::Double value, const ACSErr::Completion &c, const ACS::CBDescOut &desc)
	{
		char logStr[128];
		sprintf(logStr,
			"AsyncCBdouble::working property %s and val=%lf",
			propertyName.c_str(),
			value);
		ACS_SHORT_LOG((LM_INFO,logStr));
}

// This is executed when the value of the variable is available
// In this case it writes the value also in the thread variable
void AsyncCBdouble::done (CORBA::Double value, const ACSErr::Completion &c, const ACS::CBDescOut &desc)
	{
		char logStr[128];
		sprintf(logStr,
			"AsyncCBdouble::done property %s and val=%lf",
			propertyName.c_str(),
			value);
		ACS_SHORT_LOG((LM_INFO,logStr));
		*varToUpdate=value;
}

// The time negotiation is not yet implemented	
CORBA::Boolean AsyncCBdouble::negotiate (ACS::TimeInterval time_to_transmit, const ACS::CBDescOut &desc) 
	{
		char logStr[128];
		sprintf(logStr,"AsyncCBdouble::negotiate property %s",propertyName.c_str());
		ACS_SHORT_LOG((LM_INFO,logStr));
		return true;
}
