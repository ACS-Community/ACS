/*******************************************************************************
 * ALMA - Atacama Large Millimiter Array
 * (c) Associated Universities Inc., 2007 
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *
 * "@(#) $Id: acsncServiceCheckerExec.cpp,v 1.2 2008/07/27 15:09:30 cparedes Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * nbarriga  2007-07-04  created 
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

#include <maciSimpleClient.h>

#include <acsncServiceChecker.h>
int main(int argc, char *argv[])
{
	maci::SimpleClient client;

	// Creates and initializes the SimpleClient object

	if (client.init(argc,argv) == 0)
	{
		return -1;
	}
	else
	{
		//Must log into manager before we can really do anything
		client.login();
	}

	nc::ServiceChecker SC(client.getORB());
	while(1){
		if(SC.check(acscommon::ALMADOMAIN))ACS_SHORT_LOG((LM_DEBUG,"%s notification service operational",acscommon::ALMADOMAIN));
		if(SC.check(acscommon::ARCHIVING_DOMAIN))ACS_SHORT_LOG((LM_DEBUG,"%s notification service operational",acscommon::ARCHIVING_DOMAIN));
		if(SC.check(acscommon::LOGGING_DOMAIN))ACS_SHORT_LOG((LM_DEBUG,"%s notification service operational",acscommon::LOGGING_DOMAIN));
		ACE_OS::sleep(5);
	}
	client.disconnect();
	return 0;
}
