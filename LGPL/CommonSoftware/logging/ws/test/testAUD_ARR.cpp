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
 * "@(#) $Id: testAUD_ARR.cpp,v 1.3 2007/07/17 13:38:05 nbarriga Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * nbarriga  2007-01-30  created
 */

// Uncomment this if you are using the VLT environment
// #include "vltPort.h"


#include <stdlib.h>
#include <stdio.h>
#include <string.h>


#include <maciSimpleClient.h>

int main(int argc, char *argv[])
{
	maci::SimpleClient client;

	if (client.init(argc,argv) == 0){
		return -1;
	}
	else{
		// Log into the manager before doing anything
		client.login();
	}

        ACS_SHORT_LOG((LM_WARNING,"ACS_SHORT_LOG"));
        LOG_FULL(LM_WARNING,"main","LOG_FULL",log_audience::OPERATOR,"array01","Antenna01");
        LOG_WITH_ANTENNA_CONTEXT(LM_WARNING,"main","LOG_WITH_ANTENNA_CONTEXT","array01","Antenna01");
        LOG_TO_AUDIENCE(LM_WARNING,"main","LOG_TO_AUDIENCE",log_audience::OPERATOR);
	client.logout();

	return 0;

}








