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
 * "@(#) $Id: testLTSClient.cpp,v 1.3 2012/03/06 19:16:56 tstaig Exp $"
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

static char *rcsId="@(#) $Id: testLTSClient.cpp,v 1.3 2012/03/06 19:16:56 tstaig Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <maciSimpleClient.h>
#include <ACSLogTypeExample.h>

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

	ACSLogTypeExample::simpleLog my_simpleLog(__FILE__,__LINE__,"main");
	my_simpleLog.log();

	ACSLogTypeExample::simpleLog my_simpleLogAA(__FILE__,__LINE__,"main","Array01","Antenna01");
	my_simpleLogAA.log();


	ACSLogTypeExample::complexLog my_complexLog(__FILE__,__LINE__,"main");
	my_complexLog.setsomeDoubleMember(3.14159);
	my_complexLog.setsomeStringMember("test string");
	my_complexLog.setsomeLongMember(42);
	my_complexLog.setsomeBooleanMember(true);
	my_complexLog.log();

	ACSLogTypeExample::complexLog my_complexLogAA(__FILE__,__LINE__,"main");
        my_complexLogAA.setArray("Array01");
        my_complexLogAA.setAntenna("Antenna01");
	my_complexLogAA.setsomeDoubleMember(3.14159);
	my_complexLogAA.setsomeStringMember("test string");
	my_complexLogAA.setsomeLongMember(42);
	my_complexLogAA.setsomeBooleanMember(true);
	my_complexLogAA.log();


	client.logout();

	return 0;

}








