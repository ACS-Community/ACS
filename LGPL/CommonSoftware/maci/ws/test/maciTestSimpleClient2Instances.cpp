/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: maciTestSimpleClient.cpp,v 1.6 2008/08/26 03:18:59 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
*/


#define _POSIX_SOURCE 1
#include "vltPort.h"

static char *rcsId="@(#) $Id: maciTestSimpleClient.cpp,v 1.6 2008/08/26 03:18:59 cparedes Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <maciTestC.h>
#include <maciSimpleClient.h>
#include <logging.h>
#include <string>
#include <iostream>

using namespace maci;



/**
 * The test checks the methods of the SimpleClient
 * 
 */
int main (int argc, char **argv)
{

	SimpleClient *client1 = NULL;
	SimpleClient *client2 = NULL;

	client1 = new SimpleClient();

	try {
		client2 = new SimpleClient();
	} catch(ACSErrTypeCommon::CouldntCreateObjectExImpl &ex) {
                std::string msg = ex.toString();
                printf("%s\n", msg.c_str());
	}

	delete client1;

	client2 = new SimpleClient();
	printf("Second client has been instantiated\n");

	return 0;
}//main

