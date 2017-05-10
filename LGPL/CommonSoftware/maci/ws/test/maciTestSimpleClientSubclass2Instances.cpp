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


class SimpleClient2 : public SimpleClient {
public:
	SimpleClient2() : SimpleClient() {}
};

/**
 * The test checks the methods of the SimpleClient
 * 
 */
int main (int argc, char **argv)
{

	SimpleClient *client1 = NULL;
	SimpleClient2 *client2 = NULL;
	SimpleClient *client3 = NULL;

	client1 = new SimpleClient();
	printf("First client has been instantiated\n");

	try {
		printf("Trying to instantiated another client while the first one is alive\n");
		client2 = new SimpleClient2();
	} catch(ACSErrTypeCommon::CouldntCreateObjectExImpl &ex) {
                std::string msg = ex.toString();
                printf("%s\n", msg.c_str());
	}

	delete client1;
	printf("First client has been deleted\n");

	client2 = new SimpleClient2();
	printf("Second client has been instantiated\n");

	try {
		printf("Trying to instantiated another client while the second one is alive\n");
		client3 = new SimpleClient();
	} catch(ACSErrTypeCommon::CouldntCreateObjectExImpl &ex) {
                std::string msg = ex.toString();
                printf("%s\n", msg.c_str());
	}

	delete client2;
	printf("Second client has been deleted\n");

	client3 = new SimpleClient();
	printf("Third client has been instantiated\n");

	delete client3;
	printf("Third client has been deleted\n");

	return 0;
}//main

