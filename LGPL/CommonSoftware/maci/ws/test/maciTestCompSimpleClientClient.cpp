/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciTestCompSimpleClientClient.cpp,v 1.1 2007/10/03 20:08:20 cparedes Exp $"
*
* who       when       what
* --------  --------   ----------------------------------------------
* msekoran  2002-05-17 bugs fixed, added additional tests
* kzagar    2002-03-19 MACI test scripting; BACI no longer checked
* bjeram    2001-11-20 Cleaned up and double checked.
* gchiozzi  2001-11-15 created
*/

static char *rcsId="@(#) $Id: maciTestCompSimpleClientClient.cpp,v 1.1 2007/10/03 20:08:20 cparedes Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);


#include <maciTestC.h>
#include <maciTestUtils.h>
#include <maciSimpleClient.h>
#include <maciHelper.h>
#include <stdio.h>
#include <stdlib.h>


 using namespace maci;


int main (int argc, char **argv)
{
	ACS_SHORT_LOG ((LM_INFO, "Init maciTestCompSimpleClientClient..."));
	SimpleClient client;

	MACI_TEST::MaciTestCompSimpleClient_ptr comp;
	//MACI_TEST::MaciTestClass_ptr comp;
	if (client.init(argc,argv) == 0)
	{
		return -1;
	} else {
		//Must log into manager before we can really do anything
		ACS_SHORT_LOG ((LM_INFO, "Logging into the manager ..."));
		client.login();
	}

	try {
		comp = client.getComponent<MACI_TEST::MaciTestCompSimpleClient>("MACI_SIMPLE_CLIENT",0,true);
		//comp = client.getComponent<MACI_TEST::MaciTestClass>("MACI_SIMPLE_CLIENT",0,true);

		comp->createSimpleClient();

		client.releaseComponent ("MACI_SIMPLE_CLIENT");
		client.logout();
	} catch ( CORBA::Exception &ex ) {
		ex._tao_print_exception("main");
	}

	ACS_SHORT_LOG ((LM_INFO, "Exiting maciTestCompSimpleClientClient..."));

	return 0;
} /* end main() */






