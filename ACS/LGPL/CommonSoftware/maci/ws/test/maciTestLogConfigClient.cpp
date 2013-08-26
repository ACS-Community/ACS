/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciTestLogConfigClient.cpp,v 1.1 2007/10/03 20:08:20 cparedes Exp $"
*
* who       when       what
* --------  --------   ----------------------------------------------
* msekoran  2002-05-17 bugs fixed, added additional tests
* kzagar    2002-03-19 MACI test scripting; BACI no longer checked
* bjeram    2001-11-20 Cleaned up and double checked.
* gchiozzi  2001-11-15 created
*/

static char *rcsId="@(#) $Id: maciTestLogConfigClient.cpp,v 1.1 2007/10/03 20:08:20 cparedes Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);


#include <maciTestC.h>
#include <maciTestUtils.h>
#include <maciSimpleClient.h>
#include <maciHelper.h>
#include <stdio.h>
#include <stdlib.h>


 using namespace maci;

ACE_RCSID(maciLogConfigTestClient, maciLogConfigTestClient, "$Id: maciTestLogConfigClient.cpp,v 1.1 2007/10/03 20:08:20 cparedes Exp $")

int main (int argc, char **argv)
{
  ACS_SHORT_LOG ((LM_INFO, "Init maciLogConfigTestClient..."));
  SimpleClient client;
  MACI_TEST::LogConfigTestClass_ptr comp;
    if (client.init(argc,argv) == 0)
	{
	return -1;
	}
    else
	{
	//Must log into manager before we can really do anything
	client.login();
	}
  try
    {
	comp = client.getComponent<MACI_TEST::LogConfigTestClass>("MACI_LOG_CONFIG",0,true);

	comp->log_all();


        client.releaseComponent ("MACI_LOG_CONFIG");
    	client.logout();

    }
  catch ( CORBA::Exception &ex )
    {
      ACE_PRINT_EXCEPTION(ex, "main");
    }

  ACS_SHORT_LOG ((LM_INFO, "Exiting maciLogConfigTestClient..."));
  return 0;

} /* end main() */






