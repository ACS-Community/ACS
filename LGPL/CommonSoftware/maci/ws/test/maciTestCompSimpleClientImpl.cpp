/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciTestCompSimpleClientImpl.cpp,v 1.101 2011/06/07 23:56:38 javarias Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
*/

#include <vltPort.h>

static char *rcsId="@(#) $Id: maciTestCompSimpleClientImpl.cpp,v 1.101 2011/06/07 23:56:38 javarias Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <logging.h>
#include <maciTestCompSimpleClientImpl.h>
#include <maciContainerImpl.h>
#include <maciContainerServices.h>
#include <maciSimpleClient.h>

 using namespace maci;

/////////////////////////////////////////////////
// MaciTestCompSimpleClient
/////////////////////////////////////////////////


MaciTestCompSimpleClient::MaciTestCompSimpleClient(
    const ACE_CString& name,
    maci::ContainerServices* containerServices) :
    acscomponent::ACSComponentImpl(name,containerServices) 
{
  ACS_SHORT_LOG((LM_INFO,
                 "::MaciTestCompSimpleClient::MaciTestCompSimpleClient"));

}

MaciTestCompSimpleClient::~MaciTestCompSimpleClient()
{
  ACS_SHORT_LOG((LM_INFO,
                 "::MaciTestCompSimpleClient::~MaciTestCompSimpleClient"));
}


/* ----------------------------------------------------------------*/
/* --------------------- [ CORBA interface ] ----------------------*/
/* ----------------------------------------------------------------*/

void
MaciTestCompSimpleClient::createSimpleClient()
{
	ACS_SHORT_LOG((LM_INFO, "MaciTestCompSimpleClient::createSimpleClient"));

	SimpleClient *client = NULL;

	try {
		client = new SimpleClient();
		ACS_SHORT_LOG((LM_ERROR, "First time that SimpleClient is instantiated in a component"));
	} catch(ACSErrTypeCommon::CouldntCreateObjectExImpl &ex) {
                std::string msg = ex.toString();
		ACS_SHORT_LOG((LM_ERROR, "First time that SimpleClient thrown an exception when instantiating it"));
		ACS_SHORT_LOG((LM_ERROR, msg.c_str()));
	}

        try {
		client = new SimpleClient();
		ACS_SHORT_LOG((LM_ERROR, "Second time that SimpleClient is instantiated in a component"));
	} catch(ACSErrTypeCommon::CouldntCreateObjectExImpl &ex) {
                std::string msg = ex.toString();
		ACS_SHORT_LOG((LM_ERROR, "Second time that SimpleClient thrown an exception when instantiating it"));
		ACS_SHORT_LOG((LM_ERROR, msg.c_str()));
		
	}
}

#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(MaciTestCompSimpleClient)
