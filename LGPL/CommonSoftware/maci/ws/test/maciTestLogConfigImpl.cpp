/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciTestLogConfigImpl.cpp,v 1.1 2007/10/03 20:08:20 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
*/

#include <vltPort.h>

static char *rcsId="@(#) $Id: maciTestLogConfigImpl.cpp,v 1.1 2007/10/03 20:08:20 cparedes Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

//#include <logging.h>
#include <maciTestLogConfigImpl.h>
//#include <maciContainerImpl.h>
//#include <maciContainerServices.h>


/////////////////////////////////////////////////
// LogConfigTestClass
/////////////////////////////////////////////////


LogConfigTestClass::LogConfigTestClass(
    const ACE_CString& name,
    maci::ContainerServices* containerServices) :
    acscomponent::ACSComponentImpl(name,containerServices)


{
//  ACS_SHORT_LOG((LM_INFO,
//                 "::LogConfigTestClass::LogConfigTestClass()"));

  // simulate component failing initialization (e.g. w/o required configuration)
//  if (ACE_OS::strcmp(name.c_str(), "MACI_FAKE") == 0)
//      return;

}

LogConfigTestClass::~LogConfigTestClass()
{
}


/* ----------------------------------------------------------------*/
/* --------------------- [ CORBA interface ] ----------------------*/
/* ----------------------------------------------------------------*/

void
LogConfigTestClass::log_all ()
  throw (CORBA::SystemException)
{

  ACS_SHORT_LOG((LM_EMERGENCY, "LogConfigTestClass::test *********starting the test***********"));
   
  ACS_SHORT_LOG((LM_TRACE, "[LogConfigTestClass::test] I am a message LM_TRACE"));
  ACS_SHORT_LOG((LM_DEBUG, "[LogConfigTestClass::test] I am a message LM_DEBUG"));
  ACS_SHORT_LOG((LM_INFO, "[LogConfigTestClass::test] I am a message LM_INFO"));
  ACS_SHORT_LOG((LM_NOTICE, "[LogConfigTestClass::test] I am a message LM_NOTICE"));
  ACS_SHORT_LOG((LM_WARNING, "[LogConfigTestClass::test] I am a message LM_WARNING"));
  ACS_SHORT_LOG((LM_ERROR, "[LogConfigTestClass::test] I am a message LM_ERROR"));
  ACS_SHORT_LOG((LM_CRITICAL, "[LogConfigTestClass::test] I am a message LM_CRITICAL"));
  ACS_SHORT_LOG((LM_ALERT, "[LogConfigTestClass::test] I am a message LM_ALERT"));

  ACS_SHORT_LOG((LM_EMERGENCY, "LogConfigTestClass::test ===========finishing the test============="));
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(LogConfigTestClass)
/* ----------------------------------------------------------------*/

