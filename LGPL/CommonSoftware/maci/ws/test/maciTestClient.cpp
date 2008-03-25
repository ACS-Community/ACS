/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciTestClient.cpp,v 1.96 2008/03/25 12:53:08 bjeram Exp $"
*
* who       when       what
* --------  --------   ----------------------------------------------
* msekoran  2002-05-17 bugs fixed, added additional tests
* kzagar    2002-03-19 MACI test scripting; BACI no longer checked
* bjeram    2001-11-20 Cleaned up and double checked.
* gchiozzi  2001-11-15 created
*/

static char *rcsId="@(#) $Id: maciTestClient.cpp,v 1.96 2008/03/25 12:53:08 bjeram Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);


#include <maciTestC.h>
#include <maciTestUtils.h>
#include <maciSimpleClient.h>
#include <maciTestClientImpl.h>
#include <maciHelper.h>
#include <stdio.h>
#include <stdlib.h>
#include <acsutilPorts.h>
#include <maciErrType.h>

/*
 * Maximal length of a command passed to the MACI test client.
 */
#define MAX_COMMAND_LENGTH 2048
#define MAX_COMMAND_PARTS 32

#define SUCCESS 0
#define ERROR 1
#define FATAL 2

 using namespace maci;
 using namespace MACI_TEST;

ACE_RCSID(maciTestClient, maciTestClient, "$Id: maciTestClient.cpp,v 1.96 2008/03/25 12:53:08 bjeram Exp $")

typedef
  ACE_Hash_Map_Manager <ACE_CString, MaciTestClass_ptr, ACE_Null_Mutex>
    TestClassMap;

typedef
  ACE_Hash_Map_Manager <ACE_CString, ClientInfo, ACE_Null_Mutex>
    ClientMap;

/*
 * A command function that processes a command-line entry. Returns SUCCESS,
 * ERROR, or FATAL. If FATAL is returned, the MACI Test Client will terminate
 * as soon as possible.
 */
typedef int (*CommandProcessor)(int argc, const ACE_TCHAR *argv[]
  );

struct CommandDef
{
  ACE_TCHAR *command;
  ACE_TCHAR *params;
  ACE_TCHAR *description;
  CommandProcessor processor;
};

/*
 * GLOBAL VARIABLES
 */

/* A named list of all test classes (Components). */
TestClassMap g_TestClasses;

/* A named list of all clients. */
ClientMap    g_Clients;

/* Reference to the MACI simple client (Manager's callback) */
SimpleClient *g_Client = NULL;

/*
 * HELPER FUNCTIONS
 */

/*
 * A helper function that parses a CURL into domain and component name constituents.
 * This function is essentially a copy-paste from maciManagerImpl.cpp, except
 * that the default domain name is not set.
 */
int resolveCURL (const char * curl, char* &domain, char* &component)
{
  ACS_TRACE("resolveCURL (= maci::ManagerImpl::resolveCURL)");

  // if null or empty
  if (!curl || *curl==0)
    {
      domain = 0;
      component = 0;
      return -1;
    }

  const char prefix[] = "curl://";
  int prefixLen = ACE_OS::strlen(prefix);

  // nor "curl://" prefix -> threated as relative name
  if (ACE_OS::strnstr(curl, prefix, prefixLen)!=curl)
    {
      // local domain
      //domain = new ACE_TCHAR[ACE_OS::strlen(m_domain)+1];
      //ACE_OS::strcpy(domain, m_domain);
      domain = NULL;
      // copy curl
      component = new ACE_TCHAR[ACE_OS::strlen(curl)+1];
      ACE_OS::strcpy(component, curl);
      return 0;
    }

  // no domain specified -> local domain (eg. curl://device)
  const char* sep = ACE_OS::strchr(curl+prefixLen, '/');
  if (sep==0)                   // no '/'
    {
      // relative name taken
      // local domain
      //domain = new ACE_TCHAR[ACE_OS::strlen(m_domain)+1];
      //ACE_OS::strcpy(domain, m_domain);
      domain = NULL;
      // copy curl
      component = new ACE_TCHAR[ACE_OS::strlen(curl)-prefixLen+1];
      ACE_OS::strncpy(component, curl+prefixLen, ACE_OS::strlen(curl)-prefixLen+1);
      return 0;
    }


  domain = new ACE_TCHAR[sep-curl-prefixLen+1];
  ACE_OS::strncpy(domain, curl+prefixLen, sep-curl-prefixLen);
  domain[sep-curl-prefixLen]=0;         // terminate

  component = new ACE_TCHAR[curl+ACE_OS::strlen(curl)-sep];                   // +1-1 (skip '/')
  ACE_OS::strncpy(component, sep+1, curl+ACE_OS::strlen(curl)-sep);

  return 0;
}

ACE_TCHAR *Justify(ACE_TCHAR *buf, const ACE_TCHAR *s)
{
  strcpy(buf, s);
  ACE_TCHAR *lastSpace = buf;
  ACE_TCHAR *p = buf;
  int nLineLen = 0;
  while (*p != 0)
    {
      if (*p == ' ')
        lastSpace = p;
      ++p;
      ++nLineLen;
      if (nLineLen >= 75)
        {
          *lastSpace = '\n';
          nLineLen -= 75;
        }
    }
  /* We can return the local variable buf because it is static. */
  return buf;
}

ACE_CString GetCURL(ACE_CString name)
{
  char *domain, *componentName;
  if (resolveCURL (name.c_str (), domain, componentName) == -1)
    return ACE_CString("ERROR!!");
  return ACE_CString("curl://") + domain + "/" + name;
}

/*
 * SCRIPTING FUNCTION IMPLEMENTATIONS
 *
 * As a matter of convention, all function names start with Process,
 * followed by the name of the scripting function.
 *
 * Function ProcessTestClient contains several test cases, whereas other
 * functions just perform a single MACI task.
 */

int ProcessClient(int argc, const ACE_TCHAR *argv[]
                  )
{
  ACE_CString clientName = argv[1];
  const ACE_TCHAR *onPing = argv[2];
  if (onPing == NULL)
    onPing = "0";

  ACS_SHORT_LOG((LM_INFO, "Logging in client '%s'", clientName.c_str ()));
  if (g_Client == 0)
    {
      ACS_SHORT_LOG ((LM_ERROR, "Use 'init' command first!"));
      return FATAL;
    }

  MaciTestClientImpl *mtci = new MaciTestClientImpl (
    clientName, g_Client->manager (), atoi(onPing));
  ClientInfo* ci = g_Client->manager ()->login (mtci->_this());
  ACE_CHECK_RETURN(ERROR);

  mtci->setHandle (ci->h);

  if (g_Clients.bind (clientName, *ci) != 0)
    {
      ACS_SHORT_LOG ((LM_ERROR,
                      "Unable to save client info for '%s'",
                      clientName.c_str ()));
      return ERROR;
    }

  return SUCCESS;
}

int ProcessAdministrator(int argc, const ACE_TCHAR *argv[]
                         )
{
  ACE_CString adminName = argv[1];
  const ACE_TCHAR *onPing = argv[2];
  if (onPing == NULL)
    onPing = "0";

  ACS_SHORT_LOG((LM_INFO, "Logging in administrator '%s'", adminName.c_str ()));
  if (g_Client == 0)
    {
      ACS_SHORT_LOG ((LM_ERROR, "Use 'init' command first!"));
      return FATAL;
    }

  MaciTestAdministratorImpl *mtai = new MaciTestAdministratorImpl (
    adminName, g_Client->manager (),
    atoi(onPing));

  ClientInfo* ci = g_Client->manager ()->login (mtai->_this());
  ACE_CHECK_RETURN(ERROR);

  mtai->setHandle (ci->h);

  if (g_Clients.bind (adminName, *ci) != 0)
    {
      ACS_SHORT_LOG ((LM_ERROR,
                      "Unable to save administrator info for '%s'",
                      adminName.c_str ()));
      return ERROR;
    }

  return SUCCESS;
}

int ProcessGetComponent(int argc, const ACE_TCHAR *argv[])
{
  ACE_CString requestor = argv[1];
  ACE_CString curl = argv[3];
  bool activate = (argv[2][0] == '1') ? true : false;

  ACS_SHORT_LOG((LM_INFO,
                 "Getting component '%s' requested by '%s' (activation: %d)",
                 curl.c_str(), requestor.c_str (), int(activate)));

  CORBA::Object_ptr component = NULL;
  if (requestor == "SimpleClient")
    {
      char *domain, *componentName;
      if (resolveCURL (curl.c_str (), domain, componentName) == -1)
        return ERROR;
      try
	  {
	  component = g_Client->getComponent(componentName,
					     (((domain != 0) && strlen(domain) == 0) ? NULL : domain),
					     activate);
	  if (domain != NULL) delete domain;
	  if (componentName != NULL) delete componentName;
	  }
      catch(maciErrType::CannotGetComponentExImpl &_ex)
	  {
	  _ex.log();
	  if (domain != NULL) delete domain;
	  if (componentName != NULL) delete componentName;
	  return ERROR;
	  }//try-catch
    }
  else
    {
      ClientInfo ci;
      MaciTestClass_ptr mtc;

      if (g_Clients.find (requestor, ci) == 0)
        {
	try
	    {
	    component = g_Client->manager ()->get_component (ci.h, curl.c_str(), activate);
	    }
	catch(maciErrType::CannotGetComponentEx &_ex)
	    {
	    maciErrType::CannotGetComponentExImpl ex(_ex); 
	    ex.log();
	    return ERROR;
	    }
	catch(maciErrType::ComponentNotAlreadyActivatedEx &_ex)
	    {
	    maciErrType::ComponentNotAlreadyActivatedExImpl ex(_ex);
	    ex.log();
	    return ERROR;
	    }
	catch(maciErrType::ComponentConfigurationNotFoundEx &_ex)
	    {
	    maciErrType::ComponentConfigurationNotFoundExImpl ex(_ex);
	    ex.log();
	    return ERROR;
	    }
	catch( CORBA::SystemException &_ex )
	    {
	    ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__,
								"ProcessGetComponent");
	    corbaProblemEx.setMinor(_ex.minor());
	    corbaProblemEx.setCompletionStatus(_ex.completed());
	    corbaProblemEx.setInfo(_ex._info().c_str());
	    corbaProblemEx.log();
	    return ERROR;
	    }
	catch(...)
	    {
	    ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__, 
							    "ProcessGetComponent");
	    uex.log();
	    return ERROR;
	    }//try-catch
        }
      else if (g_TestClasses.find (requestor, mtc) == 0)
        {
	try
	    {
	    mtc->get_component (curl.c_str(), activate);
	    }
	catch( CORBA::SystemException &_ex )
	    {
	    ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__,
								"ProcessGetComponent");
	    corbaProblemEx.setMinor(_ex.minor());
	    corbaProblemEx.setCompletionStatus(_ex.completed());
	    corbaProblemEx.setInfo(_ex._info().c_str());
	    corbaProblemEx.log();
	    return ERROR;
	    }
        }
      else
        {
	ACS_SHORT_LOG ((LM_INFO,
			"Unknown requesting object '%s'.",
			requestor.c_str()));
	return ERROR;
        }
      
      ACS_SHORT_LOG ((LM_INFO,
                      "Activating component '%s'",
                      curl.c_str()));
    }

  MaciTestClass_ptr componentMTC = MaciTestClass::_narrow (component);
  ACE_CHECK_RETURN (ERROR);

  if ((componentMTC != NULL) && (g_TestClasses.bind (curl, componentMTC) != 0))
    {
      ACS_SHORT_LOG ((LM_ERROR,
                      "Unable to save component info for '%s'",
                      curl.c_str ()));
    }

  return SUCCESS;
}//ProcessGetComponent

int ProcessGet_Object(int argc, const ACE_TCHAR *argv[])
{
/// @todo this is test for old deprecated get_object which should be removed when we remove get_object
  ACE_CString requestor = argv[1];
  ACE_CString curl = argv[3];
  bool activate = (argv[2][0] == '1') ? true : false;

  ACS_SHORT_LOG((LM_INFO,
                 "Getting component (using get_object) '%s' requested by '%s' (activation: %d)",
                 curl.c_str(), requestor.c_str (), int(activate)));

  CORBA::Object_ptr component = NULL;
  if (requestor == "SimpleClient")
    {
      char *domain, *componentName;
      if (resolveCURL (curl.c_str (), domain, componentName) == -1)
        return ERROR;
      try
	  {
	  component = g_Client->get_object(componentName,
					     (((domain != 0) && strlen(domain) == 0) ? NULL : domain),
					     activate);
	  if (domain != NULL) delete domain;
	  if (componentName != NULL) delete componentName;
	  }
      catch(maciErrType::CannotGetComponentExImpl &_ex)
	  {
	  _ex.log();
	  if (domain != NULL) delete domain;
	  if (componentName != NULL) delete componentName;
	  return ERROR;
	  }//try-catch
    }
  else
    {
      ClientInfo ci;
      MaciTestClass_ptr mtc;

      if (g_Clients.find (requestor, ci) == 0)
        {
	try
	    {
	    component = g_Client->manager ()->get_component (ci.h, curl.c_str(), activate);
	    }
	catch(maciErrType::CannotGetComponentEx &_ex)
	    {
	    maciErrType::CannotGetComponentExImpl ex(_ex); 
	    ex.log();
	    return ERROR;
	    }
	catch(maciErrType::ComponentNotAlreadyActivatedEx &_ex)
	    {
	    maciErrType::ComponentNotAlreadyActivatedExImpl ex(_ex);
	    ex.log();
	    return ERROR;
	    }
	catch(maciErrType::ComponentConfigurationNotFoundEx &_ex)
	    {
	    maciErrType::ComponentConfigurationNotFoundExImpl ex(_ex);
	    ex.log();
	    return ERROR;
	    }
	catch( CORBA::SystemException &_ex )
	    {
	    ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__,
								"ProcessGetComponent");
	    corbaProblemEx.setMinor(_ex.minor());
	    corbaProblemEx.setCompletionStatus(_ex.completed());
	    corbaProblemEx.setInfo(_ex._info().c_str());
	    corbaProblemEx.log();
	    return ERROR;
	    }
	catch(...)
	    {
	    ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__, 
							    "ProcessGetComponent");
	    uex.log();
	    return ERROR;
	    }//try-catch
        }
      else if (g_TestClasses.find (requestor, mtc) == 0)
        {
	try
	    {
	    mtc->get_component (curl.c_str(), activate);
	    }
	catch( CORBA::SystemException &_ex )
	    {
	    ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__,
								"ProcessGetComponent");
	    corbaProblemEx.setMinor(_ex.minor());
	    corbaProblemEx.setCompletionStatus(_ex.completed());
	    corbaProblemEx.setInfo(_ex._info().c_str());
	    corbaProblemEx.log();
	    return ERROR;
	    }
        }
      else
        {
	ACS_SHORT_LOG ((LM_INFO,
			"Unknown requesting object '%s'.",
			requestor.c_str()));
	return ERROR;
        }
      
      ACS_SHORT_LOG ((LM_INFO,
                      "Activating component '%s'",
                      curl.c_str()));
    }

  MaciTestClass_ptr componentMTC = MaciTestClass::_narrow (component);
  ACE_CHECK_RETURN (ERROR);

  if ((componentMTC != NULL) && (g_TestClasses.bind (curl, componentMTC) != 0))
    {
      ACS_SHORT_LOG ((LM_ERROR,
                      "Unable to save component info for '%s'",
                      curl.c_str ()));
    }

  return SUCCESS;
}//ProcessGet_Object

int ProcessGetComponentInfo(int argc, const ACE_TCHAR *argv[]
		      )
{
  ACE_CString requestor = argv[1];
  ACE_CString name_wc = argv[2];
  ACE_CString type_wc = argv[3];
  bool active_only = (argv[4][0] == '1') ? true : false;

  ACS_SHORT_LOG((LM_INFO,
                 "Getting components' info of '%s' of type %s', active only: %d, requested by '%s'",
                 name_wc.c_str(), type_wc.c_str(), int(active_only), requestor.c_str ()));

  Handle h = 0;
  if (requestor == "SimpleClient")
    h = g_Client->handle ();
  else
    {
      ClientInfo ci;
      if (g_Clients.find (requestor, ci) == -1)
        {
          ACS_SHORT_LOG ((LM_ERROR, "Unknown requestor!"));
          return ERROR;
        }
      h = ci.h;
    }


  ComponentInfoSeq_var cis;
  HandleSeq_var seq = new HandleSeq(0);
  seq->length (0);
  cis = g_Client->manager ()->get_component_info (h, seq.in(), name_wc.c_str(), type_wc.c_str(), active_only);
  ACE_CHECK_RETURN (ERROR);

  ACS_SHORT_LOG((LM_INFO,
		 "%d components returned:",
		 cis->length ()));

  for (CORBA::ULong i = 0; i < cis->length (); ++i)
      ACS_SHORT_LOG((LM_INFO,
		     "\t '%s' of type '%s'",
		     cis[i].name.in(), cis[i].type.in()));


  return SUCCESS;
}

int ProcessGetClientInfo(int argc, const ACE_TCHAR *argv[]
			 )
{
  ACE_CString requestor = argv[1];
  ACE_CString name_wc = argv[2];

  ACS_SHORT_LOG((LM_INFO,
                 "Getting clients' info of '%s', requested by '%s'",
                 name_wc.c_str(), requestor.c_str ()));

  Handle h = 0;
  if (requestor == "SimpleClient")
    h = g_Client->handle ();
  else
    {
      ClientInfo ci;
      if (g_Clients.find (requestor, ci) == -1)
        {
          ACS_SHORT_LOG ((LM_ERROR, "Unknown requestor!"));
          return ERROR;
        }
      h = ci.h;
    }


  ClientInfoSeq_var cis;
  HandleSeq_var seq = new HandleSeq(0);
  seq->length (0);
  cis = g_Client->manager ()->get_client_info (h, seq.in(), name_wc.c_str());
  ACE_CHECK_RETURN (ERROR);

  ACS_SHORT_LOG((LM_INFO,
		 "%d clients returned:",
		 cis->length ()));

  for (CORBA::ULong i = 0; i < cis->length (); ++i)
      ACS_SHORT_LOG((LM_INFO,
		     "\t '%s'",
		     cis[i].name.in()));


  return SUCCESS;
}

int ProcessGetContainerInfo(int argc, const ACE_TCHAR *argv[]
			    )
{
  ACE_CString requestor = argv[1];
  ACE_CString name_wc = argv[2];

  ACS_SHORT_LOG((LM_INFO,
                 "Getting containers' info of '%s', requested by '%s'",
                 name_wc.c_str(), requestor.c_str ()));

  Handle h = 0;
  if (requestor == "SimpleClient")
    h = g_Client->handle ();
  else
    {
      ClientInfo ci;
      if (g_Clients.find (requestor, ci) == -1)
        {
          ACS_SHORT_LOG ((LM_ERROR, "Unknown requestor!"));
          return ERROR;
        }
      h = ci.h;
    }


  ContainerInfoSeq_var ais;
  HandleSeq_var seq = new HandleSeq(0);
  seq->length (0);
  ais = g_Client->manager ()->get_container_info (h, seq.in(), name_wc.c_str());
  ACE_CHECK_RETURN (ERROR);

  ACS_SHORT_LOG((LM_INFO,
		 "%d containers returned:",
		 ais->length ()));

  for (CORBA::ULong i = 0; i < ais->length (); ++i)
      ACS_SHORT_LOG((LM_INFO,
		     "\t '%s'",
		     ais[i].name.in()));


  return SUCCESS;
}


int ProcessReleaseComponent(int argc, const ACE_TCHAR *argv[]
                      )
{
  ACE_CString requestor (argv[1]);
  ACE_CString curl (argv[2]);

  ACS_SHORT_LOG((LM_INFO,
                 "Releasing component '%s' requested by '%s'",
                 curl.c_str(), requestor.c_str ()));

  Handle h = 0;
  if (requestor == "SimpleClient")
    h = g_Client->handle ();
  else
    {
      ClientInfo ci;
      if (g_Clients.find (requestor, ci) == -1)
        {
          ACS_SHORT_LOG ((LM_ERROR, "Unknown requestor!"));
          return ERROR;
        }
      h = ci.h;
    }

  try
      {
      g_Client->releaseComponent (GetCURL(curl).c_str());
      }
  catch(maciErrType::CannotReleaseComponentExImpl &_ex)
      {
      _ex.log();
      return ERROR;
      }

  MaciTestClass_ptr mtp;
  g_TestClasses.unbind (argv[1], mtp);
  //@@: What to do here?
  //CORBA::release (mtp);

  return SUCCESS;
}


//int ProcessReleaseComponents(int argc, const ACE_TCHAR *argv[]
//                       )
//{
//  ACE_CString requestor (argv[1]);
//
//  ACS_SHORT_LOG((LM_INFO,
//                 "Releasing components in the name of '%s'",
//                 requestor.c_str ()));
//
//  Handle h = 0;
//  if (requestor == "SimpleClient")
//    h = g_Client->handle ();
//  else
//    {
//      ClientInfo ci;
//      if (g_Clients.find (requestor, ci) == 0)
//        h = ci.h;
//    }
//
//  CURLSeq_var curls = new CURLSeq(argc-2);;
//  //curls->length (argc-2);
//
//  for (int i = 0; i < argc-2; ++i)
//    curls[(unsigned int)i] = CORBA::string_dup (GetCURL(argv[i+2]).c_str());
// 
//
//  g_Client->manager ()->release_components (h, curls.in ());
//  ACE_CHECK_RETURN (ERROR);
//
//  for (int i = 0; i < argc-2; ++i)
//    {
//      MaciTestClass_ptr mtp;
//      g_TestClasses.unbind (argv[i+2], mtp);
//      //@@: What to do?
//      //CORBA::release (mtp);
//    }
//
//  return SUCCESS;
//}

int ProcessInit(int argc, const ACE_TCHAR *argv[]
                )
{
  ACS_SHORT_LOG((LM_INFO,
                 "Initializing SimpleClient with these parameters:"));
  for(int i = 0; i < argc; ++i)
    ACS_SHORT_LOG((LM_INFO, "  %d: '%s'", i, argv[i]));

  if (g_Client == 0)
    g_Client = new SimpleClient();
  else
    {
      ACS_SHORT_LOG((LM_ERROR, "Command '%s' must be called exactly once!",
                     argv[0]));
      return FATAL;
    }

  g_Client->init (argc, (ACE_TCHAR**)argv);

  CORBA::String_var domain = g_Client->manager()->domain_name();
  ACE_CHECK_RETURN(FATAL);

  if (domain.in())
      ACS_SHORT_LOG((LM_INFO, "Domain: '%s'.", domain.in()));

  return (g_Client->login() == 0) ? FATAL : SUCCESS;
}

int ProcessLogout(int argc, const ACE_TCHAR *argv[]
                  )
{
  ACS_SHORT_LOG((LM_INFO, "Logging out '%s'", argv[1]));

  ClientInfo ci;

  if (g_Clients.find (argv[1], ci) == -1)
    {
      //ACS_SHORT_LOG ((LM_ERROR, "Unknown client '%s'!", argv[1]));
      //return ERROR;
      ACS_SHORT_LOG ((LM_DEBUG, "Unknown client '%s', nasty logout test!", argv[1]));
      ci.h = rand();
    }

  try
    {
      g_Client->manager ()->logout (ci.h);
      
    }
  catch (maciErrType::NoPermissionEx &no_permission)
    {
      ACS_LOG(LM_RUNTIME_CONTEXT, "ProcessLogout",
	      (LM_ERROR, "No permission exception caught."));
    }
  catch (CORBA::Exception &_ex )
    { 
      throw;
    }

  g_Clients.unbind (argv[1]);

  return SUCCESS;
}

int ProcessTestClient(int argc, const ACE_TCHAR *argv[]
                      )

{
  TEST_INIT("MACIHelperTest::extractHostnameFromCorbaloc");

#define TEST(corbaloc, result) \
  ASSERT_EQUALS_STR(MACIHelper::extractHostnameFromCorbaloc(corbaloc).c_str(),\
                    result);

  TEST("", "");
  TEST("Darth Wader", "");
  TEST("dina.ijs.si:3000/Manager", "");
  TEST("corbaloc::", "");
  TEST("corbaloc:::", "");
  TEST("corbaloc::dina.ijs.si/", "dina.ijs.si");
  TEST("corbaloc::dina.ijs.si:3000", "dina.ijs.si");
  TEST("corbaloc::dina.ijs.si:3000/", "dina.ijs.si");
  TEST("corbaloc::dina.ijs.si/Manager", "dina.ijs.si");
  TEST("corbaloc::dina.ijs.si:/Manager", "dina.ijs.si");

#undef TEST

  /*
   * The following series of tests actually tests the resolveCURL, defined
   * above in this module. However, that function is essentially a copy-paste
   * from the maciManagerImpl.cpp. When the original changes, the function
   * above should be updated accordingly for this test to remain meaningfull.
   */
#define TEST(curl, expectRet, expectDomain, expectComponent) \
  { \
    char *domain, *component; \
    int rv = resolveCURL (curl, domain, component); \
    ASSERT_EQUALS_INT (rv, expectRet); \
    if (rv != -1) \
      { \
        ASSERT_EQUALS_STR (domain, expectDomain); \
        ASSERT_EQUALS_STR (component, expectComponent); \
        if (domain != NULL) delete domain; \
        if (component != NULL) delete component; \
      } \
  }

  //TEST ("curl:", 0, 0, 0);
  //TEST ("curl://", -1, 0, 0);
  TEST ("curl://domain/component", 0, "domain", "component");
  //TEST ("curl:component", 0, 0, 0);
  TEST ("curl://component", 0, 0, "component");
  TEST ("component", 0, 0, "component");

#undef TEST

  /* Tests for MACIHelper::getManagerHostName... not tested with CDB */
  {
  //  unsetenv ("MANAGER_REFERENCE");

    const char* hostname = ACSPorts::getIP();

    ACE_TCHAR *args1[] = { "executable", "someparam", "-m" };
    ASSERT_EQUALS_STR (MACIHelper::getManagerHostname(3, args1).c_str(),
                       hostname);

    ACE_TCHAR *args2[] = { "executable", "someparam", "-m", "corbaloc::the.host.rom:3000/ManagerReference" };
    ASSERT_EQUALS_STR (MACIHelper::getManagerHostname(4, args2).c_str(),
                       "the.host.rom");

    ACE_TCHAR *args3[] = { "executable", "someparam", "-managerReference", "corbaloc::the.host.rom:3000/ManagerReference" };
    ASSERT_EQUALS_STR (MACIHelper::getManagerHostname(4, args3).c_str(),
                       "the.host.rom");

    ACE_OS::putenv ("MANAGER_REFERENCE=corbaloc::the.host.rom:3000/ManagerReference");
    ASSERT_EQUALS_STR (MACIHelper::getManagerHostname(0, 0).c_str(),
                       "the.host.rom");

//    unsetenv ("MANAGER_REFERENCE");
  }

  /* Tests for MACIHelper::resolveManager... not tested with CDB */
  {
    Manager_var mgr1 = MACIHelper::resolveManager(g_Client->getORB(), 0, 0, 2, 1);
    if (CORBA::is_nil (mgr1.ptr())) {
      ACS_SHORT_LOG ((LM_INFO, "MACIHelper::resolveManager returned nil"));
    } else {
      ACS_SHORT_LOG ((LM_INFO, "MACIHelper::resolveManager returned non-nil"));
    }

    Manager_var mgr2 = MACIHelper::resolveManager(g_Client->getORB(), "", 2, 1);
    if (CORBA::is_nil (mgr2.ptr())) {
      ACS_SHORT_LOG ((LM_INFO, "MACIHelper::resolveManager returned nil"));
    } else {
      ACS_SHORT_LOG ((LM_INFO, "MACIHelper::resolveManager returned non-nil"));
    }
  }

  /* Tests for MACIHelper::resolveNameService */
  {
    CosNaming::NamingContext_var ns =
        MACIHelper::resolveNameService(g_Client->getORB(), 2, 1);
    if (CORBA::is_nil (ns.ptr())) {
      ACS_SHORT_LOG ((LM_INFO, "MACIHelper::resolveNameService returned nil"));
    } else {
      ACS_SHORT_LOG ((LM_INFO, "MACIHelper::resolveNameService returned non-nil"));
    }
  }

  TEST_DONE;

  return (nFailures == 0) ? SUCCESS : ERROR;
}

int ProcessTestServer(int argc, const ACE_TCHAR *argv[]
                      )
{
  MaciTestClass_ptr mtc;
  ACE_CString requestor (argv[1]);

  if (g_TestClasses.find (requestor, mtc) != 0)
    {
      ACS_SHORT_LOG ((LM_INFO,
                      "Unknown requesting object '%s'.",
                      requestor.c_str()));
      return ERROR;
    }

  CORBA::Boolean rv = mtc->test ();
  ACE_CHECK_RETURN (ERROR);

  if (rv == FALSE)
    return ERROR;

  return SUCCESS;
}

int ProcessShutdown(int argc, const ACE_TCHAR *argv[]
                 )
{
  ACE_CString requestor(argv[1]);
  int action = atoi(argv[3]);

  ClientInfo ci;

  if (g_Clients.find (requestor, ci) != 0)
    {
      ACS_SHORT_LOG ((LM_INFO,
                      "Unknown requesting object '%s'.",
                      requestor.c_str()));
      return ERROR;
    }

  const char *container = NULL;
  container = argv[2];
  if (strlen (container) > 0)
    {
      ACS_SHORT_LOG((LM_INFO,
                     "Shutting down Container '%s'...", container));
      ContainerInfoSeq_var ais;
      HandleSeq_var seq = new HandleSeq(0);
      seq->length (0);
      ais = g_Client->manager ()->get_container_info (ci.h, seq.in(), container);
      ACE_CHECK_RETURN (ERROR);

      for (CORBA::ULong i = 0; i < ais->length (); ++i)
        ais[i].reference->shutdown (action);
    }
  else
    {
      ACS_SHORT_LOG((LM_INFO, "Shutting down the Manager..."));
      g_Client->manager ()->shutdown (ci.h, action);
      ACE_CHECK_RETURN (ERROR);
    }
  return SUCCESS;
}

int ProcessSleep(int argc, const ACE_TCHAR *argv[]
                 )

{
  int bDontListen = 0;
  if (argc >= 2 && argv[2][0] == '1')
    bDontListen = 1;

  ACS_SHORT_LOG((LM_INFO, "Sleeping for %s milliseconds (%d)...",
                 argv[1], bDontListen));
  ACE_Time_Value tv(0, atoi(argv[1])*1000);
  if (g_Client != NULL && !bDontListen)
    g_Client->run(tv);
  else
    ACE_OS::sleep(tv);
  return SUCCESS;
}

int ProcessReset(int argc, const ACE_TCHAR *argv[]
                 )
{
  ACS_SHORT_LOG ((LM_INFO, "Resetting..."));
  int argc2;
  const char *argv2[2];

  ClientMap::ITERATOR iClient = g_Clients.begin ();
  while (iClient != g_Clients.end ())
    {
      argv2[0] = "logout";
      argv2[1] = (*iClient).ext_id_.c_str ();
      argc2 = 1;

      ProcessLogout (argc2, argv2);
      ACE_CHECK_RETURN (ERROR);

      iClient = g_Clients.begin ();
    }

  if (g_Client != NULL)
    {
      g_Client->logout ();

      // a little hack to manager 
      // (this clas should implement administrator interface, etc..)
/*      sleep(2);
      ACS_SHORT_LOG((LM_INFO,"Calling Manager::shutdown()"));
      g_Client->manager()->shutdown(0x05000000, 2 << 8);
      ACE_CHECK_RETURN(ERROR);
*/    ACS_SHORT_LOG((LM_INFO,"Done."));
      sleep(10);

      //delete g_Client;
      g_Client = NULL;
    }

  return SUCCESS;
}

int ProcessHelp(int argc, const ACE_TCHAR *argv[]
                );

CommandDef g_Commands[] =
  {
    {
      "init",
      "<arg1>:<arg2>:...",
      "Initialize the test SimpleClient by setting up the ORB and the POA. "
      "The arguments are used in the construction of the ORB. This must "
      "be the first command, and it s appear exactly once.",
      ProcessInit
    },
    {
      "help",
      "",
      "Display usage information.",
      ProcessHelp
    },
    {
      "logout",
      "<name>",
      "Logout the given object.",
      ProcessLogout
    },
    {
      "client",
      "<name>[:<onPing>]",
      "Create a dummy client which only logs all received calls. <onPing>"
      " determines client's behavior when it receives a ping() call from"
      " the manager. If it is 0, the client returns true (all ok). If it"
      " is 1, the client returns false (malfunction). If it is 2, the"
      " client throws a CORBA::TRANSIENT (simulation of a network error).",
      ProcessClient
    },
    {
      "administrator",
      "<name>",
      "Login an administrator client.",
      ProcessAdministrator
    },
    {
      "getComponent",
      "<requestor>:<activate>:<name>",
      "Get a component from the given domain with the given name. The <object>"
      " argument specifies the name of the client on whose behalf the component"
      " is acquired. If <object> is 'SimpleClient', the Simple Client"
      " constructed with the 'init' command is used. Set <activate> to 1 in"
      " order to have the Manager activate the component if it doesn't exist yet."
      " <name> is the CURL of the component, as passed to the Manager.",
      ProcessGetComponent
    },
    {
      "get_object",
      "<requestor>:<activate>:<name>",
      "This is equivalent of getComponent,"
      " but is uses old deprecated get_object instead getComponent."
      "Get a component from the given domain with the given name. The <object>"
      " argument specifies the name of the client on whose behalf the component"
      " is acquired. If <object> is 'SimpleClient', the Simple Client"
      " constructed with the 'init' command is used. Set <activate> to 1 in"
      " order to have the Manager activate the component if it doesn't exist yet."
      " <name> is the CURL of the component, as passed to the Manager.",
      ProcessGet_Object
    },
    {
      "releaseComponent",
      "<requestor>:<name>",
      "Release a component.",
      ProcessReleaseComponent
    },
/*    {
      "releaseComponents",
      "<requestor>:<name1>:<name2>:...",
      "Release several components in one call to the manager.",
      ProcessReleaseComponents
    },
*/    {
      "getComponentInfo",
      "<requestor>:<Component name wilcard>:<Component type wildcard>:<active only>",
      "Returns components' info from the manager.",
      ProcessGetComponentInfo
    },
    {
      "getClientInfo",
      "<requestor>:<client name wilcard>",
      "Returns clients' info from the manager.",
      ProcessGetClientInfo
    },
    {
      "getContainerInfo",
      "<requestor>:<container name wilcard>",
      "Returns containers' info from the manager.",
      ProcessGetContainerInfo
    },
    {
      "testClient",
      "<name>",
      "Perform client-side testing of the MACI through the given client.",
      ProcessTestClient
    },
    {
      "testServer",
      "<name>",
      "Perform server-side testing of the MACI through the given component.",
      ProcessTestServer
    },
    {
      "sleep",
      "<millis>[:<dont-listen>]",
      "Sleep for the given period of time. If <dont-listen> is 1, the client"
      " will not accept any incoming CORBA calls (default is 0).",
      ProcessSleep
    },
    {
      "reset",
      "",
      "Reset the client by releasing all components and logging out all clients.",
      ProcessReset
    },
    {
      "shutdown",
      "<requestor>:<container>:<action>",
      "Shutdown the given Container. If no Container name is given, the"
      " Manager is shut down. If <action> is 1 when shutting down the"
      " Manager, all Containers are shutdown too. When shutting down"
      " an Container, an <action> of 0 will reload the container,"
      " 256 will reboot it, 512 will exit. Bits 0-7 are the return"
      " value of the Container.",
      ProcessShutdown
    },
    {
      /* Terminator */
      NULL,
      NULL,
      NULL
    }
  };

int ProcessCommand(const ACE_TCHAR *cmd)
{
  
  try
    {

      /*
       * Split the command string into parts, separated by colons.
       */
      ACE_TCHAR *parts[MAX_COMMAND_PARTS];
      ACE_TCHAR cmdCopy[MAX_COMMAND_LENGTH];
      strcpy(cmdCopy, cmd);
      int i = 1;
      parts[0] = cmdCopy;
      parts[1] = cmdCopy;
      while(*(parts[i]) != 0)
        {
          if(*(parts[i]) == ':')
            {
              *(parts[i]) = 0;
              parts[i+1] = ++parts[i];
              ++i;
            }
          ++parts[i];
          if(i >= MAX_COMMAND_PARTS)
            {
              ACS_SHORT_LOG((LM_ERROR,
                             "Command '%s' has too many parameters. "
                             "A command can have at most %d parameters.",
                             parts[0], MAX_COMMAND_PARTS));
              return FATAL;
            }
        }

      CommandDef *c = g_Commands;
      while (c->command != NULL)
        {
          if (strcmp(parts[0], c->command) == 0)
            {
              int rv = c->processor(i, (const ACE_TCHAR**)parts
                                    );
              
              return rv;
            }
          ++c;
        }
      ACS_SHORT_LOG((LM_ERROR, "Invalid command: '%s'", parts[0]));
    }
  catch( CORBA::Exception &ex )
    {
      ACE_PRINT_EXCEPTION (ex, "ProcessCommand");
    }

  return FATAL;
}

int ProcessHelp(int argc, const ACE_TCHAR *argv[]
                )

{
  ACE_TCHAR buf[16*1024];
  ACE_CString usage("\n");
  sprintf(buf, "Usage: %s [command] [@script]\n  ", argv[0]);
  usage += buf;
  usage += Justify(buf,
                   "A script is a file that contains several commands. "
                   "There must be exactly one command per line of the "
                   "script file. Command can be one of these:\n\n");
  CommandDef *cmd = g_Commands;
  while (cmd->command != NULL)
    {
      usage += "---- ";
      usage += cmd->command;
      if (strlen(cmd->params) > 0)
        {
          usage += ":";
          usage += cmd->params;
        }
      usage += "\n";
      usage += Justify(buf, cmd->description);
      usage += "\n\n";
      ++cmd;
    }

  ACS_SHORT_LOG((LM_INFO, usage.c_str()));

  return 1;
}

int main (int argc, char **argv)
{
  
  try
    {
      if (argc < 2)
        {
          ProcessHelp(argc, (const ACE_TCHAR**)argv);
          

          return 1;
        }

      int i, rv;
      for (i = 1; i < argc; ++i)
        {
          if(argv[i][0] == '@')
            {
              // read the script file, line by line
              FILE *f = ACE_OS::fopen(argv[i]+1, "r");
	      if (!f) return 2;
              int lineNo = 0;
              ACE_TCHAR buf[MAX_COMMAND_LENGTH];
              while(ACE_OS::fgets(buf, sizeof(buf), f) != NULL)
                {
                  ++lineNo;
                  if(strlen(buf) >= MAX_COMMAND_LENGTH)
                    {
                      ACS_SHORT_LOG((LM_ERROR,
                        "Error in script file '%s', line %d: the line is too long."
                        " Maximum line length is %d characters.",
                        argv[i]+1, lineNo, MAX_COMMAND_LENGTH));
                    }

                  ACE_TCHAR *line = buf;
                  // trim the line on the left and on the right
                  int len = strlen(line);
                  while(isspace(*line))
                    {
                      ++line;
                      --len;
                    }
                  while(len > 0 && isspace(line[len-1]))
                    {
                      --len;
                      line[len] = 0;
                    }
                  // if the line is not empty and it isn't a comment, process it
                  if(len > 0 && line[0] != '#')
                    if ((rv = ProcessCommand(line)) != SUCCESS)
                      {
                        ACS_SHORT_LOG ((LM_ERROR,
                                        "Processing of command FAILED!"));
                        if (rv == FATAL)
                          break;
                      }
                }
              ACE_OS::fclose(f);
            }
          else
            if ((rv = ProcessCommand(argv[i])) != SUCCESS)
              {
                ACS_SHORT_LOG ((LM_ERROR,
                                "Processing of command FAILED!"));
                if (rv == FATAL)
                  break;
              }
        }
    }
  catch ( CORBA::Exception &ex )
    {
      ACE_PRINT_EXCEPTION(ex, "main");
    }

  ACS_SHORT_LOG ((LM_INFO, "Exiting maciTestClient..."));
  return 0;

} /* end main() */






