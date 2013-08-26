/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciTestClassImpl.cpp,v 1.101 2011/06/07 23:56:38 javarias Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram 2002-11-28 added IDL:ALMA/MACI_TEST/ .... :1.0 to COB's type
* msekoran 2002-07-05 added hierachical COB test
* rlemke   2001-08-05 integrated with new MACI
* rlemke   2001-03-20 created from MaciTestClass
* gchiozzi 2001-02-15 Added real implementation for method descriptor()
* gchiozzi 2001-02-15 Added body of get_interface() method for Object Explorer
* gchiozzi 2001-02-15 created standard header
*/

#include <vltPort.h>

static char *rcsId="@(#) $Id: maciTestClassImpl.cpp,v 1.101 2011/06/07 23:56:38 javarias Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <logging.h>
#include <maciTestClassImpl.h>
#include <maciContainerImpl.h>
#include <maciContainerServices.h>

 using namespace maci;

/////////////////////////////////////////////////
// MaciTestClass
/////////////////////////////////////////////////


MaciTestClass::MaciTestClass(
    const ACE_CString& name,
    maci::ContainerServices* containerServices) :
    acscomponent::ACSComponentImpl(name,containerServices), 
  m_initialization(1), m_name(name)

{
  ACS_SHORT_LOG((LM_INFO,
                 "::MaciTestClass::MaciTestClass(%s)", m_name.c_str()));

  // simulate component failing initialization (e.g. w/o required configuration)
  if (ACE_OS::strcmp(name.c_str(), "MACI_FAKE") == 0)
      return;

  // Initialization successful
  m_initialization = 0;
 
}

MaciTestClass::~MaciTestClass()
{
  ACS_SHORT_LOG((LM_INFO,
                 "::MaciTestClass::~MaciTestClass(%s)", m_name.c_str()));
}


/* ----------------------------------------------------------------*/
/* --------------------- [ CORBA interface ] ----------------------*/
/* ----------------------------------------------------------------*/

CORBA::Boolean
MaciTestClass::test ()
{
  ACS_SHORT_LOG((LM_INFO, "MaciTestClass::test(%s)", m_name.c_str()));
  // do some tests here
  return true;
}

CORBA::Object_ptr
MaciTestClass::get_component (
    const char *cob_url,
    CORBA::Boolean activate
    )
{
  ACS_SHORT_LOG((LM_INFO, "MaciTestClass::get_component(%s,%s)",
                 m_name.c_str(), cob_url));
  return CORBA::Object::_nil();
}
CORBA::Long
MaciTestClass::release_component(
    const char *cob_url
    )
{
  ACS_SHORT_LOG((LM_INFO, "MaciTestClass::release_component(%s, %s)",
                 m_name.c_str(), cob_url));
  return -1;
}

void MaciTestClass::activate_internal_component ()
{
  testInternalComp = getContainerServices()->getComponent<MACI_TEST::MaciTestClass>("MACI_SUB");
}

void MaciTestClass::release_internal_component()
{
   //getContainerServices()->forceReleaseComponent("MACI_SUB");
}

/////////////////////////////////////////////////
// MaciHierarchicalTestClass
/////////////////////////////////////////////////

MaciHierarchicalTestClass::MaciHierarchicalTestClass(
    const ACE_CString& name, 
    maci::ContainerServices* containerServices) :
    acscomponent::ACSComponentImpl(name,containerServices), MaciTestClass(name, containerServices)

{
  // failure in super class
  if (m_initialization)
      return;

  // reset
  m_initialization = 1;

  ACS_SHORT_LOG((LM_INFO,
                 "::MaciHierarchicalTestClass::MaciHierarchicalTestClass(%s)", m_name.c_str()));

/*  CORBA::Object_var obj = ContainerImpl::getContainer()->get_object("MACI_SUB",0,true);

  if (CORBA::is_nil(obj.in()))
    {
      ACS_SHORT_LOG((LM_INFO,
		     "::MaciHierarchicalTestClass::MaciHierarchicalTestClass(%s) Failed to obtain SUBComponent reference.",
		     m_name.c_str()));
      return;
    }
*/
  // Initialization successful
  m_initialization = 0;
 
}

void MaciHierarchicalTestClass::execute() 
{

    MACI_TEST::MaciTestClass_var obj =
	getContainerServices()->getComponent<MACI_TEST::MaciTestClass>("MACI_SUB");

    if (CORBA::is_nil(obj.in()))
	{
	ACS_SHORT_LOG((LM_INFO,
		       "::MaciHierarchicalTestClass(%s)::execute Failed to obtain SUBComponent reference.",
		       m_name.c_str()));
	}

}


MaciHierarchicalTestClass::~MaciHierarchicalTestClass()
{
  // release component !!!
    if (m_initialization==0);

    ACS_SHORT_LOG((LM_INFO,
                 "::MaciTHierarchicalestClass::~MaciHierarchicalTestClass(%s)", m_name.c_str()));
}

/////////////////////////////////////////////////
// MaciTestConstructableClass
/////////////////////////////////////////////////
/*
MaciTestConstructableClass::MaciTestConstructableClass(PortableServer::POA_ptr poa,
						       const ACE_CString& name) 
    : m_poa(PortableServer::POA::_duplicate(poa)), m_name(name), m_initialization(1)
{
  ACS_SHORT_LOG((LM_INFO,
                 "::MaciTestConstructableClass::MaciTestConstructableClass(%s)",
                 m_name.c_str()));

  // Initialization successful
  m_initialization = 0;

}

MaciTestConstructableClass::~MaciTestConstructableClass()
{
  ACS_SHORT_LOG((LM_INFO,
                 "::MaciTestConstructableClass::~MaciTestConstructableClass(%s)",
                 m_name.c_str()));
}
*/
/* ----------------------------------------------------------------*/
/* --------------------- [ CORBA interface ] ----------------------*/
/* ----------------------------------------------------------------*/
/*
void
MaciTestConstructableClass::construct ()
{
  ACS_SHORT_LOG((LM_INFO, "MaciTestConstructableClass::construct"));
}

void
MaciTestConstructableClass::destruct ()
{
  ACS_SHORT_LOG((LM_INFO, "MaciTestConstructableClass::destruct"));
}

CORBA::Object_ptr
MaciTestConstructableClass::get_component (
    const char *cob_url,
    CORBA::Boolean activate,
    CORBA::ULong &status
{
  ACS_SHORT_LOG((LM_INFO, "MaciTestConstructableClass::get_component"));
//  return MaciTestClass::get_component(cob_url, activate, status);
  return CORBA::Object::_nil();
}

CORBA::Long
MaciTestConstructableClass::release_component(
    const char *cob_url
{
  ACS_SHORT_LOG((LM_INFO, "MaciTestConstructableClass::release_component"));
//  return MaciTestClass::release_component(cob_url);
  return 0;
}
*/
/////////////////////////////////////////////////
// MACI DLL support functions
/////////////////////////////////////////////////

ACS_DLL_UNMANGLED_EXPORT PortableServer::Servant
ConstructComponent(CORBA::ULong h,
             const char * name,
             const char * type,
             maci::ContainerServices * containerServices)
{
  ACS_SHORT_LOG((LM_INFO, "ConstructComponent(%08x, %s, %s)", h, name, type));

  if (strcmp(type, "IDL:alma/MACI_TEST/MaciTestClass:1.0") == 0)
    {
      MaciTestClass * mtc = new MaciTestClass(name,containerServices);
      if (!mtc || mtc->initialization())
	{
	  ACS_LOG(LM_RUNTIME_CONTEXT, "maciTestClassImpl::ConstructComponent",
		  (LM_ERROR, "Failed to create/initialize component: '%s'", name));
	  if (mtc)
	      mtc->_remove_ref();
	  throw ACSErrTypeCommon::CouldntCreateObjectExImpl(__FILE__, __LINE__, 
							    "maciTestClassImpl::ConstructComponent");
	}
      else
	  return mtc;
    }
  else if (strcmp(type, "IDL:alma/MACI_TEST/MaciHierarchicalTestClass:1.0") == 0)
    {
      MaciHierarchicalTestClass * mhtc = new MaciHierarchicalTestClass(name,containerServices);
      if (!mhtc || mhtc->initialization())
	{
	  ACS_LOG(LM_RUNTIME_CONTEXT, "maciTestClassImpl::ConstructComponent",
		  (LM_ERROR, "Failed to create/initialize component: '%s'", name));
	  if (mhtc)
	      mhtc->_remove_ref();
	  throw ACSErrTypeCommon::CouldntCreateObjectExImpl(__FILE__, __LINE__, 
							    "maciTestClassImpl::ConstructComponent");
	}
      else
	  return mhtc;
    }
  else
    {
      ACS_LOG(LM_RUNTIME_CONTEXT, "maciTestClassImpl::ConstructComponent",
              (LM_ERROR, "Unknown component type: '%s'", type));
      	  return 0;
	  throw ACSErrTypeCommon::CouldntCreateObjectExImpl(__FILE__, __LINE__, 
							    "maciTestClassImpl::ConstructComponent");
    }
}

ACS_DLL_UNMANGLED_EXPORT bool DLLOpen(int, char**)
{
  ACS_SHORT_LOG((LM_INFO, "maciTestClassImpl::DLLOpen"));
  return true;
}

ACS_DLL_UNMANGLED_EXPORT void DLLClose()
{
  ACS_SHORT_LOG((LM_INFO, "maciTestClassImpl::DLLClose"));
}

MaciTestOffShoot::MaciTestOffShoot()
{
}

MaciTestOffShoot::~MaciTestOffShoot()
{
}




