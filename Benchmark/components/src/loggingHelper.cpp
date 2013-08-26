/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration)
*    and Cosylab 2002, All rights reserved
*
*    This library is free software; you can redistribute it and/or
*    modify it under the terms of the GNU Lesser General Public
*    License as published by the Free Software Foundation; either
*    version 2.1 of the License, or (at your option) any later version.
*
*    This library is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*    Lesser General Public License for more details.
*
*    You should have received a copy of the GNU Lesser General Public
*    License along with this library; if not, write to the Free Software
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: loggingHelper.cpp,v 1.1 2008/10/22 02:47:49 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2003-05-23  removed exponential backoff
* msekoran  2001/12/23  created
*/

#include <vltPort.h>
#include <loggingHelper.h>

#include <ace/SString.h>

#include <acsutilPorts.h>
#include <tao/corba.h>

#include <logging.h>

#define LOG_NAMESERVICE_REFERENCE  "NAMESERVICE_REFERENCE"

bool LoggingHelper::m_terminate = false;

void
LoggingHelper::terminateResolving(bool terminate)
{
  m_terminate = terminate;
}

CosNaming::NamingContext_ptr
LoggingHelper::resolveNameService(CORBA::ORB_ptr orb,
			       int retries, unsigned int secTimeout)
{
    //ACS_TRACE("logging::LoggingHelper::resolveNameService");

  if (CORBA::is_nil(orb))
    return CosNaming::NamingContext::_nil();

  // use CORBA::ORB::resolve_intial_references

  try
    {
    // first we check if we have if we have initalize reference for NameService
    CORBA::ORB::ObjectIdList_ptr svcsList =
	orb->list_initial_services();
    // ... we can start with 12th if exsist, because from 0 to 11 are "hardcoded" SVCS
    for(unsigned int i=12; i<svcsList->length(); i++)
	{
	ACE_CString o ((*svcsList)[i]);
	if (o.find("NameService")!=ACE_CString::npos)
	    {
	    // printf("Svc %d = %s\n", i, o.c_str());
	    // we find NameService so we can use resolve_initial_references w/o using multicast for discovering NameService
	    CORBA::Object_var naming_obj =
		orb->resolve_initial_references ("NameService");


	    if (!CORBA::is_nil (naming_obj.in ()))
		{
		CosNaming::NamingContext_var naming_context =
		    CosNaming::NamingContext::_narrow (naming_obj.in ()
			);


		if (!CORBA::is_nil(naming_context.ptr()))
		    return naming_context._retn();
		}
	    }//if
	}//for

    }
  catch(CORBA::SystemException &ex)
      {
      ACE_PRINT_EXCEPTION(ex, "logging::LoggingHelper::resolveNameService");
      ACS_SHORT_LOG((LM_WARNING, "(logging::LoggingHelper::resolveNameService) CORBA exception caught! - will try to use other possiblities"));
      }
  catch(...)
    {
       ACS_SHORT_LOG((LM_WARNING, "(logging::LoggingHelper::resolveNameService) unknown exception caught! - will try to use other possiblities"));
    }

  //ACS_LOG(LM_RUNTIME_CONTEXT, "logging::LoggingHelper::resolveNameService",
  //  (LM_DEBUG,"Unable to resolve NameService reference using CORBA::ORB::resolve_intial_references()."));

  // Environment variable LOG_NAMESERVICE_REFERENCE
  ACE_TCHAR * envRef = ACE_OS::getenv (LOG_NAMESERVICE_REFERENCE);
  if (envRef && *envRef)
    {
    //ACS_LOG(0, "logging::LoggingHelper::resolveNameService",
    //      (LM_INFO, "NameService reference obtained via environment: '%s'", envRef));

      // return reference
      return resolveNameService(orb, envRef, retries, secTimeout);
    }


  // corbaloc::<hostname>:<port>/NameService
  const char* hostname = 0;
  hostname = ACSPorts::getIP();
  if (hostname==0)
    {
    //ACS_LOG(LM_RUNTIME_CONTEXT, "logging::LoggingHelper::resolveNameService",
    //      (LM_ERROR, "Failed to obtain localhost address!"));

      return CosNaming::NamingContext::_nil();
    }

  ACE_TCHAR corbalocRef[240];
  ACE_OS::sprintf(corbalocRef, "corbaloc::%s:%s/NameService", hostname, ACSPorts::getNamingServicePort().c_str());

  //ACS_LOG(0, "logging::LoggingHelper::resolveNameService",
  //  (LM_INFO, "NameService reference generated using localhost address: '%s'", corbalocRef));

  // return reference
  return resolveNameService(orb, corbalocRef, retries, secTimeout);

}

CosNaming::NamingContext_ptr
LoggingHelper::resolveNameService(CORBA::ORB_ptr orb,
			       const ACE_TCHAR * reference,
			       int retries, unsigned int secTimeout)
{
    //ACS_TRACE("logging::LoggingHelper::resolveNameService");

  if (!reference || CORBA::is_nil(orb))
    return CosNaming::NamingContext::_nil();

  //ACS_DEBUG_PARAM("logging::LoggingHelper::resolveNameService", "Resolving reference: '%s'", reference);

  unsigned int secsToWait = 3, secsWaited = 0;
  int retried = 0;
  CosNaming::NamingContext_var ref = CosNaming::NamingContext::_nil();

  while (!m_terminate)
    {
    try
    	{

	  CORBA::Object_var obj = orb->string_to_object(reference);


	  ref = CosNaming::NamingContext::_narrow(obj.in());


	  //ACS_DEBUG("logging::LoggingHelper::resolveNameService", "NameService reference narrowed.");

	  return ref._retn();
	}
    catch(...)
	{
	//ACE_PRINT_EXCEPTION(ACE_ANY_EXCEPTION, "logging::LoggingHelper::resolveNameService");
	  ref = CosNaming::NamingContext::_nil();
	}

      if (
	  ((secTimeout != 0) && (secsWaited >= secTimeout)) ||
	  ((retries > 0) && (retried >= retries))
	  )
	break;
      else
	ACE_OS::sleep(secsToWait);

      secsWaited += secsToWait;

      retried++;

    }

  return CosNaming::NamingContext::_nil();
}



