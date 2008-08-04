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
* "@(#) $Id: loggingHelper.cpp,v 1.45 2008/08/04 11:24:39 bjeram Exp $"
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



// ************************************************************************
//
// REVISION HISTORY:
//
// $Log: loggingHelper.cpp,v $
// Revision 1.45  2008/08/04 11:24:39  bjeram
// Replaced non portable comparing with _nil() with CORBA::is_nil(x).
// COMP-2596
//
// Revision 1.44  2007/05/23 08:59:11  bjeram
// solved resolveNameService that it does not use multicast for finding NameService + improved error hansling a bit
//
// Revision 1.43  2005/08/29 08:42:57  vwang
// fix problem about return str().c_str()
//
// Revision 1.42  2004/10/27 22:06:48  dfugate
// Removed CORBA::string_free's related to ACSPorts::getIP as these are no longer needed.
//
// Revision 1.41  2004/10/14 20:46:18  gchiozzi
// Cleaned up logging messages:
// - uses now only ACS_* macros and not any more ACE_* macros
// - Removed all new line characters
// - fixed problem with ACS logging that was not putting proper new line
//   characters when logging messages on stdout BEFORE the logging itself
//   was initialised.
//
// Revision 1.40  2004/03/25 22:20:09  dfugate
// Use ACSPorts::getIP instead of ACE_OS::hostname function.
//
// Revision 1.39  2004/03/17 07:38:33  bjeram
// ported to ACE 5.4 and TAO 1.4
//
// Revision 1.38  2003/10/24 19:27:07  dfugate
// Fixed a few serious bugs and now use native exceptions.
//
// Revision 1.37  2003/10/23 07:39:08  acaproni
// True native exception handling. No more extra parameters
//
// Revision 1.36  2003/10/15 22:19:08  dfugate
// Use ACSPorts class to gain access to dynamic port numbers of CORBA processes.
//
// Revision 1.35  2003/05/23 09:18:47  msekoran
// Replaced exponential backoff with 3s retries.
//
// Revision 1.34  2003/03/14 10:24:49  rgeorgie
// LGPL
//
// Revision 1.33  2002/09/23 12:45:10  vltsccm
// msekoran: loggingXMLParser fixed, memory leak removed and tat test added.
//
// Revision 1.32  2002/04/10 14:41:42  vltsccm
// logging1.32
//
// Revision 1.31  2002/03/27 16:44:27  vltsccm
// logging1.31
//
// Revision 1.30  2002/02/13 12:55:36  vltsccm
// logging1.30
//
// Revision 1.29  2002/02/08 13:40:58  vltsccm
// logging1.29
//
// Revision 1.28  2002/02/05 17:51:58  vltsccm
// logging1.28
//
// Revision 1.27  2002/02/04 08:26:32  vltsccm
// logging1.27
//
// Revision 1.26  2002/01/18 09:43:02  vltsccm
// logging1.26
//
// Revision 1.25  2002/01/16 10:41:32  vltsccm
// logging1.25
//
// Revision 1.24  2002/01/15 12:42:24  vltsccm
// logging1.24
//
// Revision 1.23  2002/01/14 21:10:53  vltsccm
// logging1.23
//
// Revision 1.22  2001/12/27 19:04:12  vltsccm
// logging1.22
//
// Revision 1.21  2001/12/24 13:32:03  vltsccm
// logging1.21
//
// Revision 1.20  2001/12/24 13:32:02  vltsccm
// logging1.20
//
// Revision 1.19  2001/12/24 13:32:02  vltsccm
// logging1.19
//
// Revision 1.18  2001/12/24 13:32:02  vltsccm
// logging1.18
//
// Revision 1.17  2001/12/24 13:32:01  vltsccm
// logging1.17
//
// Revision 1.16  2001/12/24 13:32:01  vltsccm
// logging1.16
//
// Revision 1.15  2001/12/24 13:32:01  vltsccm
// logging1.15
//
// Revision 1.14  2001/12/24 13:32:00  vltsccm
// logging1.14
//
// Revision 1.13  2001/12/24 13:32:00  vltsccm
// logging1.13
//
// Revision 1.12  2001/12/24 13:32:00  vltsccm
// logging1.12
//
// Revision 1.11  2001/12/24 13:31:59  vltsccm
// logging1.11
//
// Revision 1.10  2001/12/24 13:31:59  vltsccm
// logging1.10
//
// Revision 1.9  2001/12/24 13:31:59  vltsccm
// logging1.9
//
// Revision 1.8  2001/12/24 13:31:58  vltsccm
// logging1.8
//
// Revision 1.7  2001/12/24 13:31:58  vltsccm
// logging1.7
//
// Revision 1.6  2001/12/24 13:31:58  vltsccm
// logging1.6
//
// Revision 1.5  2001/12/24 13:31:58  vltsccm
// logging1.5
//
// Revision 1.4  2001/12/24 13:31:57  vltsccm
// logging1.4
//
// Revision 1.3  2001/12/24 13:31:57  vltsccm
// logging1.3
//
// Revision 1.2  2001/12/24 13:31:57  vltsccm
// logging1.2
//
// Revision 1.1  2001/12/24 13:31:56  vltsccm
// logging1.1
//
// Revision 1.0  2001/12/24 13:31:56  vltsccm
// logging1.0
//
//
// ************************************************************************







