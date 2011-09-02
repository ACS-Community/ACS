/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2005 
*
*This library is free software; you can redistribute it and/or
*modify it under the terms of the GNU Lesser General Public
*License as published by the Free Software Foundation; either
*version 2.1 of the License, or (at your option) any later version.
*
*This library is distributed in the hope that it will be useful,
*but WITHOUT ANY WARRANTY; without even the implied warranty of
*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*Lesser General Public License for more details.
*
*You should have received a copy of the GNU Lesser General Public
*License along with this library; if not, write to the Free Software
*Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: maciContainerThreadHook.cpp,v 1.7 2011/09/02 11:00:19 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2005-01-03  created 
*/

#include "vltPort.h"
#include "maciContainerImpl.h"
#include "maciContainerThreadHook.h"

static const char* rcsId="@(#) $Id: maciContainerThreadHook.cpp,v 1.7 2011/09/02 11:00:19 bjeram Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

using namespace maci;

void ContainerThreadHook::initThread(void* arg)
{
    ACE_UNUSED_ARG(arg);
  
    // if there is already the callback we just return
    if (ACE_LOG_MSG->msg_callback () != 0)
	{
	ACS_LOG(LM_RUNTIME_CONTEXT, "maci::ContainerThreadHook::initThread", (LM_DEBUG, "Log callback already set!"));
	return;
	}
    
    LoggingProxy *logger = maci::ContainerImpl::getLoggerProxy();
    if (logger!=0)
	{
	char *contName = ContainerImpl::getContainer()->name();
	// in some threads the logging will be initialized two times what is not harmful
	LoggingProxy::init(logger);
	LoggingProxy::ProcessName(contName);
	CORBA::string_free(contName);
	/*TBD: so far we can not check if we are dealing with baci thread
	// check if it is baci thread
	baci::BACIThreadParameter *baciThreadParam = dynamic_cast<baci::BACIThreadParameter*>(arg);
	if (baciThreadParam != 0)
	{
	LoggingProxy::ThreadName( baciThreadParam->getBACIThread()->getName().c_str() );
	}
	else
	*/
	{
	char buf[64];
	sprintf(buf, "ID=%lu", (unsigned long)ACE_OS::thr_self());
	LoggingProxy::ThreadName(buf);
	}
	ACS_LOG(LM_RUNTIME_CONTEXT, "maci::ContainerThreadHook::initThread", (LM_DEBUG, "ContainerThreadHook initiailized"));
	}
    else
	{
	// since we can not intailize the logging callback we allow logging system to log to stderr
	LoggingProxy::Flags( ACE_Log_Msg::STDERR);
	ACS_LOG(LM_RUNTIME_CONTEXT, "maci::ContainerThreadHook::initThread", (LM_ERROR, "Failed to initiailized!"));
	}
    
    return;
}//start

/*___oOo___*/
