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
* "@(#) $Id: maciSimpleClientThreadHook.cpp,v 1.4 2011/09/02 11:00:19 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2005-01-03  created 
*/

#include "vltPort.h"
#include "maciSimpleClient.h"
#include "maciSimpleClientThreadHook.h"

static const char* rcsId="@(#) $Id: maciSimpleClientThreadHook.cpp,v 1.4 2011/09/02 11:00:19 bjeram Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

using namespace maci;


void SimpleClientThreadHook::initThread(void* arg)
{
    ACS_TRACE("maci::SimpleClientThreadHook::initThread");
    
    // if there is already the callback we just return
    if (ACE_LOG_MSG->msg_callback () != 0)
	return;
    
    LoggingProxy *logger = maci::SimpleClient::getLoggerProxy();
    if (logger!=0)
	{
	// in some threads the logging will be initialized two times what is not harmful
	LoggingProxy::init(logger);
	LoggingProxy::ProcessName(SimpleClient::getProcessName());
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
	sprintf(buf, "ID=%lu", ACE_OS::thr_self());
	LoggingProxy::ThreadName(buf);
	}
	}
    else
	{
	// since we can not intailize the logging callback we allow logging system to log to stderr
	LoggingProxy::Flags( ACE_Log_Msg::STDERR);
	ACS_LOG(LM_RUNTIME_CONTEXT, "maci::SimpleClientThreadHook::initThread", (LM_ERROR, "Failed to initiailized!"));
	}
    
    return;
}//SimpleClientThreadHook::start

/*___oOo___*/
