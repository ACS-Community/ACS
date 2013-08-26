/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) Associated Universities Inc., 2002 
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
* "@(#) $Id: acsncConsumerILCompImpl.cpp,v 1.4 2006/09/01 02:20:54 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-09-26 added many more comments
* david  25/09/02  created 
*/

static char *rcsId="@(#) $Id: acsncConsumerILCompImpl.cpp,v 1.4 2006/09/01 02:20:54 cparedes Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "acsncConsumerCompImpl.h"
#include <ACSErrTypeCommon.h>

 using namespace ACSErrTypeCommon;


using namespace std;
 using namespace baci;
/********************************************************************************/
void
ConsumerCompImpl::myHandlerFunction(acsnc::EventDescription joe, void *handlerParam)
{
    ConsumerCompImpl *myself = (ConsumerCompImpl *)handlerParam;
    if(myself->m_count<5)
	{
	ACS_STATIC_SHORT_LOG((LM_ALERT, "myHandlerFunction()...value is:%d", joe.count));
	myself->m_count++;
	ACE_OS::sleep(2.50);
	}
}
/* ----------------------------------------------------------------*/
ConsumerCompImpl::ConsumerCompImpl(const ACE_CString &name,
				   maci::ContainerServices *cs) :
    acscomponent::ACSComponentImpl(name, cs)
{
    m_count = 0;
    ACS_NEW_SIMPLE_CONSUMER(m_testConsumer_p, acsnc::EventDescription, "blarIL", myHandlerFunction, (void *)this);
    try
	{
	m_testConsumer_p->addSubscription<acsnc::EventDescription>(myHandlerFunction);
	ACS_SHORT_LOG((LM_ALERT, "No exception on addSubscription...bad."));
	}
    catch(CouldntPerformActionEx err)
	{
	ACS_SHORT_LOG((LM_ALERT, "An exception on addSubscription...OK."));
	}
    m_testConsumer_p->consumerReady();
    
    //Test suspend/resume
    m_testConsumer_p->suspend();
    m_testConsumer_p->resume();
}
/* ----------------------------------------------------------------*/
ConsumerCompImpl::~ConsumerCompImpl()
{
    //test bad suspend
    try
	{
	m_testConsumer_p->suspend();
	m_testConsumer_p->suspend();
	ACS_SHORT_LOG((LM_ALERT, "No exception on suspend...OK."));
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ALERT, "An exception on suspend...bad."));
	}

    //test bad resume
    try
	{
	m_testConsumer_p->resume();
	m_testConsumer_p->resume();
	ACS_SHORT_LOG((LM_ALERT, "No exception on resume...OK."));
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ALERT, "An exception on resume...bad."));
	}
    
    //test bad filter
    try
	{
	m_testConsumer_p->addFilter("EventDescription", "hope to god this filter doesn't work");
	ACS_SHORT_LOG((LM_ALERT, "No exception on addFilter...bad."));
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ALERT, "An exception on addFilter...OK."));
	}
    
    m_testConsumer_p->disconnect();
    m_testConsumer_p = 0;
}
/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(ConsumerCompImpl)
/* ----------------------------------------------------------------*/
/*___oOo___*/
