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
* "@(#) $Id: acsncConILCompImpl.cpp,v 1.4 2006/09/01 02:20:54 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-09-26 added many more comments
* david  25/09/02  created 
*/

static char *rcsId="@(#) $Id: acsncConNamedChCompImpl.cpp,v 1.4 2006/09/01 02:20:54 cparedes Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "acsncConCompImpl.h"
#include <ACSErrTypeCommon.h>

 using namespace ACSErrTypeCommon;


using namespace std;
using namespace baci;

/********************************************************************************/
void
ConCompImpl::myHandlerFunction(acsnc::EventDescription joe, void *handlerParam)
{
    ConCompImpl *myself = (ConCompImpl *)handlerParam;
    myself->m_count++;
    //ACS_STATIC_SHORT_LOG((LM_ALERT, "Event received: %d", myself->m_count));
}
/* ----------------------------------------------------------------*/
ConCompImpl::ConCompImpl(const ACE_CString &name,
				   maci::ContainerServices *cs) :
    acscomponent::ACSComponentImpl(name, cs), m_testCon_p(0)
{
}

/* ----------------------------------------------------------------*/
ConCompImpl::~ConCompImpl()
{
    try {
        if(m_testCon_p != 0)
        {
            m_testCon_p->disconnect();
            m_testCon_p = 0;
        }
    } catch(...) {
        ACS_SHORT_LOG((LM_ERROR, "Exception thrown while disconnecting the consumer"));
    }
}

/* ----------------------------------------------------------------*/
void ConCompImpl::execTest(const char* channelName,CORBA::Boolean autoreconnect)
{
    m_count = 0;
    ACS_NEW_SIMPLE_CONSUMER(m_testCon_p, acsnc::EventDescription, channelName, 
                            myHandlerFunction, (void *)this);

    m_testCon_p->setAutoreconnect(autoreconnect);
    m_testCon_p->consumerReady();
    m_testCon_p->suspend();
    m_testCon_p->resume();
}
/* ----------------------------------------------------------------*/
void ConCompImpl::checkCounterGreaterThan(CORBA::Long value)
{
    if(m_count > value)
    {
        ACS_SHORT_LOG((LM_ALERT, ":: Great! count is greater than %d [%d]", value, m_count));
    } else {
        ACS_SHORT_LOG((LM_ERROR, ":: Expected count greater than %d but was %d", value, m_count));
    }
}

/* ----------------------------------------------------------------*/
void ConCompImpl::checkCounterLowerThan(CORBA::Long value)
{
    if(m_count < value)
    {
        ACS_SHORT_LOG((LM_ALERT, ":: Great! count is lower than %d [%d]", value, m_count));
    } else {
        ACS_SHORT_LOG((LM_ERROR, ":: Expected count lower than %d but was %d", value, m_count));
    }
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(ConCompImpl)
/* ----------------------------------------------------------------*/
/*___oOo___*/
