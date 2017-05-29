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
* "@(#) $Id: acsncSupplierCompImpl.cpp,v 1.13 2008/10/01 03:14:56 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-09-26 added many more comments
* david  25/09/02  created 
*/

static char *rcsId="@(#) $Id: acsncSupplierCompImpl.cpp,v 1.13 2008/10/01 03:14:56 cparedes Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "acsncSupplierCompImpl.h"

using namespace std;
 using namespace baci;
/* ----------------------------------------------------------------*/
SupplierCompImpl::SupplierCompImpl(const ACE_CString &name,
				   maci::ContainerServices *cs) :
    acscomponent::ACSComponentImpl(name, cs),
    m_testSupplier_p(0),
    m_numEvents(0)
{
    m_testSupplier_p = new nc::SimpleSupplier("NamedCh", this);
}
/* ----------------------------------------------------------------*/
SupplierCompImpl::~SupplierCompImpl()
{
    if (m_testSupplier_p != 0)
	{
	m_testSupplier_p->disconnect();
	m_testSupplier_p=0;
	}
}
void
SupplierCompImpl::sendEventsTh(void *arg)
{
    SupplierCompImpl *suppComp = static_cast<SupplierCompImpl*>(arg);
    //nc::SimpleSupplier *supp = suppComp->getSimpleSupplier();
    int numEvents = suppComp->getNumEvents();
    int numEventsIt = 1000;

    acsnc::EventDescription descrip;
    descrip.name = CORBA::string_dup("this is a test");
    descrip.timestamp = 41;
    descrip.count = 41;

    for(int i=0; i<numEvents; i++)
	{
        for(int j = 0;j < numEventsIt;++j)
        {
            suppComp->publishEvent(descrip);
        }
        ACE_OS::sleep(1);
	}
}
/*nc::SimpleSupplier* SupplierCompImpl::getSimpleSupplier()
{
    return m_testSupplier_p;
}
int SupplierCompImpl::getNumEvents()
{
    return m_numEvents;
}
void SupplierCompImpl::publishEvent(const acsnc::EventDescription event)
{
    if(m_testSupplier_p != NULL)
    {
        m_testSupplier_p->publishData<acsnc::EventDescription>(event);
    }
}*/

/* --------------------- [ CORBA interface ] ----------------------*/
void
SupplierCompImpl::sendEvents(short numEvents)
{
    pthread_t th1, th2, th3, th4, th5, th6;
    m_numEvents = numEvents;

    if(0 != pthread_create(&th1, NULL, SupplierCompImpl::sendEventsTh,reinterpret_cast<void*>(this)))
    {
        ACS_SHORT_LOG((LM_ERROR, ":::: Error! thread 1 cannot be created"));
    } else {
        ACS_SHORT_LOG((LM_ALERT, ":::: Thread 1 created!"));
    }

    if(0 != pthread_create(&th2, NULL, SupplierCompImpl::sendEventsTh,reinterpret_cast<void*>(this)))
    {
        ACS_SHORT_LOG((LM_ERROR, ":::: Error! thread 2 cannot be created"));
    } else {
        ACS_SHORT_LOG((LM_ALERT, ":::: Thread 2 created!"));
    }

    if(0 != pthread_create(&th3, NULL, SupplierCompImpl::sendEventsTh,reinterpret_cast<void*>(this)))
    {
        ACS_SHORT_LOG((LM_ERROR, ":::: Error! thread 3 cannot be created"));
    } else {
        ACS_SHORT_LOG((LM_ALERT, ":::: Thread 3 created!"));
    }

    if(0 != pthread_create(&th4, NULL, SupplierCompImpl::sendEventsTh,reinterpret_cast<void*>(this)))
    {
        ACS_SHORT_LOG((LM_ERROR, ":::: Error! thread 4 cannot be created"));
    } else {
        ACS_SHORT_LOG((LM_ALERT, ":::: Thread 4 created!"));
    }

    if(0 != pthread_create(&th5, NULL, SupplierCompImpl::sendEventsTh,reinterpret_cast<void*>(this)))
    {
        ACS_SHORT_LOG((LM_ERROR, ":::: Error! thread 5 cannot be created"));
    } else {
        ACS_SHORT_LOG((LM_ALERT, ":::: Thread 5 created!"));
    }

    if(0 != pthread_create(&th6, NULL, SupplierCompImpl::sendEventsTh,reinterpret_cast<void*>(this)))
    {
        ACS_SHORT_LOG((LM_ERROR, ":::: Error! thread 6 cannot be created"));
    } else {
        ACS_SHORT_LOG((LM_ALERT, ":::: Thread 6 created!"));
    }

    ACE_OS::sleep(numEvents + 5);
    
    ACS_SHORT_LOG((LM_ALERT, ":::: Joining all threads"));
    pthread_join(th1, NULL);
    pthread_join(th2, NULL);
    pthread_join(th3, NULL);
    pthread_join(th4, NULL);
    pthread_join(th5, NULL);
    pthread_join(th6, NULL);
}

void 
SupplierCompImpl::testReconn1(CORBA::Boolean autoreconnect,CORBA::Boolean ncRestarted)
{
	// Implemented in csncSupplierNamedChCompImpl.cpp
}
/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(SupplierCompImpl)
/* ----------------------------------------------------------------*/
/*___oOo___*/
