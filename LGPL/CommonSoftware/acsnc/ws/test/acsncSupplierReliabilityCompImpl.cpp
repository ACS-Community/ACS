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
* "@(#) $Id: acsncSupplierReliabilityCompImpl.cpp,v 1.3 2008/10/01 03:14:56 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-09-26 added many more comments
* david  25/09/02  created 
*/

static char *rcsId="@(#) $Id: acsncSupplierReliabilityCompImpl.cpp,v 1.3 2008/10/01 03:14:56 mgarces Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "acsncSupplierCompImpl.h"

using namespace std;
using namespace baci;


class EventProcCb : public nc::SimpleSupplier::EventProcessingCallback<acsnc::EventDescription>
{
    public:
        EventProcCb()
            : nc::SimpleSupplier::EventProcessingCallback<acsnc::EventDescription>()
        {
            reset();
        }

        virtual void eventDropped(acsnc::EventDescription event)
        {
            
            ++numEventsDropped_m;
            ACS_SHORT_LOG((LM_ALERT, "[eventDroppedCallback] numEventsDropped_m:", numEventsDropped_m));
            if(true == sent_m)
            {
                sent_m = false;            
                ++numTransitions_m;
            }
        }

        virtual void eventSent(acsnc::EventDescription event)
        {
            ++currentEvent_m;
            ++numEventsSent_m;
            if(false == sent_m)
            {
                sent_m = true;
                ++numTransitions_m;
                transitions_m.push_back(currentEvent_m);
            }
        }

        virtual void eventStoredInQueue(acsnc::EventDescription event)
        {
            ++currentEvent_m;
            ++numEventsStoredInQueue_m;
            if(true == sent_m)
            {
                sent_m = false;
                ++numTransitions_m;
                transitions_m.push_back(currentEvent_m);
            }
        }

        virtual void exceptionThrown()
        {
            ++currentEvent_m;
            ++numExceptions_m;
            if(true == sent_m)
            {
                sent_m = false;
                ++numTransitions_m;
                transitions_m.push_back(currentEvent_m);
            }
        }

        virtual ~EventProcCb()
        {
        }

        virtual void reset()
        {
            currentEvent_m = 0;
            numEventsDropped_m = 0;
            numEventsSent_m = 0;
            numEventsStoredInQueue_m = 0;
            numTransitions_m = 0;
            numExceptions_m = 0;
            transitions_m.clear();
            sent_m = true;
        }

        int32_t currentEvent_m;
        int32_t numEventsDropped_m;
        int32_t numEventsSent_m;
        int32_t numEventsStoredInQueue_m;
        int32_t numTransitions_m;
        int32_t numExceptions_m;
        std::list<int32_t> transitions_m;
        bool sent_m;
};

/* ----------------------------------------------------------------*/
SupplierCompImpl::SupplierCompImpl(const ACE_CString &name,
				   maci::ContainerServices *cs) :
    acscomponent::ACSComponentImpl(name, cs),
    m_testSupplier_p(0)
{
    m_testSupplier_p = new nc::SimpleSupplier("NamedCh", this);
    ((nc::Supplier *) m_testSupplier_p)->setEventBufferSize(5);
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
/* --------------------- [ CORBA interface ] ----------------------*/
void
SupplierCompImpl::sendEvents(short numEvents)
{
    acsnc::EventDescription descrip;
    descrip.name = CORBA::string_dup("none...this is a test");
    descrip.timestamp = 41;
    descrip.count = 41;

    EventProcCb * procCb = new EventProcCb();

    for(short i=0; i<numEvents; i++)
    {
        m_testSupplier_p->publishData<acsnc::EventDescription>(descrip, procCb);

        ACS_SHORT_LOG((LM_ALERT, "Sent an event via SimpleSupplier."));
        ACE_OS::sleep(1);
    }

    ACS_SHORT_LOG((LM_ALERT, "===  total events sent: %d", procCb->numEventsSent_m + procCb->numEventsStoredInQueue_m - procCb->numEventsDropped_m));
    ACS_SHORT_LOG((LM_ALERT, "===  Number of events dropped: %d", procCb->numEventsDropped_m));
    ACS_SHORT_LOG((LM_ALERT, "===  Number of events sent: %d", procCb->numEventsSent_m));
    ACS_SHORT_LOG((LM_ALERT, "===  Number of events queued: %d", procCb->numEventsStoredInQueue_m));
    ACS_SHORT_LOG((LM_ALERT, "===  Number of transitions: %d", procCb->numTransitions_m));
}



void 
SupplierCompImpl::testReconn1(CORBA::Boolean autoreconnect,CORBA::Boolean ncRestarted)
{
	// Implemented in acsncSupplierNamedChCompImpl.cpp
}
/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(SupplierCompImpl)
/* ----------------------------------------------------------------*/
/*___oOo___*/
