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
* "@(#) $Id: acsncSupplierILCompImpl.cpp,v 1.3 2008/10/01 03:14:56 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-09-26 added many more comments
* david  25/09/02  created 
*/

static char *rcsId="@(#) $Id: acsncSupplierNamedChCompImpl.cpp,v 1.3 2008/10/01 03:14:56 cparedes Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "acsncSupplierCompImpl.h"
#include <list>

using namespace std;
using namespace baci;

class NamedChEventProcCb : public nc::SimpleSupplier::EventProcessingCallback<acsnc::EventDescription>
{
    public:
        NamedChEventProcCb()
            : nc::SimpleSupplier::EventProcessingCallback<acsnc::EventDescription>()
        {
            reset();
        }

        virtual void eventDropped(acsnc::EventDescription event)
        {
            ++numEventsDropped_m;
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

        virtual ~NamedChEventProcCb()
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

/*
int32_t calcNumTransitions(const std::list<bool> &pubsDone,std::list<int32_t> &transitions)
{
    int32_t numTransitions = 0;
    if(pubsDone.size() > 0)
    {
        std::list<bool>::const_iterator it = pubsDone.begin();
        bool prevValue = *it;
        ++it;
        int32_t idx = 1;
        for(;it != pubsDone.end();++it,++idx)
        {
            if(prevValue != *it)
            {
                transitions.push_back(idx);
                ++numTransitions;
            }
            prevValue = *it;
        }
    }
    return numTransitions;
}*/

/*
void assertZeroTransitions(const std::list<int32_t> &transitions)
{
    int32_t numTransitions = transitions.size();
    if(numTransitions != 0)
    {
        ACS_SHORT_LOG((LM_ERROR, "Wrong number of transitions. There are %d transitions but we expected none", numTransitions));
        for(std::list<int32_t>::const_iterator it = transitions.begin();it != transitions.end();++it)
        {
            ACS_SHORT_LOG((LM_ERROR, "Transition in %d event", *it));
        }
    } else {
        ACS_SHORT_LOG((LM_INFO, "Good! There are 0 transitions"));
    }   
}*/

/*
void assertOneTransition(const std::list<int32_t> &transitions)
{
    int32_t numTransitions = transitions.size();
    if(numTransitions != 1)
    {
        ACS_SHORT_LOG((LM_ERROR, "Wrong number of transitions. There are %d transitions but we expected only one", numTransitions));
        for(std::list<int32_t>::const_iterator it = transitions.begin();it != transitions.end();++it)
        {
            ACS_SHORT_LOG((LM_ERROR, "Transition in %d event", *it));
        }
    } else {
        ACS_SHORT_LOG((LM_INFO, "Good! There is only one transition"));
    }
}*/

/*
void assertTwoTransitions(const std::list<int32_t> &transitions)
{
    int32_t numTransitions = transitions.size();
    if(numTransitions != 2)
    {
        ACS_SHORT_LOG((LM_ERROR, "Wrong number of transitions. There are %d transitions but we expected two", numTransitions));
        for(std::list<int32_t>::const_iterator it = transitions.begin();it != transitions.end();++it)
        {
            ACS_SHORT_LOG((LM_ERROR, "Transition in %d event", *it));
        }
    } else {
        ACS_SHORT_LOG((LM_INFO, "Good! There are two transitions"));
    }
}*/

void publishEvents(nc::SimpleSupplier *supp,int32_t numEvents,int32_t numSec,int32_t sleepVal,
                NamedChEventProcCb * procCb)
{
    int32_t totalNumEvents = numEvents * numSec;

    ACS_SHORT_LOG((LM_ALERT, "Start sending %d events via SimpleSupplier.", totalNumEvents));

    acsnc::EventDescription descrip;
    descrip.name = CORBA::string_dup("none...this is a test");
    descrip.timestamp = 41;
    descrip.count = 41;

    for(CORBA::Long sec = 0; sec < numSec; ++sec)
    {
        for(CORBA::Long i = 0; i < numEvents; ++i)
        {
            //int32_t numEvent = (sec * numEvents) + i;
            try {
                supp->publishData<acsnc::EventDescription>(descrip, procCb);
                //ACS_SHORT_LOG((LM_ALERT,"testReconn1 - Event %d sent", numEvent));

            // Unexpected error
            } catch(...) {
                procCb->exceptionThrown();
                //ACS_SHORT_LOG((LM_ALERT,"testReconn1 - Event %d threw an exception", numEvent));
            }
        }
        if(sleepVal > 0)
        {
            ACE_OS::sleep(sleepVal);
            ACS_SHORT_LOG((LM_ALERT, "Published events at iteration %d", sec));
        }
    }

    ACS_SHORT_LOG((LM_ALERT, "Finished sending %d events via SimpleSupplier.", totalNumEvents));
}

/* ----------------------------------------------------------------*/
SupplierCompImpl::SupplierCompImpl(const ACE_CString &name,
				   maci::ContainerServices *cs) :
    acscomponent::ACSComponentImpl(name, cs),
    m_testSupplier_p(0)
{
    m_testSupplier_p = new nc::SimpleSupplier("NamedCh", this);
}
/* ----------------------------------------------------------------*/
SupplierCompImpl::~SupplierCompImpl()
{
    try {
    if (m_testSupplier_p != 0)
	{
	m_testSupplier_p->disconnect();
	m_testSupplier_p=0;
	}
    } catch(...) {}
}
/* --------------------- [ CORBA interface ] ----------------------*/
void
SupplierCompImpl::sendEvents(short numEvents)
{
    acsnc::EventDescription descrip;
    descrip.name = CORBA::string_dup("none...this is a test");
    descrip.timestamp = 41;
    descrip.count = 41;

    for(short i=0; i<numEvents; i++)
	{
	m_testSupplier_p->publishData<acsnc::EventDescription>(descrip);
	ACS_SHORT_LOG((LM_ALERT, "Sent an event via SimpleSupplier."));
	ACE_OS::sleep(1);
	}
}

/**
 * Test used to check a supplier in the following cases:
 * - The Notify Service restarted and autoreconnection of the supplier is off.
 * - The Notify Service restarted and autoreconnection of the supplier is on.
 * - The Notify Service stopped and autoreconnection of the supplier is off.
 * - The Notify Service stopped and autoreconnection of the supplier is on.
 * When autoreconnect is off it sends 20 events, one per second.
 * When autoreconnect is on it sends 20000 events, 1000 per second.
 */
void 
SupplierCompImpl::testReconn1(CORBA::Boolean autoreconnect,CORBA::Boolean ncRestarted)
{
    int32_t numSec = 30;
    int32_t sleepVal = 1;
    int32_t numEvents = 1000; // Number of events per second;
    int32_t numExceptions = 0;

    if(!autoreconnect)
    {
        numSec = 20;
        numEvents = 1;
    }

    NamedChEventProcCb * procCb = new NamedChEventProcCb();

//------- Test autoreconnect true ---------------------------------------------
    m_testSupplier_p->setAutoreconnect(autoreconnect);
    
//    std::list<int32_t> transitions;
    publishEvents(m_testSupplier_p, numEvents, numSec, sleepVal, procCb);

    ACS_SHORT_LOG((LM_ALERT, "===  Autoreconnect: %s", autoreconnect ? "yes" : "no"));
    ACS_SHORT_LOG((LM_ALERT, "===  NC: %s", ncRestarted ? "restarted" : "stopped"));
    ACS_SHORT_LOG((LM_ALERT, "===  Number of events dropped: %d", procCb->numEventsDropped_m));
    ACS_SHORT_LOG((LM_ALERT, "===  Number of events sent: %d", procCb->numEventsSent_m));
    ACS_SHORT_LOG((LM_ALERT, "===  Number of events queued: %d", procCb->numEventsStoredInQueue_m));
    ACS_SHORT_LOG((LM_ALERT, "===  Number of transitions: %d", procCb->numTransitions_m));

    std::ostringstream oss;
    oss << "===  Transitions in: ";
    for(std::list<int32_t>::const_iterator it = procCb->transitions_m.begin();
        it != procCb->transitions_m.end();++it)
    {
        oss << *it << ",";
    }
    ACS_SHORT_LOG((LM_ALERT, oss.str().c_str()));

    ACS_SHORT_LOG((LM_ALERT, "===  Number of exceptions caught: %d", procCb->numExceptions_m));

    // Notify Service has been stopped
    if(!ncRestarted)
    {
        // Autoreconnect ON, we expect only 1 transition (From Sent to Events Queued)
        if(autoreconnect && procCb->numTransitions_m != 1)
        {
            ACS_SHORT_LOG((LM_ALERT, "===  Wrong number of transitions: %d [We expected 1]", 
                procCb->numTransitions_m));

        // Autoreconnect OFF, we expect only 1 transition (From Sent to Events Queued)
        } else if(false == autoreconnect && procCb->numTransitions_m != 1) {
            ACS_SHORT_LOG((LM_ALERT, "===  Wrong number of transitions: %d [We expected 1]", 
                procCb->numTransitions_m));
        }

    // Notify Service has been restarted
    } else {
        // Autoreconnect ON, we expect 0 or 2 transitions (Sent -> Events Queued -> Sent)
        if(autoreconnect && procCb->numTransitions_m != 0 && procCb->numTransitions_m != 2) 
        {
            ACS_SHORT_LOG((LM_ALERT, "===  Wrong number of transitions: %d [We expected 0 or 2]", 
                procCb->numTransitions_m));

        // Autoreconnect OFF, we expect 1 transition (Sent -> Exception thrown)
        } else if(false == autoreconnect && procCb->numTransitions_m != 1) {
            ACS_SHORT_LOG((LM_ALERT, "===  Wrong number of transitions: %d [We expected 1]", 
                procCb->numTransitions_m));
            
        }
    }

    //ACE_OS::sleep(1);

    delete procCb;
}
/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(SupplierCompImpl)
/* ----------------------------------------------------------------*/
/*___oOo___*/
