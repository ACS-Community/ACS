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
* "@(#) $Id: acsncSupCompImpl.cpp,v 1.4 2006/09/01 02:20:54 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-09-26 added many more comments
* david  25/09/02  created 
*/

static char *rcsId="@(#) $Id: acsncSupCompImpl.cpp,v 1.4 2006/09/01 02:20:54 cparedes Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "acsncSupCompImpl.h"
#include <ACSErrTypeCommon.h>

using namespace ACSErrTypeCommon;

using namespace std;
using namespace baci;


/* ----------------------------------------------------------------*/
SupCompImpl::SupCompImpl(const ACE_CString &name,
				   maci::ContainerServices *cs) :
    acscomponent::ACSComponentImpl(name, cs), m_testSup_p(0)
{
}

/* ----------------------------------------------------------------*/
SupCompImpl::~SupCompImpl()
{
    try {
        if(m_testSup_p != 0)
        {
            m_testSup_p->disconnect();
            m_testSup_p = 0;
        }
    } catch(...) {
        ACS_SHORT_LOG((LM_ERROR, "Exception thrown while disconnecting the supplier"));
    }
}

/* ----------------------------------------------------------------*/
void SupCompImpl::execTest(const char* channelName,CORBA::Boolean autoreconnect,
        CORBA::Long numSec,CORBA::Long numEvents,CORBA::Long sleepVal)
{
    m_count = 0;
    m_testSup_p = new nc::SimpleSupplier(channelName, this);
    m_testSup_p->setAutoreconnect(autoreconnect);

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
                m_testSup_p->publishData<acsnc::EventDescription>(descrip, 0);
                //ACS_SHORT_LOG((LM_ALERT,"testReconn1 - Event %d sent", numEvent));

            // Unexpected error
            } catch(...) {
                //procCb->exceptionThrown();
                //ACS_SHORT_LOG((LM_ALERT,"testReconn1 - Event %d threw an exception", numEvent));
            }
        }
        if(sleepVal > 0)
        {
            ACE_OS::sleep(sleepVal);
            //ACS_SHORT_LOG((LM_ALERT, "Published events at iteration %d", sec));
        }
    }
    ACS_SHORT_LOG((LM_ALERT, "Finished sending %d events via SimpleSupplier.", totalNumEvents));
    //ACE_OS::sleep(4);
}


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(SupCompImpl)
/* ----------------------------------------------------------------*/
/*___oOo___*/
