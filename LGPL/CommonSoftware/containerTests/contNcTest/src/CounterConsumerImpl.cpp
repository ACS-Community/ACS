/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) Associated Universities Inc., 2002 *
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
*
*
* "@(#) $Id: CounterConsumerImpl.cpp,v 1.2 2008/10/07 09:41:42 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* eallaert  2008-01-31  initial version
*
*/
 
#include "CounterConsumerImpl.h"
#include <ACSErrTypeCommon.h>

using namespace ACSErrTypeCommon;

using namespace std;
using namespace baci;

ACE_RCSID(contNcTest, CounterConsumerImpl, "$Id: CounterConsumerImpl.cpp,v 1.2 2008/10/07 09:41:42 cparedes Exp $")

ACE_Log_Priority LOCAL_LOGGING_LEVEL = LM_TRACE;



/* ----------------------------------------------------------------*/
void CounterConsumerImpl::counterDataHandler(COUNTER::statusBlockEvent someParam, void * handlerParam)
{
	// Keep track of how many events this instance has received.
	CounterConsumerImpl *myself = (CounterConsumerImpl *)handlerParam;
	myself->m_eventCount++;
	if (myself->contFlag) {	
		//pattern     ..= someParam.status;
		int onOff     = someParam.onOff;
		const char *myString= someParam.myString;
		int counter1  = someParam.counter1;
		int counter2  = someParam.counter2;
		int counter3  = someParam.counter3;
		bool lastFlag = someParam.flipFlop;
		// period is currently not used anywhere - comment out to avoid compiler warning
		//float period  = someParam.period;

		//if (!lastFlag) {
		if (onOff == COUNTER::ON && !lastFlag) {
			ACS_STATIC_SHORT_LOG((LM_INFO, "Counter now %d (max %d), flag will flip at %d", counter1, counter2, counter3));
		}
		else {
			ACS_STATIC_SHORT_LOG((LM_INFO, "%s received, counter is now %d", myString, counter1));
			myself->contFlag = FALSE;
			//now disconnect from channel everything
			// NOTE: There seem to be problems when this is done within the event-handler method !!!
			//myself->m_CounterConsumer_p->disconnect();
			//myself->m_CounterConsumer_p = NULL;
		}
	}
	return;

}
// "CounterConsumer" = interface name as in contNcTest_IF.idl
/* ----------------------------------------------------------------*/
CounterConsumerImpl::CounterConsumerImpl(const ACE_CString &name, 
		maci::ContainerServices * containerServices) :
			ACSComponentImpl(name, containerServices),
			m_CounterConsumer_p(NULL)
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::CounterConsumer::CounterConsumer");
	m_CounterConsumer_p = NULL;
}
/* ----------------------------------------------------------------*/
CounterConsumerImpl::~CounterConsumerImpl()
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::CounterConsumer::~CounterConsumer");

    if (m_CounterConsumer_p != NULL) {
    	m_CounterConsumer_p->disconnect();
    	m_CounterConsumer_p = NULL;
    }

    ACS_DEBUG_PARAM("::CounterConsumer::~CounterConsumer", "Destroying %s...", name());
}
/* --------------------- [ CORBA interface ] ----------------------*/
void 
CounterConsumerImpl::getBlocks ()	
{

	m_eventCount = 0;
	contFlag = TRUE;

	ACS_NEW_SIMPLE_CONSUMER(m_CounterConsumer_p,
			COUNTER::statusBlockEvent,
			COUNTER::CHANNELNAME_COUNTER,
			counterDataHandler,
			(void *)this);
	m_CounterConsumer_p->consumerReady();
	
	ACE_Time_Value time(30);
	//client.run(time);
	ACS_SHORT_LOG((LM_INFO, "CounterConsumer is ready to receive 'status' events."));
	
	return;
}

CORBA::Long
CounterConsumerImpl::waitTillDone() 
{
	if (m_CounterConsumer_p == NULL) {
		ACS_SHORT_LOG((LM_ERROR, "CounterConsumer didn't even start yet"));
		return (CORBA::Long)-1;
	}
	while (contFlag) {
		ACS_SHORT_LOG((LM_INFO, "CounterConsumer received %d blocks so far ...", m_eventCount));
		usleep(1000000ul);
	}	
	if (m_CounterConsumer_p != NULL) {
		m_CounterConsumer_p->disconnect();
		m_CounterConsumer_p = NULL;
	}
	return (CORBA::Long)m_eventCount;
}


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(CounterConsumerImpl)
/* ----------------------------------------------------------------*/

/*___oOo___*/
