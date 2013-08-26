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
* "@(#) $Id: CounterSupplierImpl.cpp,v 1.2 2008/10/07 09:41:42 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* eallaert  2008-01-31  initial version
*
*/
 
#include "CounterSupplierImpl.h"
#include <ACSErrTypeCommon.h>

ACE_RCSID(contNcTest, CounterSupplierImpl, "$Id: CounterSupplierImpl.cpp,v 1.2 2008/10/07 09:41:42 cparedes Exp $")

ACE_Log_Priority LOCAL_LOGGING_LEVEL = LM_TRACE;

// "CounterSupplier" = interface name as in contNcTest_IF.idl
/* ----------------------------------------------------------------*/
CounterSupplierImpl::CounterSupplierImpl(const ACE_CString &name, 
		maci::ContainerServices * containerServices) :
			ACSComponentImpl(name, containerServices),
			m_CounterSupplier_p(NULL)
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::CounterSupplier::CounterSupplier");
	m_CounterSupplier_p =  new nc::SimpleSupplier("counter", this);
}
/* ----------------------------------------------------------------*/
CounterSupplierImpl::~CounterSupplierImpl()
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::CounterSupplier::~CounterSupplier");

    if (m_CounterSupplier_p != NULL) {
    	m_CounterSupplier_p->disconnect();
    	m_CounterSupplier_p = NULL;
    }

    ACS_DEBUG_PARAM("::CounterSupplier::~CounterSupplier", "Destroying %s...", name());
}
/* --------------------- [ CORBA interface ] ----------------------*/
CORBA::Long
CounterSupplierImpl::sendBlocks (const CORBA::Long initialVal,
		const CORBA::Long lastVal,
		const CORBA::Long changeVal,
		const CORBA::Float period)	
{

	CORBA::Boolean flag = false;
	
	ACS_SHORT_LOG((LM_INFO, "sendBlocks called ...."));

	ACS_SHORT_LOG((LM_INFO, "Ready to send NC events..."));
	// Send the events
	int val = (int)initialVal;
	int eventCount = 0;
	
	::COUNTER::statusBlockEvent t_data;
	
	while (val < (int)lastVal) {
		if (val < (int)changeVal)
			flag = false;
		else
			flag = true;

		t_data.onOff    = COUNTER::ON;
		//t_data.onOff    = 1.0;
		t_data.myString = "C++ supplier";
		t_data.counter1 = val;
		t_data.counter2 = lastVal;
		t_data.counter3 = changeVal;
		t_data.flipFlop = flag;
		t_data.period   = period;
		m_CounterSupplier_p->publishData<COUNTER::statusBlockEvent>(t_data);
		eventCount++;
		ACS_SHORT_LOG((LM_INFO, "Counting ongoing with period %.3fs up to %d, now %d", period,  lastVal, val));
		val++;
		usleep((unsigned long)(period * 1000000.f));
	}

	// Tell consumers this is the last event
	t_data.onOff    = COUNTER::OFF;
	//t_data.onOff    = 0.0;
	t_data.myString = "Last event from C++ supplier";
	t_data.counter1 = val;
	t_data.counter2 = lastVal;
	t_data.counter3 = changeVal;
	t_data.flipFlop = true;
	t_data.period   = period;
	m_CounterSupplier_p->publishData<COUNTER::statusBlockEvent>(t_data);
	eventCount++;
	ACS_SHORT_LOG((LM_INFO, "Counter stopped, last value %d", val));

	
	return (CORBA::Long)eventCount;
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(CounterSupplierImpl)
/* ----------------------------------------------------------------*/

/*___oOo___*/
