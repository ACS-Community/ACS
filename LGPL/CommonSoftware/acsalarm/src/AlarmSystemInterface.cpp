/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
#include "AlarmSystemInterface.h"
#include "logging.h"

using std::vector;
using std::string;
using std::iterator;
using std::auto_ptr;
using acsalarm::FaultState;
using acsalarm::AlarmSystemInterface;

/**
 * Push a collection of fault states.
 * @param states
 *
 * TODO later: @throws ASIException if the fault state collection can not be pushed.
 */
void AlarmSystemInterface::push(vector<FaultState> & states)
{
	ACS_TRACE("AlarmSystemInterface::push(vector<FaultState>)");
	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_mutex);
	commonPush(states, false);
}

/**
 * Push a fault state.
 * @param state the fault state change to push.
 *
 * TODO later: @throws ASIException if the fault state can not be pushed.
 */
void AlarmSystemInterface::push(FaultState & state)
{
	ACS_TRACE("AlarmSystemInterface::push(FaultState)");
	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_mutex);
	// create a vector and populate with the (single) fault state,
	// to be passed to the buildMessageXML method
	vector<FaultState> states;
	FaultState* st=(FaultState*)&state;
	states.push_back((FaultState)*st);

	commonPush(states, false);
}

/**
 * Push the set of active fault states.
 * @param activeFaults the active fault states.
 *
 * TODO later: @throws ASIException if the fault state active list can not be pushed.
 */
void AlarmSystemInterface::pushActiveList(vector<FaultState> & activeFaults)
{
	ACS_TRACE("AlarmSystemInterface::pushActiveList()");
	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_mutex);
	commonPush(activeFaults, true);
}

/**
 * Private method to push a collection of fault states, containing the
 * logic which is common to both the push() and pushActiveList() methods.
 *
 * NOTE: This method is executed only when the CERN alarm system is in use
 *       because ACSAlarmSystemInterfaceProxy redefines the push...()
 *
 * @param states
 * @param backup whether we are sending 'backup' alarms or not. backup alarms
 *        are alarms in the active list that are sent on startup, when the source
 *        starts and periodically according to the expected backup frequency.
 *
 * TODO later: @throws ASIException if the fault state collection can not be pushed.
 */
void AlarmSystemInterface::commonPush(vector<FaultState> & states, bool backup)
{
	ACS_TRACE("AlarmSystemInterface::commonPush()");
	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_mutex);
	// create the ASIMessage, supplying the faults which are to be published to the alarm server
	auto_ptr<vector<FaultState> > statesAutoPtr(new vector<FaultState>(states));
	ASIMessage asiMessage(statesAutoPtr);

	// populate the ASIMessage's source timestamp (with the current time)
	auto_ptr<Timestamp> timestampPtr(new Timestamp());
	asiMessage.setSourceTimestamp(timestampPtr);

	// populate the ASIMessage's source name
	asiMessage.setSourceName(sourceName);

	// populate the ASIMessage's source hostname
	asiMessage.setSourceHostname(hostName);

	// set the ASIMessage's backup flag
	asiMessage.setBackup(backup);

	// set the ASIMessage's version
	asiMessage.setVersion(configuration.getASIVersion());

	// publish the ASIMessage to the alarm server
	publishMessage(asiMessage);

	// Log an LM_ALARM message for each fault state in the vector
	for (vector<FaultState>::iterator it = states.begin(); it!=states.end(); ++it/* increment operand is used to move to next element*/) {
	    char msgA[16];
	    sprintf(msgA,"%d",(*it).getCode());
	    string msg="Alarm sent: <";
		msg+=(*it).getFamily();
		msg+=",";
		msg+=(*it).getMember();
		msg+=",";
		msg+=msgA;
		msg+="> ";
		msg+=(*it).getDescriptor();
		ACS_SHORT_LOG((LM_DEBUG, msg.c_str()));
	}
}

