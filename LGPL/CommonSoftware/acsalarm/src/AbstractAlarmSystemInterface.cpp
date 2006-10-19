#include "AbstractAlarmSystemInterface.h" 
#include "logging.h"

using namespace acsalarm;

/**
 * Push a collection of fault states.
 * @param states
 *
 * TODO later:
 * @throws ASIException if the fault state collection can not be pushed.
 */
void AbstractAlarmSystemInterface::push(vector<FaultState> & states)
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AbstractAlarmSystemInterface::push(vector<FaultState>): entering.");
	commonPush(states, false);
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AbstractAlarmSystemInterface::push(vector<FaultState>): exiting.");
}

/**
 * Push a fault state.
 * @param state the fault state change to push.
 *
 * TODO later:
 * @throws ASIException if the fault state can not be pushed.
 */
void AbstractAlarmSystemInterface::push(FaultState & state)
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AbstractAlarmSystemInterface::push(FaultState): entering.");

	// create a vector and populate with the (single) fault state, 
	// to be passed to the buildMessageXML method
	vector<FaultState> states;
	FaultState* st=(FaultState*)&state;
	states.push_back((FaultState)*st);

	commonPush(states, false);
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AbstractAlarmSystemInterface::push(FaultState): exiting.");
}

/**
 * Push the set of active fault states.
 * @param activeFaults the active fault states.
 *
 * TODO later:
 * @throws ASIException if the fault state active list can not be pushed.
 */
void AbstractAlarmSystemInterface::pushActiveList(vector<FaultState> & activeFaults)
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AbstractAlarmSystemInterface::pushActiveList(): entering.");
	commonPush(activeFaults, true);
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AbstractAlarmSystemInterface::pushActiveList(): exiting.");
}

/**
 * Private method to push a collection of fault states, containing the
 * logic which is common to both the push() and pushActiveList() methods.
 *
 * @param states
 * @param backup whether we are sending 'backup' alarms or not. backup alarms
 *        are alarms in the active list that are sent on startup, when the source
 *        starts and periodically according to the expected backup frequency.
 *
 * TODO later:
 * @throws ASIException if the fault state collection can not be pushed.
 */
void AbstractAlarmSystemInterface::commonPush(vector<FaultState> & states, bool backup)
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AbstractAlarmSystemInterface::commonPush(): entering.");

	// create the ASIMessage, supplying the faults which are to be published to the alarm server
	vector<FaultState> * statesPtr = new vector<FaultState>(states);
	auto_ptr<vector<FaultState> > statesAutoPtr(statesPtr); 
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

	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AbstractAlarmSystemInterface::commonPush(): exiting.");
}
