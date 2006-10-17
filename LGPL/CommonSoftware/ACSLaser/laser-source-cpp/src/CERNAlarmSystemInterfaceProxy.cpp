#include <memory>
#include <unistd.h>
#include <sys/param.h>
#include <dlfcn.h>
#include "CERNAlarmSystemInterfaceProxy.h"
#include "asiConfigurationConstants.h"
#include <logging.h>

using namespace acsalarm;
using namespace laserSource;
using asiConfigurationConstants::ALARM_SOURCE_NAME;

/*
 * Default no-args constructor.
 */
CERNAlarmSystemInterfaceProxy::CERNAlarmSystemInterfaceProxy()
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceProxy::CERNAlarmSystemInterfaceProxy(): entering.");
	laserPublisher = NULL;
	setSourceName(ALARM_SOURCE_NAME);
	init();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceProxy::CERNAlarmSystemInterfaceProxy(): exiting.");
}

/*
 * Constructor.
 * @param theSourceName the name of the source. This should normally be the
 * one (and only one) source name defined in asiConfigurationConstants.h ALARM_SOURCE_NAME.
 */
CERNAlarmSystemInterfaceProxy::CERNAlarmSystemInterfaceProxy(string theSourceName)
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceProxy::CERNAlarmSystemInterfaceProxy(string): entering.");
	laserPublisher = NULL;
	setSourceName(theSourceName);
	string expectedSrcName(ALARM_SOURCE_NAME);
	if(theSourceName != expectedSrcName)
	{
		string logString =  "CERNAlarmSystemInterfaceProxy::CERNAlarmSystemInterfaceProxy(string): source name: " 
			+ theSourceName + " will not work; all alarms should use: " + ALARM_SOURCE_NAME;
		myLoggerSmartPtr->log(Logging::Logger::LM_WARNING, logString);
	}
	init();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceProxy::CERNAlarmSystemInterfaceProxy(string): exiting.");
}

/*
 * Destructor.
 */
CERNAlarmSystemInterfaceProxy::~CERNAlarmSystemInterfaceProxy()
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceProxy::~CERNAlarmSystemInterfaceProxy(): entering.");
	if (laserPublisher != NULL) {
		delete laserPublisher;
		laserPublisher = NULL;
	}
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceProxy::~CERNAlarmSystemInterfaceProxy(): exiting.");
}

/**
 * Close and deallocate resources.
 */
void CERNAlarmSystemInterfaceProxy::close()
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceProxy::close(): entering.");
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceProxy::close(): exiting.");
}

/**
 * Push a collection of fault states.
 * @param states
 *
 * TODO later:
 * @throws ASIException if the fault state collection can not be pushed.
 */
void CERNAlarmSystemInterfaceProxy::push(vector<ACSFaultState> & states)
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceProxy::push(vector<ACSFaultState>): entering.");
	commonPush(states, false);
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceProxy::push(vector<ACSFaultState>): exiting.");
}

/**
 * Push a fault state.
 * @param state the fault state change to push.
 *
 * TODO later:
 * @throws ASIException if the fault state can not be pushed.
 */
void CERNAlarmSystemInterfaceProxy::push(ACSFaultState & state)
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceProxy::push(ACSFaultState): entering.");
	// create a vector and populate with the (single) fault state, 
	// to be passed to the buildMessageXML method

	vector<ACSFaultState> states;
	ACSFaultState* st=(ACSFaultState*)&state;
	states.push_back((ACSFaultState)*st);

	commonPush(states, false);
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceProxy::push(ACSFaultState): exiting.");
}

/**
 * Push the set of active fault states.
 * @param activeFaults the active fault states.
 *
 * TODO later:
 * @throws ASIException if the fault state active list can not be pushed.
 */
void CERNAlarmSystemInterfaceProxy::pushActiveList(vector<ACSFaultState> & activeFaults)
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceProxy::pushActiveList(): entering.");
	commonPush(activeFaults, true);
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceProxy::pushActiveList(): exiting.");
}

/*
 * Sends a message to the alarm server, by loading a DLL that was specified in the configuration;
 * by doing this indirection, we facilitate the decoupling of the core laser cpp alarm source logic
 * from the actual communication mechanism, e.g. ACS/CORBA. Someone who wishes to use a different 
 * communication mechanism need only implement their own DLL and the remainder of this code can be
 * reused as-is without any changes.
 *
 * The name of the shared library is configurable, currently in
 * asiConfigurationConstants.h, referenced through the Configuration class method 
 * getPublisherDLLPath(), but eventually (TODO later:) to be moved to the CDB. 
 * The name of the entry point to the DLL is well defined, not configurable, and is 
 * referenced through the Configuration class method getPublisherFactoryFunctionPtr().
 * 
 * TODO later: "syncbuffer" for maintaining active list, etc.
 */
bool CERNAlarmSystemInterfaceProxy::publishMessageDLL(ASIMessage msg)
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceProxy::publishMessageDLL(): entering.");
	bool retVal = false;

	// Instantiate the laserPublisher via the DLL, if it has not already been instantiated
	if(NULL == laserPublisher)
	{
		// create the topic on which to publish the alarm, by appending 
		// the source name to the topic prefix provided by the configuration 
		// (should look something like: CMW.ALARM_SYSTEM.ALARMS.SOURCES.ALARM_SYSTEM_SOURCES)
		string topicName(configuration.getAlarmsTopic());
		topicName.append(".");
		topicName.append(msg.getSourceName());

		// get the pointer to the publish function from the loaded DLL
		void *hndl = dlopen(configuration.getPublisherDLLPath().c_str(), RTLD_NOW|RTLD_GLOBAL);
		if(hndl == NULL)
		{
			string errString = "CERNAlarmSystemInterfaceProxy::publishMessageDLL(): could not open DLL; error was:\n\n" + string(dlerror()); 
			myLoggerSmartPtr->log(Logging::Logger::LM_ERROR, errString);
			// TODO - throw an exception rather than returning...
			return false;
		}
		void * publisherFactoryFunctionPtr = dlsym(hndl, configuration.getPublisherFactoryFunctionName().c_str());
		laserPublisher = ((AlarmPublisher*(*)(string))(publisherFactoryFunctionPtr))(topicName);

		// check for success/failure
		if (NULL != laserPublisher)
		{
			retVal = true;
		}
		else 
		{
			string errString = "CERNAlarmSystemInterfaceProxy::publishMessageDLL(): could not get publisher from DLL; error was:\n\n" + string(dlerror()); 
			myLoggerSmartPtr->log(Logging::Logger::LM_ERROR, errString);
			// TODO - throw an exception rather than returning...
			return false;
		}
	}

	// publish the alarm via the class/method loaded from the DLL
	laserPublisher->publishAlarm(msg);

	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceProxy::publishMessageDLL(): exiting.");
	return retVal;
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
void CERNAlarmSystemInterfaceProxy::commonPush(vector<ACSFaultState> & states, bool backup)
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceProxy::commonPush(): entering.");
	// create the ASIMessage, supplying the faults which are to be published to the alarm server
	vector<ACSFaultState> * statesPtr = new vector<ACSFaultState>(states);
	auto_ptr<vector<ACSFaultState> > statesAutoPtr(statesPtr); 
	ASIMessage asiMessage(statesAutoPtr);

	// populate the ASIMessage's source timestamp (with the current time)
	auto_ptr<Timestamp> timestampPtr(new Timestamp());
	asiMessage.setSourceTimestamp(timestampPtr);

	// populate the ASIMessage's source name
	asiMessage.setSourceName(sourceName);

	// populate the ASIMessage's source hostname
	asiMessage.setSourceHostname(hostname);

	// set the ASIMessage's backup flag
	asiMessage.setBackup(backup);

	// set the ASIMessage's version
	asiMessage.setVersion(configuration.getASIVersion());
	
	// publish the ASIMessage to the alarm server
	publishMessageDLL(asiMessage);

	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceProxy::commonPush(): exiting.");
}

// initialization logic used by the constructors
void CERNAlarmSystemInterfaceProxy::init()
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceProxy::init(): entering.");

	// TODO later: portability/platform-specific issues with using gethostname()?
	char name[MAXHOSTNAMELEN + 1];
	gethostname(name, MAXHOSTNAMELEN);
	string nameStr(name);
	hostname = (nameStr);

	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceProxy::init(): exiting.");
}
