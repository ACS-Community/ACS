#include <iostream>
#include <memory>
#include <unistd.h>
#include <sys/param.h>
#include <dlfcn.h>
#include "AlarmSystemInterfaceProxy.h"
#include "asiConfigurationConstants.h"

using std::cout;
using std::cerr;
using namespace laserSource;
using asiConfigurationConstants::ALARM_SOURCE_NAME;

/*
 * Default no-args constructor.
 */
AlarmSystemInterfaceProxy::AlarmSystemInterfaceProxy(CosNaming::NamingContext_ptr naming_p)
{
	m_naming_p=CosNaming::NamingContext::_duplicate(naming_p);
	cout << "AlarmSystemInterfaceProxy::AlarmSystemInterfaceProxy(): entering...\n";
	laserPublisher = NULL;
	setSourceName(ALARM_SOURCE_NAME);
	init();
	cout << "AlarmSystemInterfaceProxy::AlarmSystemInterfaceProxy(): exiting...\n";
}

/*
 * Constructor.
 * @param theSourceName the name of the source. This should normally be the
 * one (and only one) source name defined in asiConfigurationConstants.h ALARM_SOURCE_NAME.
 */
AlarmSystemInterfaceProxy::AlarmSystemInterfaceProxy(string theSourceName,CosNaming::NamingContext_ptr naming_p )
{
	m_naming_p=CosNaming::NamingContext::_duplicate(naming_p);
	cout << "AlarmSystemInterfaceProxy::AlarmSystemInterfaceProxy(string): entering...\n";
	laserPublisher = NULL;
	setSourceName(theSourceName);
	string expectedSrcName(ALARM_SOURCE_NAME);
	if(theSourceName != expectedSrcName)
	{
		cout << "\n\nWARNING - source name: " << theSourceName << " will not work; all alarms should use: " << ALARM_SOURCE_NAME << "\n\n";
	}
	init();
	cout << "AlarmSystemInterfaceProxy::AlarmSystemInterfaceProxy(string): exiting...\n";
}

/*
 * Destructor.
 */
AlarmSystemInterfaceProxy::~AlarmSystemInterfaceProxy()
{
	cout << "AlarmSystemInterfaceProxy::~AlarmSystemInterfaceProxy(): entering...\n";
	CORBA::release(m_naming_p);
	m_naming_p=CosNaming::NamingContext::_nil();
	if (laserPublisher != NULL) {
		delete laserPublisher;
		laserPublisher = NULL;
	}
	cout << "AlarmSystemInterfaceProxy::~AlarmSystemInterfaceProxy(): exiting...\n";
}

/**
 * Close and deallocate resources.
 */
void AlarmSystemInterfaceProxy::close()
{
}

/**
 * Push a collection of fault states.
 * @param states
 *
 * TODO later:
 * @throws ASIException if the fault state collection can not be pushed.
 */
void AlarmSystemInterfaceProxy::push(vector<FaultState> & states)
{
	cout << "AlarmSystemInterfaceProxy::push(vector): entering...\n";
	commonPush(states, true);
	cout << "AlarmSystemInterfaceProxy::push(vector): exiting...\n";
}

/**
 * Push a fault state.
 * @param state the fault state change to push.
 *
 * TODO later:
 * @throws ASIException if the fault state can not be pushed.
 */
void AlarmSystemInterfaceProxy::push(FaultState & state)
{
	cout << "AlarmSystemInterfaceProxy::push(FaultState): entering...\n";
	// create a vector and populate with the (single) fault state, 
	// to be passed to the buildMessageXML method

	vector<FaultState> states;
	states.push_back(state);

	commonPush(states, false);
	cout << "AlarmSystemInterfaceProxy::push(FaultState): exiting...\n";
}

/**
 * Push the set of active fault states.
 * @param activeFaults the active fault states.
 *
 * TODO later:
 * @throws ASIException if the fault state active list can not be pushed.
 */
void AlarmSystemInterfaceProxy::pushActiveList(vector<FaultState> & activeFaults)
{
	cout << "AlarmSystemInterfaceProxy::pushActiveList(): entering...\n";
	push(activeFaults);
	cout << "AlarmSystemInterfaceProxy::pushActiveList(): exiting...\n";
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
bool AlarmSystemInterfaceProxy::publishMessageDLL(ASIMessage msg)
{
	cout << "AlarmSystemInterfaceProxy::publishMessageDLL(): entering...\n";
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
			cerr << "FATAL error: could not open DLL; error was:\n\n" << dlerror() << std::endl; 
			exit(-1);
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
			cerr << "FATAL error: could not get publisher from DLL; error was:\n\n" << dlerror() << std::endl; 
			exit(-1);
		}
	}

	// publish the alarm via the class/method loaded from the DLL
	laserPublisher->publishAlarm(msg);

	cout << "AlarmSystemInterfaceProxy::publishMessageDLL(): exiting...\n";
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
void AlarmSystemInterfaceProxy::commonPush(vector<FaultState> & states, bool backup)
{
	cout << "AlarmSystemInterfaceProxy::commonPush(): entering...\n";
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
	asiMessage.setSourceHostname(hostname);

	// set the ASIMessage's backup flag
	asiMessage.setBackup(backup);

	// set the ASIMessage's version
	asiMessage.setVersion(configuration.getASIVersion());
	
	// publish the ASIMessage to the alarm server
	publishMessageDLL(asiMessage);
	//publishMessage(asiMessage);
	cout << "AlarmSystemInterfaceProxy::commonPush(): exiting...\n";
}

// initialization logic used by the constructors
void AlarmSystemInterfaceProxy::init()
{
	cout << "AlarmSystemInterfaceProxy::init(): entering...\n";
	// TODO later: portability/platform-specific issues with using gethostname()?
	char name[MAXHOSTNAMELEN + 1];
	gethostname(name, MAXHOSTNAMELEN);
	string nameStr(name);
	hostname = (nameStr);
	cout << "AlarmSystemInterfaceProxy::init(): exiting...\n";
}
