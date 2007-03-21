#include <memory>
#include <unistd.h>
#include <sys/param.h>
#include <dlfcn.h>
#include "CERNAlarmSystemInterfaceProxy.h"
#include "asiConfigurationConstants.h"
#include "AcsAlarmPublisher.h"
#include "logging.h"

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
	string expectedSrcName(ALARM_SOURCE_NAME);
	if(theSourceName != expectedSrcName)
	{
		string logString =  "CERNAlarmSystemInterfaceProxy::CERNAlarmSystemInterfaceProxy(string) all should use " + expectedSrcName+" as source (source "+theSourceName+" forced to "+ALARM_SOURCE_NAME+")";
		myLoggerSmartPtr->log(Logging::Logger::LM_WARNING, logString);
		theSourceName=expectedSrcName;
	}
	setSourceName(theSourceName);
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

// initialization logic
void CERNAlarmSystemInterfaceProxy::init()
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AlarmSystemInterface::init(): entering.");

	laserPublisher = NULL; 

	// TODO later: portability/platform-specific issues with using gethostname()?
	char name[MAXHOSTNAMELEN + 1];
	gethostname(name, MAXHOSTNAMELEN);
	string nameStr(name);
	hostName = (nameStr);

	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AlarmSystemInterface::init(): exiting.");
}

// cleanup logic
void CERNAlarmSystemInterfaceProxy::close()
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AlarmSystemInterface::close(): entering.");
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AlarmSystemInterface::close(): exiting.");

}

/*
 * Sends a message to the alarm server.
 *
 * TODO later: "syncbuffer" for maintaining active list, etc.
 */
bool CERNAlarmSystemInterfaceProxy::publishMessage(ASIMessage msg)
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceProxy::publishMessage(): entering.");
	bool retVal = false;

	// create the topic on which to publish the alarm, by appending
	// the source name to the topic prefix provided by the configuration
	// (should look something like: CMW.ALARM_SYSTEM.ALARMS.SOURCES.ALARM_SYSTEM_SOURCES)
	string topicName(configuration.getAlarmsTopic());
	topicName.append(".");
	topicName.append(msg.getSourceName());

	// TODO - can we avoid new'ing this each time?
	laserPublisher = new AcsAlarmPublisher(topicName);

	// publish the alarm 
	laserPublisher->publishAlarm(msg);

	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceProxy::publishMessage(): exiting.");
	return retVal;
}

