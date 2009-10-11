#include <memory>
#include <unistd.h>
#include <sys/param.h>
#include <dlfcn.h>
#include "CERNAlarmSystemInterfaceProxy.h"
#include "asiConfigurationConstants.h"
#include "AcsAlarmPublisher.h"
#include "logging.h"

using asiConfigurationConstants::ALARM_SOURCE_NAME;
using std::string;
using laserSource::CERNAlarmSystemInterfaceProxy;
using acsalarm::ASIMessage;

/*
 * Default no-args constructor.
 */
CERNAlarmSystemInterfaceProxy::CERNAlarmSystemInterfaceProxy()
{
	ACS_TRACE("CERNAlarmSystemInterfaceProxy::CERNAlarmSystemInterfaceProxy()");
	setSourceName(ALARM_SOURCE_NAME);
	init();
}

/*
 * Constructor.
 * @param theSourceName the name of the source. This should normally be the
 * one (and only one) source name defined in asiConfigurationConstants.h ALARM_SOURCE_NAME.
 */
CERNAlarmSystemInterfaceProxy::CERNAlarmSystemInterfaceProxy(string theSourceName)
{
	ACS_TRACE("CERNAlarmSystemInterfaceProxy::CERNAlarmSystemInterfaceProxy(string)");
	string expectedSrcName(ALARM_SOURCE_NAME);
	if(theSourceName != expectedSrcName)
	{
		string logString =  "CERNAlarmSystemInterfaceProxy::CERNAlarmSystemInterfaceProxy(string) all should use " + expectedSrcName+" as source (source "+theSourceName+" forced to "+ALARM_SOURCE_NAME+")";
		ACS_SHORT_LOG((LM_WARNING, logString.c_str()));
		theSourceName=expectedSrcName;
	}
	setSourceName(theSourceName);
	init();
}

/*
 * Destructor.
 */
CERNAlarmSystemInterfaceProxy::~CERNAlarmSystemInterfaceProxy()
{
	ACS_TRACE("CERNAlarmSystemInterfaceProxy::~CERNAlarmSystemInterfaceProxy()");
	if (laserPublisher != NULL) {
		delete laserPublisher;
		laserPublisher = NULL;
	}
}

// initialization logic
void CERNAlarmSystemInterfaceProxy::init()
{
	ACS_TRACE("AlarmSystemInterface::init()");

	laserPublisher = NULL; 

	// TODO later: portability/platform-specific issues with using gethostname()?
	char name[MAXHOSTNAMELEN + 1];
	gethostname(name, MAXHOSTNAMELEN);
	string nameStr(name);
	hostName = (nameStr);

}

// cleanup logic
void CERNAlarmSystemInterfaceProxy::close()
{
	ACS_TRACE("AlarmSystemInterface::close()");
}

/*
 * Sends a message to the alarm server.
 *
 * TODO later: "syncbuffer" for maintaining active list, etc.
 */
bool CERNAlarmSystemInterfaceProxy::publishMessage(ASIMessage msg)
{
	ACS_TRACE("CERNAlarmSystemInterfaceProxy::publishMessage()");
	bool retVal = false;

	// create the topic on which to publish the alarm, by appending
	// the source name to the topic prefix provided by the configuration
	// (should look something like: CMW.ALARM_SYSTEM.ALARMS.SOURCES.ALARM_SYSTEM_SOURCES)
	string topicName(configuration.getAlarmsTopic());
	topicName.append(".");
	topicName.append(msg.getSourceName());

	if(laserPublisher == NULL)
	{
		laserPublisher = new AcsAlarmPublisher(topicName);
	}

	// publish the alarm 
	laserPublisher->publishAlarm(msg);

	return retVal;
}

