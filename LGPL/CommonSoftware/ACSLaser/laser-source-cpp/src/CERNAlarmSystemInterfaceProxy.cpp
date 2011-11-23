/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) National Research Council of Canada, 2005
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
*/
#include <memory>
#include <unistd.h>
#include <sys/param.h>
#include <dlfcn.h>
#include <acserrACSbaseExImpl.h>
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

	// create the topic on which to publish the alarm, by appending
	// the source name to the topic prefix provided by the configuration
	// (should look something like: CMW.ALARM_SYSTEM.ALARMS.SOURCES.ALARM_SYSTEM_SOURCES)
	string topicName(configuration.getAlarmsTopic());
	topicName.append(".");
	topicName.append(msg.getSourceName());

	// If the laserPublisher has not yet been built then try to build a new one.
	//
	// If an exception happens at this point we log a message but do not
	// propagate the exception to avoid problems to the caller
	// i.e. we do not want that the caller crashes because of a problem in the
	// alarm system
	if(laserPublisher == NULL)
	{
		try {
			laserPublisher = new AcsAlarmPublisher(topicName);
		} catch (ACSErr::ACSbaseExImpl ex) {
			laserPublisher=NULL;
			ex.log();
			return false;;
		} catch (...) {
			laserPublisher=NULL;
			ACS_SHORT_LOG((LM_ERROR,"Error building the AcsAlarmPublisher"))
			return false;
		}
	}

	// publish the alarm
	try {
		laserPublisher->publishAlarm(msg);
	}catch (ACSErr::ACSbaseExImpl ex) {
		laserPublisher=NULL;
		ex.log();
		return false;;
	} catch (...) {
		ACS_SHORT_LOG((LM_ERROR,"Error publishing source alarms %s: ",msg.toXML().c_str()));
		return false;
	}

	return true;
}

