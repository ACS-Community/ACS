#include "AcsAlarmPublisher.h"
#include "ACSJMSMessageEntityS.h"
#include <orbsvcs/CosNamingC.h>
#include <logging.h>
#include <acsncHelper.h>
#include <ACSAlarmSystemInterfaceFactory.h>

using acsalarm::ASIMessage;
using nc::Helper;
using laserSource::AcsAlarmPublisher;

CosNaming::NamingContext_var AcsAlarmPublisher::naming_v;

/*
 * Constructor.
 * @param topicName the name of the topic for the notification channel which will
 *        be used for communication with the laser alarm server.
 */
AcsAlarmPublisher::AcsAlarmPublisher(std::string topicName)
{
	ACS_TRACE("AcsAlarmPublisher::AcsAlarmPublisher()");

	ACS_SHORT_LOG((LM_DEBUG,"AcsAlarmPublisher::AcsAlarmPublisher(): about to instantiate the alarm supplier."));
	alarmSupplier = new AlarmSupplier(topicName.c_str());
	ACS_SHORT_LOG((LM_DEBUG,"AcsAlarmPublisher::AcsAlarmPublisher(): instantiated the alarm supplier."));

	// initialize the AlarmSupplier with the naming context
	CosNaming::NamingContext_ptr naming_p=getNamingService();
	alarmSupplier->init(naming_p);

	ACS_SHORT_LOG((LM_DEBUG,"AcsAlarmPublisher::AcsAlarmPublisher(): init called on alarm supplier."));
}

/*
 * Destructor. Cleans up the shared SimpleSupplier instance.
 */
AcsAlarmPublisher::~AcsAlarmPublisher()
{
	ACS_TRACE("AcsAlarmPublisher::~AcsAlarmPublisher()");
	if(NULL != alarmSupplier)
	{
		// disconnect the AlarmSupplier.
		alarmSupplier->disconnect();
		delete(alarmSupplier);
		alarmSupplier = NULL;
	}
}

/*
 * Public method to publish an alarm to the laser alarm server.
 */
bool AcsAlarmPublisher::publishAlarm(ASIMessage msg)
{
	ACS_TRACE("AcsAlarmPublisher::publishAlarm()");
	alarmSupplier->publishEvent(msg);
	return true;
}

CosNaming::NamingContext_var AcsAlarmPublisher::getNamingService() {
	ACS_TRACE("AcsAlarmPublisher::getNamingService()");
	if (!CORBA::is_nil(naming_v)) {
		return naming_v;
	}
	maci::Manager_ptr mgr = ACSAlarmSystemInterfaceFactory::getManager();
	CORBA::Object_var namingObj = mgr->get_service(0, "NameService", true);
	naming_v = CosNaming::NamingContext::_narrow(namingObj.ptr());

	if(CORBA::is_nil(naming_v)) {
		ACS_SHORT_LOG((LM_ERROR,"AcsAlarmPublisher::AcsAlarmPublisher(): naming_v was nil."));
	}
	else {
		ACS_SHORT_LOG((LM_DEBUG,"AcsAlarmPublisher::AcsAlarmPublisher(): naming_v was not nil."));
	}
	return AcsAlarmPublisher::naming_v;
}

