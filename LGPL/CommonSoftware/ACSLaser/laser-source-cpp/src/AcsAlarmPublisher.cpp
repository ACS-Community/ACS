#include "AcsAlarmPublisher.h"
#include "ACSJMSMessageEntityS.h"
#include <orbsvcs/CosNamingC.h>
#include <logging.h>
#include <acsncHelper.h>
#include <ACSAlarmSystemInterfaceFactory.h>

using acsalarm::ASIMessage;
using nc::Helper;
using laserSource::AcsAlarmPublisher;

/*
 * Constructor.
 * @param topicName the name of the topic for the notification channel which will 
 *        be used for communication with the laser alarm server.
 */
AcsAlarmPublisher::AcsAlarmPublisher(std::string topicName)
{ 
	myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AcsAlarmPublisher::AcsAlarmPublisher(): entering.");


	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AcsAlarmPublisher::AcsAlarmPublisher(): about to instantiate the alarm supplier.");
	alarmSupplier = new AlarmSupplier(topicName.c_str());
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AcsAlarmPublisher::AcsAlarmPublisher(): instantiated the alarm supplier.");

	// initialize the AlarmSupplier with the naming context
	maci::Manager_ptr mgr = ACSAlarmSystemInterfaceFactory::getManager();
  	CORBA::Object_var namingObj = mgr->get_service(0, "NameService", true);
	CosNaming::NamingContext_var naming_context = CosNaming::NamingContext::_narrow(namingObj.ptr());
  	CosNaming::NamingContext_ptr naming_p = naming_context.ptr();
		
	if(CORBA::is_nil(naming_p)) {
		myLoggerSmartPtr->log(Logging::Logger::LM_ERROR, "AcsAlarmPublisher::AcsAlarmPublisher(): naming_p was nil.");
	}
	else {
		myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AcsAlarmPublisher::AcsAlarmPublisher(): naming_p was not nil.");
	}
	alarmSupplier->init(naming_p);
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AcsAlarmPublisher::AcsAlarmPublisher(): init called on alarm supplier.");
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AcsAlarmPublisher::AcsAlarmPublisher(): exiting.");
}

/*
 * Destructor. Cleans up the shared SimpleSupplier instance.
 */
AcsAlarmPublisher::~AcsAlarmPublisher()
{
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AcsAlarmPublisher::~AcsAlarmPublisher(): entering.");
	if(NULL != alarmSupplier)
	{
		// disconnect the AlarmSupplier.
		alarmSupplier->disconnect();
		delete(alarmSupplier);
		alarmSupplier = NULL;
	}
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AcsAlarmPublisher::~AcsAlarmPublisher(): exiting.");
}

/*
 * Public method to publish an alarm to the laser alarm server.
 */
bool AcsAlarmPublisher::publishAlarm(ASIMessage msg)
{
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AcsAlarmPublisher::publishAlarm(): entering.");
	alarmSupplier->publishEvent(msg);
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AcsAlarmPublisher::publishAlarm(): exiting.");
	return true;
}
