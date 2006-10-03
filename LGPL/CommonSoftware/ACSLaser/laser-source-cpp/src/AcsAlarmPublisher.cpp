#include "AcsAlarmPublisher.h"
#include "ACSJMSMessageEntityS.h"
#include <orbsvcs/CosNotifyChannelAdminS.h>
#include <orbsvcs/CosNotifyCommC.h>
#include <orbsvcs/CosNamingC.h>
#include <logging.h>
#include <acsncHelper.h>

using namespace laserAlarmPublisher;
using nc::Helper;

// initialize the static variables for the singletons, etc.
AlarmPublisher* AcsAlarmPublisher::singletonInstance = NULL;
AlarmSupplier* AcsAlarmPublisher::alarmSupplier = NULL;
ACE_Mutex* AcsAlarmPublisher::singletonMutex = new ACE_Mutex;
ACE_Mutex* AcsAlarmPublisher::alarmSupplierMutex = new ACE_Mutex;

/*
 * Constructor.
 * @param topicName the name of the topic for the notification channel which will 
 *        be used for communication with the laser alarm server.
 */
AcsAlarmPublisher::AcsAlarmPublisher(string topicName)
{ 
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AcsAlarmPublisher::AcsAlarmPublisher(): entering.");

	// create the shared AlarmSupplier
  	ACE_Guard<ACE_Mutex> guard(*AcsAlarmPublisher::alarmSupplierMutex);
	if(NULL == getAlarmSupplier())
	{
		myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AcsAlarmPublisher::AcsAlarmPublisher(): about to instantiate the alarm supplier.");
		setAlarmSupplier(new AlarmSupplier(topicName.c_str()));
		myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AcsAlarmPublisher::AcsAlarmPublisher(): instantiated the alarm supplier.");

		// initialize the AlarmSupplier with the naming context
		CosNaming::NamingContext_ptr naming_p = Helper::resolveNamingServiceStatic(NULL, string("AlarmSystem"));
		if(CORBA::is_nil(naming_p)) {
			myLoggerSmartPtr->log(Logging::Logger::LM_ERROR, "AcsAlarmPublisher::AcsAlarmPublisher(): naming_p was nil.");
		}
		else {
			myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AcsAlarmPublisher::AcsAlarmPublisher(): naming_p was not nil.");
		}
		getAlarmSupplier()->init(naming_p);
		myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AcsAlarmPublisher::AcsAlarmPublisher(): init called on alarm supplier.");
	}
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AcsAlarmPublisher::AcsAlarmPublisher(): exiting.");
}

/*
 * Destructor. Cleans up the shared SimpleSupplier instance.
 */
AcsAlarmPublisher::~AcsAlarmPublisher()
{
	// TODO: Rethink this. May need something more along the lines of
	// a reference counting scheme rather than a singleton(?) - for now, we have the destructor
	// deleting everything each time, making the singleton a bit dubious. This is a temporary fix
	// to get the ACS-5.0.4 patch done on time. What we probably really want is something that hangs around
	// until *all* clients of it are finished (hence the reference counting suggestion), and then (and only then)
	// releases all the dynamically allocated resources.

	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AcsAlarmPublisher::~AcsAlarmPublisher(): entering.");
	if(NULL != getAlarmSupplier())
	{
		// disconnect the AlarmSupplier.
		getAlarmSupplier()->disconnect();
		setAlarmSupplier(NULL);
	}

	delete AcsAlarmPublisher::singletonMutex;
	AcsAlarmPublisher::singletonMutex = NULL;

	delete AcsAlarmPublisher::alarmSupplierMutex;
	AcsAlarmPublisher::alarmSupplierMutex = NULL;

	singletonInstance = NULL;
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AcsAlarmPublisher::~AcsAlarmPublisher(): exiting.");
}

/*
 * Returns the singleton instance, creating it if necessary.
 */
AlarmPublisher* AcsAlarmPublisher::getInstance(string topicName)
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AcsAlarmPublisher::getInstance(): entering.");

	if(NULL == singletonMutex) {
		singletonMutex = new ACE_Mutex;
	}
	if(NULL == alarmSupplierMutex) {
		alarmSupplierMutex = new ACE_Mutex;
	}

  	ACE_Guard<ACE_Mutex> guard(*AcsAlarmPublisher::singletonMutex);
	if(NULL == singletonInstance)
	{
		singletonInstance = new AcsAlarmPublisher(topicName);
	}


	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AcsAlarmPublisher::getInstance(): exiting.");
	return singletonInstance;
}

/*
 * Public method to publish an alarm to the laser alarm server.
 */
bool AcsAlarmPublisher::publishAlarm(ASIMessage msg)
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AcsAlarmPublisher::publishAlarm(): entering.");
	if(NULL != getAlarmSupplier())
	{
		getAlarmSupplier()->publishEvent(msg);
	} else {
		// TODO: throw an exception here?
		myLoggerSmartPtr->log(Logging::Logger::LM_ERROR, "AcsAlarmPublisher::publishAlarm(): alarm not published; alarm supplier is null.");
	}
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AcsAlarmPublisher::publishAlarm(): exiting.");
	return true;
}


/*
 * Simple factory method to return an instance of AlarmPublisher as an entry point 
 * for clients of the shared library. Other implementations (i.e. different concrete classes which
 * extend the abstract base class, AlarmPublisher) would return instances of different concrete classes
 * in their corresponding factory methods; that is, to implement a new concrete AlarmPublisher class, 
 * extend AlarmPublisher, implement the abstract publishAlarm method, and create a factory method
 * like the one below which returns an instance of *your* concrete class.
 */
extern "C" 
{
	AlarmPublisher * getAlarmPublisher(string topicName)
	{
		Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
		myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AcsAlarmPublisher::getAlarmPublisher(): entering.");
		AlarmPublisher * retVal = AcsAlarmPublisher::getInstance(topicName);
		myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AcsAlarmPublisher::getAlarmPublisher(): exiting.");
		return retVal;
	}
};
