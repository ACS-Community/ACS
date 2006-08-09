#include <iostream>
#include "AcsAlarmPublisher.h"
#include "ACSJMSMessageEntityS.h"
#include "acsncORBHelper.h"
#include <orbsvcs/CosNotifyChannelAdminS.h>
#include <orbsvcs/CosNotifyCommC.h>
#include <orbsvcs/CosNamingC.h>

using std::cout;
using namespace laserAlarmPublisher;

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
AcsAlarmPublisher::AcsAlarmPublisher(string topicName,CosNaming::NamingContext_ptr naming_p)
{
	cout << "AcsAlarmPublisher::AcsAlarmPublisher(): entering...\n";
	// create the shared AlarmSupplier
  	ACE_Guard<ACE_Mutex> guard(*AcsAlarmPublisher::alarmSupplierMutex);
	if(NULL == getAlarmSupplier())
	{
		cout << "AcsAlarmPublisher::AcsAlarmPublisher(): about to instantiate the alarm supplier.\n";
		setAlarmSupplier(new AlarmSupplier(topicName.c_str()));
		cout << "AcsAlarmPublisher::AcsAlarmPublisher(): instantiated the alarm supplier\n";
		// initialize the AlarmSupplier with the naming context
		getAlarmSupplier()->init(naming_p);
	}
	cout << "AcsAlarmPublisher::AcsAlarmPublisher(): exiting...\n";
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

	cout << "STEVE::AcsAlarmPublisher::~AcsAlarmPublisher(): entering...\n";
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
	cout << "STEVE::AcsAlarmPublisher::~AcsAlarmPublisher(): exiting...\n";
}

/*
 * Returns the singleton instance, creating it if necessary.
 */
AlarmPublisher* AcsAlarmPublisher::getInstance(string topicName,CosNaming::NamingContext_ptr naming_p)
{
	cout << "AcsAlarmPublisher::getInstance("<<topicName<<"): entering...\n";

	if(NULL == singletonMutex) {
		singletonMutex = new ACE_Mutex;
	}
	if(NULL == alarmSupplierMutex) {
		alarmSupplierMutex = new ACE_Mutex;
	}

  	ACE_Guard<ACE_Mutex> guard(*AcsAlarmPublisher::singletonMutex);
	if(NULL == singletonInstance)
	{
		singletonInstance = new AcsAlarmPublisher(topicName,naming_p);
	}

	cout << "AcsAlarmPublisher::getInstance(): exiting...\n";
	return singletonInstance;
}

/*
 * Public method to publish an alarm to the laser alarm server.
 */
bool AcsAlarmPublisher::publishAlarm(ASIMessage msg)
{
	cout<<"AcsAlarmPublisher::publishAlarm(): entering\n";
	if(NULL != getAlarmSupplier())
	{
		getAlarmSupplier()->publishEvent(msg);
	} else {
		cout<<"Alarm not published: alarm supplier is NULL!!!\n";
	}
	cout<<"AcsAlarmPublisher::publishAlarm(): exiting\n";
	return true;
}


/*
 * Simple factory method to return an instance of AlarmPublisher as an entry point 
 * for clients of the shared library. Other implementations (i.e. different concrete classes which
 * extend the abstract base class, AlarmPublisher) would return instances of different concrete classes
 * in their factory corresponding methods; that is, to implement a new concrete AlarmPublisher class, 
 * extend AlarmPublisher, implement the abstract publishAlarm method, and create a factory method
 * like the one below which returns an instance of *your* concrete class.
 */
extern "C" 
{
	AlarmPublisher * getAlarmPublisher(string topicName,CosNaming::NamingContext_ptr naming_p)
	{
		cout<< "extern C getAlarmPublisher(): DLL entry point, entering...\n";
		AlarmPublisher * retVal = AcsAlarmPublisher::getInstance(topicName,naming_p);
		cout << "extern C getAlarmPublisher(): DLL entry point, exiting...\n";
		return retVal;
	}
};
