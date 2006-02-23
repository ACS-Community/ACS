#include <iostream>
#include "AcsAlarmPublisher.h"
#include "ACSJMSMessageEntityS.h"
#include "acsncORBHelper.h"

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
AcsAlarmPublisher::AcsAlarmPublisher(string topicName)
{
	cout << "AcsAlarmPublisher::AcsAlarmPublisher(): entering...\n";
	// create the shared AlarmSupplier
  	ACE_Guard<ACE_Mutex> guard(*AcsAlarmPublisher::alarmSupplierMutex);
	if(NULL == getAlarmSupplier())
	{
		cout << "AcsAlarmPublisher::AcsAlarmPublisher(): about to instantiate the alarm supplier.\n";
		setAlarmSupplier(new AlarmSupplier(topicName.c_str()));
		cout << "AcsAlarmPublisher::AcsAlarmPublisher(): instantiated the alarm supplier\n";
		initializeCorbaServices();
	}
	cout << "AcsAlarmPublisher::AcsAlarmPublisher(): exiting...\n";
}

/*
 * Destructor. Cleans up the shared SimpleSupplier instance.
 */
AcsAlarmPublisher::~AcsAlarmPublisher()
{
	cout << "AcsAlarmPublisher::~AcsAlarmPublisher(): entering...\n";
	if(NULL != getAlarmSupplier())
	{
		// disconnect the AlarmSupplier.
		getAlarmSupplier()->disconnect();
		setAlarmSupplier(NULL);
		// NOTE: delete should not be called on the alarmSupplier
		// see, e.g., http://almasw.hq.eso.org/almasw/bin/view/ACS/FAQCppCompPropertyDestroy
	}

	delete AcsAlarmPublisher::singletonMutex;
	delete AcsAlarmPublisher::alarmSupplierMutex;
	singletonInstance = NULL;
	cout << "AcsAlarmPublisher::~AcsAlarmPublisher(): exiting...\n";
}

/*
 * Returns the singleton instance, creating it if necessary.
 */
AlarmPublisher* AcsAlarmPublisher::getInstance(string topicName)
{
	cout << "AcsAlarmPublisher::getInstance(): entering...\n";
  	ACE_Guard<ACE_Mutex> guard(*AcsAlarmPublisher::singletonMutex);
	if(NULL == singletonInstance)
	{
		singletonInstance = new AcsAlarmPublisher(topicName);
	}
	return singletonInstance;
	cout << "AcsAlarmPublisher::getInstance(): exiting...\n";
}

/*
 * Public method to publish an alarm to the laser alarm server.
 */
bool AcsAlarmPublisher::publishAlarm(ASIMessage msg)
{
	if(NULL != getAlarmSupplier())
	{
		getAlarmSupplier()->publishEvent(msg);
	}

	return true;
}

/*
 * Private method for initialization of CORBA notification channel services.
 */
void AcsAlarmPublisher::initializeCorbaServices()
{
	cout << "AcsAlarmPublisher::initializeCorbaServices(): entering\n";

	// start an ORB 
	nc::ORBHelper *orbHelper = new nc::ORBHelper();
	orbHelper->runOrb();

	// get the naming service/context
	CORBA::ORB_ptr orb = orbHelper->getORB();
	CORBA::Object_var naming_obj = orb->resolve_initial_references ("NameService");
	CosNaming::NamingContext_var naming_context = CosNaming::NamingContext::_narrow(naming_obj.in());
	
	// initialize the AlarmSupplier with the naming context
	getAlarmSupplier()->init(naming_context.in());

	// delete the orb helper
	delete orbHelper;
	cout << "AcsAlarmPublisher::initializeCorbaServices(): exiting\n";
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
	AlarmPublisher * getAlarmPublisher(string topicName)
	{
		cout<< "extern C getAlarmPublisher(): DLL entry point, entering...\n";
		AlarmPublisher * retVal = AcsAlarmPublisher::getInstance(topicName);
		cout << "extern C getAlarmPublisher(): DLL entry point, exiting...\n";
		return retVal;
	}
};
