
/**
 * %todo GCH: Alarm System is not yet supportedin VxWorks
 *            Therefore I cut the calls out for the time being.
 */
#ifdef MAKE_VXWORKS
#else
#import <ACSAlarmSystemInterfaceFactory.h>
#import <ACSFaultState.h>
#import <ACSAlarmSystemInterface.h>
#import <faultStateConstants.h>
#endif


/*********************************** IMPLEMENTATION of AlarmSystemMonitor */
template<class TPROP>
AlarmSystemMonitor<TPROP>::AlarmSystemMonitor(TPROP * property, EventDispatcher * eventDispatcher) :
    eventDispatcher_mp(eventDispatcher),  property_mp(property), alarmRaised_m(0)
{
    ACS_TRACE("baci::AlarmSystemMonitor&lt;&gt;::AlarmSystemMonitor");
//    subscribe to event dispatcher
    eventDispatcher_mp->subscribe(this);
}//AlarmSystemMonitor

template<class TPROP>
AlarmSystemMonitor<TPROP>::~AlarmSystemMonitor()
{
    ACS_TRACE("baci::AlarmSystemMonitor&lt;&gt;::~AlarmSystemMonitor");
//    unsubscribe to event dispatcher
    eventDispatcher_mp->unsubscribe(this);
}//~AlarmSystemMonitor

template<class TPROP>
void AlarmSystemMonitor<TPROP>::sendAlarm(std::string family, std::string member, int code, bool active) {
/**
 * %todo GCH: Alarm System is not yet supportedin VxWorks
 *            Therefore I cut the calls out for the time being.
 */
#ifdef MAKE_VXWORKS
#else
	// Create the fault state
	auto_ptr<laserSource::ACSFaultState> fs  = ACSAlarmSystemInterfaceFactory::createFaultState(family,member,code);
	if (active) {
		fs->setDescriptor(faultState::ACTIVE_STRING);
	} else {
		fs->setDescriptor(faultState::TERMINATE_STRING);
	}
	// create a Timestamp and use it to configure the FaultState
	Timestamp * tstampPtr = new Timestamp();
	auto_ptr<Timestamp> tstampAutoPtr(tstampPtr);
	fs->setUserTimestamp(tstampAutoPtr);
	// create a Properties object and configure it, then assign to the FaultState
	Properties * propsPtr = new Properties();
	propsPtr->setProperty(faultState::ASI_PREFIX_PROPERTY_STRING, "prefix");
	propsPtr->setProperty(faultState::ASI_SUFFIX_PROPERTY_STRING, "suffix");
	propsPtr->setProperty("TEST_PROPERTY", "TEST_VALUE");
	auto_ptr<Properties> propsAutoPtr(propsPtr);
	fs->setUserProperties(propsAutoPtr);
	this->property_mp->getAlarmSource()->push(*fs);
#endif
}
