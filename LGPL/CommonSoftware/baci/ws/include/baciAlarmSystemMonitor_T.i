
/**
 * %todo GCH: Alarm System is not yet supportedin VxWorks
 *            Therefore I cut the calls out for the time being.
 */
#ifndef MAKE_VXWORKS
#include <ACSAlarmSystemInterface.h>
#include <faultStateConstants.h>
#include "ACSAlarmSystemInterfaceFactory.h"
#include "acsErrTypeAlarmSourceFactory.h"
#endif

/*********************************** IMPLEMENTATION of AlarmSystemMonitor */
template<class TPROP>
AlarmSystemMonitor<TPROP>::AlarmSystemMonitor(TPROP * property, EventDispatcher * eventDispatcher) :
    eventDispatcher_mp(eventDispatcher),  property_mp(property), alarmRaised_m(0)
{
    ACS_TRACE("baci::AlarmSystemMonitor&lt;&gt;::AlarmSystemMonitor");

    try
	{
/**
  * %todo GCH: Alarm System is not yet supportedin VxWorks
  *            Therefore I cut the calls out for the time being.
  */
#ifndef MAKE_VXWORKS
	this->alarmSource_map = ACSAlarmSystemInterfaceFactory::createSource(property_mp->name());
#endif
	} catch (acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl &ex) {
/* %tod: throw new exception
		 std::string procName="ROcommonImpl::ROcommonImpl(";
		 procName+=name.c_str();
		 procName+=",...)";
		 baciErrTypeProperty::PropertySetInitValueExImpl newEx(ex.getErrorTrace(),__FILE__,__LINE__,procName.c_str());
		 newEx.addData("Property",name.c_str());
		 throw newEx;
*/
	throw;
	}//try-catch

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
	auto_ptr<acsalarm::ACSFaultState> fs  = ACSAlarmSystemInterfaceFactory::createFaultState(family,member,code);
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
	alarmSource_map->push(*fs);
#endif
}
