
/**
 * %todo GCH: Alarm System is not yet supportedin VxWorks
 *            Therefore I cut the calls out for the time being.
 */
#ifndef MAKE_VXWORKS
#include <AlarmSystemInterface.h>
#include <faultStateConstants.h>
#include "ACSAlarmSystemInterfaceFactory.h"
#include "acsErrTypeAlarmSourceFactory.h"
#endif

#include "baciAlarmSystemMonitorBase.h"


/*********************************** IMPLEMENTATION of AlarmSystemMonitorBase */
baci::AlarmSystemMonitorBase::AlarmSystemMonitorBase(EventDispatcher * eventDispatcher) :
    eventDispatcher_mp(eventDispatcher),  alarmRaised_m(0)
{
	faultFamily_m = "BACIProperty";
	faultMember_m = "";
/**
  * %todo GCH: Alarm System is not yet supported in VxWorks
  *            Therefore I cut the calls out for the time being.
  */
#ifndef MAKE_VXWORKS
    try
    {
    	this->alarmSource_map = ACSAlarmSystemInterfaceFactory::createSource("ALARM_SYSTEM_SOURCES");
    } catch (acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl &ex) {
    	/* %todo: throw new exception
		 std::string procName="ROcommonImpl::ROcommonImpl(";
		 procName+=name.c_str();
		 procName+=",...)";
		 baciErrTypeProperty::PropertySetInitValueExImpl newEx(ex.getErrorTrace(),__FILE__,__LINE__,procName.c_str());
		 newEx.addData("Property",name.c_str());
		 throw newEx;
    	 */
    	throw;
    }//try-catch
#endif
}//AlarmSystemMonitorBase

baci::AlarmSystemMonitorBase::~AlarmSystemMonitorBase()
{
}//~AlarmSystemMonitorBase

void baci::AlarmSystemMonitorBase::setProperty(const char *name, const char *prop)
{
	this->alarmProperties_m[name] = prop;
}//setProperty


void baci::AlarmSystemMonitorBase::setFaultFamily(const char *ff)
{
	ACE_TRACE("baci::AlarmSystemMonitorBase::setFaultFamily");
	ACE_Guard<ACE_Recursive_Thread_Mutex>  protSect(faultStructMutex_m);

	// first we have to clear actual alarm if there is any
	if (this->alarmRaised_m !=0)
	{
		clearAlarm();
		this->faultFamily_m = ff;
		recheckAlarm();
	}
	else
	{
		this->faultFamily_m = ff;
	}//if-else
}//setFaultFamily

void baci::AlarmSystemMonitorBase::setFaultMember(const char *fm)
{
	ACE_TRACE("baci::AlarmSystemMonitorBase::setFaultMember");
	ACE_Guard<ACE_Recursive_Thread_Mutex>  protSect(faultStructMutex_m);

	// first we have to clear actual alarm if there is any
	if (this->alarmRaised_m !=0)
	{
		clearAlarm();
		this->faultMember_m = fm;
		recheckAlarm();
	}
	else
	{	this->faultMember_m = fm;

	}
}//setFaultMember

void baci::AlarmSystemMonitorBase::clearAlarm()
{
	sendAlarm(lastAlarmFaultCode_m, false);
	alarmRaised_m = 0;
}//clearAlarm

void baci::AlarmSystemMonitorBase::recheckAlarm()
{
	ACSErr::Completion c;
	ACS::CBDescOut desc;
	check(lastAlarmValue_m, c, desc);
}//recheckAlarm

void baci::AlarmSystemMonitorBase::sendAlarm(int code, bool active) {
/**
 * %todo GCH: Alarm System is not yet supported in VxWorks
 *            Therefore I cut the calls out for the time being.
 */
#ifndef MAKE_VXWORKS
	// Create the fault state
	auto_ptr<acsalarm::FaultState> fs  = ACSAlarmSystemInterfaceFactory::createFaultState(faultFamily_m, faultMember_m, code);
	if (active) {
		fs->setDescriptor(faultState::ACTIVE_STRING);
	} else {
		fs->setDescriptor(faultState::TERMINATE_STRING);
	}
	// create a Timestamp and use it to configure the FaultState
	acsalarm::Timestamp * tstampPtr = new acsalarm::Timestamp();
	auto_ptr<acsalarm::Timestamp> tstampAutoPtr(tstampPtr);
	fs->setUserTimestamp(tstampAutoPtr);

	// create a Properties object and configure it, then assign to the FaultState
	//BJE could be better to create/set properties just once.
	// If the interface allows to reset the value of a certain property, for example Value
	acsalarm::Properties * propsPtr = new acsalarm::Properties();

	for( std::map<std::string, std::string>::iterator prop = alarmProperties_m.begin() ; prop != alarmProperties_m.end(); ++prop )
	{
		propsPtr->setProperty(prop->first,  prop->second);
	}//for

	auto_ptr<acsalarm::Properties> propsAutoPtr(propsPtr);
	fs->setUserProperties(propsAutoPtr);
	lastAlarmFaultCode_m = code;
	alarmSource_map->push(*fs);
#endif
}//sendAlarm
