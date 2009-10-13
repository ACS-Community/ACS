
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


/*********************************** IMPLEMENTATION of AlarmSystemMonitor */
template<class TPROP>
baci::AlarmSystemMonitor<TPROP>::AlarmSystemMonitor(TPROP * property, EventDispatcher * eventDispatcher) :
    baci::AlarmSystemMonitorBase(eventDispatcher),  property_mp(property)
{
    ACS_TRACE("baci::AlarmSystemMonitor&lt;&gt;::AlarmSystemMonitor");

#ifndef MAKE_VXWORKS
    try
    {
    	//here we have access to the property so we can set several things
       	faultFamily_m = property->getAlarmFaultFamily();
    	faultMember_m = property->getAlarmFaultMember();

    	alarmLevel_m = property->getAlarmLevel();

    	std::ostringstream ostr;
    	ostr << this->alarmLevel_m << std::ends;
    	std::string tstr = ostr.str();
    	this->setProperty("BACI_Level", tstr.c_str());

    	this->setProperty("BACI_Property", property->name());
    	this->setProperty("BACI_Description", property->description());

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

//    subscribe to event dispatcher
    eventDispatcher_mp->subscribe(this);
}//AlarmSystemMonitor

template<class TPROP>
baci::AlarmSystemMonitor<TPROP>::~AlarmSystemMonitor()
{
    ACS_TRACE("baci::AlarmSystemMonitor&lt;&gt;::~AlarmSystemMonitor");
//    unsubscribe to event dispatcher
    eventDispatcher_mp->unsubscribe(this);
}//~AlarmSystemMonitor
