
#include "AlarmSystemInterface.h"
#include "CERNAlarmSystemInterfaceProxy.h"
#include "CERNAlarmSystemInterfaceFactory.h"
#include "asiConfigurationConstants.h"

using asiConfigurationConstants::ALARM_SOURCE_NAME;
using namespace acsalarm;
using namespace laserSource;

/**
 * Constructor
 */
CERNAlarmSystemInterfaceFactory::CERNAlarmSystemInterfaceFactory()
{
	myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceFactory::constructor(): entering.");
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceFactory::constructor(): exiting.");
}

/**
 * Destructor
 */
CERNAlarmSystemInterfaceFactory::~CERNAlarmSystemInterfaceFactory()
{
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceFactory::destructor(): entering.");
	done();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceFactory::destructor(): exiting.");
}

/**
 * Init the object of the class: must be called before using the other
 * methods of this class otherwise an exception will be thrown.
 * Return true if the initialization went ok
 */
bool CERNAlarmSystemInterfaceFactory::init()
{
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceFactory::init(): entering.");
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceFactory::init(): exiting.");
	return true;
}
	
/**
 * Release the resources: must be called when finished using the
 * methods of this class
 */
void CERNAlarmSystemInterfaceFactory::done()
{
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceFactory::done(): entering.");
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceFactory::done(): exiting.");
}

/**
 * Create a new instance of an alarm system interface.
 * @param sourceName the source name.
 * @return the interface instance.
 * @throws ASIException if the AlarmSystemInterface instance can not be created.
 */
auto_ptr<AlarmSystemInterface> CERNAlarmSystemInterfaceFactory::createSource(string sourceName)
{
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceFactory::createSource(): entering.");

	auto_ptr<AlarmSystemInterface> asIfAutoPtr(new CERNAlarmSystemInterfaceProxy(sourceName));

	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceFactory::createSource(): exiting.");
	return asIfAutoPtr;
}
	
/**
 * Create a new instance of an alarm system interface binding it to the default source source.
 * @return the interface instance.
 * @throws ASIException if the AlarmSystemInterface instance can not be created.
 */
auto_ptr<AlarmSystemInterface> CERNAlarmSystemInterfaceFactory::createSource()
{
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceFactory::createSource(): entering.");
	return createSource(ALARM_SOURCE_NAME);
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceFactory::createSource(): entering.");
}

/*
 * Simple factory method to return an instance of CERNAlarmSystemInterfaceFactory as an entry point 
 * for clients of the shared library. 
 */
extern "C" 
{
	CERNAlarmSystemInterfaceFactory * getAlarmSystemInterfaceFactory()
	{
		Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
		myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceFactory::getAlarmSystemInterface(): entering.");
		CERNAlarmSystemInterfaceFactory * retVal = new CERNAlarmSystemInterfaceFactory();
		myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "CERNAlarmSystemInterfaceFactory::getAlarmSystemInterface(): exiting.");
		return retVal;
	}
};

