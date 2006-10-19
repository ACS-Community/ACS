
#include "AbstractAlarmSystemInterface.h"
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
}

/**
 * Destructor
 */
CERNAlarmSystemInterfaceFactory::~CERNAlarmSystemInterfaceFactory()
{
	done();
}

/**
 * Init the object of the class: must be called before using the other
 * methods of this class otherwise an exception will be thrown.
 * Return true if the initialization went ok
 */
bool CERNAlarmSystemInterfaceFactory::init()
{
	return true;
}
	
/**
 * Release the resources: must be called when finished using the
 * methods of this class
 */
void CERNAlarmSystemInterfaceFactory::done()
{
}

/**
 * Create a new instance of an alarm system interface.
 * @param sourceName the source name.
 * @return the interface instance.
 * @throws ASIException if the AlarmSystemInterface instance can not be created.
 */
auto_ptr<AbstractAlarmSystemInterface> CERNAlarmSystemInterfaceFactory::createSource(string sourceName)
{
	AbstractAlarmSystemInterface * asIfProxyPtr = new CERNAlarmSystemInterfaceProxy(sourceName);
	auto_ptr<AbstractAlarmSystemInterface> asIfAutoPtr(asIfProxyPtr);
	return asIfAutoPtr;
}
	
/**
 * Create a new instance of an alarm system interface binding it to the default source source.
 * @return the interface instance.
 * @throws ASIException if the AlarmSystemInterface instance can not be created.
 */
auto_ptr<AbstractAlarmSystemInterface> CERNAlarmSystemInterfaceFactory::createSource()
{
	return createSource(ALARM_SOURCE_NAME);
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
