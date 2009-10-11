
#include "AlarmSystemInterface.h"
#include "CERNAlarmSystemInterfaceProxy.h"
#include "CERNAlarmSystemInterfaceFactory.h"
#include "asiConfigurationConstants.h"

using asiConfigurationConstants::ALARM_SOURCE_NAME;
using std::string;
using std::auto_ptr;
using laserSource::CERNAlarmSystemInterfaceFactory;
using acsalarm::AlarmSystemInterface;

/**
 * Constructor
 */
CERNAlarmSystemInterfaceFactory::CERNAlarmSystemInterfaceFactory()
{
	ACS_TRACE("CERNAlarmSystemInterfaceFactory::constructor()");
}

/**
 * Destructor
 */
CERNAlarmSystemInterfaceFactory::~CERNAlarmSystemInterfaceFactory()
{
	ACS_TRACE("CERNAlarmSystemInterfaceFactory::destructor()");
	done();
}

/**
 * Init the object of the class: must be called before using the other
 * methods of this class otherwise an exception will be thrown.
 * Return true if the initialization went ok
 */
bool CERNAlarmSystemInterfaceFactory::init()
{
	ACS_TRACE("CERNAlarmSystemInterfaceFactory::init()");
	return true;
}
	
/**
 * Release the resources: must be called when finished using the
 * methods of this class
 */
void CERNAlarmSystemInterfaceFactory::done()
{
	ACS_TRACE("CERNAlarmSystemInterfaceFactory::done()");
}

/**
 * Create a new instance of an alarm system interface.
 * @param sourceName the source name.
 * @return the interface instance.
 * @throws ASIException if the AlarmSystemInterface instance can not be created.
 */
AlarmSystemInterface* CERNAlarmSystemInterfaceFactory::createSource(string sourceName)
{
	ACS_TRACE("CERNAlarmSystemInterfaceFactory::createSource()");
	return new CERNAlarmSystemInterfaceProxy(sourceName);
}
	
/**
 * Create a new instance of an alarm system interface binding it to the default source source.
 * @return the interface instance.
 * @throws ASIException if the AlarmSystemInterface instance can not be created.
 */
AlarmSystemInterface* CERNAlarmSystemInterfaceFactory::createSource()
{
	ACS_TRACE("CERNAlarmSystemInterfaceFactory::createSource()");
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
		ACS_TRACE("CERNAlarmSystemInterfaceFactory::getAlarmSystemInterface()");
		CERNAlarmSystemInterfaceFactory * retVal = new CERNAlarmSystemInterfaceFactory();
		return retVal;
	}
};

