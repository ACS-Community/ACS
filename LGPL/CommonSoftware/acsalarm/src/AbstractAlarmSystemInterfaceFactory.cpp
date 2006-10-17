#include "AbstractAlarmSystemInterfaceFactory.h"
#include "ACSFaultState.h"

using namespace laserSource;

//TODO: add namespace

/** Factory method for creating ACSFaultState instances.
 * @return a new ACSFaultState instance.
 * @param family the fault family.
 * @param member the fault member.
 * @param code the fault code.
 */
auto_ptr<laserSource::ACSFaultState> AbstractAlarmSystemInterfaceFactory::createFaultState(string family, string member, int code)
{
	ACSFaultState * fsPtr = new ACSFaultState(family, member, code);
	auto_ptr<ACSFaultState> fsAutoPtr(fsPtr);
	return fsAutoPtr;
}

/**
 * Create a fault state 
 * @return a new ACSFaultState instance
 */
auto_ptr<laserSource::ACSFaultState> AbstractAlarmSystemInterfaceFactory::createFaultState()
{
	ACSFaultState * fsPtr = new ACSFaultState();
	auto_ptr<ACSFaultState> fsAutoPtr(fsPtr);
	return fsAutoPtr;
}

