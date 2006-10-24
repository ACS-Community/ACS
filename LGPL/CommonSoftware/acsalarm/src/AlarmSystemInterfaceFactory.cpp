#include "AlarmSystemInterfaceFactory.h"
#include "FaultState.h"

using namespace acsalarm;

//TODO: add namespace

/** Factory method for creating FaultState instances.
 * @return a new FaultState instance.
 * @param family the fault family.
 * @param member the fault member.
 * @param code the fault code.
 */
auto_ptr<FaultState> AlarmSystemInterfaceFactory::createFaultState(string family, string member, int code)
{
	FaultState * fsPtr = new FaultState(family, member, code);
	auto_ptr<FaultState> fsAutoPtr(fsPtr);
	return fsAutoPtr;
}

/**
 * Create a fault state 
 * @return a new FaultState instance
 */
auto_ptr<FaultState> AlarmSystemInterfaceFactory::createFaultState()
{
	FaultState * fsPtr = new FaultState();
	auto_ptr<FaultState> fsAutoPtr(fsPtr);
	return fsAutoPtr;
}
