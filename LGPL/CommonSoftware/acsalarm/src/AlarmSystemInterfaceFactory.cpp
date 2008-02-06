#include "AlarmSystemInterfaceFactory.h"
#include "FaultState.h"

using std::auto_ptr;
using std::string;
using acsalarm::FaultState;

//TODO: add namespace

/** Factory method for creating FaultState instances.
 * @return a new FaultState instance.
 * @param family the fault family.
 * @param member the fault member.
 * @param code the fault code.
 */
auto_ptr<FaultState> AlarmSystemInterfaceFactory::createFaultState(string family, string member, int code)
{
	auto_ptr<FaultState> fsAutoPtr(new FaultState(family, member, code));
	return fsAutoPtr;
}

/**
 * Create a fault state 
 * @return a new FaultState instance
 */
auto_ptr<FaultState> AlarmSystemInterfaceFactory::createFaultState()
{
	auto_ptr<FaultState> fsAutoPtr(new FaultState());
	return fsAutoPtr;
}
