#include "ACSFaultStateImpl.h"

using namespace laserSource;

laserSource::ACSFaultStateImpl::ACSFaultStateImpl(): ACSFaultState() {}

laserSource::ACSFaultStateImpl::ACSFaultStateImpl(ACSFaultState& fs) {
	setFamily(fs.getFamily());
	setMember(fs.getMember());
	setCode(fs.getCode());
	setDescriptor(fs.getDescriptor());
}

laserSource::ACSFaultStateImpl::ACSFaultStateImpl(string family, string member, int code): ACSFaultState(family,member,code) {}

string laserSource::ACSFaultStateImpl::toXML(int amountToIndent) {
	return "";
}

void laserSource::ACSFaultStateImpl::setUserProperties(auto_ptr<Properties> theProperties) {}

Properties & laserSource::ACSFaultStateImpl::getUserProperties() { return props; }

void laserSource::ACSFaultStateImpl::setUserTimestamp(auto_ptr<Timestamp> theTimestamp) {}

Timestamp & laserSource::ACSFaultStateImpl::getUserTimestamp() { return dummyTimestamp; }

bool laserSource::ACSFaultStateImpl::getActivatedByBackup() {return false; }

void laserSource::ACSFaultStateImpl::setActivatedByBackup(bool newActivatedByBackup) {}

bool laserSource::ACSFaultStateImpl::getTerminatedByBackup() { return true; }

void laserSource::ACSFaultStateImpl::setTerminatedByBackup(bool newTerminatedByBackup) {}
