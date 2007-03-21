#include "FaultState.h"
#include "utilConstants.h"
#include <sstream>

using std::string;
using namespace acsalarm;
using std::stringstream;

/**
 * Default Constructor
 */
FaultState::FaultState()
{
	auto_ptr<Timestamp> tstamp(new Timestamp());
	setUserTimestamp(tstamp);
}

/**
 * Constructor for initializing a fault state with values
 */
FaultState::FaultState(string theFamily, string theMember, int theCode)
{
	setFamily(theFamily);
	setMember(theMember);
	setCode(theCode);
	auto_ptr<Timestamp> tstamp(new Timestamp());
	setUserTimestamp(tstamp);
}

/**
 * Copy constructor.
 */
FaultState::FaultState(const FaultState & fltState)
{
	*this = fltState;
}

/**
 * Destructor
 */
FaultState::~FaultState()
{
}

/*
 * Assignment operator
 */
FaultState & FaultState::operator=(const FaultState & rhs)
{
	setFamily(rhs.getFamily());
	setCode(rhs.getCode());
	setMember(rhs.getMember());
	setDescriptor(rhs.getDescriptor());
	setActivatedByBackup(rhs.getActivatedByBackup());
	setTerminatedByBackup(rhs.getTerminatedByBackup());

	if(NULL != rhs.userTimestamp.get())
	{
		Timestamp * tstampPtr = new Timestamp(*(rhs.userTimestamp));
		auto_ptr<Timestamp> tstampAptr(tstampPtr);
		setUserTimestamp(tstampAptr);
	}

	if(NULL != rhs.userProperties.get())
	{
		Properties * propsPtr = new Properties(*(rhs.userProperties));
		auto_ptr<Properties> propsAptr(propsPtr);
		setUserProperties(propsAptr);
	}

	return *this;
}

/**
 * Returns an XML representation of the fault state. NOTE: this 
 * will not be a complete XML document, but just a fragment.
 *
 * @param amountToIndent the amount (in spaces) to indent for readability
 *
 * For example:
 *
 * <fault-state family="AlarmSource" member="ALARM_SOURCE_ANTENNA" code="1">
 *     <descriptor>TERMINATE</descriptor>
 *     <user-properties>
 *        <property name="ASI_PREFIX" value="prefix"/>
 *        <property name="TEST_PROPERTY" value="TEST_VALUE"/>
 *        <property name="ASI_SUFFIX" value="suffix"/>
 *     </user-properties>
 *     <user-timestamp seconds="1129902763" microseconds="105000"/>
 *  </fault-state>
 */
string FaultState::toXML(int amountToIndent)
{
	string retVal;

	// generate the fault-state opening element
	// e.g. <fault-state family="AlarmSource" member="ALARM_SOURCE_ANTENNA" code="1">
	for(int x = 0; x < amountToIndent; x++)
	{
		retVal += SPACE;
	}
	retVal += LESS_THAN_SIGN;
	retVal += FAULT_STATE_ELEMENT_NAME;
	retVal += SPACE;

	// output the fault's family
	retVal += FAULT_STATE_FAMILY_ATTRIBUTE_NAME;
	retVal += EQUALS_SIGN;
	retVal += DOUBLE_QUOTE;
	retVal += getFamily();
	retVal += DOUBLE_QUOTE;
	retVal += SPACE;

	// output the fault's member
	retVal += FAULT_STATE_MEMBER_ATTRIBUTE_NAME;
	retVal += EQUALS_SIGN;
	retVal += DOUBLE_QUOTE;
	retVal += getMember();
	retVal += DOUBLE_QUOTE;
	retVal += SPACE;

	// output the fault's code
	retVal += FAULT_STATE_CODE_ATTRIBUTE_NAME;
	retVal += EQUALS_SIGN;
	retVal += DOUBLE_QUOTE;
	stringstream strStream;
	strStream << getCode();	
	retVal.append(strStream.str());

	retVal += DOUBLE_QUOTE;
	retVal += GREATER_THAN_SIGN;
	retVal += NEWLINE;

	// indent for readability
	for(int x = 0; x < amountToIndent+3; x++)
	{
		retVal += SPACE;
	}
	
	// generate the descriptor element
	// e.g. <descriptor>TERMINATE</descriptor>
	retVal += LESS_THAN_SIGN;
	retVal += FAULT_STATE_DESCRIPTOR_ELEMENT_NAME;
	retVal += GREATER_THAN_SIGN;
	retVal += getDescriptor();
	retVal += LESS_THAN_SIGN;
	retVal += FORWARD_SLASH;
	retVal += FAULT_STATE_DESCRIPTOR_ELEMENT_NAME;
	retVal += GREATER_THAN_SIGN;
	retVal += NEWLINE;

	// generate the properties element
	// e.g. 
	//
	// <user-properties>
	// 	<property name="ASI_PREFIX" value="prefix"/>
	// 	<property name="TEST_PROPERTY" value="TEST_VALUE"/>
	//		<property name="ASI_SUFFIX" value="suffix"/>
	// </user-properties>

	if(NULL == userProperties.get()) {
		// TODO: throw an exception or log an error
	}
	else {
		retVal += userProperties->toXML(amountToIndent+3);
	}

	// generate the user timestamp element
	// e.g. <user-timestamp seconds="1129902763" microseconds="105000"/>

	if(NULL == userTimestamp.get()) {
		// TODO: throw an exception or log an error
	}
	else {
		retVal += userTimestamp->toXML(USER_TIMESTAMP_ELEMENT_NAME, amountToIndent+3);
	}

	// generate the fault-state closing element
	// e.g. </fault-state>
	for(int x = 0; x < amountToIndent; x++)
	{
		retVal += SPACE;
	}
	retVal += LESS_THAN_SIGN;
	retVal += FORWARD_SLASH;
	retVal += FAULT_STATE_ELEMENT_NAME;
	retVal += GREATER_THAN_SIGN;
	retVal += NEWLINE;

	return retVal;
}

/** 
  * Fault family accessor method.
  * @param faultFamily the fault family.
  */
void  FaultState::setFamily(const string & faultFamily) {
	unsigned int pos;
	do
        {
                pos=faultFamily.find(":");
                if (pos!=string::npos)
                {
                        faultFamily.replace(pos,1,"#");
                }
        }
	while(pos!=string::npos);
	family=faultFamily;
}

/** 
  * Fault member accessor method.
  * @param member the fault member.
*/
void  FaultState::setMember(const string & newFaultMember) {
	unsigned int pos;
	do 
	{
		pos=newFaultMember.find(":");
		if (pos!=string::npos) 
		{
			newFaultMember.replace(pos,1,"#");
		}
	} 
	while(pos!=string::npos);
	member=newFaultMember;
}

