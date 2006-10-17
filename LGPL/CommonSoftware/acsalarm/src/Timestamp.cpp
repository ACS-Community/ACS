#include "Timestamp.h"
#include <sys/time.h>
#include <sstream>
#include <iostream>

using namespace acsalarm;
using std::stringstream;

/**
 * Default no-args constructor, creates an instance with the time at instantiation.
 */
Timestamp::Timestamp()
{
	// TODO later: portability issues with gettimeofday?
	// i.e. will this work on non-unix platforms? do we care?
	timeval tim;
	gettimeofday(&tim, NULL);
	setSeconds(tim.tv_sec);
	setMicroSeconds(tim.tv_usec);
}

/**
 * Constructor to instantiate and configure a Timestamp with a time.
 */
Timestamp::Timestamp(long secs, long microSecs)
{
	setSeconds(secs);
	setMicroSeconds(microSecs);
}

/*
 * Copy constructor
 */
Timestamp::Timestamp(const Timestamp & ts)
{
	seconds = ts.seconds;
	microSeconds = ts.microSeconds;
}

/**
 * Destructor.
 */
Timestamp::~Timestamp()
{
}

/*
 * Assignment operator.
 */
Timestamp & Timestamp::operator=(const Timestamp & rhs)
{
	setSeconds(rhs.seconds);
	setMicroSeconds(rhs.microSeconds);
	return *this;
}

/*
 * Equality operator.
 */
int Timestamp::operator==(const Timestamp &rhs) const
{
	int retVal = 1;
	if(rhs.seconds != seconds || rhs.microSeconds != microSeconds)
	{
		retVal = 0;
	}
	return retVal;
}

/**
 * Returns an XML fragment (NOT a complete document) representing the timestamp, for
 * use in the message that is transported from alarm source to alarm server.
 *
 * @param elementName the element name when generating the XML fragment,
 *        for instance in the example below the elementName is "source-timestamp"
 *
 * For example:
 *
 * <source-timestamp seconds="1129902763" microseconds="132000"/>
 *
 * @param amountToIndent - used to specify a level of indentation (in spaces) for readability
 */
string Timestamp::toXML(string elementName, int amountToIndent)
{
	string retVal;

	for(int x = 0; x < amountToIndent; x++)
	{
		retVal += SPACE;
	}
	retVal += LESS_THAN_SIGN;
	retVal += elementName;
	retVal += SPACE;
	retVal += USER_TIMESTAMP_SECONDS_ATTRIBUTE_NAME;
	retVal += EQUALS_SIGN;
	retVal += DOUBLE_QUOTE;

	// declare a stringstream, then output the seconds
	stringstream strStream;
	strStream << getSeconds();	
	retVal.append(strStream.str());

	retVal += DOUBLE_QUOTE;
	retVal += SPACE;
	retVal += USER_TIMESTAMP_MICROSECONDS_ATTRIBUTE_NAME;
	retVal += EQUALS_SIGN;
	retVal += DOUBLE_QUOTE;

	// reset stream, then output the microseconds
	strStream.str("");
	strStream << getMicroSeconds();	
	retVal.append(strStream.str());

	retVal += DOUBLE_QUOTE;
	retVal += FORWARD_SLASH;
	retVal += GREATER_THAN_SIGN;
	retVal += NEWLINE;
	
	return retVal;
}
