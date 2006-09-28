#include "Properties.h"
#include "utilConstants.h"
#include <logging.h>
#include <string>

using namespace laserUtil;
using std::string;

/*
 * Default no-args constructor.
 */
Properties::Properties()
{
}

/* 
 * Copy constructor
 */
Properties::Properties(const Properties & rhs)
{
	*this = rhs;
}

/*
 * Destructor.
 */
Properties::~Properties()
{
}

/* 
 * Assignment operator
 */
Properties & Properties::operator=(const Properties & rhs)
{
	propertiesMap = rhs.propertiesMap;
	return *this;
}

/*
 * Equality operator.
 */
int Properties::operator==(const Properties &rhs) const
{
	int retVal = 1;
	if(propertiesMap != rhs.propertiesMap)
	{
		retVal = 0;
	}
	return retVal;
}

/*
 * Searches for the property with the specified key in this property list.
 * @key the key to search for in the map.
 */
string Properties::getProperty(string key)
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	if(myLoggerSmartPtr != NULL) {
		myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "Properties::getProperty(): entering");
	}

	string retVal;
	map<string, string >::iterator mapEntry = propertiesMap.find(key);
	if(mapEntry != propertiesMap.end())
	{
		string logStr = "Properties::getProperty(): entry for: " + key + " was found"; 
		if(myLoggerSmartPtr != NULL) {
			myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, logStr); 
		}
		retVal = mapEntry->second;
	}
	else 
	{ 
		string logStr = "Properties::getProperty(): entry for: " + key + " was not found"; 
		if(myLoggerSmartPtr != NULL) {
			myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, logStr); 
		}
	}

	if(myLoggerSmartPtr != NULL) {
		myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "Properties::getProperty(): exiting");
	}
	return retVal;
} 

/*
 * Returns an enumeration of all the keys in this property list, 
 * including distinct keys in the default property list if a key 
 * of the same name has not already been found from the main properties list.
 */
auto_ptr<vector<string> > Properties::propertyNames()
{
	auto_ptr<vector<string> > retVal;
	retVal.reset(new vector<string>);
	map<string, string>::iterator mapIter;
	for(mapIter = propertiesMap.begin(); mapIter != propertiesMap.end(); mapIter++ )
	{
		retVal->push_back(mapIter->first);
	}
	return retVal;
}

/* 
 * Calls the map method insert.
 * @param key the key for the map entry to be inserted
 * @value the value for the map entry to be inserted
 */
void Properties::setProperty(string key, string value) throw(invalid_argument)
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	if(myLoggerSmartPtr != NULL) {
		myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "Properties::setProperty(): entering.");
	}
	if(0 == key.length())
	{
		// NULL passed in for key; not allowed
		throw invalid_argument("zero-length key not allowed");
	}
	string logStr = "Properties::setProperty(): inserting key: " + key + " and value: " + value;
	if(myLoggerSmartPtr != NULL) {
		myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, logStr);
	}
	propertiesMap.insert(PropertyMapEntryType( key, value ) );
	if(myLoggerSmartPtr != NULL) {
		myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "Properties::setProperty(): exiting.");
	}
}

/**
 * Returns an XML fragment (NOT a complete document) representing all of 
 * the properties contained in this table, for use in the message transported
 * from an alarm source to the alarm server.
 * 
 * For example:
 * 
 *  <user-properties>
 *              <property name="ASI_PREFIX" value="prefix"/>
 *              <property name="TEST_PROPERTY" value="TEST_VALUE"/>
 *              <property name="ASI_SUFFIX" value="suffix"/>
 *  </user-properties>
 */
string Properties::toXML(int amountToIndent)
{
	string retString;

	if(0 != propertiesMap.size())
	{
		// Generate the user properties opening element tag
		// e.g. <user-properties>
		for(int x = 0; x < amountToIndent; x++)
		{
			retString += SPACE;
		}
		retString += LESS_THAN_SIGN;
		retString += USER_PROPERTIES_ELEMENT_NAME;
		retString += GREATER_THAN_SIGN;
		retString += NEWLINE;
	
		// For each property, generate the appropriate XML, 
		// e.g. <property name="ASI_PREFIX" value="prefix"/>
		map<string, string>::iterator mapIter;
		for(mapIter = propertiesMap.begin(); mapIter != propertiesMap.end(); mapIter++ )
		{
			for(int x = 0; x < amountToIndent + 3; x++)
			{
				retString += SPACE;
			}
			retString += LESS_THAN_SIGN;
			retString += USER_PROPERTIES_PROPERTY_ELEMENT_NAME;
			retString += SPACE;
			retString += USER_PROPERTIES_NAME_ATTRIBUTE_NAME;
			retString += EQUALS_SIGN;
			retString += DOUBLE_QUOTE;
			retString += mapIter->first;
			retString += DOUBLE_QUOTE;
			retString += SPACE;
			retString += USER_PROPERTIES_VALUE_ATTRIBUTE_NAME;
			retString += EQUALS_SIGN;
			retString += DOUBLE_QUOTE;
			retString += mapIter->second;
			retString += DOUBLE_QUOTE;
			retString += FORWARD_SLASH;
			retString += GREATER_THAN_SIGN;
			retString += NEWLINE;
		}

		// Generate the user properties closing element tag
		// e.g. </user-properties>
		for(int x = 0; x < amountToIndent; x++)
		{
			retString += SPACE;
		}
		retString += LESS_THAN_SIGN;
		retString += FORWARD_SLASH;
		retString += USER_PROPERTIES_ELEMENT_NAME;
		retString += GREATER_THAN_SIGN;
		retString += NEWLINE;
	}

	return retString;
}
