#include "Properties.h"
#include "utilConstants.h"

#include <iostream>
using std::cout;

using namespace laserUtil;

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
	string retVal;
	map<string, string >::iterator mapEntry = propertiesMap.find(key);
	if(mapEntry != propertiesMap.end())
	{
		cout << "found it\n";
		retVal = mapEntry->second;
	}
	else { cout << "entry for: " << key << " was not found\n"; }

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
	if(0 == key.length())
	{
		// TODO later: can a NULL be passed in? if so, check for it and throw exception 
		throw invalid_argument("zero-length key not allowed");
	}
	//cout << "inserting key: " << key << " and value: " << value << std::endl;
	propertiesMap.insert(PropertyMapEntryType( key, value ) );
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
